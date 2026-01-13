// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Weak},
    time::Instant,
};

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{kms::render::gles::GbmGlowBackend, render::element::DamageElement},
    config::ScreenFilter,
    shell::{
        CosmicMappedRenderElement, OverviewMode, SeatExt, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
        element::CosmicMappedKey,
        focus::{FocusTarget, Stage, render_input_order, target::WindowGroup},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        layout::tiling::ANIMATION_DURATION,
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            compositor::FRAME_TIME_FILTER,
            data_device::get_dnd_icon,
            screencopy::{FrameHolder, SessionData, render_session},
        },
        protocols::workspace::WorkspaceHandle,
    },
};

use cosmic::Theme;
use element::FromGlesError;
use smithay::{
    backend::{
        allocator::{Fourcc, dmabuf::Dmabuf},
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            Bind, Blit, Color32F, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, Texture,
            TextureFilter,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                Element, Id, Kind, RenderElement, WeakId,
                surface::{WaylandSurfaceRenderElement, render_elements_from_surface_tree},
                texture::{TextureRenderBuffer, TextureRenderElement},
                utils::{
                    ConstrainAlign, ConstrainScaleBehavior, CropRenderElement, Relocate,
                    RelocateRenderElement, RescaleRenderElement, constrain_render_elements,
                },
            },
            gles::{
                GlesError, GlesPixelProgram, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformName, UniformType,
                element::{PixelShaderElement, TextureShaderElement},
            },
            glow::GlowRenderer,
            multigpu::{Error as MultiError, MultiFrame, MultiRenderer},
            sync::SyncPoint,
        },
    },
    input::Seat,
    output::{Output, OutputModeSource, OutputNoMode},
    utils::{
        IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Size, Time, Transform,
    },
    wayland::{dmabuf::get_dmabuf, session_lock::LockSurface},
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod animations;
pub mod blur;

pub mod cursor;
pub mod element;
use self::element::{AsGlowRenderer, CosmicElement};

use super::kms::Timings;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiFrame<'a, 'frame, 'buffer> =
    MultiFrame<'a, 'a, 'frame, 'buffer, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiError = MultiError<GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;

pub enum RendererRef<'a> {
    Glow(&'a mut GlowRenderer),
    GlMulti(GlMultiRenderer<'a>),
}

impl AsRef<GlowRenderer> for RendererRef<'_> {
    fn as_ref(&self) -> &GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_ref(),
        }
    }
}

impl AsMut<GlowRenderer> for RendererRef<'_> {
    fn as_mut(&mut self) -> &mut GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_mut(),
        }
    }
}

pub static CLEAR_COLOR: Color32F = Color32F::new(0.153, 0.161, 0.165, 1.0);
pub static OUTLINE_SHADER: &str = include_str!("./shaders/rounded_outline.frag");
pub static RECTANGLE_SHADER: &str = include_str!("./shaders/rounded_rectangle.frag");
pub static POSTPROCESS_SHADER: &str = include_str!("./shaders/offscreen.frag");
pub static BLUR_SHADER: &str = include_str!("./shaders/blur.frag");
pub static SHADOW_SHADER: &str = include_str!("./shaders/shadow.frag");
pub static BLURRED_BACKDROP_SHADER: &str = include_str!("./shaders/blurred_backdrop.frag");
pub static GROUP_COLOR: [f32; 3] = [0.788, 0.788, 0.788];
pub static ACTIVE_GROUP_COLOR: [f32; 3] = [0.58, 0.922, 0.922];

/// Default blur radius in pixels (design spec: blur(50px))
/// CSS blur(50px) uses a Gaussian blur. Kawase blur approximates this
/// with multiple iterations. A higher value creates more spread.
pub const DEFAULT_BLUR_RADIUS: f32 = 50.0;
/// Number of blur iterations for stronger effect
/// More iterations with smaller offsets = smoother blur without tiling artifacts
/// 8 iterations provides smooth CSS blur(50px) equivalent
pub const BLUR_ITERATIONS: u32 = 8;

use once_cell::sync::Lazy;
/// Global cache for blurred textures per window
///
/// This cache allows the blur render path to store blurred textures
/// that can be read by the floating layout renderer.
/// Key is (output_name, window_key_hash), value is the blurred texture.
/// Each blur window gets its own texture capturing everything behind it.
use std::sync::RwLock;

/// Blurred texture data for a window
#[derive(Debug, Clone)]
pub struct BlurredTextureInfo {
    /// The blurred texture buffer
    pub texture: TextureRenderBuffer<GlesTexture>,
    /// Size of the texture
    pub size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
}

/// Cache key combining output name and window key hash
type BlurCacheKey = (String, u64);

/// Global cache of blurred textures per window per output
/// For iterative rendering: each blur window has its own cached texture
/// capturing everything behind it (including other blurred windows below)
pub static BLUR_TEXTURE_CACHE: Lazy<RwLock<HashMap<BlurCacheKey, BlurredTextureInfo>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Legacy: Global cache for single blur texture per output (fallback)
pub static BLUR_TEXTURE_CACHE_LEGACY: Lazy<RwLock<HashMap<String, BlurredTextureInfo>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag to skip blur backdrops during background capture
/// When true, floating_layout.render() should skip adding blur backdrop elements
pub static SKIP_BLUR_BACKDROPS: AtomicBool = AtomicBool::new(false);

/// Check if blur backdrops should be skipped
pub fn should_skip_blur_backdrops() -> bool {
    SKIP_BLUR_BACKDROPS.load(Ordering::Relaxed)
}

/// Set whether blur backdrops should be skipped
pub fn set_skip_blur_backdrops(skip: bool) {
    SKIP_BLUR_BACKDROPS.store(skip, Ordering::Relaxed);
}

use crate::shell::element::CosmicMapped;

thread_local! {
    /// Z-index threshold for excluding windows during blur capture
    /// Windows at or above this global z-index will be excluded
    static EXCLUDE_Z_THRESHOLD: RefCell<Option<usize>> = const { RefCell::new(None) };
}

/// Set the z-index threshold for excluding windows during blur capture
/// Windows at this index or higher (further up in the stack) will be excluded
pub fn set_exclude_z_threshold(threshold: usize) {
    EXCLUDE_Z_THRESHOLD.with(|t| {
        *t.borrow_mut() = Some(threshold);
    });
}

/// Clear the z-index exclusion threshold
pub fn clear_exclude_z_threshold() {
    EXCLUDE_Z_THRESHOLD.with(|t| {
        *t.borrow_mut() = None;
    });
}

/// Check if a window at the given z-index should be excluded
pub fn is_z_index_excluded(z_idx: usize) -> bool {
    EXCLUDE_Z_THRESHOLD.with(|t| {
        t.borrow()
            .map(|threshold| z_idx >= threshold)
            .unwrap_or(false)
    })
}

thread_local! {
    /// Window currently being grabbed/moved - should be excluded from blur capture
    static GRABBED_WINDOW: RefCell<Option<CosmicMapped>> = const { RefCell::new(None) };
}

/// Set the currently grabbed window (for blur exclusion)
pub fn set_grabbed_window(window: Option<CosmicMapped>) {
    GRABBED_WINDOW.with(|w| {
        if window.is_some() {
            tracing::debug!("Setting grabbed window for blur exclusion");
        }
        *w.borrow_mut() = window;
    });
}

/// Check if a window is currently being grabbed/moved
pub fn is_window_grabbed(window: &CosmicMapped) -> bool {
    GRABBED_WINDOW.with(|w| {
        let borrowed = w.borrow();
        if let Some(ref grabbed) = *borrowed {
            // Log the comparison to debug why it might not match
            let matched = grabbed == window;
            if !matched {
                // Debug: Log the window class names to see if they're the same window
                let grabbed_class = grabbed.active_window().app_id();
                let window_class = window.active_window().app_id();
                tracing::debug!(
                    grabbed_class = %grabbed_class,
                    window_class = %window_class,
                    "Grabbed window comparison - not matching"
                );
            } else {
                tracing::debug!("Window is grabbed - excluding from blur capture");
            }
            matched
        } else {
            false
        }
    })
}

/// Compute a hash for a window key (for cache lookup)
fn window_key_hash(key: &CosmicMappedKey) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

/// Store a blurred texture for a specific window on an output
/// This is used for iterative multi-pass blur where each blur window
/// gets its own texture capturing everything behind it.
pub fn cache_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
    info: BlurredTextureInfo,
) {
    let key = (output_name.to_string(), window_key_hash(window_key));
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific window on an output
pub fn get_cached_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), window_key_hash(window_key));
    if let Ok(cache) = BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all blur textures for an output (e.g., on window close)
pub fn clear_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

/// Store a blurred texture for an output (legacy single-pass fallback)
pub fn cache_blur_texture(output_name: &str, info: BlurredTextureInfo) {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE_LEGACY.write() {
        cache.insert(output_name.to_string(), info);
    }
}

/// Get the cached blurred texture for an output (legacy single-pass fallback)
pub fn get_cached_blur_texture(output_name: &str) -> Option<BlurredTextureInfo> {
    if let Ok(cache) = BLUR_TEXTURE_CACHE_LEGACY.read() {
        cache.get(output_name).cloned()
    } else {
        None
    }
}

/// Clear the blur texture cache for an output
#[allow(dead_code)]
pub fn clear_blur_texture(output_name: &str) {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE_LEGACY.write() {
        cache.remove(output_name);
    }
}

/// State for managing blur effect rendering per-output
///
/// Blur requires rendering background content to an offscreen texture,
/// applying a two-pass gaussian blur (horizontal + vertical), then
/// compositing the result behind the target surface.
///
/// This implementation uses a "previous frame" approach where the blur
/// source is the previous frame's content, avoiding the need to render
/// the scene twice per frame.
#[derive(Debug)]
pub struct BlurRenderState {
    /// Texture containing the previous frame's content (used as blur source)
    pub background_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for blur passes (ping)
    pub texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for blur passes (pong)
    pub texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Damage tracker for the blur textures
    pub damage_tracker: Option<OutputDamageTracker>,
    /// Size of the blur textures
    pub texture_size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
    /// Whether blur has been applied this frame
    pub blur_applied: bool,
}

impl Default for BlurRenderState {
    fn default() -> Self {
        Self {
            background_texture: None,
            texture_a: None,
            texture_b: None,
            damage_tracker: None,
            texture_size: Size::from((0, 0)),
            scale: Scale::from(1.0),
            blur_applied: false,
        }
    }
}

impl BlurRenderState {
    /// Create or resize blur textures if needed
    pub fn ensure_textures<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
        // Only recreate if size changed
        if self.texture_size == size
            && self.texture_a.is_some()
            && self.texture_b.is_some()
            && self.background_texture.is_some()
        {
            return Ok(());
        }

        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);

        // Create background texture (stores previous frame for blur source)
        let bg_tex = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        self.background_texture = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            bg_tex,
            1,
            Transform::Normal,
            None,
        ));

        // Create texture A (ping)
        let tex_a = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        self.texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create texture B (pong)
        let tex_b = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        self.texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create damage tracker
        self.damage_tracker = Some(OutputDamageTracker::new(size, scale, Transform::Normal));

        self.texture_size = size;
        self.scale = scale;
        self.blur_applied = false;
        Ok(())
    }

    /// Get background texture (previous frame content)
    pub fn background_texture(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.background_texture.as_ref()
    }

    /// Get texture A for rendering
    pub fn texture_a(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_a.as_ref()
    }

    /// Get texture B for rendering
    pub fn texture_b(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_b.as_ref()
    }

    /// Check if blur state is ready for rendering
    pub fn is_ready(&self) -> bool {
        self.background_texture.is_some() && self.texture_a.is_some() && self.texture_b.is_some()
    }

    /// Create blurred texture elements for the specified blur regions
    ///
    /// This method returns texture elements for blur regions.
    /// The caller should:
    /// 1. First render the scene to background_texture
    /// 2. Call apply_blur_passes() to blur into texture_b
    /// 3. Then call create_blur_elements() to get blurred texture elements
    /// 4. Render those elements at blur window locations
    ///
    /// This uses texture_b as the source since that's where the blurred result is stored
    /// after apply_blur_passes().
    pub fn create_blur_elements(
        &self,
        blur_regions: &[BlurRegionInfo],
    ) -> Vec<TextureRenderElement<GlesTexture>> {
        if !self.is_ready() || blur_regions.is_empty() {
            tracing::warn!("create_blur_elements: not ready or no regions");
            return Vec::new();
        }

        let tex_size = self.texture_size;
        let mut elements = Vec::new();

        // Use texture_b as the source - it contains the blurred result after apply_blur_passes()
        // Falls back to background_texture if blur hasn't been applied
        let source_texture = if self.blur_applied {
            tracing::info!("Using texture_b (blurred result)");
            self.texture_b.as_ref()
        } else {
            tracing::info!("Using background_texture (fallback, blur not applied)");
            self.background_texture.as_ref()
        };

        let Some(blurred_texture) = source_texture else {
            tracing::warn!("No source texture available");
            return elements;
        };

        for region in blur_regions {
            let phys_geo: Rectangle<i32, Physical> = region
                .geometry
                .as_logical()
                .to_physical_precise_round(self.scale.x);

            // Calculate source region in the texture (normalized to texture size)
            let src_rect = Rectangle::<f64, Logical>::new(
                (
                    phys_geo.loc.x as f64 / tex_size.w as f64,
                    phys_geo.loc.y as f64 / tex_size.h as f64,
                )
                    .into(),
                (
                    phys_geo.size.w as f64 / tex_size.w as f64,
                    phys_geo.size.h as f64 / tex_size.h as f64,
                )
                    .into(),
            );

            tracing::info!(
                phys_geo_x = phys_geo.loc.x,
                phys_geo_y = phys_geo.loc.y,
                phys_geo_w = phys_geo.size.w,
                phys_geo_h = phys_geo.size.h,
                src_rect_x = src_rect.loc.x,
                src_rect_y = src_rect.loc.y,
                src_rect_w = src_rect.size.w,
                src_rect_h = src_rect.size.h,
                tex_size_w = tex_size.w,
                tex_size_h = tex_size.h,
                alpha = region.alpha,
                "Creating blur element"
            );

            let elem = TextureRenderElement::from_texture_render_buffer(
                Point::<f64, Physical>::from((phys_geo.loc.x as f64, phys_geo.loc.y as f64)),
                blurred_texture,
                Some(region.alpha),
                Some(src_rect),
                None,
                Kind::Unspecified,
            );

            elements.push(elem);
        }

        elements
    }
}

/// Apply two-pass Gaussian blur using ping-pong rendering
///
/// - `renderer`: The renderer to use
/// - `src_texture`: Source texture to blur (will not be modified)
/// - `ping_texture`: First intermediate texture
/// - `pong_texture`: Second intermediate texture (will contain final result)
/// - `tex_size`: Size of all textures
/// - `scale`: Scale factor for damage tracking
/// - `iterations`: Number of blur iterations
///
/// After this function, `pong_texture` contains the blurred result.
#[allow(dead_code)]
pub fn apply_blur_passes<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    ping_texture: &mut TextureRenderBuffer<GlesTexture>,
    pong_texture: &mut TextureRenderBuffer<GlesTexture>,
    tex_size: Size<i32, Physical>,
    scale: Scale<f64>,
    iterations: u32,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let _span = tracing::info_span!(
        "blur_passes",
        iterations = iterations,
        tex_w = tex_size.w,
        tex_h = tex_size.h,
    )
    .entered();

    let blur_shader = BlurShader::get(renderer);
    let blur_radius = DEFAULT_BLUR_RADIUS;

    // First pass: src -> ping (horizontal blur)
    apply_single_blur_pass(
        renderer,
        src_texture,
        ping_texture,
        &blur_shader,
        blur_radius,
        tex_size,
        scale,
        true,
    )?;

    // Second pass: ping -> pong (vertical blur)
    apply_single_blur_pass(
        renderer,
        ping_texture,
        pong_texture,
        &blur_shader,
        blur_radius,
        tex_size,
        scale,
        false,
    )?;

    // Additional iterations for smoother blur
    for _ in 1..iterations {
        // Horizontal: pong -> ping
        apply_single_blur_pass(
            renderer,
            pong_texture,
            ping_texture,
            &blur_shader,
            blur_radius,
            tex_size,
            scale,
            true,
        )?;

        // Vertical: ping -> pong
        apply_single_blur_pass(
            renderer,
            ping_texture,
            pong_texture,
            &blur_shader,
            blur_radius,
            tex_size,
            scale,
            false,
        )?;
    }

    tracing::info!(
        iterations = iterations,
        tex_w = tex_size.w,
        tex_h = tex_size.h,
        blur_radius = blur_radius,
        "Blur passes complete"
    );

    Ok(())
}

/// Apply a single blur pass from src to dst
#[allow(dead_code)]
fn apply_single_blur_pass<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    dst_texture: &mut TextureRenderBuffer<GlesTexture>,
    blur_shader: &GlesTexProgram,
    blur_radius: f32,
    tex_size: Size<i32, Physical>,
    _scale: Scale<f64>,
    horizontal: bool,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    tracing::debug!(
        horizontal = horizontal,
        blur_radius = blur_radius,
        tex_size_w = tex_size.w,
        tex_size_h = tex_size.h,
        "Applying single blur pass"
    );

    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        src_texture,
        Some(1.0),
        None,
        None,
        Kind::Unspecified,
    );

    let blur_elem = TextureShaderElement::new(
        src_elem,
        blur_shader.clone(),
        vec![
            Uniform::new("tex_size", [tex_size.w as f32, tex_size.h as f32]),
            Uniform::new("blur_radius", blur_radius),
            Uniform::new("direction", if horizontal { 0.0 } else { 1.0 }),
        ],
    );

    dst_texture.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        // Get a frame to render with
        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, tex_size, Transform::Normal)?;

        // Clear the framebuffer first
        use smithay::backend::renderer::Color32F;
        use smithay::backend::renderer::Frame;
        let clear_damage = [Rectangle::from_size(tex_size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Render the blur element using the RenderElement trait
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;
        use smithay::utils::Buffer as BufferCoords;

        // Source: full texture in buffer coordinates
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((tex_size.w as f64, tex_size.h as f64).into());
        // Destination: full output in physical coordinates
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(tex_size);
        let damage = [dst];

        <TextureShaderElement as RenderElement<GlowRenderer>>::draw(
            &blur_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
        )?;

        // Finish the frame
        drop(frame);

        tracing::debug!(horizontal = horizontal, "Blur pass render completed");

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = tex_size.to_logical(1);
        let damage_rect = Rectangle::from_size(tex_size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(())
}

/// Shader for applying blur effects to surfaces
pub struct BlurShader(pub GlesTexProgram);

/// Shader for rendering blurred backdrop (samples from pre-blurred texture)
pub struct BlurredBackdropShader(pub GlesTexProgram);

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Usage {
    OverviewBackdrop,
    Overlay,
    MoveGrabIndicator,
    FocusIndicator,
    PotentialGroupIndicator,
    SnappingIndicator,
}

#[derive(Clone)]
pub enum Key {
    Static(WeakId),
    Group(Weak<()>),
    Window(Usage, CosmicMappedKey),
}
impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Key::Static(id) => id.hash(state),
            Key::Group(arc) => (arc.as_ptr() as usize).hash(state),
            Key::Window(usage, window) => {
                usage.hash(state);
                window.hash(state);
            }
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Static(s1), Key::Static(s2)) => s1 == s2,
            (Key::Group(g1), Key::Group(g2)) => Weak::ptr_eq(g1, g2),
            (Key::Window(u1, w1), Key::Window(u2, w2)) => u1 == u2 && w1 == w2,
            _ => false,
        }
    }
}
impl Eq for Key {}
impl From<WindowGroup> for Key {
    fn from(group: WindowGroup) -> Self {
        Key::Group(group.alive.clone())
    }
}
impl From<Id> for Key {
    fn from(id: Id) -> Self {
        Key::Static(id.downgrade())
    }
}

#[derive(PartialEq)]
struct IndicatorSettings {
    thickness: u8,
    radius: [u8; 4],
    alpha: f32,
    color: [f32; 3],
}
type IndicatorCache = RefCell<HashMap<Key, (IndicatorSettings, PixelShaderElement)>>;

impl IndicatorShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<IndicatorShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn focus_element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        mut element_geo: Rectangle<i32, Local>,
        thickness: u8,
        radius: [u8; 4],
        alpha: f32,
        active_window_hint: [f32; 3],
    ) -> PixelShaderElement {
        let t = thickness as i32;
        element_geo.loc -= (t, t).into();
        element_geo.size += (t * 2, t * 2).into();

        IndicatorShader::element(
            renderer,
            key,
            element_geo,
            thickness,
            radius,
            alpha,
            active_window_hint,
        )
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        thickness: u8,
        radius: [u8; 4],
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = IndicatorSettings {
            thickness,
            radius,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| IndicatorCache::new(HashMap::new()));
        let mut cache = user_data.get::<IndicatorCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(w) => w.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let thickness: f32 = thickness as f32;
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, //TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("thickness", thickness),
                    Uniform::new(
                        "radius",
                        [
                            radius[0] as f32 + thickness / 2.,
                            radius[1] as f32 + thickness / 2.,
                            radius[2] as f32 + thickness / 2.,
                            radius[3] as f32 + thickness / 2.,
                        ],
                    ),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct BackdropShader(pub GlesPixelProgram);

#[derive(PartialEq)]
struct BackdropSettings {
    radius: f32,
    alpha: f32,
    color: [f32; 3],
}
type BackdropCache = RefCell<HashMap<Key, (BackdropSettings, PixelShaderElement)>>;

impl BackdropShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BackdropShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        radius: f32,
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = BackdropSettings {
            radius,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| BackdropCache::new(HashMap::new()));
        let mut cache = user_data.get::<BackdropCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(a) => a.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, // TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("radius", radius),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct ShadowShader(pub GlesPixelProgram);

#[derive(PartialEq)]
struct ShadowSettings {
    shadow_radius: f32,
    shadow_blur: f32,
    shadow_offset: [f32; 2],
    alpha: f32,
    color: [f32; 3],
}
type ShadowCache = RefCell<HashMap<Key, (ShadowSettings, PixelShaderElement)>>;

impl ShadowShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<ShadowShader>()
            .expect("ShadowShader not initialized")
            .0
            .clone()
    }

    /// Create a shadow element for blur windows
    ///
    /// - `geo`: The geometry of the shadow element (should be larger than the window by `blur` on each side)
    /// - `corner_radius`: Corner radius of the window
    /// - `blur`: Shadow blur spread in pixels
    /// - `offset`: Shadow offset (x, y)
    /// - `alpha`: Overall alpha
    /// - `color`: Shadow color RGB
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        corner_radius: f32,
        blur: f32,
        offset: [f32; 2],
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = ShadowSettings {
            shadow_radius: corner_radius,
            shadow_blur: blur,
            shadow_offset: offset,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| ShadowCache::new(HashMap::new()));
        let mut cache = user_data.get::<ShadowCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(a) => a.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None,
                alpha,
                vec![
                    Uniform::new("shadow_color", color),
                    Uniform::new("shadow_radius", corner_radius),
                    Uniform::new("shadow_blur", blur),
                    Uniform::new("shadow_offset", offset),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct PostprocessShader(pub GlesTexProgram);

impl BlurShader {
    /// Get the blur shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    /// Create a horizontal blur pass element from a texture
    ///
    /// The blur shader requires two passes (horizontal + vertical) for proper gaussian blur.
    /// This creates the horizontal pass. The output should be rendered to an intermediate
    /// texture, then `element_vertical` should be called on that result.
    pub fn element_horizontal<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", 0.0), // 0.0 = horizontal
            ],
        )
    }

    /// Create a vertical blur pass element from a texture
    ///
    /// This should be called on the output of `element_horizontal` for proper
    /// two-pass gaussian blur.
    pub fn element_vertical<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", 1.0), // 1.0 = vertical
            ],
        )
    }

    /// Create a single-pass blur element (simplified blur for performance)
    ///
    /// For a proper gaussian blur, two passes (horizontal + vertical) are needed.
    /// This creates a single-pass blur which is faster but lower quality.
    /// Use `element_horizontal` followed by `element_vertical` for better quality.
    pub fn element_single_pass<R: AsGlowRenderer>(
        renderer: &R,
        texture_elem: TextureRenderElement<GlesTexture>,
        blur_radius: f32,
        horizontal: bool,
    ) -> TextureShaderElement {
        let shader = Self::get(renderer);
        let geo = texture_elem.geometry(1.0.into());

        TextureShaderElement::new(
            texture_elem,
            shader,
            vec![
                Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
                Uniform::new("blur_radius", blur_radius),
                Uniform::new("direction", if horizontal { 0.0 } else { 1.0 }),
            ],
        )
    }
}

impl BlurredBackdropShader {
    /// Get the blurred backdrop shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurredBackdropShader>()
            .expect("BlurredBackdropShader not initialized")
            .0
            .clone()
    }

    /// Create a blurred backdrop element from a pre-blurred texture
    ///
    /// This crops the correct region from the blurred background texture,
    /// applies a tint overlay, and masks with rounded corners.
    ///
    /// # Arguments
    /// * `renderer` - The renderer to use
    /// * `blurred_texture` - The pre-blurred background texture (full screen)
    /// * `element_geo` - The geometry of the element (where the backdrop appears)
    /// * `screen_size` - The full screen size for coordinate mapping
    /// * `scale` - Output scale factor for coordinate conversion
    /// * `transform` - Output transform for coordinate conversion
    /// * `corner_radius` - Corner radius for rounded rectangle mask
    /// * `alpha` - Overall opacity
    /// * `tint_color` - Tint overlay color
    /// * `tint_strength` - How much tint to apply (0.0 = none, 1.0 = full)
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        blurred_texture: &TextureRenderBuffer<GlesTexture>,
        element_geo: Rectangle<i32, Local>,
        screen_size: Size<i32, Physical>,
        scale: f64,
        transform: Transform,
        corner_radius: f32,
        alpha: f32,
        tint_color: [f32; 3],
        tint_strength: f32,
    ) -> TextureShaderElement {
        use crate::utils::geometry::RectLocalExt;

        let shader = Self::get(renderer);

        // Convert from Local to Logical, then to Physical coordinates
        // The blur texture is captured in physical pixels, so we need physical coordinates
        // for the src_rect to crop the correct region
        let phys_geo: Rectangle<i32, Physical> =
            element_geo.as_logical().to_physical_precise_round(scale);

        // Position the element at the physical location (scaled from logical)
        // This is critical for HiDPI: element_geo is in logical coords, but rendering
        // happens in physical coords
        let location: Point<f64, Physical> = (phys_geo.loc.x as f64, phys_geo.loc.y as f64).into();

        // If transform is not Normal, we need to transform the coordinates
        // Transform affects how coordinates map to the buffer
        let transformed_phys_geo = if transform != Transform::Normal {
            // Transform the location within the screen bounds
            let transformed_loc = transform.transform_point_in(phys_geo.loc, &screen_size);
            let transformed_size = transform.transform_size(phys_geo.size);
            Rectangle::new(transformed_loc, transformed_size)
        } else {
            phys_geo
        };

        // Calculate src_rect to crop the correct region from the blur texture
        // The blur texture is in physical pixel coordinates, so src_rect must be too.
        // TextureRenderElement interprets src_rect as being in the texture's native
        // coordinate space (which for our blur texture is physical pixels).
        let src_rect = Rectangle::<f64, Logical>::new(
            (
                transformed_phys_geo.loc.x as f64,
                transformed_phys_geo.loc.y as f64,
            )
                .into(),
            (
                transformed_phys_geo.size.w as f64,
                transformed_phys_geo.size.h as f64,
            )
                .into(),
        );

        // Output size should be in logical coordinates - this is the size the element
        // appears on screen. The src_rect crops from physical pixels, but output_size
        // specifies the logical output dimensions.
        let output_size = Size::<i32, Logical>::from((element_geo.size.w, element_geo.size.h));

        let source_elem = TextureRenderElement::from_texture_render_buffer(
            location,
            blurred_texture,
            Some(alpha),
            Some(src_rect),
            Some(output_size),
            Kind::Unspecified,
        );

        TextureShaderElement::new(
            source_elem,
            shader,
            vec![
                Uniform::new("alpha", alpha),
                // Use physical size for shader calculations (corner radius, etc.)
                // The shader operates in physical pixel space
                Uniform::new("size", [phys_geo.size.w as f32, phys_geo.size.h as f32]),
                Uniform::new("screen_size", [screen_size.w as f32, screen_size.h as f32]),
                // Use physical position for shader calculations
                Uniform::new(
                    "element_pos",
                    [phys_geo.loc.x as f32, phys_geo.loc.y as f32],
                ),
                // Scale corner radius to physical pixels
                Uniform::new("corner_radius", corner_radius * scale as f32),
                Uniform::new("tint_color", tint_color),
                Uniform::new("tint_strength", tint_strength),
            ],
        )
    }
}

pub fn init_shaders(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
    {
        let egl_context = renderer.egl_context();
        if egl_context.user_data().get::<IndicatorShader>().is_some()
            && egl_context.user_data().get::<BackdropShader>().is_some()
            && egl_context.user_data().get::<PostprocessShader>().is_some()
            && egl_context.user_data().get::<BlurShader>().is_some()
            && egl_context
                .user_data()
                .get::<BlurredBackdropShader>()
                .is_some()
        {
            return Ok(());
        }
    }

    let outline_shader = renderer.compile_custom_pixel_shader(
        OUTLINE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("radius", UniformType::_4f),
        ],
    )?;
    let rectangle_shader = renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;
    let postprocess_shader = renderer.compile_custom_texture_shader(
        POSTPROCESS_SHADER,
        &[
            UniformName::new("invert", UniformType::_1f),
            UniformName::new("color_mode", UniformType::_1f),
        ],
    )?;
    let blur_shader = renderer.compile_custom_texture_shader(
        BLUR_SHADER,
        &[
            UniformName::new("tex_size", UniformType::_2f),
            UniformName::new("blur_radius", UniformType::_1f),
            UniformName::new("direction", UniformType::_1f),
        ],
    )?;
    let shadow_shader = renderer.compile_custom_pixel_shader(
        SHADOW_SHADER,
        &[
            UniformName::new("shadow_color", UniformType::_3f),
            UniformName::new("shadow_radius", UniformType::_1f),
            UniformName::new("shadow_blur", UniformType::_1f),
            UniformName::new("shadow_offset", UniformType::_2f),
        ],
    )?;
    let blurred_backdrop_shader = renderer.compile_custom_texture_shader(
        BLURRED_BACKDROP_SHADER,
        &[
            UniformName::new("alpha", UniformType::_1f),
            UniformName::new("size", UniformType::_2f),
            UniformName::new("screen_size", UniformType::_2f),
            UniformName::new("element_pos", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_1f),
            UniformName::new("tint_color", UniformType::_3f),
            UniformName::new("tint_strength", UniformType::_1f),
        ],
    )?;

    let egl_context = renderer.egl_context();
    egl_context
        .user_data()
        .insert_if_missing(|| IndicatorShader(outline_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BackdropShader(rectangle_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| PostprocessShader(postprocess_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BlurShader(blur_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| ShadowShader(shadow_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BlurredBackdropShader(blurred_backdrop_shader));

    Ok(())
}

/// Information about a blur region for rendering
#[derive(Debug, Clone)]
pub struct BlurRegionInfo {
    /// The geometry of the blur region in local coordinates
    pub geometry: Rectangle<i32, Local>,
    /// Corner radius for the blur region
    pub corner_radius: f32,
    /// Alpha/opacity of the blur
    pub alpha: f32,
}

/// Create a blurred texture element from the background
///
/// This is a simplified blur that uses the blur shader directly on the source texture.
/// For a full implementation, this would need multi-pass rendering.
#[profiling::function]
pub fn create_blur_element<R>(
    renderer: &R,
    source_texture: &TextureRenderBuffer<GlesTexture>,
    blur_region: &BlurRegionInfo,
    output_scale: f64,
) -> TextureShaderElement
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let blur_shader = BlurShader::get(renderer);
    let blur_radius = DEFAULT_BLUR_RADIUS;

    // Convert geometry to physical coordinates
    let phys_geo: Rectangle<i32, Physical> = blur_region
        .geometry
        .as_logical()
        .to_physical_precise_round(output_scale);

    // Create source element from the background texture
    // We'll sample from the region corresponding to where the blur window is
    let location: Point<f64, Physical> = (phys_geo.loc.x as f64, phys_geo.loc.y as f64).into();
    let source_elem = TextureRenderElement::from_texture_render_buffer(
        location,
        source_texture,
        Some(blur_region.alpha),
        None, // Sample whole texture, positioning handles region
        None,
        Kind::Unspecified,
    );

    let geo = source_elem.geometry(1.0.into());

    // Apply blur shader (single pass - for full blur would need two passes)
    // Using horizontal direction - for better results, would need ping-pong passes
    TextureShaderElement::new(
        source_elem,
        blur_shader,
        vec![
            Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
            Uniform::new("blur_radius", blur_radius),
            Uniform::new("direction", 0.5), // Diagonal blur as compromise
        ],
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    None,
    NotDefault,
    All,
}

#[profiling::function]
pub fn cursor_elements<'a, 'frame, R>(
    renderer: &mut R,
    seats: impl Iterator<Item = &'a Seat<State>>,
    zoom_state: Option<&ZoomState>,
    theme: &Theme,
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
) -> Vec<CosmicElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    let scale = output.current_scale().fractional_scale();
    let (focal_point, zoom_scale) = zoom_state
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));
    let mut elements = Vec::new();

    for seat in seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = pointer.current_location() - output.current_location().to_f64();

        if mode != CursorMode::None {
            elements.extend(
                cursor::draw_cursor(
                    renderer,
                    seat,
                    location,
                    scale.into(),
                    zoom_scale,
                    now,
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(|(elem, hotspot)| {
                    CosmicElement::Cursor(RescaleRenderElement::from_element(
                        RelocateRenderElement::from_element(
                            elem,
                            Point::from((-hotspot.x, -hotspot.y)),
                            Relocate::Relative,
                        ),
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    ))
                }),
            );
        }

        if !exclude_dnd_icon {
            if let Some(dnd_icon) = get_dnd_icon(seat) {
                elements.extend(
                    cursor::draw_dnd_icon(
                        renderer,
                        &dnd_icon.surface,
                        (location + dnd_icon.offset.to_f64()).to_i32_round(),
                        scale,
                    )
                    .into_iter()
                    .map(CosmicElement::Dnd),
                );
            }
        }

        let theme = theme.cosmic();
        // Skip move grab render when capturing for blur - the grabbed window
        // is always on top and shouldn't be included in the blur source
        if !should_skip_blur_backdrops() {
            if let Some(grab_elements) = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|state| {
                    state.render::<CosmicMappedRenderElement<R>, R>(renderer, output, theme)
                })
            {
                elements.extend(grab_elements.into_iter().map(|elem| {
                    CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                        elem,
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    ))
                }));
            }
        }

        if let Some((grab_elements, should_scale)) = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
            .map(|state| {
                (
                    state.render::<CosmicMappedRenderElement<R>, R>(renderer, output),
                    !state.is_in_screen_space(),
                )
            })
        {
            elements.extend(grab_elements.into_iter().map(|elem| {
                CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                    elem,
                    if should_scale {
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round()
                    } else {
                        Point::from((0, 0))
                    },
                    if should_scale { zoom_scale } else { 1.0 },
                ))
            }));
        }
    }

    elements
}

#[cfg(not(feature = "debug"))]
pub type EguiState = ();

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElementFilter {
    All,
    ExcludeWorkspaceOverview,
    LayerShellOnly,
    /// Exclude windows with blur effect (for capturing background to blur)
    ExcludeBlurWindows,
}

pub fn output_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    _fps: Option<(&EguiState, &Timings)>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    #[cfg(feature = "debug")]
    let mut debug_elements = {
        let output_geo = output.geometry();
        let shell_guard = shell.read();
        let seats = shell_guard.seats.iter().cloned().collect::<Vec<_>>();
        let debug_active = shell_guard.debug_active;
        std::mem::drop(shell_guard);
        let scale = output.current_scale().fractional_scale();

        if let Some((state, timings)) = _fps {
            vec![
                fps_ui(
                    _gpu,
                    debug_active,
                    &seats,
                    renderer.glow_renderer_mut(),
                    state,
                    timings,
                    Rectangle::from_size(
                        (output_geo.size.w.min(400), output_geo.size.h.min(800)).into(),
                    ),
                    scale,
                )
                .map_err(FromGlesError::from_gles_error)
                .map_err(RenderError::Rendering)?
                .into(),
            ]
        } else {
            Vec::new()
        }
    };

    let shell_guard = shell.read();
    let Some((previous_workspace, workspace)) = shell_guard.workspaces.active(output) else {
        #[cfg(not(feature = "debug"))]
        return Ok(Vec::new());
        #[cfg(feature = "debug")]
        return Ok(debug_elements);
    };

    let (previous_idx, idx) = shell_guard.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);

    std::mem::drop(shell_guard);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };
    let zoom_state = shell.read().zoom_state().cloned();

    #[allow(unused_mut)]
    let workspace_elements = workspace_elements(
        _gpu,
        renderer,
        shell,
        zoom_state.as_ref(),
        now,
        output,
        previous_workspace,
        workspace,
        cursor_mode,
        element_filter,
    )?;

    #[cfg(feature = "debug")]
    {
        debug_elements.extend(workspace_elements);
        Ok(debug_elements)
    }
    #[cfg(not(feature = "debug"))]
    Ok(workspace_elements)
}

#[profiling::function]
pub fn workspace_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements = Vec::new();

    let shell_ref = shell.read();
    let seats = shell_ref.seats.iter().cloned().collect::<Vec<_>>();
    if seats.is_empty() {
        return Ok(Vec::new());
    }
    let theme = shell_ref.theme().clone();
    let scale = output.current_scale().fractional_scale();
    // we don't want to hold a shell lock across `cursor_elements`,
    // that is prone to deadlock with the main-thread on some grabs.
    std::mem::drop(shell_ref);

    elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        zoom_level,
        &theme,
        now,
        output,
        cursor_mode,
        element_filter == ElementFilter::ExcludeWorkspaceOverview,
    ));

    let shell = shell.read();
    let overview = shell.overview_mode();
    let (resize_mode, resize_indicator) = shell.resize_mode();
    let resize_indicator = resize_indicator.map(|indicator| (resize_mode, indicator));
    let swap_tree = if let Some(Trigger::KeyboardSwap(_, desc)) = overview.0.active_trigger() {
        if current.0 != desc.handle {
            shell
                .workspaces
                .space_for_handle(&desc.handle)
                .map(|w| w.tiling_layer.tree())
        } else {
            None
        }
    } else {
        None
    };
    let overview = (
        overview.0,
        overview.1.map(|indicator| (indicator, swap_tree)),
    );
    let last_active_seat = shell.seats.last_active();
    let move_active = last_active_seat
        .user_data()
        .get::<SeatMoveGrabState>()
        .unwrap()
        .lock()
        .unwrap()
        .is_some();
    let focused_output = last_active_seat.focused_or_active_output();
    let set = shell.workspaces.sets.get(output).ok_or(OutputNoMode)?;
    let workspace = set
        .workspaces
        .iter()
        .find(|w| w.handle == current.0)
        .ok_or(OutputNoMode)?;
    let is_active_space = workspace.output == focused_output;
    let active_hint = if shell.active_hint {
        theme.cosmic().active_hint as u8
    } else {
        0
    };

    let output_size = output
        .geometry()
        .size
        .as_logical()
        .to_physical_precise_round(scale);
    let (focal_point, zoom_scale) = zoom_level
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));

    let crop_to_output = |element: WorkspaceRenderElement<R>| {
        CropRenderElement::from_element(
            RescaleRenderElement::from_element(
                element,
                focal_point
                    .as_logical()
                    .to_physical(output.current_scale().fractional_scale())
                    .to_i32_round(),
                zoom_scale,
            ),
            scale,
            Rectangle::from_size(output_size),
        )
    };

    render_input_order::<()>(&shell, output, previous, current, element_filter, |stage| {
        match stage {
            Stage::ZoomUI => {
                elements.extend(ZoomState::render(renderer, output));
            }
            Stage::SessionLock(lock_surface) => {
                elements.extend(
                    session_lock_elements(renderer, output, lock_surface)
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                );
            }
            Stage::LayerPopup {
                popup, location, ..
            } => {
                elements.extend(
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        popup.wl_surface(),
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        Scale::from(scale),
                        1.0,
                        FRAME_TIME_FILTER,
                    )
                    .into_iter()
                    .flat_map(crop_to_output)
                    .map(Into::into),
                );
            }
            Stage::LayerSurface { layer, location } => {
                elements.extend(
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        layer.wl_surface(),
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        Scale::from(scale),
                        1.0,
                        FRAME_TIME_FILTER,
                    )
                    .into_iter()
                    .flat_map(crop_to_output)
                    .map(Into::into),
                );
            }
            Stage::OverrideRedirect { surface, location } => {
                elements.extend(surface.wl_surface().into_iter().flat_map(|surface| {
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        &surface,
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        Scale::from(scale),
                        1.0,
                        FRAME_TIME_FILTER,
                    )
                    .into_iter()
                    .flat_map(crop_to_output)
                    .map(Into::into)
                }));
            }
            Stage::StickyPopups(layout) => {
                let alpha = match &overview.0 {
                    OverviewMode::Started(_, started) => {
                        (1.0 - (Instant::now().duration_since(*started).as_millis()
                            / ANIMATION_DURATION.as_millis()) as f32)
                            .max(0.0)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Ended(_, ended) => {
                        ((Instant::now().duration_since(*ended).as_millis()
                            / ANIMATION_DURATION.as_millis()) as f32)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Active(_) => 0.6,
                    OverviewMode::None => 1.0,
                };

                elements.extend(
                    layout
                        .render_popups(renderer, alpha)
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                );
            }
            Stage::Sticky(layout) => {
                let alpha = match &overview.0 {
                    OverviewMode::Started(_, started) => {
                        (1.0 - (Instant::now().duration_since(*started).as_millis()
                            / ANIMATION_DURATION.as_millis()) as f32)
                            .max(0.0)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Ended(_, ended) => {
                        ((Instant::now().duration_since(*ended).as_millis()
                            / ANIMATION_DURATION.as_millis()) as f32)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Active(_) => 0.6,
                    OverviewMode::None => 1.0,
                };

                let current_focus = (!move_active && is_active_space)
                    .then_some(last_active_seat)
                    .map(|seat| workspace.focus_stack.get(seat));

                elements.extend(
                    layout
                        .render(
                            renderer,
                            current_focus.as_ref().and_then(|stack| {
                                stack.last().and_then(|t| match t {
                                    FocusTarget::Window(w) => Some(w),
                                    _ => None,
                                })
                            }),
                            resize_indicator.clone(),
                            active_hint,
                            alpha,
                            theme.cosmic(),
                        )
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                )
            }
            Stage::WorkspacePopups { workspace, offset } => {
                elements.extend(
                    match workspace.render_popups(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        theme.cosmic(),
                    ) {
                        Ok(elements) => {
                            elements
                                .into_iter()
                                .flat_map(crop_to_output)
                                .map(|element| {
                                    CosmicElement::Workspace(RelocateRenderElement::from_element(
                                        element,
                                        offset.to_physical_precise_round(scale),
                                        Relocate::Relative,
                                    ))
                                })
                        }
                        Err(_) => {
                            return ControlFlow::Break(Err(OutputNoMode));
                        }
                    },
                );
            }
            Stage::Workspace { workspace, offset } => {
                elements.extend(
                    match workspace.render(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        resize_indicator.clone(),
                        active_hint,
                        theme.cosmic(),
                    ) {
                        Ok(elements) => {
                            elements
                                .into_iter()
                                .flat_map(crop_to_output)
                                .map(|element| {
                                    CosmicElement::Workspace(RelocateRenderElement::from_element(
                                        element,
                                        offset.to_physical_precise_round(scale),
                                        Relocate::Relative,
                                    ))
                                })
                        }
                        Err(_) => {
                            return ControlFlow::Break(Err(OutputNoMode));
                        }
                    },
                );
            }
        };

        ControlFlow::Continue(())
    })?;

    Ok(elements)
}

fn session_lock_elements<R>(
    renderer: &mut R,
    output: &Output,
    lock_surface: Option<&LockSurface>,
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    R::TextureId: Clone + 'static,
{
    if let Some(surface) = lock_surface {
        let scale = Scale::from(output.current_scale().fractional_scale());
        render_elements_from_surface_tree(
            renderer,
            surface.wl_surface(),
            (0, 0),
            scale,
            1.0,
            FRAME_TIME_FILTER,
        )
    } else {
        Vec::new()
    }
}

// Used for mirroring and postprocessing
#[derive(Debug)]
pub struct PostprocessState {
    pub texture: TextureRenderBuffer<GlesTexture>,
    pub damage_tracker: OutputDamageTracker,
    pub cursor_texture: Option<TextureRenderBuffer<GlesTexture>>,
    pub cursor_damage_tracker: Option<OutputDamageTracker>,
    pub output_config: PostprocessOutputConfig,
}

impl PostprocessState {
    pub fn new_with_renderer<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        renderer: &mut R,
        format: Fourcc,
        output_config: PostprocessOutputConfig,
    ) -> Result<Self, R::Error> {
        let size = output_config.size;
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let opaque_regions = vec![Rectangle::from_size(buffer_size)];

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            Some(opaque_regions),
        );

        // Don't use `from_output` to avoid applying output transform
        let damage_tracker =
            OutputDamageTracker::new(size, output_config.fractional_scale, Transform::Normal);

        Ok(PostprocessState {
            texture: texture_buffer,
            damage_tracker,
            cursor_texture: None,
            cursor_damage_tracker: None,
            output_config,
        })
    }

    pub fn track_cursor<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);

        if let (Some(tex), Some(tracker)) = (
            self.cursor_texture.as_ref(),
            self.cursor_damage_tracker.as_ref(),
        ) {
            if tex.format().is_some_and(|f| f == format)
                && tracker.mode()
                    == &(OutputModeSource::Static {
                        size,
                        scale,
                        transform: Transform::Normal,
                    })
            {
                return Ok(());
            }
        }

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;

        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            None,
        );

        let damage_tracker = OutputDamageTracker::new(size, scale, Transform::Normal);

        self.cursor_texture = Some(texture_buffer);
        self.cursor_damage_tracker = Some(damage_tracker);

        Ok(())
    }

    pub fn remove_cursor(&mut self) {
        self.cursor_texture.take();
        self.cursor_damage_tracker.take();
    }
}

#[derive(Debug, PartialEq)]
pub struct PostprocessOutputConfig {
    pub size: Size<i32, Physical>,
    pub fractional_scale: f64,
}

impl PostprocessOutputConfig {
    pub fn for_output_untransformed(output: &Output) -> Self {
        Self {
            // Apply inverse of output transform to mode size to get correct size
            // for an untransformed render.
            size: output.current_transform().invert().transform_size(
                output
                    .current_mode()
                    .map(|mode| mode.size)
                    .unwrap_or_default(),
            ),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }

    pub fn for_output(output: &Output) -> Self {
        Self {
            size: output
                .current_mode()
                .map(|mode| mode.size)
                .unwrap_or_default(),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }
}

#[derive(Debug, Default)]
pub struct ScreenFilterStorage {
    pub filter: ScreenFilter,
    pub state: Option<PostprocessState>,
}

#[profiling::function]
pub fn render_output<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    screen_filter: &'d mut ScreenFilterStorage,
    loop_handle: &calloop::LoopHandle<'static, State>,
) -> Result<RenderOutputResult<'d>, RenderError<R::Error>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<GlesTexture>
        + Blit
        + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    // Use default blur state (not actually used for blur rendering yet)
    let mut blur_state = BlurRenderState::default();
    render_output_with_blur(
        gpu,
        renderer,
        target,
        damage_tracker,
        age,
        shell,
        now,
        output,
        cursor_mode,
        screen_filter,
        loop_handle,
        &mut blur_state,
    )
}

/// Render output with blur support
#[profiling::function]
pub fn render_output_with_blur<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    screen_filter: &'d mut ScreenFilterStorage,
    loop_handle: &calloop::LoopHandle<'static, State>,
    blur_state: &mut BlurRenderState,
) -> Result<RenderOutputResult<'d>, RenderError<R::Error>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<GlesTexture>
        + Blit
        + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let shell_ref = shell.read();
    let (previous_workspace, workspace_ref) = shell_ref
        .workspaces
        .active(output)
        .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace_ref.handle, idx);
    let zoom_state = shell_ref.zoom_state().cloned();

    // Check if there are blur windows and get their geometries
    let has_blur = workspace_ref.has_blur_windows();
    let blur_geometries = if has_blur {
        workspace_ref.blur_window_geometries(1.0)
    } else {
        Vec::new()
    };

    // Debug logging for blur detection
    tracing::debug!(
        has_blur = has_blur,
        blur_geometry_count = blur_geometries.len(),
        screen_filter_noop = screen_filter.filter.is_noop(),
        "Blur detection in render_output_with_blur"
    );

    std::mem::drop(shell_ref);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };

    let mut postprocess_texture = None;
    let result = if !screen_filter.filter.is_noop() {
        if screen_filter.state.as_ref().is_none_or(|state| {
            state.output_config != PostprocessOutputConfig::for_output_untransformed(output)
        }) {
            screen_filter.state = Some(
                PostprocessState::new_with_renderer(
                    renderer,
                    target.format().unwrap_or(Fourcc::Abgr8888),
                    PostprocessOutputConfig::for_output_untransformed(output),
                )
                .map_err(RenderError::Rendering)?,
            );
        }

        let state = screen_filter.state.as_mut().unwrap();
        let mut result = Err(RenderError::OutputNoMode(OutputNoMode));
        state
            .texture
            .render()
            .draw::<_, RenderError<R::Error>>(|tex| {
                let mut target = renderer.bind(tex).map_err(RenderError::Rendering)?;
                result = render_workspace(
                    gpu,
                    renderer,
                    &mut target,
                    &mut state.damage_tracker,
                    1,
                    None,
                    shell,
                    zoom_state.as_ref(),
                    now,
                    output,
                    previous_workspace,
                    workspace,
                    cursor_mode,
                    element_filter,
                );
                std::mem::drop(target);
                postprocess_texture = Some(tex.clone());

                Ok(if let Ok((res, _)) = result.as_ref() {
                    renderer.wait(&res.sync).map_err(RenderError::Rendering)?;
                    let transform = output.current_transform();
                    let area = tex.size().to_logical(1, transform);

                    res.damage
                        .cloned()
                        .map(|v| {
                            v.into_iter()
                                .map(|r| r.to_logical(1).to_buffer(1, transform, &area))
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default()
                } else {
                    Vec::new()
                })
            })?;

        if result.is_ok() {
            let texture_elem = TextureRenderElement::from_texture_render_buffer(
                (0., 0.),
                &state.texture,
                Some(1.0),
                None,
                None,
                Kind::Unspecified,
            );

            let postprocess_texture_shader = renderer
                .glow_renderer_mut()
                .egl_context()
                .user_data()
                .get::<PostprocessShader>()
                .expect("OffscreenShader should be available through `init_shaders`");
            let texture_geometry =
                texture_elem.geometry(output.current_scale().fractional_scale().into());
            let elements = {
                let texture_elem = TextureShaderElement::new(
                    texture_elem,
                    postprocess_texture_shader.0.clone(),
                    vec![
                        Uniform::new(
                            "invert",
                            if screen_filter.filter.inverted {
                                1.
                            } else {
                                0.
                            },
                        ),
                        Uniform::new(
                            "color_mode",
                            screen_filter
                                .filter
                                .color_filter
                                .map(|val| val as u8 as f32)
                                .unwrap_or(0.),
                        ),
                    ],
                );
                constrain_render_elements(
                    std::iter::once(texture_elem),
                    (0, 0),
                    Rectangle::from_size(
                        output
                            .geometry()
                            .size
                            .as_logical()
                            .to_f64()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                    ),
                    texture_geometry,
                    ConstrainScaleBehavior::Fit,
                    ConstrainAlign::CENTER,
                    1.0,
                )
                .map(CosmicElement::Postprocess)
                .collect::<Vec<_>>()
            };

            damage_tracker.render_output(renderer, target, age, &elements, CLEAR_COLOR)?;
        }

        result
    } else if has_blur && !blur_geometries.is_empty() {
        // Iterative multi-pass blur (macOS-style):
        // For each blur window (bottom to top in Z-order):
        //   1. Capture scene up to (but excluding) that window
        //   2. Apply blur and cache per-window texture
        // Then render final scene where each blur window uses its cached texture

        let output_name = output.name();
        tracing::debug!(
            blur_geometry_count = blur_geometries.len(),
            output = %output_name,
            "Entering iterative multi-pass blur path"
        );

        let output_size = output
            .current_mode()
            .ok_or(RenderError::OutputNoMode(OutputNoMode))?
            .size
            .to_logical(1)
            .to_physical_precise_round(output.current_scale().fractional_scale());
        let scale = Scale::from(output.current_scale().fractional_scale());
        let format = target.format().unwrap_or(Fourcc::Abgr8888);

        // Ensure blur textures are allocated
        blur_state
            .ensure_textures(renderer, format, output_size, scale)
            .map_err(RenderError::Rendering)?;

        // Get blur windows GROUPED by shared capture requirements
        // Consecutive blur windows (no non-blur windows between them) share a single capture
        let blur_groups = {
            let shell_ref = shell.read();
            let (_, workspace_ref) = shell_ref
                .workspaces
                .active(output)
                .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
            workspace_ref.blur_windows_grouped(1.0)
        };

        let total_windows: usize = blur_groups.iter().map(|g| g.windows.len()).sum();
        tracing::debug!(
            blur_group_count = blur_groups.len(),
            total_blur_windows = total_windows,
            "Processing blur windows in {} groups (optimized from {} captures to {})",
            blur_groups.len(),
            total_windows,
            blur_groups.len()
        );

        // Get the currently grabbed window (if any) to exclude it from blur capture
        {
            let shell_ref = shell.read();
            let last_active_seat = shell_ref.seats.last_active();
            let grabbed_window = last_active_seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|s| s.element());
            if grabbed_window.is_some() {
                tracing::debug!("Found grabbed window to exclude from blur");
            }
            set_grabbed_window(grabbed_window);
        }

        // Optimized iterative blur: one capture per GROUP, then blur for each window in group
        let mut any_blur_applied = false;
        for group in &blur_groups {
            tracing::debug!(
                capture_z_threshold = group.capture_z_threshold,
                windows_in_group = group.windows.len(),
                "Capturing blur for group (single capture for {} windows)",
                group.windows.len()
            );

            // Set z-index threshold for the ENTIRE group (use the lowest z-index)
            // All windows in this group see the same "below content"
            set_exclude_z_threshold(group.capture_z_threshold);
            set_skip_blur_backdrops(true);

            // Capture scene elements ONCE for the entire group
            let capture_elements: Vec<CosmicElement<R>> = workspace_elements(
                gpu,
                renderer,
                shell,
                zoom_state.as_ref(),
                now,
                output,
                previous_workspace,
                workspace,
                cursor_mode,
                element_filter,
            )?;

            clear_exclude_z_threshold();
            set_skip_blur_backdrops(false);

            // Render captured elements to background texture (once per group)
            let bg_render_ok = if let Some(bg_texture) = blur_state.background_texture.as_mut() {
                let mut blur_dt = OutputDamageTracker::new(output_size, scale, Transform::Normal);

                let render_result = (|| {
                    let mut gles_frame = bg_texture.render();

                    let render_res = gles_frame.draw::<_, RenderError<R::Error>>(|tex| {
                        let bound = renderer.bind(tex).map_err(RenderError::Rendering)?;
                        let mut bound_target = bound;
                        let res = blur_dt.render_output(
                            renderer,
                            &mut bound_target,
                            0, // Full redraw
                            &capture_elements,
                            CLEAR_COLOR,
                        );

                        match res {
                            Ok(_) => Ok(Vec::new()),
                            Err(e) => Err(e.into()),
                        }
                    });

                    render_res.map_err(|_| ())
                })();

                render_result.is_ok()
            } else {
                false
            };

            // Apply blur passes ONCE and cache for ALL windows in this group
            // Since they share the same background, they share the same blurred result
            if bg_render_ok && blur_state.is_ready() {
                if let (Some(bg), Some(ping), Some(pong)) = (
                    blur_state.background_texture.as_ref().cloned(),
                    blur_state.texture_a.as_mut(),
                    blur_state.texture_b.as_mut(),
                ) {
                    let blur_result = apply_blur_passes(
                        renderer,
                        &bg,
                        ping,
                        pong,
                        output_size,
                        scale,
                        BLUR_ITERATIONS,
                    );

                    if blur_result.is_ok() {
                        // Cache the SAME blurred texture for ALL windows in this group
                        for (window_key, _geometry, _alpha, z_idx) in &group.windows {
                            cache_blur_texture_for_window(
                                &output_name,
                                window_key,
                                BlurredTextureInfo {
                                    texture: pong.clone(),
                                    size: output_size,
                                    scale,
                                },
                            );
                            tracing::debug!(
                                global_z_idx = z_idx,
                                "Cached shared blur texture for window in group"
                            );
                        }
                        any_blur_applied = true;
                    } else {
                        tracing::warn!(
                            capture_z_threshold = group.capture_z_threshold,
                            "Blur passes failed for group"
                        );
                    }
                }
            }
        }

        set_grabbed_window(None); // Clear grabbed window after all captures
        blur_state.blur_applied = any_blur_applied;

        // Also cache a global fallback for windows that don't have per-window cache
        // (This uses the last captured blur, which is the full background)
        if any_blur_applied {
            if let Some(pong) = blur_state.texture_b.as_ref() {
                cache_blur_texture(
                    &output_name,
                    BlurredTextureInfo {
                        texture: pong.clone(),
                        size: output_size,
                        scale,
                    },
                );
            }
        }

        // Final render: collect ALL elements (blur backdrops will find their per-window cached textures)
        let final_elements: Vec<CosmicElement<R>> = workspace_elements(
            gpu,
            renderer,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            element_filter,
        )?;

        // Render final scene with blur backdrops to actual target
        let result = damage_tracker.render_output(
            renderer,
            target,
            if any_blur_applied { 0 } else { age }, // Force full redraw if blur active
            &final_elements,
            CLEAR_COLOR,
        )?;

        Ok((result, final_elements))
    } else {
        render_workspace(
            gpu,
            renderer,
            target,
            damage_tracker,
            age,
            None,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            element_filter,
        )
    };

    match result {
        Ok((res, mut elements)) => {
            for (session, frame) in output.take_pending_frames() {
                if let Some(pending_image_copy_data) = render_session::<_, _, GlesTexture>(
                    renderer,
                    session.user_data().get::<SessionData>().unwrap(),
                    frame,
                    output.current_transform(),
                    |buffer, renderer, offscreen, dt, age, additional_damage| {
                        let old_len = if !additional_damage.is_empty() {
                            let area = output
                                .current_mode()
                                .ok_or(RenderError::OutputNoMode(OutputNoMode))
                                .map(
                                    |mode| {
                                        mode.size
                                            .to_logical(1)
                                            .to_buffer(1, Transform::Normal)
                                            .to_f64()
                                    }, /* TODO: Mode is Buffer..., why is this Physical in the first place */
                                )?;

                            let old_len = elements.len();
                            elements.extend(
                                additional_damage
                                    .into_iter()
                                    .map(|rect| {
                                        rect.to_f64()
                                            .to_logical(
                                                output.current_scale().fractional_scale(),
                                                output.current_transform(),
                                                &area,
                                            )
                                            .to_i32_round()
                                    })
                                    .map(DamageElement::new)
                                    .map(Into::into),
                            );

                            Some(old_len)
                        } else {
                            None
                        };

                        let res = dt.damage_output(age, &elements)?;

                        if let Some(old_len) = old_len {
                            elements.truncate(old_len);
                        }

                        let mut sync = SyncPoint::default();

                        if let (Some(damage), _) = &res {
                            // TODO: On Vulkan, may need to combine sync points instead of just using latest?
                            let blit_to_buffer =
                                |renderer: &mut R, blit_from: &mut R::Framebuffer<'_>| {
                                    if let Ok(dmabuf) = get_dmabuf(buffer) {
                                        let mut dmabuf_clone = dmabuf.clone();
                                        let mut fb = renderer.bind(&mut dmabuf_clone)?;
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                &mut fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    } else {
                                        let fb = offscreen
                                            .expect("shm buffers should have offscreen target");
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    }

                                    Result::<_, R::Error>::Ok(())
                                };

                            // we would want to just assign a different framebuffer to a variable, depending on the code-path,
                            // but then rustc tries to equate the lifetime of target with the lifetime of our temporary fb...
                            // So instead of duplicating all the code, we use a closure..
                            if let Some(tex) = postprocess_texture.as_mut() {
                                let mut fb = renderer.bind(tex).map_err(RenderError::Rendering)?;
                                blit_to_buffer(renderer, &mut fb)
                                    .map_err(RenderError::Rendering)?;
                            } else {
                                blit_to_buffer(renderer, target).map_err(RenderError::Rendering)?;
                            }
                        }

                        Ok(RenderOutputResult {
                            damage: res.0,
                            sync,
                            states: res.1,
                        })
                    },
                )? {
                    pending_image_copy_data.send_success_when_ready(
                        output.current_transform(),
                        loop_handle,
                        now,
                    );
                }
            }

            Ok(res)
        }
        Err(err) => Err(err),
    }
}

#[profiling::function]
pub fn render_workspace<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    additional_damage: Option<Vec<Rectangle<i32, Logical>>>,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<(RenderOutputResult<'d>, Vec<CosmicElement<R>>), RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements: Vec<CosmicElement<R>> = workspace_elements(
        gpu,
        renderer,
        shell,
        zoom_level,
        now,
        output,
        previous,
        current,
        cursor_mode,
        element_filter,
    )?;

    if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(output).as_logical();
        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| rect.intersection(output_geo))
                .map(DamageElement::new)
                .map(Into::<CosmicElement<R>>::into),
        );
    }

    let res = damage_tracker.render_output(
        renderer,
        target,
        age,
        &elements,
        CLEAR_COLOR, // TODO use a theme neutral color
    );

    res.map(|res| (res, elements))
}
