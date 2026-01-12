// SPDX-License-Identifier: GPL-3.0-only

//! Real-time background blur implementation using two-pass Gaussian blur.
//!
//! This module provides blur rendering for windows that request the KDE blur protocol.
//! The algorithm works by:
//! 1. Rendering background elements to an offscreen texture
//! 2. Applying horizontal blur pass
//! 3. Applying vertical blur pass
//! 4. Compositing the blurred result at the blur window's location
//!
//! The two-pass Gaussian blur provides high quality results while being
//! efficient enough for real-time use.

use smithay::{
    backend::renderer::{
        Offscreen,
        damage::OutputDamageTracker,
        element::{
            Element, Kind,
            texture::{TextureRenderBuffer, TextureRenderElement},
        },
        gles::{GlesError, GlesTexture, GlesTexProgram, Uniform, element::TextureShaderElement},
    },
    utils::{Logical, Physical, Point, Rectangle, Scale, Size, Transform},
};
use smithay::backend::allocator::Fourcc;

use super::element::AsGlowRenderer;

/// Default blur radius in pixels (design spec: blur(50px))
pub const DEFAULT_BLUR_RADIUS: f32 = 50.0;

/// Configuration for blur effect
#[derive(Debug, Clone, Copy)]
pub struct BlurConfig {
    /// Blur radius/strength (higher = more blur)
    pub radius: f32,
    /// Number of blur iterations (higher = smoother but slower)
    pub iterations: u32,
    /// Alpha/opacity of the blur effect
    pub alpha: f32,
}

impl Default for BlurConfig {
    fn default() -> Self {
        Self {
            radius: DEFAULT_BLUR_RADIUS,
            iterations: 2,
            alpha: 1.0,
        }
    }
}

/// Per-output state for blur rendering
///
/// Manages offscreen textures used for the blur passes.
/// Uses ping-pong rendering for multi-pass blur.
#[derive(Debug)]
pub struct BlurOutputState {
    /// Texture A for ping-pong blur passes
    pub texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Texture B for ping-pong blur passes
    pub texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Damage tracker for optimized rendering
    pub damage_tracker: Option<OutputDamageTracker>,
    /// Current texture size
    pub size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
}

impl Default for BlurOutputState {
    fn default() -> Self {
        Self {
            texture_a: None,
            texture_b: None,
            damage_tracker: None,
            size: Size::from((0, 0)),
            scale: Scale::from(1.0),
        }
    }
}

impl BlurOutputState {
    /// Create a new blur state
    pub fn new() -> Self {
        Self::default()
    }

    /// Ensure textures are allocated and sized correctly
    pub fn ensure_textures<R>(
        &mut self,
        renderer: &mut R,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<bool, GlesError>
    where
        R: AsGlowRenderer + Offscreen<GlesTexture>,
    {
        // Only reallocate if size changed
        if self.size == size && self.texture_a.is_some() && self.texture_b.is_some() {
            return Ok(true);
        }

        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let format = Fourcc::Abgr8888;

        // Create texture A
        let tex_a = match Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size) {
            Ok(tex) => tex,
            Err(e) => {
                tracing::warn!("Failed to create blur texture A: {:?}", e);
                return Ok(false);
            }
        };
        self.texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create texture B
        let tex_b = match Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size) {
            Ok(tex) => tex,
            Err(e) => {
                tracing::warn!("Failed to create blur texture B: {:?}", e);
                return Ok(false);
            }
        };
        self.texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create damage tracker
        self.damage_tracker = Some(OutputDamageTracker::new(size, scale, Transform::Normal));

        self.size = size;
        self.scale = scale;

        tracing::debug!(
            width = size.w,
            height = size.h,
            "Allocated blur textures"
        );

        Ok(true)
    }

    /// Get texture A
    pub fn texture_a(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_a.as_ref()
    }

    /// Get texture B
    pub fn texture_b(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_b.as_ref()
    }

    /// Get mutable texture A
    pub fn texture_a_mut(&mut self) -> Option<&mut TextureRenderBuffer<GlesTexture>> {
        self.texture_a.as_mut()
    }

    /// Get mutable texture B
    pub fn texture_b_mut(&mut self) -> Option<&mut TextureRenderBuffer<GlesTexture>> {
        self.texture_b.as_mut()
    }

    /// Current texture size
    pub fn size(&self) -> Size<i32, Physical> {
        self.size
    }
    
    /// Check if textures are ready
    pub fn is_ready(&self) -> bool {
        self.texture_a.is_some() && self.texture_b.is_some()
    }
}

/// Information about a blur region to be rendered
#[derive(Debug, Clone)]
pub struct BlurRegion {
    /// The region in physical coordinates where blur should be applied
    pub region: Rectangle<i32, Physical>,
    /// Blur configuration for this region
    pub config: BlurConfig,
}

impl BlurRegion {
    /// Create a new blur region with default config
    pub fn new(region: Rectangle<i32, Physical>) -> Self {
        Self {
            region,
            config: BlurConfig::default(),
        }
    }

    /// Create a blur region with custom config
    pub fn with_config(region: Rectangle<i32, Physical>, config: BlurConfig) -> Self {
        Self { region, config }
    }
}

/// Create a horizontal blur pass element from a texture
pub fn create_horizontal_blur_element(
    shader: GlesTexProgram,
    texture: &TextureRenderBuffer<GlesTexture>,
    location: Point<f64, Physical>,
    blur_radius: f32,
    alpha: f32,
) -> TextureShaderElement {
    let texture_elem = TextureRenderElement::from_texture_render_buffer(
        location,
        texture,
        Some(alpha),
        None,
        None,
        Kind::Unspecified,
    );
    
    let geo = texture_elem.geometry(1.0.into());
    
    TextureShaderElement::new(
        texture_elem,
        shader,
        vec![
            Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
            Uniform::new("blur_radius", blur_radius),
            Uniform::new("direction", 0.0), // horizontal
        ],
    )
}

/// Create a vertical blur pass element from a texture
pub fn create_vertical_blur_element(
    shader: GlesTexProgram,
    texture: &TextureRenderBuffer<GlesTexture>,
    location: Point<f64, Physical>,
    blur_radius: f32,
    alpha: f32,
) -> TextureShaderElement {
    let texture_elem = TextureRenderElement::from_texture_render_buffer(
        location,
        texture,
        Some(alpha),
        None,
        None,
        Kind::Unspecified,
    );
    
    let geo = texture_elem.geometry(1.0.into());
    
    TextureShaderElement::new(
        texture_elem,
        shader,
        vec![
            Uniform::new("tex_size", [geo.size.w as f32, geo.size.h as f32]),
            Uniform::new("blur_radius", blur_radius),
            Uniform::new("direction", 1.0), // vertical
        ],
    )
}

/// Create a texture element from a blur texture for final compositing
pub fn create_blur_composite_element(
    texture: &TextureRenderBuffer<GlesTexture>,
    location: Point<f64, Physical>,
    src_region: Option<Rectangle<f64, Logical>>,
    alpha: f32,
) -> TextureRenderElement<GlesTexture> {
    TextureRenderElement::from_texture_render_buffer(
        location,
        texture,
        Some(alpha),
        src_region,
        None,
        Kind::Unspecified,
    )
}

/// Check if any windows in a collection have blur enabled
pub fn has_blur_windows<'a, I, W>(windows: I) -> bool
where
    I: IntoIterator<Item = &'a W>,
    W: HasBlur + 'a,
{
    windows.into_iter().any(|w| w.has_blur())
}

/// Trait for elements that may have blur enabled
pub trait HasBlur {
    /// Check if this element has blur enabled
    fn has_blur(&self) -> bool;
}
