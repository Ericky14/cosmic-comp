// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC surface embedding protocol (zcosmic_surface_embed_manager_v1)
//!
//! This protocol allows clients to embed foreign toplevel windows within their own surfaces.
//! The embedded surface can be interactive (receiving input) or display-only.

pub use generated::{zcosmic_embedded_surface_v1, zcosmic_surface_embed_manager_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/surface_embed.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/surface_embed.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{IsAlive, Logical, Rectangle},
    wayland::compositor::{Cacheable, with_states},
};
use std::sync::Mutex;
use tracing::{debug, warn};

/// Render mode for embedded surfaces
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EmbedRenderMode {
    /// Live rendering - compositor renders the surface directly
    #[default]
    Live,
    /// Screencopy mode - frames are captured and sent to client
    Screencopy,
}

/// State for a single embedded surface
#[derive(Debug)]
pub struct EmbeddedSurfaceData<W: Clone> {
    /// The parent surface this is embedded into
    pub parent: Weak<WlSurface>,
    /// The embedded toplevel window
    pub toplevel: Option<W>,
    /// The toplevel identifier string
    pub toplevel_id: String,
    /// Current geometry within parent surface
    pub geometry: Rectangle<i32, Logical>,
    /// Whether input should be routed to the embedded surface
    pub interactive: bool,
    /// Render mode
    pub render_mode: EmbedRenderMode,
    /// Pending geometry (before commit)
    pending_geometry: Option<Rectangle<i32, Logical>>,
    /// Pending interactive state
    pending_interactive: Option<bool>,
    /// Pending render mode
    pending_render_mode: Option<EmbedRenderMode>,
    /// Whether the embedded toplevel is still valid
    pub valid: bool,
}

impl<W: Clone> EmbeddedSurfaceData<W> {
    fn new(parent: Weak<WlSurface>, toplevel: W, toplevel_id: String) -> Self {
        Self {
            parent,
            toplevel: Some(toplevel),
            toplevel_id,
            geometry: Rectangle::new((0, 0).into(), (100, 100).into()),
            interactive: false,
            render_mode: EmbedRenderMode::Live,
            pending_geometry: None,
            pending_interactive: None,
            pending_render_mode: None,
            valid: true,
        }
    }

    fn new_inert(parent: Weak<WlSurface>, toplevel_id: String) -> Self {
        Self {
            parent,
            toplevel: None,
            toplevel_id,
            geometry: Rectangle::default(),
            interactive: false,
            render_mode: EmbedRenderMode::Live,
            pending_geometry: None,
            pending_interactive: None,
            pending_render_mode: None,
            valid: false,
        }
    }

    fn commit(&mut self) {
        if let Some(geometry) = self.pending_geometry.take() {
            self.geometry = geometry;
        }
        if let Some(interactive) = self.pending_interactive.take() {
            self.interactive = interactive;
        }
        if let Some(render_mode) = self.pending_render_mode.take() {
            self.render_mode = render_mode;
        }
    }
}

/// Global state for embedded surfaces
pub type EmbeddedSurfaceState<W> = Mutex<EmbeddedSurfaceData<W>>;

/// Data stored per-surface tracking all embeds on that surface
#[derive(Debug, Default)]
pub struct SurfaceEmbedData {
    /// List of embedded surfaces on this parent surface
    pub embeds:
        std::cell::RefCell<Vec<Weak<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1>>>,
}

impl Cacheable for SurfaceEmbedData {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        // Clean up dead references
        self.embeds.borrow_mut().retain(|e| e.upgrade().is_ok());
        self.clone()
    }

    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into.embeds.borrow_mut() = self.embeds.into_inner();
    }
}

impl Clone for SurfaceEmbedData {
    fn clone(&self) -> Self {
        Self {
            embeds: std::cell::RefCell::new(self.embeds.borrow().clone()),
        }
    }
}

/// Manager state for the surface embed protocol
#[derive(Debug)]
pub struct SurfaceEmbedManagerState {
    global: GlobalId,
}

impl SurfaceEmbedManagerState {
    /// Create a new surface embed manager global
    pub fn new<D, W>(dh: &DisplayHandle) -> SurfaceEmbedManagerState
    where
        D: GlobalDispatch<
                zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
                SurfaceEmbedManagerGlobalData,
            > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
            + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
            + SurfaceEmbedHandler<Window = W>
            + 'static,
        W: Clone + Send + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, _>(
                1,
                SurfaceEmbedManagerGlobalData {
                    filter: Box::new(|_| true),
                },
            );
        SurfaceEmbedManagerState { global }
    }

    /// Create a new surface embed manager global with a client filter
    pub fn new_with_filter<D, W, F>(dh: &DisplayHandle, filter: F) -> SurfaceEmbedManagerState
    where
        D: GlobalDispatch<
                zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
                SurfaceEmbedManagerGlobalData,
            > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
            + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
            + SurfaceEmbedHandler<Window = W>
            + 'static,
        W: Clone + Send + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh
            .create_global::<D, zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, _>(
                1,
                SurfaceEmbedManagerGlobalData {
                    filter: Box::new(filter),
                },
            );
        SurfaceEmbedManagerState { global }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Global data for the surface embed manager
pub struct SurfaceEmbedManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl std::fmt::Debug for SurfaceEmbedManagerGlobalData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SurfaceEmbedManagerGlobalData")
            .field("filter", &"<fn>")
            .finish()
    }
}

/// Handler trait for surface embedding
pub trait SurfaceEmbedHandler: Sized {
    /// The window type used by the compositor
    type Window: Clone + Send + IsAlive + 'static;

    /// Get the surface embed manager state
    fn surface_embed_state(&mut self) -> &mut SurfaceEmbedManagerState;

    /// Resolve a toplevel identifier string to a window
    fn window_from_toplevel_id(&self, toplevel_id: &str) -> Option<Self::Window>;

    /// Called when an embed is created
    fn embed_created(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        let _ = (parent, toplevel, embed);
    }

    /// Called when an embed geometry changes
    fn embed_geometry_changed(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        geometry: Rectangle<i32, Logical>,
    ) {
        let _ = (parent, toplevel, geometry);
    }

    /// Called when an embed is destroyed
    fn embed_destroyed(&mut self, parent: &WlSurface, toplevel: &Self::Window) {
        let _ = (parent, toplevel);
    }
}

impl<D, W>
    GlobalDispatch<
        zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
        SurfaceEmbedManagerGlobalData,
        D,
    > for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1>,
        _global_data: &SurfaceEmbedManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &SurfaceEmbedManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D, W> Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, (), D>
    for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
        request: zcosmic_surface_embed_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_surface_embed_manager_v1::Request::Destroy => {
                // Nothing to do, the object is destroyed
            }
            zcosmic_surface_embed_manager_v1::Request::EmbedToplevel {
                id,
                parent,
                toplevel_id,
            } => {
                // Resolve the toplevel ID to a window
                let window = match state.window_from_toplevel_id(&toplevel_id) {
                    Some(w) => w,
                    None => {
                        warn!("embed_toplevel: invalid toplevel id '{}'", toplevel_id);
                        // Create an inert object
                        let embedded_data =
                            EmbeddedSurfaceData::new_inert(parent.downgrade(), toplevel_id);
                        data_init.init(id, Mutex::new(embedded_data));
                        return;
                    }
                };

                debug!(
                    "Creating embedded surface for toplevel '{}' in parent {:?}",
                    toplevel_id,
                    parent.id()
                );

                let embedded_data =
                    EmbeddedSurfaceData::new(parent.downgrade(), window.clone(), toplevel_id);
                let embed = data_init.init(id, Mutex::new(embedded_data));

                // Register the embed on the parent surface
                with_states(&parent, |states| {
                    let data = states.data_map.get_or_insert(SurfaceEmbedData::default);
                    data.embeds.borrow_mut().push(embed.downgrade());
                });

                // Notify handler
                state.embed_created(&parent, &window, &embed);
            }
        }
    }
}

impl<D, W>
    Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>, D>
    for SurfaceEmbedManagerState
where
    D: GlobalDispatch<
            zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1,
            SurfaceEmbedManagerGlobalData,
        > + Dispatch<zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1, ()>
        + Dispatch<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1, EmbeddedSurfaceState<W>>
        + SurfaceEmbedHandler<Window = W>
        + 'static,
    W: Clone + Send + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
        request: zcosmic_embedded_surface_v1::Request,
        data: &EmbeddedSurfaceState<W>,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let mut embed_data = data.lock().unwrap();

        if !embed_data.valid {
            return;
        }

        match request {
            zcosmic_embedded_surface_v1::Request::Destroy => {
                // Notify handler if we have valid data
                if let (Ok(parent), Some(_toplevel)) =
                    (embed_data.parent.upgrade(), embed_data.toplevel.as_ref())
                {
                    // Clean up the embed from the parent surface
                    with_states(&parent, |states| {
                        if let Some(data) = states.data_map.get::<SurfaceEmbedData>() {
                            data.embeds
                                .borrow_mut()
                                .retain(|e| e.upgrade().map(|e| &e != resource).unwrap_or(false));
                        }
                    });

                    drop(embed_data);
                    let embed_data = data.lock().unwrap();
                    if let Some(toplevel) = embed_data.toplevel.as_ref() {
                        state.embed_destroyed(&parent, toplevel);
                    }
                }
            }
            zcosmic_embedded_surface_v1::Request::SetGeometry {
                x,
                y,
                width,
                height,
            } => {
                if width <= 0 || height <= 0 {
                    resource.post_error(
                        zcosmic_embedded_surface_v1::Error::InvalidGeometry,
                        "width and height must be positive",
                    );
                    return;
                }
                embed_data.pending_geometry =
                    Some(Rectangle::new((x, y).into(), (width, height).into()));
            }
            zcosmic_embedded_surface_v1::Request::SetInteractive { interactive } => {
                embed_data.pending_interactive = Some(interactive != 0);
            }
            zcosmic_embedded_surface_v1::Request::SetRenderMode { mode } => {
                embed_data.pending_render_mode = Some(match mode {
                    0 => EmbedRenderMode::Live,
                    1 => EmbedRenderMode::Screencopy,
                    _ => EmbedRenderMode::Live,
                });
            }
            zcosmic_embedded_surface_v1::Request::Commit => {
                let old_geometry = embed_data.geometry;
                embed_data.commit();

                // Notify handler if geometry changed
                if old_geometry != embed_data.geometry {
                    if let (Ok(parent), Some(toplevel)) =
                        (embed_data.parent.upgrade(), embed_data.toplevel.clone())
                    {
                        let geometry = embed_data.geometry;
                        drop(embed_data);
                        state.embed_geometry_changed(&parent, &toplevel, geometry);
                    }
                }
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
        data: &EmbeddedSurfaceState<W>,
    ) {
        let embed_data = data.lock().unwrap();
        if let (Ok(parent), Some(toplevel)) =
            (embed_data.parent.upgrade(), embed_data.toplevel.as_ref())
        {
            // Clean up the embed from the parent surface
            with_states(&parent, |states| {
                if let Some(data) = states.data_map.get::<SurfaceEmbedData>() {
                    data.embeds
                        .borrow_mut()
                        .retain(|e| e.upgrade().map(|e| &e != resource).unwrap_or(false));
                }
            });

            state.embed_destroyed(&parent, toplevel);
        }
    }
}

/// Helper to get embedded surfaces for a parent surface
pub fn embedded_surfaces_for(
    surface: &WlSurface,
) -> Vec<zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1> {
    with_states(surface, |states| {
        states
            .data_map
            .get::<SurfaceEmbedData>()
            .map(|data| {
                data.embeds
                    .borrow()
                    .iter()
                    .filter_map(|e| e.upgrade().ok())
                    .collect()
            })
            .unwrap_or_default()
    })
}

/// Macro to delegate the surface embed protocol
#[macro_export]
macro_rules! delegate_surface_embed {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty, $window: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1: $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerGlobalData
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_surface_embed_manager_v1::ZcosmicSurfaceEmbedManagerV1: ()
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::surface_embed::zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1: $crate::wayland::protocols::surface_embed::EmbeddedSurfaceState<$window>
        ] => $crate::wayland::protocols::surface_embed::SurfaceEmbedManagerState);
    };
}

pub(crate) use delegate_surface_embed;
