// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer shadow protocol (layer_shadow_manager_v1)
//!
//! This protocol allows clients to request shadow rendering for layer shell surfaces.
//! The compositor will render a drop shadow behind surfaces that have this enabled.

pub use generated::{layer_shadow_manager_v1, layer_shadow_surface_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/layer-shadow.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/layer-shadow.xml");
}

use smithay::utils::HookId;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{add_pre_commit_hook, with_states},
};
use std::sync::Mutex;

type SurfaceHookId = Mutex<Option<(HookId, Weak<layer_shadow_surface_v1::LayerShadowSurfaceV1>)>>;

/// Data associated with a shadow surface object
pub struct LayerShadowData(pub Mutex<LayerShadowInternal>);

pub struct LayerShadowInternal {
    pub surface: Weak<WlSurface>,
    pub enabled: bool,
}

/// Pending shadow state for a surface (double-buffered)
#[derive(Debug, Clone, Default)]
pub struct PendingLayerShadow {
    /// Whether shadow is enabled (None = no change pending)
    pub enabled: Option<bool>,
}

/// Committed shadow state for a surface
#[derive(Debug, Clone, Default)]
pub struct LayerShadowState {
    /// Whether shadow is enabled
    pub enabled: bool,
}

/// State for the layer shadow manager protocol
#[derive(Debug)]
pub struct LayerShadowManagerState {
    global: GlobalId,
}

impl LayerShadowManagerState {
    /// Create a new layer shadow manager global
    pub fn new<D>(dh: &DisplayHandle) -> LayerShadowManagerState
    where
        D: GlobalDispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
            + Dispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
            + Dispatch<layer_shadow_surface_v1::LayerShadowSurfaceV1, LayerShadowData>
            + LayerShadowHandler
            + 'static,
    {
        let global = dh.create_global::<D, layer_shadow_manager_v1::LayerShadowManagerV1, _>(1, ());
        LayerShadowManagerState { global }
    }

    /// Get the global ID of the layer shadow manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for layer shadow events
pub trait LayerShadowHandler {
    /// Get the layer shadow state
    fn layer_shadow_state(&mut self) -> &mut LayerShadowManagerState;
}

impl<D> GlobalDispatch<layer_shadow_manager_v1::LayerShadowManagerV1, (), D>
    for LayerShadowManagerState
where
    D: GlobalDispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_surface_v1::LayerShadowSurfaceV1, LayerShadowData>
        + LayerShadowHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_shadow_manager_v1::LayerShadowManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_shadow_manager_v1::LayerShadowManagerV1, (), D> for LayerShadowManagerState
where
    D: GlobalDispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_surface_v1::LayerShadowSurfaceV1, LayerShadowData>
        + LayerShadowHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_shadow_manager_v1::LayerShadowManagerV1,
        request: layer_shadow_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_shadow_manager_v1::Request::Destroy => {
                // Global destroyed, nothing to do
            }
            layer_shadow_manager_v1::Request::GetShadow { id, surface } => {
                // Check if surface already has a shadow object
                let shadow_exists = with_states(&surface, |surface_data| {
                    let hook_id = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                    let guard = hook_id.lock().unwrap();
                    guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
                });

                if shadow_exists.unwrap_or_default() {
                    resource.post_error(
                        layer_shadow_manager_v1::Error::ShadowExists as u32,
                        format!(
                            "{resource:?} LayerShadowSurfaceV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = LayerShadowData(Mutex::new(LayerShadowInternal {
                    surface: surface.downgrade(),
                    enabled: false,
                }));
                let obj = data_init.init(id, data);
                let obj_downgrade = obj.downgrade();

                let needs_hook = shadow_exists.is_none();
                if needs_hook {
                    let hook_id = add_pre_commit_hook::<D, _>(&surface, move |_, _dh, surface| {
                        // Pre-commit hook - apply pending shadow state
                        with_states(surface, |surface_data| {
                            let pending = surface_data
                                .data_map
                                .get::<Mutex<PendingLayerShadow>>()
                                .map(|p| p.lock().unwrap().clone())
                                .unwrap_or_default();

                            if let Some(enabled) = pending.enabled {
                                let state = surface_data.data_map.get_or_insert_threadsafe(|| {
                                    Mutex::new(LayerShadowState::default())
                                });
                                state.lock().unwrap().enabled = enabled;
                            }

                            // Clear pending state
                            if let Some(pending_state) =
                                surface_data.data_map.get::<Mutex<PendingLayerShadow>>()
                            {
                                *pending_state.lock().unwrap() = PendingLayerShadow::default();
                            }
                        });
                    });

                    with_states(&surface, |surface_data| {
                        let hook_state = surface_data.data_map.get::<SurfaceHookId>().unwrap();
                        *hook_state.lock().unwrap() = Some((hook_id, obj_downgrade));
                    });
                } else {
                    with_states(&surface, |surface_data| {
                        let hook_state = surface_data.data_map.get::<SurfaceHookId>().unwrap();
                        let mut guard = hook_state.lock().unwrap();
                        if let Some((_, ref mut weak)) = *guard {
                            *weak = obj_downgrade;
                        }
                    });
                }
            }
        }
    }
}

impl<D> Dispatch<layer_shadow_surface_v1::LayerShadowSurfaceV1, LayerShadowData, D>
    for LayerShadowManagerState
where
    D: GlobalDispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_manager_v1::LayerShadowManagerV1, ()>
        + Dispatch<layer_shadow_surface_v1::LayerShadowSurfaceV1, LayerShadowData>
        + LayerShadowHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_shadow_surface_v1::LayerShadowSurfaceV1,
        request: layer_shadow_surface_v1::Request,
        data: &LayerShadowData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let internal = data.0.lock().unwrap();
        let surface = match internal.surface.upgrade() {
            Ok(s) => s,
            Err(_) => {
                if !matches!(request, layer_shadow_surface_v1::Request::Destroy) {
                    resource.post_error(
                        layer_shadow_surface_v1::Error::SurfaceDestroyed as u32,
                        "The wl_surface was destroyed",
                    );
                }
                return;
            }
        };
        drop(internal);

        match request {
            layer_shadow_surface_v1::Request::Destroy => {
                // Set pending state to disable shadow
                with_states(&surface, |surface_data| {
                    let pending = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| Mutex::new(PendingLayerShadow::default()));
                    pending.lock().unwrap().enabled = Some(false);
                });
            }
            layer_shadow_surface_v1::Request::Enable => {
                with_states(&surface, |surface_data| {
                    let pending = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| Mutex::new(PendingLayerShadow::default()));
                    pending.lock().unwrap().enabled = Some(true);
                });
            }
            layer_shadow_surface_v1::Request::Disable => {
                with_states(&surface, |surface_data| {
                    let pending = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| Mutex::new(PendingLayerShadow::default()));
                    pending.lock().unwrap().enabled = Some(false);
                });
            }
        }
    }
}

/// Delegate macro for layer shadow protocol
#[macro_export]
macro_rules! delegate_layer_shadow {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_shadow::layer_shadow_manager_v1::LayerShadowManagerV1: ()
        ] => $crate::wayland::protocols::layer_shadow::LayerShadowManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_shadow::layer_shadow_manager_v1::LayerShadowManagerV1: ()
        ] => $crate::wayland::protocols::layer_shadow::LayerShadowManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_shadow::layer_shadow_surface_v1::LayerShadowSurfaceV1: $crate::wayland::protocols::layer_shadow::LayerShadowData
        ] => $crate::wayland::protocols::layer_shadow::LayerShadowManagerState);
    };
}

/// Check if a surface has shadow enabled
pub fn surface_has_shadow(surface: &WlSurface) -> bool {
    with_states(surface, |surface_data| {
        surface_data
            .data_map
            .get::<Mutex<LayerShadowState>>()
            .map(|s| s.lock().unwrap().enabled)
            .unwrap_or(false)
    })
}
