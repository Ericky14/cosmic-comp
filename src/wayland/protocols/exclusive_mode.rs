// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the COSMIC exclusive mode protocol (zcosmic_exclusive_mode_v1)
//!
//! This protocol allows clients to request exclusive mode for their window.
//! When exclusive mode is enabled, all other toplevel windows on the same
//! output are minimized. When disabled or the window closes, they are restored.

pub use generated::{zcosmic_exclusive_mode_manager_v1, zcosmic_exclusive_mode_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/exclusive_mode.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/exclusive_mode.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    backend::GlobalId, protocol::wl_surface::WlSurface,
};
use std::sync::Mutex;
use tracing::{debug, info, warn};

/// User data for the exclusive mode controller
pub struct ExclusiveModeControllerData {
    /// The wl_surface this controls (should be root surface of an xdg_toplevel)
    pub surface: Weak<WlSurface>,
    /// Whether exclusive mode is currently enabled
    pub enabled: Mutex<bool>,
    /// IDs of windows that were minimized by this exclusive mode request
    /// These will be restored when exclusive mode is disabled
    pub minimized_window_ids: Mutex<Vec<u64>>,
}

/// State for the exclusive mode manager protocol
#[derive(Debug)]
pub struct ExclusiveModeState {
    global: GlobalId,
    /// Currently active exclusive mode controller (only one can be active at a time)
    active_controller: Mutex<Option<Weak<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1>>>,
}

impl ExclusiveModeState {
    /// Create a new exclusive mode manager global
    pub fn new<D>(dh: &DisplayHandle) -> ExclusiveModeState
    where
        D: GlobalDispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
            + Dispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
            + Dispatch<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1, ExclusiveModeControllerData>
            + ExclusiveModeHandler
            + 'static,
    {
        let global = dh.create_global::<
            D,
            zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1,
            _,
        >(1, ());
        ExclusiveModeState {
            global,
            active_controller: Mutex::new(None),
        }
    }

    /// Get the global ID
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// Check if there's an active exclusive mode
    pub fn has_active_exclusive(&self) -> bool {
        let guard = self.active_controller.lock().unwrap();
        guard
            .as_ref()
            .map(|weak| weak.upgrade().is_ok())
            .unwrap_or(false)
    }

    /// Set the active controller
    pub fn set_active(
        &self,
        controller: Option<Weak<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1>>,
    ) {
        *self.active_controller.lock().unwrap() = controller;
    }
}

/// Result of enabling exclusive mode
#[derive(Debug)]
pub enum EnableExclusiveModeResult {
    /// Exclusive mode was enabled, with the list of minimized window IDs
    Enabled(Vec<u64>),
    /// Exclusive mode could not be enabled due to session being paused
    SessionPaused,
    /// Exclusive mode could not be enabled because surface was not found
    SurfaceNotFound,
}

/// Handler trait for exclusive mode events
pub trait ExclusiveModeHandler {
    /// Get the exclusive mode state
    fn exclusive_mode_state(&mut self) -> &mut ExclusiveModeState;

    /// Called when a client requests to enable exclusive mode.
    /// The handler should minimize all other windows and return the result.
    /// On success, returns the list of window IDs that were minimized.
    fn enable_exclusive_mode(&mut self, surface: &WlSurface) -> EnableExclusiveModeResult;

    /// Called when exclusive mode should be disabled.
    /// The handler should restore the windows with the given IDs.
    fn disable_exclusive_mode(&mut self, surface: &WlSurface, window_ids: &[u64]);
}

impl<D> GlobalDispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, (), D>
    for ExclusiveModeState
where
    D: GlobalDispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1, ExclusiveModeControllerData>
        + ExclusiveModeHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, (), D>
    for ExclusiveModeState
where
    D: GlobalDispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1, ExclusiveModeControllerData>
        + ExclusiveModeHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1,
        request: zcosmic_exclusive_mode_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_exclusive_mode_manager_v1::Request::GetExclusiveMode { id, surface } => {
                debug!(
                    surface_id = surface.id().protocol_id(),
                    "Creating exclusive mode controller for surface"
                );
                let controller_data = ExclusiveModeControllerData {
                    surface: surface.downgrade(),
                    enabled: Mutex::new(false),
                    minimized_window_ids: Mutex::new(Vec::new()),
                };
                data_init.init(id, controller_data);
            }
            zcosmic_exclusive_mode_manager_v1::Request::Destroy => {}
        }
    }
}

impl<D> Dispatch<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1, ExclusiveModeControllerData, D>
    for ExclusiveModeState
where
    D: GlobalDispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1, ()>
        + Dispatch<zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1, ExclusiveModeControllerData>
        + ExclusiveModeHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1,
        request: zcosmic_exclusive_mode_v1::Request,
        data: &ExclusiveModeControllerData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_exclusive_mode_v1::Request::SetExclusive { exclusive } => {
                let Ok(surface) = data.surface.upgrade() else {
                    warn!("Surface no longer alive for exclusive mode request");
                    return;
                };

                let enable = exclusive != 0;
                let mut enabled_guard = data.enabled.lock().unwrap();
                let was_enabled = *enabled_guard;

                if enable && !was_enabled {
                    // Check if another controller already has exclusive mode
                    if state.exclusive_mode_state().has_active_exclusive() {
                        info!("Another window already has exclusive mode, rejecting request");
                        resource.failed("Another window already has exclusive mode".to_string());
                        return;
                    }

                    // Enable exclusive mode
                    info!(
                        surface_id = surface.id().protocol_id(),
                        "Enabling exclusive mode"
                    );

                    let result = state.enable_exclusive_mode(&surface);

                    match result {
                        EnableExclusiveModeResult::Enabled(minimized_ids) => {
                            let count = minimized_ids.len() as u32;

                            // Store the minimized window IDs
                            *data.minimized_window_ids.lock().unwrap() = minimized_ids;
                            *enabled_guard = true;

                            // Mark this controller as the active one
                            state
                                .exclusive_mode_state()
                                .set_active(Some(resource.downgrade()));

                            resource.enabled(count);
                        }
                        EnableExclusiveModeResult::SessionPaused => {
                            info!("Session is paused, rejecting exclusive mode request");
                            resource.failed("Session is paused, try again later".to_string());
                        }
                        EnableExclusiveModeResult::SurfaceNotFound => {
                            warn!("Surface not found for exclusive mode");
                            resource.failed("Surface not found".to_string());
                        }
                    }
                } else if !enable && was_enabled {
                    // Disable exclusive mode
                    info!(
                        surface_id = surface.id().protocol_id(),
                        "Disabling exclusive mode"
                    );

                    let minimized_ids = data.minimized_window_ids.lock().unwrap().clone();
                    let count = minimized_ids.len() as u32;

                    state.disable_exclusive_mode(&surface, &minimized_ids);

                    // Clear the stored IDs
                    data.minimized_window_ids.lock().unwrap().clear();
                    *enabled_guard = false;

                    // Clear the active controller
                    state.exclusive_mode_state().set_active(None);

                    resource.disabled(count);
                }
            }
            zcosmic_exclusive_mode_v1::Request::Destroy => {
                // If exclusive mode was enabled, disable it on destroy
                let Ok(surface) = data.surface.upgrade() else {
                    return;
                };

                let enabled = *data.enabled.lock().unwrap();
                if enabled {
                    info!(
                        surface_id = surface.id().protocol_id(),
                        "Exclusive mode controller destroyed while enabled, restoring windows"
                    );

                    let minimized_ids = data.minimized_window_ids.lock().unwrap().clone();
                    state.disable_exclusive_mode(&surface, &minimized_ids);
                    state.exclusive_mode_state().set_active(None);
                }
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1,
        data: &ExclusiveModeControllerData,
    ) {
        // If exclusive mode was enabled, disable it
        let enabled = *data.enabled.lock().unwrap();
        if enabled {
            if let Ok(surface) = data.surface.upgrade() {
                info!(
                    surface_id = surface.id().protocol_id(),
                    "Exclusive mode controller destroyed, restoring windows"
                );

                let minimized_ids = data.minimized_window_ids.lock().unwrap().clone();
                state.disable_exclusive_mode(&surface, &minimized_ids);
            }
            state.exclusive_mode_state().set_active(None);
        }
    }
}

/// Macro to delegate exclusive mode protocol handling
#[macro_export]
macro_rules! delegate_exclusive_mode {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::exclusive_mode::zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1: ()
        ] => $crate::wayland::protocols::exclusive_mode::ExclusiveModeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::exclusive_mode::zcosmic_exclusive_mode_manager_v1::ZcosmicExclusiveModeManagerV1: ()
        ] => $crate::wayland::protocols::exclusive_mode::ExclusiveModeState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::exclusive_mode::zcosmic_exclusive_mode_v1::ZcosmicExclusiveModeV1: $crate::wayland::protocols::exclusive_mode::ExclusiveModeControllerData
        ] => $crate::wayland::protocols::exclusive_mode::ExclusiveModeState);
    };
}
pub(crate) use delegate_exclusive_mode;
