// SPDX-License-Identifier: GPL-3.0-only

//! Handler implementation for the voice mode protocol

use crate::shell::SeatExt;
use crate::shell::grabs::SeatMoveGrabState;
use crate::state::State;
use crate::utils::geometry::{RectGlobalExt, RectLocalExt, SizeExt};
use crate::utils::prelude::OutputExt;
use crate::wayland::protocols::voice_mode::{
    OrbState, VoiceModeHandler, VoiceModeState, delegate_voice_mode,
};
use smithay::desktop::space::SpaceElement;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Resource;
use smithay::wayland::seat::WaylandFocus;
use tracing::info;

impl VoiceModeHandler for State {
    fn voice_mode_state(&mut self) -> &mut VoiceModeState {
        &mut self.common.voice_mode_state
    }

    fn activate_voice_mode(&mut self, focused_surface: Option<&WlSurface>) -> OrbState {
        info!(?focused_surface, "Activating voice mode");

        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = seat.active_output();

        // Check if a window is currently being grabbed/dragged
        let grabbed_window_info: Option<(WlSurface, smithay::utils::Rectangle<i32, smithay::utils::Logical>, String)> = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|grab_state| {
                grab_state.lock().ok().and_then(|guard| {
                    guard.as_ref().and_then(|state| {
                        let mapped = state.element();
                        let surface = mapped.active_window().wl_surface()?.into_owned();
                        let surface_id = surface.id().to_string();
                        // Get the grabbed window's current geometry (SpaceElement::geometry returns Logical)
                        let geo = mapped.geometry();
                        Some((surface, geo, surface_id))
                    })
                })
            });

        // Determine which receiver to use and get window geometry if attaching
        // Priority: grabbed window > focused window > default receiver
        let (receiver_surface, window_geo) = if let Some((grabbed_surface, geo, surface_id)) = &grabbed_window_info {
            // Check if grabbed window has a voice receiver
            if self
                .common
                .voice_mode_state
                .has_receiver_for_surface(grabbed_surface)
            {
                info!("Using grabbed window as voice receiver");
                (Some(grabbed_surface.clone()), Some((geo.clone(), surface_id.clone())))
            } else if let Some(surface) = focused_surface {
                // Fall back to focused surface check
                if self
                    .common
                    .voice_mode_state
                    .has_receiver_for_surface(surface)
                {
                    // Get the focused window's geometry (not grabbed)
                    let keyboard = seat.get_keyboard().unwrap();
                    let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                        crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                            let workspace = shell.active_space(&output)?;
                            let local_geo = workspace.element_geometry(mapped)?;
                            let geo = local_geo.to_global(&output).as_logical();
                            let surface_id = mapped.active_window().wl_surface()?.id().to_string();
                            Some((geo, surface_id))
                        }
                        _ => None,
                    });
                    (Some(surface.clone()), geo_and_id)
                } else {
                    (None, None)
                }
            } else {
                (None, None)
            }
        } else if let Some(surface) = focused_surface {
            // No grabbed window, check focused surface
            if self
                .common
                .voice_mode_state
                .has_receiver_for_surface(surface)
            {
                // Get the focused window's geometry and surface ID from the workspace
                let keyboard = seat.get_keyboard().unwrap();
                let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                    crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                        // Get element geometry from workspace (gives position in workspace coordinates)
                        let workspace = shell.active_space(&output)?;
                        let local_geo = workspace.element_geometry(mapped)?;
                        // Convert to global/logical coordinates for rendering
                        let geo = local_geo.to_global(&output).as_logical();
                        // Get surface ID for reliable window matching during render
                        let surface_id = mapped.active_window().wl_surface()?.id().to_string();
                        Some((geo, surface_id))
                    }
                    _ => None,
                });
                (Some(surface.clone()), geo_and_id)
            } else {
                // Focused surface doesn't have a receiver, fall back to default
                (None, None)
            }
        } else {
            (None, None)
        };

        let orb_state = if let Some(ref surface) = receiver_surface {
            if let Some((geo, surface_id)) = window_geo {
                // Attach orb to the focused window
                info!("Attaching orb to receiver surface at {:?} (surface_id: {})", geo, surface_id);
                let output_geo = output.geometry();
                // Request show attached - orb will burst directly in window
                shell
                    .voice_orb_state
                    .request_show_attached(geo, output_geo.size.as_logical(), surface_id);
                shell.enter_voice_mode();

                // Send start to the window-specific receiver
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_surface(surface, OrbState::Attached);

                // Send orb_attached event
                self.common
                    .voice_mode_state
                    .send_orb_attached(surface, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);

                OrbState::Attached
            } else {
                // Shouldn't happen, but fall back to floating
                shell.voice_orb_state.request_show_floating();
                shell.enter_voice_mode();
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_default(OrbState::Floating);
                OrbState::Floating
            }
        } else {
            // No focused receiver, use default receiver with floating orb
            info!("Showing floating orb (using default receiver)");
            // Request orb show - will start after window fade completes
            shell.voice_orb_state.request_show_floating();
            shell.enter_voice_mode();
            drop(shell);
            self.common
                .voice_mode_state
                .send_start_to_default(OrbState::Floating);
            OrbState::Floating
        };

        orb_state
    }

    fn deactivate_voice_mode(&mut self) {
        info!("Deactivating voice mode");

        // Send stop to active receiver
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();
    }

    fn cancel_voice_mode(&mut self) {
        info!("Cancelling voice mode");

        // Send cancel to active receiver
        self.common.voice_mode_state.send_cancel();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();
    }
}

delegate_voice_mode!(State);
