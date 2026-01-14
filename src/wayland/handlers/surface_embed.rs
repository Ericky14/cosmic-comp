// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::CosmicSurface,
    state::State,
    wayland::protocols::surface_embed::{
        SurfaceEmbedHandler, SurfaceEmbedManagerState, delegate_surface_embed,
        zcosmic_embedded_surface_v1,
    },
};
use smithay::{
    desktop::space::SpaceElement,
    reexports::wayland_server::{Resource, protocol::wl_surface::WlSurface},
    utils::{Logical, Rectangle},
};
use std::sync::Mutex;
use tracing::debug;

/// State stored per-toplevel for embed rectangles
#[derive(Default)]
pub struct EmbedToplevelState {
    pub rectangles: Vec<(
        smithay::reexports::wayland_server::Weak<WlSurface>,
        Rectangle<i32, Logical>,
    )>,
}

impl SurfaceEmbedHandler for State {
    type Window = CosmicSurface;

    fn surface_embed_state(&mut self) -> &mut SurfaceEmbedManagerState {
        &mut self.common.surface_embed_state
    }

    fn window_from_toplevel_id(&self, toplevel_id: &str) -> Option<Self::Window> {
        // Look through all spaces and windows to find one matching the ID
        // The ID format is the app_id (for now - could be UUID in future)
        let shell = self.common.shell.read();
        for mapped in shell.workspaces.spaces().flat_map(|s| s.mapped()) {
            for (surface, _point) in mapped.windows() {
                if surface.app_id() == toplevel_id || surface.title() == toplevel_id {
                    return Some(surface.clone());
                }
            }
        }
        None
    }

    fn embed_created(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        embed: &zcosmic_embedded_surface_v1::ZcosmicEmbeddedSurfaceV1,
    ) {
        debug!(
            "Embedded surface created: {:?} in parent {:?}",
            toplevel.title(),
            parent.id()
        );

        // Send initial configure with toplevel's preferred size
        let geometry = toplevel.geometry();
        embed.configure(geometry.size.w, geometry.size.h);
    }

    fn embed_geometry_changed(
        &mut self,
        parent: &WlSurface,
        toplevel: &Self::Window,
        geometry: Rectangle<i32, Logical>,
    ) {
        debug!(
            "Embedded surface geometry changed: {:?} -> {:?}",
            toplevel.title(),
            geometry
        );

        // Store the embed rectangle for input routing
        let user_data = toplevel.user_data();
        let embed_state = user_data.get_or_insert(|| Mutex::new(EmbedToplevelState::default()));
        let mut state = embed_state.lock().unwrap();
        // Remove any existing rectangle for this parent
        state
            .rectangles
            .retain(|(s, _)| s.upgrade().map(|s| s != *parent).unwrap_or(false));
        // Add the new rectangle
        state.rectangles.push((parent.downgrade(), geometry));
    }

    fn embed_destroyed(&mut self, parent: &WlSurface, toplevel: &Self::Window) {
        debug!(
            "Embedded surface destroyed: {:?} from parent {:?}",
            toplevel.title(),
            parent.id()
        );

        // Clean up the rectangle
        let user_data = toplevel.user_data();
        if let Some(embed_state) = user_data.get::<Mutex<EmbedToplevelState>>() {
            let mut state = embed_state.lock().unwrap();
            state
                .rectangles
                .retain(|(s, _)| s.upgrade().map(|s| s != *parent).unwrap_or(false));
        }
    }
}

delegate_surface_embed!(State, CosmicSurface);
