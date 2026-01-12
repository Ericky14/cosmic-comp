// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::blur::{BlurHandler, BlurState, delegate_blur};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;

impl BlurHandler for State {
    fn blur_state(&mut self) -> &mut BlurState {
        &mut self.common.blur_state
    }

    fn blur_set(&mut self, surface: &WlSurface) {
        // Schedule a redraw for the affected surface
        if let Some(output) = self.common.shell.read().visible_output_for_surface(surface) {
            self.backend.schedule_render(output);
        }
    }

    fn blur_unset(&mut self, surface: &WlSurface) {
        // Schedule a redraw for the affected surface
        if let Some(output) = self.common.shell.read().visible_output_for_surface(surface) {
            self.backend.schedule_render(output);
        }
    }
}

delegate_blur!(State);
