// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_home_visibility;
use crate::state::State;
use crate::wayland::protocols::home_visibility::{HomeVisibilityHandler, HomeVisibilityState};

impl HomeVisibilityHandler for State {
    fn home_visibility_state(&self) -> &HomeVisibilityState {
        &self.common.home_visibility_state
    }

    fn set_surface_visibility(&mut self, surface_id: u32, home_only: bool) {
        let mut shell = self.common.shell.write();
        shell.set_surface_home_only(surface_id, home_only);
    }

    fn remove_surface_visibility(&mut self, surface_id: u32) {
        let mut shell = self.common.shell.write();
        shell.remove_home_only_surface(surface_id);
    }
}

delegate_home_visibility!(State);
