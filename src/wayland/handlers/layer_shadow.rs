// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_layer_shadow;
use crate::state::State;
use crate::wayland::protocols::layer_shadow::{LayerShadowHandler, LayerShadowManagerState};

impl LayerShadowHandler for State {
    fn layer_shadow_state(&mut self) -> &mut LayerShadowManagerState {
        &mut self.common.layer_shadow_state
    }
}

delegate_layer_shadow!(State);
