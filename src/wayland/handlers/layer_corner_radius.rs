// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::layer_corner_radius::{
    LayerCornerRadiusHandler, LayerCornerRadiusState, delegate_layer_corner_radius,
};

impl LayerCornerRadiusHandler for State {
    fn layer_corner_radius_state(&mut self) -> &mut LayerCornerRadiusState {
        &mut self.common.layer_corner_radius_state
    }
}

delegate_layer_corner_radius!(State);
