// Blurred backdrop shader - samples from pre-blurred background texture
// Used for KDE blur protocol / frosted glass effect
//
// The texture has already been cropped to the window region via src_rect,
// so v_coords are [0,1] within the window bounds. This shader just:
// 1. Samples the blurred texture directly
// 2. Applies a semi-transparent tint overlay
// 3. Masks with rounded corners

#version 100

precision mediump float;

// Standard texture sampling uniforms (provided by TextureShaderElement)
uniform sampler2D tex;
varying vec2 v_coords;

// Custom uniforms
uniform float alpha;          // Overall opacity
uniform vec2 size;            // Element size in pixels
uniform vec2 screen_size;     // Full screen size (unused now, kept for compatibility)
uniform vec2 element_pos;     // Element position on screen (unused now)
uniform float corner_radius;  // Corner radius for rounded rect mask
uniform vec3 tint_color;      // Tint overlay color
uniform float tint_strength;  // How much tint to apply (0.0 = none, 1.0 = full)

// SDF for rounded box
float rounded_box(in vec2 p, in vec2 b, in float r) {
    vec2 q = abs(p) - b + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

void main() {
    // Sample the blurred texture at the interpolated coordinates
    // v_coords are already mapped to the correct src_rect region by Smithay
    vec4 sample_color = texture2D(tex, v_coords);
    
    // Output the blurred backdrop
    gl_FragColor = vec4(sample_color.rgb, 1.0);
}
