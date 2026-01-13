// Blurred backdrop shader - frosted glass effect
// Implements: background: rgba(255, 255, 255, 0.10); backdrop-filter: blur(50px);
//
// The texture has already been blurred and cropped to the window region via src_rect.
// This shader:
// 1. Samples the blurred texture
// 2. Applies a semi-transparent white overlay (frosted glass tint)
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
uniform vec3 tint_color;      // Tint overlay color (e.g., white = 1.0, 1.0, 1.0)
uniform float tint_strength;  // Tint opacity (0.10 for 10% white overlay)

// SDF for rounded box
float rounded_box(in vec2 p, in vec2 b, in float r) {
    vec2 q = abs(p) - b + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

void main() {
    // Sample the blurred texture
    vec4 blurred = texture2D(tex, v_coords);
    
    // Apply tint overlay: blend tint_color on top with tint_strength opacity
    // This mimics: background: rgba(255, 255, 255, 0.10) on top of blurred background
    // Using standard alpha blending: result = tint * tint_alpha + blurred * (1 - tint_alpha)
    vec3 tinted = mix(blurred.rgb, tint_color, tint_strength);
    
    // Apply rounded corner mask
    vec2 half_size = size * 0.5;
    vec2 pos = (v_coords - 0.5) * size;  // Convert from [0,1] to pixel coords centered at origin
    float dist = rounded_box(pos, half_size, corner_radius);
    
    // Soft edge for anti-aliasing (1 pixel fade)
    float mask = 1.0 - smoothstep(-1.0, 0.0, dist);
    
    // Final output with alpha and corner mask
    gl_FragColor = vec4(tinted, alpha * mask);
}
