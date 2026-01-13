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
uniform float alpha;           // Overall opacity
uniform vec2 size;             // Element size in pixels
uniform vec2 screen_size;      // Full screen size (unused now, kept for compatibility)
uniform vec2 element_pos;      // Element position on screen (unused now)
uniform float corner_radius_tl;   // Top-left corner radius
uniform float corner_radius_tr;   // Top-right corner radius
uniform float corner_radius_br;   // Bottom-right corner radius
uniform float corner_radius_bl;   // Bottom-left corner radius
uniform vec3 tint_color;       // Tint overlay color (e.g., white = 1.0, 1.0, 1.0)
uniform float tint_strength;   // Tint opacity (0.10 for 10% white overlay)

// Signed distance field for a rounded box with per-corner radii
// p: position relative to box center
// b: box half-size
// r: corner radii (top_right, bottom_right, bottom_left, top_left)
float rounded_box(vec2 p, vec2 b, vec4 r) {
    r.xy = (p.x > 0.0) ? r.xy : r.zw;
    r.x = (p.y > 0.0) ? r.x : r.y;
    vec2 q = abs(p) - b + r.x;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r.x;
}

void main() {
    // Sample the blurred texture
    vec4 blurred = texture2D(tex, v_coords);
    
    // v_coords is in full texture space [0,1] representing the whole screen.
    // Convert to screen pixel coordinates, then to local element coordinates.
    vec2 screen_pos = v_coords * screen_size;
    vec2 pixel_coords = screen_pos - element_pos;
    
    // Calculate SDF for rounded corners
    // Convert to centered coordinates (SDF expects center at origin)
    vec2 center = size / 2.0;
    vec2 half_size = size / 2.0;
    // Corner radii order for rounded_box: top_right, bottom_right, bottom_left, top_left
    vec4 corners = vec4(corner_radius_tr, corner_radius_br, corner_radius_bl, corner_radius_tl);
    
    float dist = rounded_box(pixel_coords - center, half_size, corners);
    
    // Discard pixels outside the rounded rectangle (use SDF directly for sharp cutoff)
    if (dist > 0.0) discard;
    
    // Smooth antialiased edge (only for pixels inside the shape)
    float mask = 1.0 - smoothstep(-1.0, 0.0, dist);
    
    // Apply tint overlay: blend tint_color on top with tint_strength opacity
    vec3 tinted = mix(blurred.rgb, tint_color, tint_strength);
    
    // Final output with alpha and corner mask (premultiplied alpha)
    gl_FragColor = vec4(tinted * mask, blurred.a * alpha * mask);
}
