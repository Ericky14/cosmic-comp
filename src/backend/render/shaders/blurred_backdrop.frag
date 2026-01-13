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

// Calculate alpha for rounded corners using distance from corner centers
float rounding_alpha(vec2 coords, vec2 size, float cr_tl, float cr_tr, float cr_br, float cr_bl) {
    vec2 center;
    float radius;

    // Top-left corner
    if (coords.x < cr_tl && coords.y < cr_tl) {
        radius = cr_tl;
        center = vec2(radius, radius);
    // Top-right corner
    } else if (size.x - cr_tr < coords.x && coords.y < cr_tr) {
        radius = cr_tr;
        center = vec2(size.x - radius, radius);
    // Bottom-right corner
    } else if (size.x - cr_br < coords.x && size.y - cr_br < coords.y) {
        radius = cr_br;
        center = vec2(size.x - radius, size.y - radius);
    // Bottom-left corner
    } else if (coords.x < cr_bl && size.y - cr_bl < coords.y) {
        radius = cr_bl;
        center = vec2(radius, size.y - radius);
    } else {
        return 1.0;
    }

    float dist = distance(coords, center);
    return 1.0 - smoothstep(radius - 0.5, radius + 0.5, dist);
}

void main() {
    // Sample the blurred texture
    vec4 blurred = texture2D(tex, v_coords);
    
    // Apply tint overlay: blend tint_color on top with tint_strength opacity
    vec3 tinted = mix(blurred.rgb, tint_color, tint_strength);
    
    // v_coords is in full texture space [0,1] representing the whole screen.
    // Convert to screen pixel coordinates, then to local element coordinates.
    vec2 screen_pos = v_coords * screen_size;
    vec2 pixel_coords = screen_pos - element_pos;
    
    // Apply rounded corner mask
    float mask = rounding_alpha(pixel_coords, size, corner_radius_tl, corner_radius_tr, corner_radius_br, corner_radius_bl);
    
    // Discard pixels in rounded corners to avoid any artifacts
    if (mask < 0.01) discard;
    
    // Final output with alpha and corner mask
    gl_FragColor = vec4(tinted, blurred.a * alpha * mask);
}
