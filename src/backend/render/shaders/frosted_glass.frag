#version 100

precision mediump float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

// Blur parameters
uniform vec3 tint_color;      // Tint color for the frosted effect
uniform float tint_strength;  // How much tint to apply (0-1)
uniform float corner_radius;  // Corner radius for the blur area
uniform float blur_amount;    // Visual blur simulation strength

// SDF for rounded box
float rounded_box(in vec2 p, in vec2 b, in float r) {
    vec2 q = abs(p) - b + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

// Pseudo-random function for noise
float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898, 78.233))) * 43758.5453123);
}

void main() {
    vec2 center = size / 2.0;
    vec2 location = v_coords * size;
    
    // Calculate distance from rounded rectangle edge
    float dist = rounded_box(location - center, size / 2.0, corner_radius);
    float mask = 1.0 - smoothstep(-1.0, 1.0, dist);
    
    // Create a frosted glass effect
    // Since we can't sample the background in a pixel shader, we simulate
    // the blur effect with noise + tint
    
    // Add subtle noise to simulate blur granularity
    float noise = random(v_coords * 100.0) * 0.03 * blur_amount;
    
    // Mix the tint color with a lighter shade for the frosted effect
    vec3 base_color = tint_color;
    vec3 highlight = vec3(1.0); // White highlight
    
    // Create a gradient effect that simulates depth
    float gradient = v_coords.y * 0.1;
    vec3 final_color = mix(base_color, highlight, tint_strength + gradient + noise);
    
    // Apply alpha based on mask and overall opacity
    float final_alpha = mask * alpha;
    
    vec4 result = vec4(final_color, final_alpha);
    
    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.4, 0.4, 0.3) + result * 0.7;
    #endif
    
    gl_FragColor = result;
}
