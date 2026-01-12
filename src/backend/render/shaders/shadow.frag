precision mediump float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

// Shadow parameters
uniform vec3 shadow_color;   // Shadow color (typically dark)
uniform float shadow_radius; // Corner radius of the shadowed element
uniform float shadow_blur;   // Blur spread of the shadow
uniform vec2 shadow_offset;  // Shadow offset (x, y)

// SDF for rounded box
float rounded_box(in vec2 p, in vec2 b, in float r)
{
    vec2 q = abs(p) - b + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

void main() {
    vec2 location = v_coords * size;
    
    // Calculate the inner box size (the actual window)
    // The shadow element is larger than the window by shadow_blur on each side
    vec2 inner_size = size - shadow_blur * 2.0;
    vec2 inner_center = size / 2.0 + shadow_offset;
    
    // Distance from the rounded rectangle (negative inside, positive outside)
    float dist = rounded_box(location - inner_center, inner_size / 2.0, shadow_radius);
    
    // Create soft shadow falloff
    // The shadow starts at the edge of the box and fades out over shadow_blur distance
    float shadow_alpha = 1.0 - smoothstep(-shadow_blur * 0.1, shadow_blur, dist);
    
    // Apply a more realistic shadow curve (darker near the edge, fading outward)
    shadow_alpha = shadow_alpha * shadow_alpha * (3.0 - 2.0 * shadow_alpha); // smootherstep
    
    vec4 final_color = vec4(shadow_color, shadow_alpha * alpha);

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        final_color = vec4(0.3, 0.0, 0.3, 0.2) + final_color * 0.8;
    #endif

    gl_FragColor = final_color;
}
