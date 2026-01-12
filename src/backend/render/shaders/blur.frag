#version 100

//_DEFINES_

#if defined(EXTERNAL)
#extension GL_OES_EGL_image_external : require
#endif

precision highp float;

#if defined(EXTERNAL)
uniform samplerExternalOES tex;
#else
uniform sampler2D tex;
#endif

uniform float alpha;
varying vec2 v_coords;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

// Blur uniforms
uniform vec2 tex_size;      // Size of the texture in pixels
uniform float blur_radius;  // Blur radius in pixels (0-64)
uniform float direction;    // 0.0 = horizontal, 1.0 = vertical

// 13-tap Gaussian kernel for high quality blur
// These weights correspond to a sigma of ~2.0 for the base kernel
// The actual blur radius is controlled by the sample spacing
void main() {
    vec2 tex_offset = 1.0 / tex_size;
    
    // Clamp blur_radius to a reasonable range
    float radius = clamp(blur_radius, 0.0, 64.0);
    
    // Calculate step size based on blur radius
    // Higher radius = larger steps between samples
    float step_size = max(1.0, radius / 6.0);
    
    // Gaussian weights for 13-tap kernel (sigma ≈ 2.0)
    // Normalized so they sum to 1.0
    float weights[7];
    weights[0] = 0.1964825501511404;
    weights[1] = 0.2969069646728344;
    weights[2] = 0.09447039785044732;
    weights[3] = 0.010381362401148057;
    weights[4] = 0.0;
    weights[5] = 0.0;
    weights[6] = 0.0;
    
    // Dynamically adjust weights based on blur radius for quality
    if (radius > 16.0) {
        // Use wider kernel for larger blur
        weights[0] = 0.14;
        weights[1] = 0.24;
        weights[2] = 0.16;
        weights[3] = 0.06;
        weights[4] = 0.02;
        weights[5] = 0.005;
        weights[6] = 0.001;
    }
    
    // Start with center sample
    vec4 result = texture2D(tex, v_coords) * weights[0];
    
    // Direction vector: (1,0) for horizontal, (0,1) for vertical
    vec2 dir = direction < 0.5 ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    
    // Sample in both directions from center
    for (int i = 1; i < 7; ++i) {
        float weight = weights[i];
        if (weight <= 0.0) continue;
        
        float offset = float(i) * step_size;
        vec2 sample_offset = dir * tex_offset * offset;
        
        // Sample positive and negative directions
        vec4 sample_pos = texture2D(tex, v_coords + sample_offset);
        vec4 sample_neg = texture2D(tex, v_coords - sample_offset);
        
        result += (sample_pos + sample_neg) * weight;
    }
    
    // Apply alpha
    result.a *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.2, 0.4, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
