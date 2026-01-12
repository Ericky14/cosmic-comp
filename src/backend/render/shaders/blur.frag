// Kawase dual-filter blur shader
// Based on "An investigation of fast real-time GPU-based image blur algorithms" 
// by Marius Bjørge (ARM Mali)
//
// Kawase blur provides high quality blur with fewer texture samples than
// traditional Gaussian blur, especially for large blur radii like 50px.
// Each pass samples 5 points in a diamond pattern, and multiple iterations
// stack to create a smooth Gaussian-like result.

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
uniform float blur_radius;  // Blur radius - controls offset scale
uniform float direction;    // 0.0 = horizontal-ish (down), 1.0 = vertical-ish (up)

void main() {
    vec2 texel = 1.0 / tex_size;
    
    // Calculate offset based on blur radius
    // For 50px blur with ~4 iterations, offset of ~3-4 per pass works well
    float offset = blur_radius / 12.0;
    
    // Kawase blur samples in a diamond/cross pattern
    // This achieves a box-blur-like effect that stacks into Gaussian
    vec2 half_texel = texel * 0.5;
    
    // For direction 0 (downsample/horizontal-focused):
    // Sample center + 4 corners offset by (offset, offset) pattern
    // For direction 1 (upsample/vertical-focused):
    // Sample center + 4 diagonal points at (offset*2, offset*2)
    
    float scale = direction < 0.5 ? offset : offset * 2.0;
    
    // Sample pattern: center + 4 diagonal samples (Kawase pattern)
    vec4 sum = texture2D(tex, v_coords) * 4.0;
    
    // Sample at diagonal offsets - this creates the blur spread
    sum += texture2D(tex, v_coords + texel * vec2(-scale, -scale));
    sum += texture2D(tex, v_coords + texel * vec2( scale, -scale));
    sum += texture2D(tex, v_coords + texel * vec2(-scale,  scale));
    sum += texture2D(tex, v_coords + texel * vec2( scale,  scale));
    
    // Add axis-aligned samples for smoother result
    sum += texture2D(tex, v_coords + texel * vec2(-scale, 0.0));
    sum += texture2D(tex, v_coords + texel * vec2( scale, 0.0));
    sum += texture2D(tex, v_coords + texel * vec2(0.0, -scale));
    sum += texture2D(tex, v_coords + texel * vec2(0.0,  scale));
    
    // Average: center (4) + 4 diagonals (1 each) + 4 axis (1 each) = 12
    vec4 result = sum / 12.0;
    
    // Apply alpha
    result.a *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.2, 0.4, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
