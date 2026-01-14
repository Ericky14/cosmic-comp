// Kawase dual-filter blur shader
// Based on "An investigation of fast real-time GPU-based image blur algorithms" 
// by Marius Bjørge (ARM Mali)
//
// Tuned to replicate Figma blur 100:
// - Figma blur 100 ≈ Gaussian σ ≈ 25–30px, effective diameter ~100–120px
// - Heavy downsampling (1/8) + many iterations (24) + wide offset
// - Softened center weight for more Gaussian-like energy distribution

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
uniform float blur_radius;  // Blur radius - controls offset scale (100 for Figma blur 100)
uniform float direction;    // 0.0 = horizontal-ish (down), 1.0 = vertical-ish (up)

void main() {
    vec2 texel = 1.0 / tex_size;
    
    // Calculate offset based on blur radius
    // At 1/8 resolution, small offsets spread widely in screen space.
    // Use smaller offsets with many iterations to avoid banding/stripes.
    // offset = 100 / 32 = 3.125 texels, but at 1/8 res = ~25px screen spread per pass
    // With 24 iterations this accumulates to strong blur without visible stripes.
    float offset = blur_radius / 32.0;
    
    // Direction controls pass type:
    // - direction 0: downsample passes (base offset)
    // - direction 1: upsample passes (slightly larger offset)
    float scale = direction < 0.5 ? offset : offset * 1.5;
    
    // Kawase blur with softened center weight for Figma-like result
    // Reduced center weight (2.0 instead of 4.0) pushes more energy outward,
    // creating a softer, more Gaussian-like blur matching Figma's look
    vec4 sum = texture2D(tex, v_coords) * 2.0;
    
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
    
    // Average: center (2) + 4 diagonals (1 each) + 4 axis (1 each) = 10
    vec4 result = sum / 10.0;
    
    // Apply alpha
    result.a *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.2, 0.4, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
