#version 450 core

const float PI = 3.141592654;
const float GAMMA = 2.2;
const float DITHER_ENTROPY = 409.0;
const float DITHER_SIN_SCALAR = 1500.0;
const float DITHER_STRENGTH = 0.005;

struct ColorGradeStruct
{
    float colorGradeStrength;
};

layout(set = 0, binding = 0) uniform ColorGradeUniform { ColorGradeStruct colorGrade; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;
layout(set = 0, binding = 2) uniform texture2D colorGradeLutTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;
layout(set = 1, binding = 1) uniform sampler colorGradeLutSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

float computeDither()
{
    float seed = texCoords.x + texCoords.y * DITHER_ENTROPY;
    return fract(DITHER_SIN_SCALAR * sin(seed)) * DITHER_STRENGTH;
}

// Apply a color grade lut in the style of Naty Hoffman's 'Color Enhancement for Videogames' (SIGGRAPH 2010).
// The lut is an unwrapped 3d lut laid out as a horizontal strip of N slices of N x N texels each (so N^2 x N texels in
// total, such as 256 x 16 or 1024 x 32), where red increases to the right within each slice, green increases
// downward, and blue increases from slice to slice. The given color is expected to be display-encoded.
vec3 applyColorGradeLut(vec3 color)
{
    // ensure lut has strip dimensions (it may not when a lazily-loaded lut is not yet fully available)
    ivec2 lutSize = textureSize(sampler2D(colorGradeLutTexture, colorGradeLutSampler), 0);
    if (lutSize.y < 2 || lutSize.x != lutSize.y * lutSize.y) return color;

    // compute the two nearest blue slices and the blend between them
    float n = float(lutSize.y);
    color = clamp(color, 0.0, 1.0);
    float slice = color.b * (n - 1.0);
    float slice0 = floor(slice);
    float slice1 = min(slice0 + 1.0, n - 1.0);
    float sliceBlend = slice - slice0;

    // sample red and green bilinearly within each slice at texel centers, then blend slices for trilinear result
    vec2 texCoordsInner = (color.rg * (n - 1.0) + 0.5) / vec2(n * n, n);
    vec3 color0 = textureLod(sampler2D(colorGradeLutTexture, colorGradeLutSampler), texCoordsInner + vec2(slice0 / n, 0.0), 0.0).rgb;
    vec3 color1 = textureLod(sampler2D(colorGradeLutTexture, colorGradeLutSampler), texCoordsInner + vec2(slice1 / n, 0.0), 0.0).rgb;
    return mix(color0, color1, sliceBlend);
}

void main()
{
    // sample color
    vec3 color = texture(sampler2D(inputTexture, inputSampler), texCoords, 0).rgb;

    // apply gamma correction
    color = pow(color, vec3(1.0 / GAMMA));

    // apply color grade in display-encoded space since that's the space luts are authored in
    if (colorGrade.colorGradeStrength > 0.0)
        color = mix(color, applyColorGradeLut(color), colorGrade.colorGradeStrength);

    // apply dithering to make up for precision reduction from quantization
    // sourced from - https://lettier.github.io/3d-game-shaders-for-beginners/film-grain.html
    color.rgb += vec3(computeDither());

    // write fragment
    frag = vec4(color, 1.0);
}
