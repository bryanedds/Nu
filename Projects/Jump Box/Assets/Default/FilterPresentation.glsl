#shader vertex
#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 460 core

const float GAMMA = 2.2;

uniform float lightExposure;
uniform int toneMapType;
uniform vec3 toneMapSlope;
uniform vec3 toneMapOffset;
uniform vec3 toneMapPower;
uniform float toneMapSaturation;
uniform float toneMapWhitePoint;
uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

vec3 applyAgXToneMap(vec3 color, vec3 slope, vec3 offset, vec3 power, float saturation)
{
    const mat3 LINEAR_REC2020_TO_LINEAR_SRGB = mat3(
        1.6605, -0.1246, -0.0182,
        -0.5876, 1.1329, -0.1006,
        -0.0728, -0.0083, 1.1187);

    const mat3 LINEAR_SRGB_TO_LINEAR_REC2020 = mat3(
        0.6274, 0.0691, 0.0164,
        0.3293, 0.9195, 0.0880,
        0.0433, 0.0113, 0.8956);

    // Converted to column major from blender: https://github.com/blender/blender/blob/fc08f7491e7eba994d86b610e5ec757f9c62ac81/release/datafiles/colormanagement/config.ocio#L358
    const mat3 AgXInsetMatrix = mat3(
        0.856627153315983, 0.137318972929847, 0.11189821299995,
        0.0951212405381588, 0.761241990602591, 0.0767994186031903,
        0.0482516061458583, 0.101439036467562, 0.811302368396859);

    // Converted to column major and inverted from https://github.com/EaryChow/AgX_LUT_Gen/blob/ab7415eca3cbeb14fd55deb1de6d7b2d699a1bb9/AgXBaseRec2020.py#L25
    // https://github.com/google/filament/blob/bac8e58ee7009db4d348875d274daf4dd78a3bd1/filament/src/ToneMapper.cpp#L273-L278
    const mat3 AgXOutsetMatrix = mat3(
        1.1271005818144368, -0.1413297634984383, -0.14132976349843826,
        -0.11060664309660323, 1.157823702216272, -0.11060664309660294,
        -0.016493938717834573, -0.016493938717834257, 1.2519364065950405);

    const float AgxMinEv = -12.47393;
    const float AgxMaxEv = 4.026069;

    color = LINEAR_SRGB_TO_LINEAR_REC2020 * color; // From three.js

    // 1. agx()
    // Input transform (inset)
    color = AgXInsetMatrix * color;

    color = max(color, 1e-10); // From Filament: avoid 0 or negative numbers for log2

    // Log2 space encoding
    color = clamp(log2(color), AgxMinEv, AgxMaxEv);
    color = (color - AgxMinEv) / (AgxMaxEv - AgxMinEv);

    color = clamp(color, 0.0, 1.0); // From Filament

    // Apply sigmoid function approximation
    // Mean error^2: 3.6705141e-06
    vec3 x2 = color * color;
    vec3 x4 = x2 * x2;
    color = + 15.5     * x4 * x2
            - 40.14    * x4 * color
            + 31.96    * x4
            - 6.868    * x2 * color
            + 0.4298   * x2
            + 0.1191   * color
            - 0.00232;

    // 2. agxLook()
    color = pow(color * slope + offset, power);
    const vec3 lw = vec3(0.2126, 0.7152, 0.0722);
    float luma = dot(color, lw);
    color = luma + saturation * (color - luma);

    // 3. agxEotf()
    // Inverse input transform (outset)
    color = AgXOutsetMatrix * color;

    // sRGB IEC 61966-2-1 2.2 Exponent Reference EOTF Display
    // NOTE: We're linearizing the output here. Comment/adjust when
    // *not* using a sRGB render target
    color = pow(max(vec3(0.0), color), vec3(2.2)); // From filament: max()

    color = LINEAR_REC2020_TO_LINEAR_SRGB * color; // From three.js
    // Gamut mapping. Simple clamp for now.
    color = clamp(color, 0.0, 1.0);

    return color;
}

vec3 applyReinhardToneMap(vec3 x)
{
    return x / (1.0 + x);
}

vec3 applyReinhardExtendedToneMap(vec3 x, float whitePoint)
{
    return (x * (1.0 + x / (whitePoint * whitePoint))) / (1.0 + x);
}

vec3 applyUnrealToneMap(vec3 x)
{
    return x / (x + 0.155) * 1.019;
}

vec3 applyAcesFilmicToneMap(vec3 x)
{
    const float a = 2.51;
    const float b = 0.03;
    const float c = 2.43;
    const float d = 0.59;
    const float e = 0.14;
    return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

vec3 applyAcesFittedToneMap(vec3 color)
{
    // sRGB => XYZ => D65_2_D60 => AP1 => RRT_SAT
    const mat3 ACESInputMat =
    {
        {0.59719, 0.35458, 0.04823},
        {0.07600, 0.90834, 0.01566},
        {0.02840, 0.13383, 0.83777}
    };

    // ODT_SAT => XYZ => D60_2_D65 => sRGB
    const mat3 ACESOutputMat =
    {
        { 1.60475, -0.53108, -0.07367},
        {-0.10208,  1.10813, -0.00605},
        {-0.00327, -0.07276,  1.07602}
    };

    color = color * ACESInputMat;

    // Apply RRT and ODT
    vec3 a = color * (color + 0.0245786f) - 0.000090537f;
    vec3 b = color * (0.983729f * color + 0.4329510f) + 0.238081f;
    color = a / b;

    color = color * ACESOutputMat;

    color = clamp(color, vec3(0.0), vec3(1.0));

    return color;
}

vec3 applyUncharted2ToneMap(vec3 x)
{
    const float A = 0.15;
    const float B = 0.50;
    const float C = 0.10;
    const float D = 0.20;
    const float E = 0.02;
    const float F = 0.30;
    const float W = 11.2; // white point
    vec3 curr = ((x*(A*x+C*B)+D*E)/(x*(A*x+B)+D*F)) - E/F;
    vec3 whiteScale = ((vec3(W)*(A*vec3(W)+C*B)+D*E)/(vec3(W)*(A*vec3(W)+B)+D*F)) - E/F;
    return curr / whiteScale;
}

vec3 applyUncharted2FilmicToneMap(vec3 x)
{
    vec3 X = max(vec3(0.0), x - 0.004);
    vec3 result = (X * (6.2 * X + 0.5)) / (X * (6.2 * X + 1.7) + 0.06);
    return pow(result, vec3(2.2));
}

vec3 applyLottesToneMap(vec3 x)
{
    const vec3 a = vec3(1.6);
    const vec3 d = vec3(0.977);
    const vec3 hdrMax = vec3(8.0);
    const vec3 midIn = vec3(0.18);
    const vec3 midOut = vec3(0.267);

    const vec3 b =
        (-pow(midIn, a) + pow(hdrMax, a) * midOut) /
        ((pow(hdrMax, a * d) - pow(midIn, a * d)) * midOut);

    const vec3 c =
        (pow(hdrMax, a * d) * pow(midIn, a) - pow(hdrMax, a) * pow(midIn, a * d) * midOut) /
        ((pow(hdrMax, a * d) - pow(midIn, a * d)) * midOut);

    return pow(x, a) / (pow(x, a * d) * b + c);
}

vec3 applyKronosNeutralToneMap(vec3 color)
{
    const float startCompression = 0.8 - 0.04;
    const float desaturation = 0.15;

    float x = min(color.r, min(color.g, color.b));
    float offset = x < 0.08 ? x - 6.25 * x * x : 0.04;
    color -= offset;

    float peak = max(color.r, max(color.g, color.b));
    if (peak < startCompression) return color;

    const float d = 1.0 - startCompression;
    float newPeak = 1.0 - d * d / (peak + d - startCompression);
    color *= newPeak / peak;

    float g = 1.0 - 1.0 / (desaturation * (peak - newPeak) + 1.0);
    return mix(color, vec3(newPeak), g);
}

void main()
{
    // apply tone mapping
    vec3 color = texture(inputTexture, texCoordsOut, 0).xyz;
    switch (toneMapType)
    {
        case 0: color = applyAgXToneMap(color * lightExposure, toneMapSlope, toneMapOffset, toneMapPower, toneMapSaturation); break;
        case 1: color = applyReinhardToneMap(color * lightExposure); break;
        case 2: color = applyReinhardExtendedToneMap(color * lightExposure, toneMapWhitePoint); break;
        case 3: color = applyUnrealToneMap(color * lightExposure); break;
        case 4: color = applyAcesFittedToneMap(color * lightExposure); break;
        case 5: color = applyAcesFilmicToneMap(color * lightExposure); break;
        case 6: color = applyUncharted2ToneMap(color * lightExposure); break;
        case 7: color = applyUncharted2FilmicToneMap(color * lightExposure); break;
        case 8: color = applyLottesToneMap(color * lightExposure); break;
        default: color = applyKronosNeutralToneMap(color * lightExposure); break;
    }

    // apply gamma correction
    color = pow(color, vec3(1.0 / GAMMA));

    // fin
    frag = vec4(color, 1.0);
}
