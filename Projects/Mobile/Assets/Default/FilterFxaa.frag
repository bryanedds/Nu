#version 450 core

struct FxaaStruct
{
    float spanMax;
    float reduceMinDivisor;
    float reduceMulDivisor;
};

layout(set = 0, binding = 0) uniform FxaaUniform { FxaaStruct fxaa; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    // compute required reciprocals
    const float reduceMin = 1.0 / fxaa.reduceMinDivisor;
    const float reduceMul = 1.0 / fxaa.reduceMulDivisor;

    // compute texel size
    vec2 texelSize = 1.0 / textureSize(sampler2D(inputTexture, inputSampler), 0).xy;

    // compute luminosity values
    vec3 lum = vec3(0.299, 0.587, 0.114);
    float lumTL = dot(lum, texture(sampler2D(inputTexture, inputSampler), vec2(-1.0, -1.0) * texelSize + texCoords.xy).xyz);
    float lumTR = dot(lum, texture(sampler2D(inputTexture, inputSampler), vec2(+1.0, -1.0) * texelSize + texCoords.xy).xyz);
    float lumBL = dot(lum, texture(sampler2D(inputTexture, inputSampler), vec2(-1.0, +1.0) * texelSize + texCoords.xy).xyz);
    float lumBR = dot(lum, texture(sampler2D(inputTexture, inputSampler), vec2(+1.0, +1.0) * texelSize + texCoords.xy).xyz);
    float lumCC = dot(lum, texture(sampler2D(inputTexture, inputSampler), texCoords.xy).xyz);

    // compute blur direction
    vec2 dir;
    dir.x = -((lumTL + lumTR) - (lumBL + lumBR));
    dir.y = +((lumTL + lumBL) - (lumTR + lumBR));
    float dirReduce = max((lumTL + lumTR + lumBL + lumBR) * reduceMul * 0.25, reduceMin);
    float inverseDirAdjustment = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(vec2(fxaa.spanMax), max(vec2(-fxaa.spanMax), dir * inverseDirAdjustment)) * texelSize;

    // sample the texture in two locations along the computed direction to create an initial blurred color
    vec3 result1 = 0.5 * (
        texture(sampler2D(inputTexture, inputSampler), dir * vec2(1.0 / 3.0 - 0.5) + texCoords.xy).xyz +
        texture(sampler2D(inputTexture, inputSampler), dir * vec2(2.0 / 3.0 - 0.5) + texCoords.xy).xyz);

    // sample the texture at additional points and blend them with the initial blur to refine the result
    vec3 result2 = result1 * 0.5 + 0.25 * (
        texture(sampler2D(inputTexture, inputSampler), dir * vec2(0.0 / 3.0 - 0.5) + texCoords.xy).xyz +
        texture(sampler2D(inputTexture, inputSampler), dir * vec2(3.0 / 3.0 - 0.5) + texCoords.xy).xyz);

    // compute the minimum and maximum luminosity of the surrounding texels to use for edge detection
    float lumMin = min(lumCC, min(min(lumTL, lumTR), min(lumBL, lumBR)));
    float lumMax = max(lumCC, max(max(lumTL, lumTR), max(lumBL, lumBR)));
    float lumResult2 = dot(lum, result2);

    // write    
    frag =
        lumResult2 < lumMin || lumResult2 > lumMax ?
        vec4(result1, 1.0) :
        vec4(result2, 1.0);
}
