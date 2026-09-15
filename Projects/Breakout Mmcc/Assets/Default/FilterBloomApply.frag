#version 450 core

struct BloomApplyStruct
{
    float strength;
};

layout(set = 0, binding = 0) uniform BloomApplyUniform { BloomApplyStruct bloomApply; };
layout(set = 0, binding = 1) uniform texture2D bloomFilterTexture;
layout(set = 0, binding = 2) uniform texture2D compositionTexture;

layout(set = 1, binding = 0) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 bloomBlurColor = texture(sampler2D(bloomFilterTexture, filteredSampler), texCoords).rgb;
    vec3 sceneColor = texture(sampler2D(compositionTexture, filteredSampler), texCoords).rgb;
    frag = vec4(mix(sceneColor, bloomBlurColor, bloomApply.strength), 0.0);
}
