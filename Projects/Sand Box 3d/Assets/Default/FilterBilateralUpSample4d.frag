#version 450 core

const float SHARPNESS = 0.05;

layout(set = 0, binding = 0) uniform texture2D downSampledColorTexture;
layout(set = 0, binding = 1) uniform texture2D downSampledDepthTexture;
layout(set = 0, binding = 2) uniform texture2D depthTexture;

layout(set = 1, binding = 0) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    // compute up-sampled texture size
    vec2 texelSize = vec2(1.0) / textureSize(sampler2D(depthTexture, filteredSampler), 0).xy;
    vec2 offsets[4] =
        vec2[4](
            vec2(-texelSize.x, texelSize.y),
            texelSize,
            vec2(texelSize.x, -texelSize.y),
            -texelSize);

    // compute up-sampled color sum and weight
    float depth = texture(sampler2D(depthTexture, filteredSampler), texCoords, 0).x;
    vec4 colorSum = vec4(0.0);
    float weight = 0.0;
    for (int i = 0; i < 4; ++i)
    {
        vec4 colorDownSampled = texture(sampler2D(downSampledColorTexture, filteredSampler), texCoords + offsets[i], 0);
        float depthDownSampled = texture(sampler2D(downSampledDepthTexture, filteredSampler), texCoords + offsets[i], 0).x;
        float weightDownSampled = max(0.0, 1.0 - abs(depthDownSampled - depth) * SHARPNESS);
        colorSum += colorDownSampled * weightDownSampled;
        weight += weightDownSampled;
    }

    // write color when weighted
    if (weight > 0.0) frag = colorSum / weight;
}
