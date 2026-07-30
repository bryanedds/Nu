#version 450 core

layout(set = 0, binding = 0) uniform texture2D colorTexture;
layout(set = 0, binding = 1) uniform texture2D depthTexture;

layout(set = 1, binding = 0) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 colorOut;
layout(location = 1) out float depthOut;

void main()
{
    colorOut = texture(sampler2D(colorTexture, filteredSampler), texCoords);
    depthOut = texture(sampler2D(depthTexture, filteredSampler), texCoords).x;
}
