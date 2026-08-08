#version 450 core

struct ChromaticAberrationStruct
{
    vec3 channelOffsets;
    vec2 focalPoint;
};

layout(set = 0, binding = 0) uniform ChromaticAberrationUniform { ChromaticAberrationStruct chromaticAberration; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    vec2 direction = texCoords - (chromaticAberration.focalPoint + vec2(0.5));
    frag = texture(sampler2D(inputTexture, inputSampler), texCoords);
    frag.r = texture(sampler2D(inputTexture, inputSampler), texCoords + direction * vec2(chromaticAberration.channelOffsets.r)).r;
    frag.g = texture(sampler2D(inputTexture, inputSampler), texCoords + direction * vec2(chromaticAberration.channelOffsets.g)).g;
    frag.b = texture(sampler2D(inputTexture, inputSampler), texCoords + direction * vec2(chromaticAberration.channelOffsets.b)).b;
}
