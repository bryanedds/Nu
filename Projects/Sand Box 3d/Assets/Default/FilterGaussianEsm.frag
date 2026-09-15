#version 450 core

struct GaussianEsmStruct
{
    vec2 scale;
    float radius;
};

layout(set = 0, binding = 0) uniform GaussianEsmUniform { GaussianEsmStruct gaussianEsm; };

layout(set = 1, binding = 0) uniform texture2D inputTexture;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec2 frag;

void main()
{
    vec2 scalar =
        gaussianEsm.scale *
        gaussianEsm.radius;
    frag =
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2(-2.0) * scalar).xy * (1.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2(-1.0) * scalar).xy * (4.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 0.0) * scalar).xy * (6.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 1.0) * scalar).xy * (4.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 2.0) * scalar).xy * (1.0 / 16.0);
}
