#version 450 core

struct GaussianEsm
{
    vec2 scale;
    float radius;
};

layout(set = 0, binding = 0) uniform GaussianEsmBlock { GaussianEsm gaussianEsm; };

layout(set = 1, binding = 0) uniform texture2D esmTexture;

layout(set = 2, binding = 0) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec2 frag;

void main()
{
    vec2 scalar =
        gaussianEsm.scale *
        gaussianEsm.radius;
    frag =
        texture(sampler2D(esmTexture, filteredSampler), texCoordsOut + vec2(-2.0) * scalar).xy * (1.0 / 16.0) +
        texture(sampler2D(esmTexture, filteredSampler), texCoordsOut + vec2(-1.0) * scalar).xy * (4.0 / 16.0) +
        texture(sampler2D(esmTexture, filteredSampler), texCoordsOut + vec2( 0.0) * scalar).xy * (6.0 / 16.0) +
        texture(sampler2D(esmTexture, filteredSampler), texCoordsOut + vec2( 1.0) * scalar).xy * (4.0 / 16.0) +
        texture(sampler2D(esmTexture, filteredSampler), texCoordsOut + vec2( 2.0) * scalar).xy * (1.0 / 16.0);
}
