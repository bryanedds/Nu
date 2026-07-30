#version 450 core

struct GaussianDofStruct
{
    vec2 scale;
    float radius;
};

layout(set = 0, binding = 0) uniform GaussianDofUniform { GaussianDofStruct gaussianDof; };

layout(set = 1, binding = 0) uniform texture2D inputTexture;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    vec2 scalar =
        gaussianDof.scale *
        gaussianDof.radius;
    frag =
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2(-2.0) * scalar) * (1.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2(-1.0) * scalar) * (4.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 0.0) * scalar) * (6.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 1.0) * scalar) * (4.0 / 16.0) +
        texture(sampler2D(inputTexture, inputSampler), texCoords + vec2( 2.0) * scalar) * (1.0 / 16.0);
}
