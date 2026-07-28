#version 450 core

layout(set = 1, binding = 0) uniform texture2D inputTexture;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;
layout(location = 1) in vec4 color;

layout(location = 0) out vec4 frag;

void main()
{
    frag = color * texture(sampler2D(inputTexture, inputSampler), texCoords);
}
