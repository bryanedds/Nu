#version 450 core

layout(binding = 2) uniform texture2D tex;

layout(set = 1, binding = 0) uniform sampler samp;

layout(location = 0) in vec2 texCoords;
layout(location = 1) in vec4 color;

layout(location = 0) out vec4 frag;

void main()
{
    frag = color * texture(sampler2D(tex, samp), texCoords);
}
