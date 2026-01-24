#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

layout(push_constant) uniform pc {
    int drawId;
};

layout(binding = 1) uniform sampler2D tex[];
layout(location = 0) in vec2 texCoords;
layout(location = 1) in vec4 color;
layout(location = 0) out vec4 frag;

void main()
{
    frag = color * texture(tex[drawId], texCoords);
}
