#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

layout(push_constant) uniform PushConstant {
    layout(offset = 80) vec4 color;
    int drawId;
};

layout(binding = 0) uniform sampler2D tex[];
layout(location = 0) in vec2 texCoords;
layout(location = 0) out vec4 frag;

void main()
{
    frag = color * texture(tex[drawId], texCoords);
}
