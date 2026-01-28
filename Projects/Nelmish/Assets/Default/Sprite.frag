#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

layout(push_constant) uniform PushConstant {
    int drawId;
};

layout(binding = 1) uniform sampler2D tex[];

layout(binding = 2) uniform SpriteFrag {
    vec4 color;
} spriteFrag[];

layout(location = 0) in vec2 texCoords;
layout(location = 0) out vec4 frag;

void main()
{
    frag = spriteFrag[drawId].color * texture(tex[drawId], texCoords);
}
