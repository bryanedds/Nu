#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

struct SpriteFrag
{
    vec4 color;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
};

layout(binding = 1) uniform SpriteFragBlock
{
    SpriteFrag sprite;
} spriteFrag[];

layout(binding = 2) uniform sampler2D tex[];

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    SpriteFrag sprite = spriteFrag[drawId].sprite;
    frag = sprite.color * texture(tex[drawId], texCoords);
}
