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

layout(binding = 1) buffer readonly SpriteFragBlock
{
    SpriteFrag sprite;
} spriteFrag[];

layout(binding = 2) uniform texture2D tex[];

layout(set = 1, binding = 0) uniform sampler samp;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    SpriteFrag sprite = spriteFrag[drawId].sprite;
    frag = sprite.color * texture(sampler2D(tex[drawId], samp), texCoords);
}
