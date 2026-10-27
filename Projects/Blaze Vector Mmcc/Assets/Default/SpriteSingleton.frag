#version 450 core

struct SpriteFragStruct
{
    vec4 color;
};

layout(set = 0, binding = 1) uniform SpriteFragUniform { SpriteFragStruct sprite; };

layout(set = 1, binding = 0) uniform texture2D inputTexture;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    frag = sprite.color * texture(sampler2D(inputTexture, inputSampler), texCoords);
}
