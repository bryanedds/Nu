#version 450 core

const int VERTS = 4;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0));

struct SpriteVert
{
    mat4 modelViewProjection;
    vec4 texCoords4;
};

layout(binding = 0) buffer readonly SpriteVertBlock
{
    SpriteVert sprite;
} spriteVert;

layout(location = 0) in vec2 position;

layout(location = 0) out vec2 texCoords;

void main()
{
    int vertexId = gl_VertexIndex % VERTS;
    vec4 filt = FILTERS[vertexId];
    SpriteVert sprite = spriteVert.sprite;
    gl_Position = sprite.modelViewProjection * vec4(position.x, position.y, 0, 1);
    texCoords = vec2(sprite.texCoords4.x * filt.x + sprite.texCoords4.z * filt.z, sprite.texCoords4.y * filt.y + sprite.texCoords4.w * filt.w);
}
