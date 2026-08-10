#version 450 core

const int VERTS = 6;
const int SPRITE_BATCH_SIZE = 192;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0),
        vec4(1.0, 1.0, 0.0, 0.0));

struct SpriteStruct
{
    vec4 perimeter;
    vec2 pivot;
    float rotation;
    vec4 texCoords;
    vec4 color;
};

struct ViewProjectionStruct
{
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform SpriteUniform { SpriteStruct sprites[SPRITE_BATCH_SIZE]; };
layout(set = 0, binding = 1) uniform ViewProjectionUniform { ViewProjectionStruct viewProjection; };

layout(location = 0) out vec2 texCoordsOut;
layout(location = 1) out vec4 colorOut;

vec2 rotate(vec2 v, float a)
{
    float s = sin(a);
    float c = cos(a);
    mat2 m = mat2(c, -s, s, c);
    return m * v;
}

void main()
{
    // compute ids
    int spriteId = gl_VertexIndex / VERTS;
    int vertexId = gl_VertexIndex % VERTS;

    // compute position
    vec4 filt = FILTERS[vertexId];
    SpriteStruct sprite = sprites[spriteId];
    vec4 perimeter = sprite.perimeter * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    vec2 pivot = sprite.pivot;
    vec2 positionRotated = rotate(position + pivot, sprite.rotation) - pivot;
    gl_Position = viewProjection.viewProjection * vec4(positionRotated.x, positionRotated.y, 0, 1);

    // compute tex coords
    vec4 texCoords4 = sprite.texCoords * filt;
    texCoordsOut = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);

    // compute color
    colorOut = sprite.color;
}
