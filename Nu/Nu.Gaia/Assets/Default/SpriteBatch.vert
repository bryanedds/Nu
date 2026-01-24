#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

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

layout (push_constant) uniform pc {
    int drawId;
};

layout (binding = 0) uniform SpriteBatch {
    vec4 perimeters[SPRITE_BATCH_SIZE];
    vec2 pivots[SPRITE_BATCH_SIZE];
    float rotations[SPRITE_BATCH_SIZE];
    vec4 texCoordses[SPRITE_BATCH_SIZE];
    vec4 colors[SPRITE_BATCH_SIZE];
    mat4 viewProjection;
} sb[];

layout (location = 0) out vec2 texCoords;
layout (location = 1) out vec4 color;

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
    vec4 perimeter = sb[drawId].perimeters[spriteId] * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    vec2 pivot = sb[drawId].pivots[spriteId];
    vec2 positionRotated = rotate(position + pivot, sb[drawId].rotations[spriteId]) - pivot;
    gl_Position = sb[drawId].viewProjection * vec4(positionRotated.x, positionRotated.y, 0, 1);

    // compute tex coords
    vec4 texCoords4 = sb[drawId].texCoordses[spriteId] * filt;
    texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);

    // compute color
    color = sb[drawId].colors[spriteId];
}
