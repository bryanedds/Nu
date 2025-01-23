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

layout (binding = 0) uniform pr { vec4 perimeters[SPRITE_BATCH_SIZE]; };
layout (binding = 1) uniform pv { vec2 pivots[SPRITE_BATCH_SIZE]; };
layout (binding = 2) uniform r { float rotations[SPRITE_BATCH_SIZE]; };
layout (binding = 3) uniform tc { vec4 texCoordses[SPRITE_BATCH_SIZE]; };
layout (binding = 4) uniform c { vec4 colors[SPRITE_BATCH_SIZE]; };
layout (binding = 5) uniform vp { mat4 viewProjection; };
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
    vec4 perimeter = perimeters[spriteId] * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    vec2 pivot = pivots[spriteId];
    vec2 positionRotated = rotate(position + pivot, rotations[spriteId]) - pivot;
    vec4 prePosition = viewProjection * vec4(positionRotated.x, positionRotated.y, 0, 1);
    gl_Position = vec4(prePosition.x, -prePosition.y, prePosition.z, prePosition.w);

    // compute tex coords
    vec4 texCoords4 = texCoordses[spriteId] * filt;
    texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);

    // compute color
    color = colors[spriteId];
}
