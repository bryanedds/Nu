#shader vertex
#version 410

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

uniform vec4 perimeters[SPRITE_BATCH_SIZE];
uniform vec2 pivots[SPRITE_BATCH_SIZE];
uniform float rotations[SPRITE_BATCH_SIZE];
uniform vec4 texCoordses[SPRITE_BATCH_SIZE];
uniform vec4 colors[SPRITE_BATCH_SIZE];
uniform mat4 viewProjection;
out vec2 texCoords;
out vec4 color;

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
    int spriteId = gl_VertexID / VERTS;
    int vertexId = gl_VertexID % VERTS;

    // compute position
    vec4 filt = FILTERS[vertexId];
    vec4 perimeter = perimeters[spriteId] * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    vec2 pivot = pivots[spriteId];
    vec2 positionRotated = rotate(position + pivot, rotations[spriteId]) - pivot;
    gl_Position = viewProjection * vec4(positionRotated.x, positionRotated.y, 0, 1);

    // compute tex coords
    vec4 texCoords4 = texCoordses[spriteId] * filt;
    texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);

    // compute color
    color = colors[spriteId];
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require
layout (bindless_sampler) uniform sampler2D tex;
in vec2 texCoords;
in vec4 color;
out vec4 frag;
void main()
{
    frag = color * texture(tex, texCoords);
}
