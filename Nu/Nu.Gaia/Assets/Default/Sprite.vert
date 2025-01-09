#shader vertex
#version 410

const int VERTS = 4;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0));

layout (location = 0) in vec2 position;

layout (binding = 0) uniform Mvp {
    mat4 modelViewProjection;
} mvp;

layout (binding = 1) uniform Tc4 {
    vec4 texCoords4;
} tc4;

layout (location = 0) out vec2 texCoords;
void main()
{
    int vertexId = gl_VertexID % VERTS;
    vec4 filt = FILTERS[vertexId];
    gl_Position = mvp.modelViewProjection * vec4(position.x, position.y, 0, 1);
    texCoords = vec2(tc4.texCoords4.x * filt.x + tc4.texCoords4.z * filt.z, tc4.texCoords4.y * filt.y + tc4.texCoords4.w * filt.w);
}
