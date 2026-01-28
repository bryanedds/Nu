#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

const int VERTS = 4;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0));

layout(push_constant) uniform PushConstant {
    int drawId;
};

layout(binding = 0) uniform TransformTexCoords {
    mat4 modelViewProjection;
    vec4 texCoords4;
} ttc[];

layout(location = 0) in vec2 position;
layout(location = 0) out vec2 texCoords;

void main()
{
    int vertexId = gl_VertexIndex % VERTS;
    vec4 filt = FILTERS[vertexId];
    gl_Position = ttc[drawId].modelViewProjection * vec4(position.x, position.y, 0, 1);
    texCoords = vec2(ttc[drawId].texCoords4.x * filt.x + ttc[drawId].texCoords4.z * filt.z, ttc[drawId].texCoords4.y * filt.y + ttc[drawId].texCoords4.w * filt.w);
}
