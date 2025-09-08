#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

const int VERTS = 4;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0));

layout (push_constant) uniform pc { int drawId; };
layout (location = 0) in vec2 position;
layout (binding = 0) uniform a { mat4 modelViewProjection; } modelViewProjection[];
layout (binding = 1) uniform b { vec4 texCoords4; } texCoords4[];
layout (location = 0) out vec2 texCoords;
void main()
{
    int vertexId = gl_VertexIndex % VERTS;
    vec4 filt = FILTERS[vertexId];
    vec4 prePosition = modelViewProjection[drawId].modelViewProjection * vec4(position.x, position.y, 0, 1);
    gl_Position = vec4(prePosition.x, -prePosition.y, prePosition.z, prePosition.w);
    texCoords = vec2(texCoords4[drawId].texCoords4.x * filt.x + texCoords4[drawId].texCoords4.z * filt.z, texCoords4[drawId].texCoords4.y * filt.y + texCoords4[drawId].texCoords4.w * filt.w);
}
