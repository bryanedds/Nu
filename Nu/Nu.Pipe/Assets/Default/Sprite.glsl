#shader vertex
#version 410

const int VERTS = 4;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0));

in vec2 position;
uniform mat4 modelViewProjection;
uniform vec4 texCoords4;
out vec2 texCoords;
void main()
{
    int vertexId = gl_VertexID % VERTS;
    vec4 filt = FILTERS[vertexId];
    gl_Position = modelViewProjection * vec4(position.x, position.y, 0, 1);
    texCoords = vec2(texCoords4.x * filt.x + texCoords4.z * filt.z, texCoords4.y * filt.y + texCoords4.w * filt.w);
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require
layout (bindless_sampler) uniform sampler2D tex;
uniform vec4 color;
in vec2 texCoords;
layout (location = 0) out vec4 frag;
void main()
{
    frag = color * texture(tex, texCoords);
}
