#version 450 core

struct SkyBoxVert
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

layout(binding = 0) uniform SkyBoxVertBlock
{
    SkyBoxVert skyBox;
};

layout(location = 0) in vec3 position;

layout(location = 0) out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (skyBox.viewProjection * vec4(position, 1.0)).xyww;
}
