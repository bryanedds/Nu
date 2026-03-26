#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

struct SkyBoxVert
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
};

layout(binding = 0) buffer readonly SkyBoxVertBlock
{
    SkyBoxVert skyBox;
} skyBoxVert[];

layout(location = 0) in vec3 position;

layout(location = 0) out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (skyBoxVert[drawId].skyBox.viewProjection * vec4(position, 1.0)).xyww;
}
