#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

struct SkyBoxFrag
{
    vec3 color;
    float brightness;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
};

layout(binding = 1) uniform SkyBoxFragBlock
{
    SkyBoxFrag skyBox;
} skyBoxFrag[];

layout(binding = 2) uniform samplerCube cubeMap[];

layout(location = 0) in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    SkyBoxFrag skyBox = skyBoxFrag[drawId].skyBox;
    vec4 color4 = vec4(skyBox.color, 1.0);
    frag = texture(cubeMap[drawId], texCoordsOut) * color4 * skyBox.brightness;
}
