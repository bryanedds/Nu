#version 450 core

struct SkyBoxFrag
{
    vec3 color;
    float brightness;
};

layout(binding = 1) uniform SkyBoxFragBlock
{
    SkyBoxFrag skyBox;
};

layout(binding = 2) uniform samplerCube cubeMap;

layout(location = 0) in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec4 color4 = vec4(skyBox.color, 1.0);
    frag = texture(cubeMap, texCoordsOut) * color4 * skyBox.brightness;
}
