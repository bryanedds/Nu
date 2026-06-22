#version 450 core

struct SkyBox
{
    vec3 color;
    float brightness;
};

layout(set = 0, binding = 1) buffer readonly SkyBoxBlock { SkyBox skyBox; };

layout(set = 1, binding = 0) uniform textureCube cubeMap;

layout(set = 2, binding = 0) uniform sampler samp;

layout(location = 0) in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec4 color4 = vec4(skyBox.color, 1.0);
    frag = texture(samplerCube(cubeMap, samp), texCoordsOut) * color4 * skyBox.brightness;
}
