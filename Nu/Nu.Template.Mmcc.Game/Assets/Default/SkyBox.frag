#version 450 core

const float GAMMA = 2.2;

struct SkyBox
{
    vec3 color;
    float brightness;
};

layout(set = 0, binding = 1) uniform SkyBoxBlock { SkyBox skyBox; };

layout(set = 1, binding = 0) uniform textureCube cubeMap;

layout(set = 2, binding = 0) uniform sampler samp;

layout(location = 0) in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 color = texture(samplerCube(cubeMap, samp), texCoordsOut).rgb * skyBox.color * skyBox.brightness;
    frag = vec4(pow(color, vec3(GAMMA)), 1.0); // NOTE: we approximately linearize color since we're not yet loading sky boxes from HDR.
}
