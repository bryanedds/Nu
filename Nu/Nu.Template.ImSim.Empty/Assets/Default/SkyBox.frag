#version 450 core

layout (binding = 3) uniform a { vec3 color; } color;
layout (binding = 4) uniform b { float brightness; } brightness;
layout (binding = 5) uniform c { samplerCube cubeMap; } cubeMap;

layout (location = 0) in vec3 texCoordsOut;

layout (location = 0) out vec4 frag;

void main()
{
    vec4 color4 = vec4(color.color, 1.0);
    frag = texture(cubeMap.cubeMap, texCoordsOut) * color4 * brightness.brightness;
}
