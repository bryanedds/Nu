#version 450 core

layout (binding = 0) uniform a { mat4 view; } view;
layout (binding = 1) uniform b { mat4 projection; } projection;
layout (binding = 2) uniform c { mat4 viewProjection; } viewProjection;

layout (location = 0) in vec3 position;

layout (location = 0) out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (viewProjection.viewProjection * vec4(position, 1.0)).xyww;
}
