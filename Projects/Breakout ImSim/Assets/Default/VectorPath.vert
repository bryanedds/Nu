#version 450 core

layout (push_constant) uniform pc { int drawId; };
layout (location = 0) in vec2 position;
layout (location = 1) in vec4 color;
layout (binding = 0) uniform a { mat4 modelViewProjection; } modelViewProjection[];
layout (location = 0) out vec4 fragColor;

void main()
{
    gl_Position = modelViewProjection[drawId].modelViewProjection * vec4(position.x, position.y, 0, 1);
    fragColor = color;
}
