#version 450 core

struct VectorPath
{
    mat4 modelViewProjection;
};

layout(binding = 0) buffer readonly VectorPathBlock
{
    VectorPath vectorPath;
} vectorPath;

layout (location = 0) in vec2 position;
layout (location = 1) in vec4 color;
layout (location = 0) out vec4 fragColor;

void main()
{
    gl_Position = vectorPath.vectorPath.modelViewProjection * vec4(position.x, position.y, 0, 1);
    fragColor = color;
}
