#version 450 core

struct Transform
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

layout(binding = 0) buffer readonly TransformBlock
{
    Transform transform;
} transform;

layout(location = 0) in vec3 position;

layout(location = 0) out vec3 positionOut;

void main()
{
    positionOut = position;
    gl_Position = transform.transform.viewProjection * vec4(positionOut, 1.0);
}
