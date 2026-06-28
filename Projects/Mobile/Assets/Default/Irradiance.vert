#version 450 core

struct Eye
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };

layout(location = 0) in vec3 position;

layout(location = 0) out vec3 positionOut;

void main()
{
    positionOut = position;
    gl_Position = eye.viewProjection * vec4(positionOut, 1.0);
}
