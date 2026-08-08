#version 450 core

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };

layout(location = 0) in vec3 position;
layout(location = 6) in mat4 model;

layout(location = 0) out vec4 positionOut;

void main()
{
	positionOut = model * vec4(position, 1.0);
	gl_Position = eye.viewProjection * positionOut;
}
