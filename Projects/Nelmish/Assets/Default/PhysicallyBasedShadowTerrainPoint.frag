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

layout(location = 0) in vec4 position;

layout(location = 0) out float depth;

void main()
{
	depth = length(position.xyz - eye.center); // linear, world space depth
}
