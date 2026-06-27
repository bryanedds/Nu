#version 450 core

struct ShadowFrag
{
    vec3 eyeCenter;
    float lightShadowExponent;
};

layout(set = 0, binding = 1) buffer readonly ShadowFragBlock { ShadowFrag shadowFrag; };

layout(location = 0) out float depth;

layout(location = 0) in vec4 positionOut;

void main()
{
	depth = length(positionOut.xyz - shadowFrag.eyeCenter); // linear, world space depth
}
