#version 450 core

struct ShadowFragStruct
{
    vec3 eyeCenter;
    float lightShadowExponent;
};

layout(set = 0, binding = 1) uniform ShadowFragUniform { ShadowFragStruct shadowFrag; };

layout(location = 0) in vec4 positionOut;

layout(location = 0) out float depth;

void main()
{
	depth = length(positionOut.xyz - shadowFrag.eyeCenter); // linear, world space depth
}
