#version 450 core

struct ShadowFragStruct
{
    vec3 eyeCenter;
    float lightShadowExponent;
};

layout(set = 0, binding = 1) uniform ShadowFragUniform { ShadowFragStruct shadowFrag; };

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // linear, screen space depth
	depths.y = exp(shadowFrag.lightShadowExponent * depths.x);
}
