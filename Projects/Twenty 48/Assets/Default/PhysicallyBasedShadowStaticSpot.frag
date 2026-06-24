#version 450 core

struct ShadowFrag
{
    vec3 eyeCenter;
    float lightShadowExponent;
};

layout(set = 0, binding = 1) buffer readonly ShadowFragBlock { ShadowFrag shadowFrag; };

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // non-linear, screen space depth
	depths.y = exp(shadowFrag.lightShadowExponent * depths.x);
}
