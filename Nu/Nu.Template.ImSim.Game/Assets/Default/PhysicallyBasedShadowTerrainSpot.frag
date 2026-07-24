#version 450 core

struct Lighting3
{
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
    int layersCount;
};

layout(set = 0, binding = 1) uniform Lighting3Block { Lighting3 lighting; };

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // non-linear, screen space depth
	depths.y = exp(lighting.lightShadowExponent * depths.x);
}
