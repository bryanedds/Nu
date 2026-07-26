#version 450 core

struct TerrainFrag
{
    int layersCount;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
};

layout(set = 0, binding = 1) uniform TerrainFragBlock { TerrainFrag terrainFrag; };

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // non-linear, screen space depth
	depths.y = exp(terrainFrag.lightShadowExponent * depths.x);
}
