#version 450 core

struct TerrainFragStruct
{
    int layersCount;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
};

layout(set = 0, binding = 1) uniform TerrainFragUniform { TerrainFragStruct terrainFrag; };

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // linear, screen space depth
	depths.y = exp(terrainFrag.lightShadowExponent * depths.x);
}
