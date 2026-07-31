#version 450 core

const float GAMMA = 2.2;
const int TERRAIN_LAYERS_MAX = 6;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as terrainFrag config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as terrainFrag config property.

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct TerrainFragStruct
{
    int layersCount;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(set = 0, binding = 1) uniform TerrainFragUniform { TerrainFragStruct terrainFrag; };

layout(set = 1, binding = 0) uniform texture2D albedoTextures[TERRAIN_LAYERS_MAX];
layout(set = 1, binding = 1) uniform texture2D roughnessTextures[TERRAIN_LAYERS_MAX];
layout(set = 1, binding = 2) uniform texture2D ambientOcclusionTextures[TERRAIN_LAYERS_MAX];
layout(set = 1, binding = 3) uniform texture2D normalTextures[TERRAIN_LAYERS_MAX];
layout(set = 1, binding = 4) uniform texture2D heightTextures[TERRAIN_LAYERS_MAX];

layout(set = 2, binding = 0) uniform sampler materialSampler;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec4 blends[2];
layout(location = 5) in vec3 tint;
layout(location = 6) flat in vec4 albedo;
layout(location = 7) flat in vec4 material;
layout(location = 8) flat in vec4 heightPlus;

layout(location = 0) out float depthOut;
layout(location = 1) out vec3 albedoOut;
layout(location = 2) out vec4 materialOut;
layout(location = 3) out vec4 normalPlusOut;
layout(location = 4) out vec4 subdermalPlusOut;
layout(location = 5) out vec4 scatterPlusOut;

vec3 decodeNormal(vec2 normalEncoded)
{
    vec2 xy = normalEncoded * 2.0 - 1.0;
    float z = sqrt(max(0.0, 1.0 - dot(xy, xy)));
    return normalize(vec3(xy, z));
}

void main()
{
    // compute spatial converters
    vec3 q1 = dFdx(position.xyz);
    vec3 q2 = dFdy(position.xyz);
    vec2 st1 = dFdx(texCoords);
    vec2 st2 = dFdy(texCoords);
    vec3 normal = normalize(normal);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    tangent = normalize(tangent - normal * dot(normal, tangent));
    binormal = cross(normal, tangent);
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute height blend, height, and ignore local light maps
    float heightBlend = 0.0;
    for (int i = 0; i < min(terrainFrag.layersCount, TERRAIN_LAYERS_MAX); ++i)
        heightBlend += texture(sampler2D(heightTextures[i], materialSampler), texCoords).r * blends[i/4][i%4];
    float height = heightBlend * heightPlus.x;

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eye.center;
    vec3 positionTangent = toTangent * position.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoords - parallax;

    // compute albedo and material blends
    vec4 albedoBlend = vec4(0.0);
    float roughnessBlend = 0.0;
    float ambientOcclusionBlend = 0.0;
    vec3 normalBlend = vec3(0.0);
    for (int i = 0; i < min(terrainFrag.layersCount, TERRAIN_LAYERS_MAX); ++i)
    {
        float blend = blends[i/4][i%4];
        albedoBlend += texture(sampler2D(albedoTextures[i], materialSampler), texCoords) * blend;
        vec4 roughness = texture(sampler2D(roughnessTextures[i], materialSampler), texCoords);
        roughnessBlend += (roughness.a == 1.0f ? roughness.r : roughness.a) * blend;
        ambientOcclusionBlend += texture(sampler2D(ambientOcclusionTextures[i], materialSampler), texCoords).b * blend;
        normalBlend += decodeNormal(texture(sampler2D(normalTextures[i], materialSampler), texCoords).xy) * blend;
    }

    // compute normal and ignore local height maps
    normalPlusOut.xyz = normalize(toWorld * normalize(normalBlend));
    normalPlusOut.w = heightPlus.y;

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = roughnessBlend;
    vec3 du = dFdx(normalPlusOut.xyz);
    vec3 dv = dFdy(normalPlusOut.xyz);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));

    // populate remaining outputs
    depthOut = gl_FragCoord.z;
    albedoOut = pow(albedoBlend.rgb, vec3(GAMMA)) * tint * albedo.rgb;
    materialOut = vec4(roughness * material.g, 0.0, ambientOcclusionBlend * material.b, 0.0);
    subdermalPlusOut = vec4(0.0);
    scatterPlusOut = vec4(0.0);
}
