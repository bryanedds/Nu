#version 450 core

const float PI = 3.141592654;
const int LIGHT_MAPS_MAX = 26;

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };
layout(set = 0, binding = 1) uniform texture2D depthTexture;
layout(set = 0, binding = 2) uniform texture2D normalPlusTexture;
layout(set = 0, binding = 3) uniform texture2D lightMappingTexture;
layout(set = 0, binding = 4) uniform texture2D irradianceMap;
layout(set = 0, binding = 5) uniform texture2D irradianceMaps[LIGHT_MAPS_MAX];

layout(set = 1, binding = 0) uniform sampler unfilteredSampler;
layout(set = 1, binding = 1) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    vec4 clip = vec4(texCoords * 2.0 - 1.0, 1.0, 1.0);
    vec4 rayView = eye.projectionInverse * clip;
    rayView /= rayView.w;
    vec3 positionView = rayView.xyz * (depth / -rayView.z);
    return eye.viewInverse * vec4(positionView, 1.0);
}

// The irradiance (divided by pi) in a direction from an irradiance map, the spherical harmonic coefficients (bands 0
// through 2) of a light map's irradiance, one per texel.
vec3 computeIrradiance(texture2D irradianceMap, vec3 direction)
{
    vec3 d = normalize(direction);
    vec3 irradiance =
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(0, 0), 0).rgb * 0.282095 +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(1, 0), 0).rgb * 0.488603 * d.y +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(2, 0), 0).rgb * 0.488603 * d.z +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(3, 0), 0).rgb * 0.488603 * d.x +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(4, 0), 0).rgb * 1.092548 * d.x * d.y +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(5, 0), 0).rgb * 1.092548 * d.y * d.z +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(6, 0), 0).rgb * 0.315392 * (3.0 * d.z * d.z - 1.0) +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(7, 0), 0).rgb * 1.092548 * d.x * d.z +
        texelFetch(sampler2D(irradianceMap, unfilteredSampler), ivec2(8, 0), 0).rgb * 0.546274 * (d.x * d.x - d.y * d.y);
    return max(irradiance, vec3(0.0));
}

void main()
{
    // ensure fragment was written
    float depth = texture(sampler2D(depthTexture, unfilteredSampler), texCoords).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoords);

    // retrieve remaining data from geometry buffers
    vec3 normal = normalize(texture(sampler2D(normalPlusTexture, unfilteredSampler), texCoords).xyz);

    // retrieve light mapping data
    vec4 lmData = texture(sampler2D(lightMappingTexture, unfilteredSampler), texCoords);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute irradiance
    vec3 irradiance = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        irradiance = computeIrradiance(irradianceMap, normal);
    }
    else if (lm2 == -1)
    {
        // compute blended irradiance
        vec3 irradiance1 = computeIrradiance(irradianceMaps[lm1], normal);
        vec3 irradiance2 = computeIrradiance(irradianceMap, normal);
        irradiance = mix(irradiance1, irradiance2, lmRatio);
    }
    else
    {
        // compute blended irradiance
        vec3 irradiance1 = computeIrradiance(irradianceMaps[lm1], normal);
        vec3 irradiance2 = computeIrradiance(irradianceMaps[lm2], normal);
        irradiance = mix(irradiance1, irradiance2, lmRatio);
    }

    // write
    frag = vec4(irradiance, 1.0);
}
