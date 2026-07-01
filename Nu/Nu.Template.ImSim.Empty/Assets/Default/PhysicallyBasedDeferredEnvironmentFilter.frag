#version 450 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const int LIGHT_MAPS_MAX = 26;

struct Eye
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct LightMap
{
    vec3 lightMapOrigins;
    vec3 lightMapMins;
    vec3 lightMapSizes;
    vec3 lightMapAmbientColors;
    float lightMapAmbientBrightnesses;
};

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) buffer readonly LightMapsBlock { LightMap lightMaps[LIGHT_MAPS_MAX]; };
layout(set = 0, binding = 2) uniform texture2D depthTexture;
layout(set = 0, binding = 3) uniform texture2D materialTexture;
layout(set = 0, binding = 4) uniform texture2D normalPlusTexture;
layout(set = 0, binding = 5) uniform texture2D clearCoatPlusTexture;
layout(set = 0, binding = 6) uniform texture2D lightMappingTexture;
layout(set = 0, binding = 7) uniform textureCube environmentFilterMap;
layout(set = 0, binding = 8) uniform textureCube environmentFilterMaps[LIGHT_MAPS_MAX];

layout(set = 1, binding = 0) uniform sampler colorSampler;
layout(set = 1, binding = 1) uniform sampler environmentFilterSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

float signNotZero(float f)
{
    return f >= 0.0 ? 1.0 : -1.0;
}

vec2 signNotZero(vec2 v)
{
    return vec2(signNotZero(v.x), signNotZero(v.y));
}

vec3 decodeOctahedral(vec2 o)
{
    vec3 v = vec3(o.x, o.y, 1.0 - abs(o.x) - abs(o.y));
    if (v.z < 0.0) v.xy = (1.0 - abs(v.yx)) * signNotZero(v.xy);
    return normalize(v);
}

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

vec3 parallaxCorrection(vec3 lightMapOrigin, vec3 lightMapMin, vec3 lightMapSize, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eye.center;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMapMin + lightMapSize - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMapMin - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMapOrigin;
}

vec3 computeEnvironmentFilter(vec4 position, vec3 normal, float roughness, vec4 lmData)
{
    // retrieve light mapping data
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute environment filter
    vec3 v = normalize(eye.center - position.xyz);
    vec3 environmentFilter = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        vec3 r = reflect(-v, normal);
        environmentFilter = textureLod(samplerCube(environmentFilterMap, environmentFilterSampler), r, roughness * REFLECTION_LOD_MAX).rgb;
    }
    else if (lm2 == -1)
    {
        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMaps[lm1].lightMapOrigins, lightMaps[lm1].lightMapMins, lightMaps[lm1].lightMapSizes, position.xyz, normal);
        vec3 r2 = reflect(-v, normal);
        vec3 environmentFilter1 = textureLod(samplerCube(environmentFilterMaps[lm1], environmentFilterSampler), r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(samplerCube(environmentFilterMap, environmentFilterSampler), r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, lmRatio);
    }
    else
    {
        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMaps[lm1].lightMapOrigins, lightMaps[lm1].lightMapMins, lightMaps[lm1].lightMapSizes, position.xyz, normal);
        vec3 r2 = parallaxCorrection(lightMaps[lm2].lightMapOrigins, lightMaps[lm2].lightMapMins, lightMaps[lm2].lightMapSizes, position.xyz, normal);
        vec3 environmentFilter1 = textureLod(samplerCube(environmentFilterMaps[lm1], environmentFilterSampler), r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(samplerCube(environmentFilterMaps[lm2], environmentFilterSampler), r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, lmRatio);
    }

    // fin
    return environmentFilter;
}

void main()
{
    // ensure fragment was written
    float depth = texture(sampler2D(depthTexture, colorSampler), texCoordsOut).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoordsOut);

    // retrieve remaining data from geometry buffers
    float roughness = texture(sampler2D(materialTexture, colorSampler), texCoordsOut).r;
    vec3 normal = normalize(texture(sampler2D(normalPlusTexture, colorSampler), texCoordsOut).xyz);
    vec4 clearCoatPlus = texture(sampler2D(clearCoatPlusTexture, colorSampler), texCoordsOut);
    float clearCoat = clearCoatPlus.r;
    float clearCoatRoughness = clearCoatPlus.g;
    vec3 clearCoatNormal = decodeOctahedral(clearCoatPlus.ba);

    // compute environment filters
    vec4 lmData = texture(sampler2D(lightMappingTexture, colorSampler), texCoordsOut);
    vec3 environmentFilter = computeEnvironmentFilter(position, normal, roughness, lmData);
    vec3 clearCoatEnvironmentFilter = computeEnvironmentFilter(position, clearCoatNormal, clearCoatRoughness, lmData);
    environmentFilter = mix(environmentFilter, clearCoatEnvironmentFilter, clearCoat);

    // write
    frag = vec4(environmentFilter, 1.0);
}
