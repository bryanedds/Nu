#version 450 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0f;
const float ENVIRONMENT_FILTER_REFRACTED_SATURATION = 2.0;
const int LIGHT_MAPS_MAX = 2;
const int LIGHTS_MAX = 9;
const int SHADOW_TEXTURES_MAX = 12;
const int SHADOW_MAPS_MAX = 12;
const float SHADOW_DIRECTIONAL_SEAM_INSET = 0.05; // TODO: see if this should be proportionate to shadow texel size.
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;
const float SHADOW_CASCADE_SEAM_INSET = 0.001;
const float SHADOW_CASCADE_DENSITY_BONUS = 0.5;
const float SHADOW_FOV_MAX = 2.1;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as lighting config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as lighting config property.

const vec4 SSVF_DITHERING[4] =
    vec4[4](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct LightingStruct
{
    float lightCutoffMargin;
    vec3 lightAmbientColor;
    float lightAmbientBrightness;
    float lightAmbientBoostCutoff;
    float lightAmbientBoostScalar;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
    int fogEnabled;
    int fogType;
    float fogStart;
    float fogFinish;
    float fogDensity;
    vec4 fogColor;
    int ssvfEnabled;
    float ssvfIntensity;
    int ssvfSteps;
    float ssvfAsymmetry;
    int ssrrEnabled;
    float ssrrIntensity;
    float ssrrDetail;
    int ssrrRefinementsMax;
    float ssrrRayThickness;
    float ssrrDistanceCutoff;
    float ssrrDistanceCutoffMargin;
    float ssrrEdgeHorizontalMargin;
    float ssrrEdgeVerticalMargin;
    int ssrlEnabled;
    float ssrlIntensity;
    float ssrlDetail;
    int ssrlRefinementsMax;
    float ssrlRayThickness;
    float ssrlTowardEyeCutoff;
    float ssrlDepthCutoff;
    float ssrlDepthCutoffMargin;
    float ssrlDistanceCutoff;
    float ssrlDistanceCutoffMargin;
    float ssrlRoughnessCutoff;
    float ssrlRoughnessCutoffMargin;
    float ssrlSlopeCutoff;
    float ssrlSlopeCutoffMargin;
    float ssrlEdgeHorizontalMargin;
    float ssrlEdgeVerticalMargin;
    float shadowNear;
};

struct LightMapStruct
{
    vec3 origin;
    vec3 min;
    vec3 size;
    vec3 ambientColor;
    float ambientBrightness;
};

struct LightsGeneralStruct
{
    int lightMapsCount;
    float lightMapSingletonBlendMargin;
    int lightsCount;
};

struct LightStruct
{
    vec3 origin;
    vec3 direction;
    vec3 color;
    float brightness;
    float attenuationLinear;
    float attenuationQuadratic;
    float cutoff;
    int lightType;
    float coneInner;
    float coneOuter;
    int desireFog;
    int shadowIndex;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(set = 0, binding = 1) uniform LightingUniform { LightingStruct lighting; };
layout(set = 0, binding = 2) uniform texture2D depthTexture;
layout(set = 0, binding = 3) uniform texture2D colorTexture;
layout(set = 0, binding = 4) uniform texture2D brdfTexture;
layout(set = 0, binding = 5) uniform textureCube irradianceMap;
layout(set = 0, binding = 6) uniform textureCube environmentFilterMap;

layout(set = 1, binding = 0) uniform texture2D albedoTexture;
layout(set = 1, binding = 1) uniform texture2D roughnessTexture;
layout(set = 1, binding = 2) uniform texture2D metallicTexture;
layout(set = 1, binding = 3) uniform texture2D ambientOcclusionTexture;
layout(set = 1, binding = 4) uniform texture2D emissionTexture;
layout(set = 1, binding = 5) uniform texture2D normalTexture;
layout(set = 1, binding = 6) uniform texture2D heightTexture;

layout(set = 2, binding = 1) uniform LightMapUniform { LightMapStruct lightMaps[LIGHT_MAPS_MAX]; };
layout(set = 2, binding = 2) uniform LightsGeneralUniform { LightsGeneralStruct lightsGeneral; };
layout(set = 2, binding = 3) uniform LightUniform { LightStruct lights[LIGHTS_MAX]; };
layout(set = 2, binding = 4) uniform ShadowMatricesUniform { mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS]; };
layout(set = 2, binding = 5) uniform textureCube irradianceMaps[LIGHT_MAPS_MAX];
layout(set = 2, binding = 6) uniform textureCube environmentFilterMaps[LIGHT_MAPS_MAX];
layout(set = 2, binding = 7) uniform texture2DArray shadowTextures;
layout(set = 2, binding = 8) uniform textureCube shadowMaps[SHADOW_MAPS_MAX];
layout(set = 2, binding = 9) uniform texture2DArray shadowCascades[SHADOW_CASCADES_MAX];

layout(set = 3, binding = 0) uniform sampler unfilteredSampler;
layout(set = 3, binding = 1) uniform sampler filteredSampler;
layout(set = 3, binding = 2) uniform sampler materialSampler;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) flat in vec4 albedo;
layout(location = 4) flat in vec4 material;
layout(location = 5) flat in vec4 heightPlus;
layout(location = 6) flat in vec4 subsurfacePlus;

layout(location = 0) out vec4 frag;

float saturate(float v)
{
    return clamp(v, 0.0f, 1.0);
}

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

vec3 saturate(vec3 color, float boost)
{
    float luma = dot(color, vec3(0.2126, 0.7152, 0.0722)); // compute perceived luminance (Rec. 709)
    return mix(vec3(luma), color, boost); // interpolate between grayscale and original color
}

vec3 decodeNormal(vec2 normalEncoded)
{
    vec2 xy = normalEncoded * 2.0 - 1.0;
    float z = sqrt(max(0.0, 1.0 - dot(xy, xy)));
    return normalize(vec3(xy, z));
}

bool inBounds(vec3 point, vec3 min, vec3 size)
{
    return
        all(greaterThanEqual(point, min)) &&
        all(lessThanEqual(point, min + size));
}

vec4 depthToPosition(float depth, vec2 texCoords)
{
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, depth, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

float distanceToOutside(vec3 point, vec3 boxMin, vec3 boxSize)
{
    vec3 boxMax = boxMin + boxSize;
    float dx = min(point.x - boxMin.x, boxMax.x - point.x);
    float dy = min(point.y - boxMin.y, boxMax.y - point.y);
    float dz = min(point.z - boxMin.z, boxMax.z - point.z);
    return min(dx, min(dy, dz));
}

vec2 rayBoxIntersectionRatios(vec3 rayOrigin, vec3 rayDirection, vec3 boxMin, vec3 boxSize)
{
    vec3 rayDirectionInv = vec3(1.0) / rayDirection;
    vec3 boxMax = boxMin + boxSize;
    vec3 t1 = (boxMin - rayOrigin) * rayDirectionInv;
    vec3 t2 = (boxMax - rayOrigin) * rayDirectionInv;
    vec3 tMin = min(t1, t2);
    vec3 tMax = max(t1, t2);
    float tEnter = max(max(tMin.x / boxSize.x, tMin.y / boxSize.y), tMin.z / boxSize.z);
    float tExit = min(min(tMax.x / boxSize.x, tMax.y / boxSize.y), tMax.z / boxSize.z);
    return tEnter < tExit ? vec2(tEnter, tExit) : vec2(0.0);
}

float distributionGGX(vec3 normal, vec3 h, float roughness)
{
    float a = roughness * roughness;
    float aPow2 = a * a;
    float nDotH = saturate(dot(normal, h));
    float nDotHPow2 = nDotH * nDotH;
    float nom = aPow2;
    float denom = nDotHPow2 * (aPow2 - 1.0) + 1.0;
    denom = PI * denom * denom;
    return nom / denom;
}

float geometrySchlickGGX(float nDotV, float roughness)
{
    float r = roughness + 1.0;
    float k = r * r / 8.0;
    float nom = nDotV;
    float denom = nDotV * (1.0 - k) + k;
    return nom / denom;
}

float geometrySchlick(vec3 normal, vec3 v, vec3 l, float roughness)
{
    float nDotV = saturate(dot(normal, v));
    float nDotL = saturate(dot(normal, l));
    float ggx2 = geometrySchlickGGX(nDotV, roughness);
    float ggx1 = geometrySchlickGGX(nDotL, roughness);
    return ggx1 * ggx2;
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

float computeDepthRatio(vec3 minA, vec3 sizeA, vec3 minB, vec3 sizeB, vec3 position, vec3 normal)
{
    vec3 centerA = minA + sizeA * 0.5;
    vec3 centerB = minB + sizeB * 0.5;
    vec3 direction = normalize(cross(cross(centerB - centerA, normal), normal));
    vec3 intersectionMin = max(minA, minB);
    vec3 intersectionSize = min(minA + sizeA, minB + sizeB) - intersectionMin;
    vec2 intersectionRatios = rayBoxIntersectionRatios(position, direction, intersectionMin, intersectionSize);
    return intersectionRatios != vec2(0.0) ? intersectionRatios.y / (intersectionRatios.y - intersectionRatios.x) : 0.5;
}

vec3 parallaxCorrection(LightMapStruct lightMap, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eye.center;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMap.min + lightMap.size - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMap.min - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMap.origin;
}

float fadeShadowScalar(vec2 shadowTexCoords, float shadowScalar)
{
    vec2 normalized = abs(shadowTexCoords * 2.0 - 1.0);
    float fadeScalar =
        max(smoothstep(0.85, 1.0, normalized.x),
            smoothstep(0.85, 1.0, normalized.y));
    return 1.0 - (1.0 - shadowScalar) * (1.0 - fadeScalar);
}

float computeShadowScalarPoint(vec4 position, vec3 lightOrigin, int shadowIndex)
{
    vec3 positionShadow = position.xyz - lightOrigin;
    float shadowZ = length(positionShadow);
    float shadowHits = 0.0;
    for (int i = 0; i < lighting.lightShadowSamples; ++i)
    {
        for (int j = 0; j < lighting.lightShadowSamples; ++j)
        {
            for (int k = 0; k < lighting.lightShadowSamples; ++k)
            {
                vec3 offset = (vec3(i, j, k) - vec3(lighting.lightShadowSamples / 2.0)) * (lighting.lightShadowSampleScalar / lighting.lightShadowSamples);
                shadowHits += shadowZ - lighting.lightShadowBias > texture(samplerCube(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], filteredSampler), positionShadow + offset).x ? 1.0 : 0.0;
            }
        }
    }
    return 1.0 - shadowHits / (lighting.lightShadowSamples * lighting.lightShadowSamples * lighting.lightShadowSamples);
}

float computeShadowScalarSpot(vec4 position, float lightConeOuter, int shadowIndex)
{
    mat4 shadowMatrix = shadowMatrices[shadowIndex];
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
        shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
        shadowTexCoordsProj.z >= 0.0 && shadowTexCoordsProj.z < 1.0)
    {
        vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
        float shadowZ = shadowTexCoordsProj.z;
        float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(sampler2DArray(shadowTextures, filteredSampler), vec3(shadowTexCoords, float(shadowIndex))).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lighting.lightShadowDensity);
        shadowScalar = lightConeOuter > SHADOW_FOV_MAX ? fadeShadowScalar(shadowTexCoords, shadowScalar) : shadowScalar;
        return shadowScalar;
    }
    return 1.0;
}

float computeShadowScalarDirectional(vec4 position, int shadowIndex)
{
    mat4 shadowMatrix = shadowMatrices[shadowIndex];
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.x < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET &&
        shadowTexCoordsProj.y >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.y < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET &&
        shadowTexCoordsProj.z >= SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET)
    {
        vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
        float shadowZ = shadowTexCoordsProj.z;
        float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(sampler2DArray(shadowTextures, filteredSampler), vec3(shadowTexCoords, float(shadowIndex))).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lighting.lightShadowDensity);
        return shadowScalar;
    }
    return 1.0;
}

float computeShadowScalarCascaded(vec4 position, float shadowCutoff, int shadowIndex)
{
    for (int i = 0; i < SHADOW_CASCADE_LEVELS; ++i)
    {
        mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + i];
        vec4 positionShadowClip = shadowMatrix * position;
        vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
        if (shadowTexCoordsProj.x >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.x < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.y >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.y < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.z >= SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_CASCADE_SEAM_INSET)
        {
            vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
            float shadowZ = shadowTexCoordsProj.z;
            float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
            float shadowDepthExp = texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], filteredSampler), vec3(shadowTexCoords, float(i))).y;
            float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
            float densityScalar = 1.0f + float(i) * SHADOW_CASCADE_DENSITY_BONUS;
            shadowScalar = pow(shadowScalar, lighting.lightShadowDensity * densityScalar);
            return shadowScalar;
        }
    }
    return 1.0;
}

vec3 computeFogAccumPoint(vec4 position, LightStruct light)
{
    // compute ray info
    vec3 startPosition = eye.center;
    vec3 stopPosition = position.xyz;
    vec3 rayVector = stopPosition - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, light.direction);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = light.shadowIndex;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = light.origin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(light.cutoff * (1.0 - lighting.lightCutoffMargin), light.cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.attenuationLinear * distance + light.attenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -light.direction));
            float halfConeInner = light.coneInner * 0.5;
            float halfConeOuter = light.coneOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scattering approximated with Henyey-Greenstein phase function
            float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute depths
            vec3 positionShadow = currentPosition - light.origin;
            float shadowZ = length(positionShadow);
            float shadowDepth = texture(samplerCube(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], filteredSampler), positionShadow).x;

            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = light.origin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(light.cutoff * (1.0 - lighting.lightCutoffMargin), light.cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.attenuationLinear * distance + light.attenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -light.direction));
            float halfConeInner = light.coneInner * 0.5;
            float halfConeOuter = light.coneOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scattering approximated with Henyey-Greenstein phase function
                float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * light.color * light.brightness * lighting.ssvfIntensity;
}

vec3 computeFogAccumSpot(vec4 position, LightStruct light)
{
    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, light.direction);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = light.shadowIndex;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = light.origin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(light.cutoff * (1.0 - lighting.lightCutoffMargin), light.cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.attenuationLinear * distance + light.attenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -light.direction));
            float halfConeInner = light.coneInner * 0.5;
            float halfConeOuter = light.coneOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[shadowIndex];
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoordsProj.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowTextures, filteredSampler), vec3(shadowTexCoords, float(shadowIndex))).x : 1.0;

            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = light.origin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(light.cutoff * (1.0 - lighting.lightCutoffMargin), light.cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.attenuationLinear * distance + light.attenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -light.direction));
            float halfConeInner = light.coneInner * 0.5;
            float halfConeOuter = light.coneOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    
    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * light.color * light.brightness * lighting.ssvfIntensity;
}

vec3 computeFogAccumDirectional(vec4 position, LightStruct light)
{
    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, light.direction);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    int shadowIndex = light.shadowIndex;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
            result += fogMoment;

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[shadowIndex];
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoordsProj.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowTextures, filteredSampler), vec3(shadowTexCoords, float(shadowIndex))).x : 1.0;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / lighting.ssvfSteps) * light.color * light.brightness * lighting.ssvfIntensity;
}

vec3 computeFogAccumCascaded(vec4 position, LightStruct light)
{
    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, light.direction);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    int shadowIndex = light.shadowIndex;
    vec3 currentPosition = startPosition + step * dithering;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // compute depths
                mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + j];
                vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
                vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
                vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
                bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
                float shadowZ = shadowTexCoordsProj.z;
                float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], filteredSampler), vec3(shadowTexCoords, float(i))).x : 1.0;

                // step through ray, accumulating fog light moment
                if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
                {
                    // mie scaterring approximated with Henyey-Greenstein phase function
                    float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
                    float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));
                    result += fogMoment;
                }
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / (lighting.ssvfSteps * SHADOW_CASCADE_LEVELS)) * light.color * light.brightness * lighting.ssvfIntensity;
}

void computeSsrr(float depth, vec4 position, vec3 normal, float refractiveIndex, float subsurfaceCutoff, float subsurfaceCutoffMargin, inout vec3 diffuseScreen, inout float diffuseSurfaceWeight, inout float diffuseScreenWeight)
{
    // compute view values
    vec4 positionView = eye.view * position;
    vec3 positionViewNormal = normalize(positionView.xyz);
    vec3 normalView = mat3(eye.view) * normal;
    vec3 refractionView = refract(positionViewNormal, normalView, refractiveIndex);
    vec4 startView = vec4(positionView.xyz, 1.0);
    vec4 stopView = vec4(positionView.xyz + refractionView * lighting.ssrrDistanceCutoff, 1.0);
    float eyeDistanceFromPlane = abs(dot(normalView, positionView.xyz));

    // compute the fragment at which to start marching
    vec2 texSize = textureSize(sampler2D(depthTexture, filteredSampler), 0).xy;
    vec4 startFrag4 = eye.projection * startView;
    vec2 startFrag = startFrag4.xy / startFrag4.w;
    startFrag = startFrag * 0.5 + 0.5;
    startFrag *= texSize;

    // compute the fragment at which to end marching as well as total length
    vec4 stopFrag4 = eye.projection * stopView;
    vec2 stopFrag = stopFrag4.xy / stopFrag4.w;
    stopFrag = stopFrag * 0.5 + 0.5;
    stopFrag *= texSize;
    float lengthFrag = length(stopFrag - startFrag);

    // initialize current fragment
    vec2 currentFrag = startFrag;
    vec2 currentTexCoords = currentFrag / texSize;
    float currentDepth = depth;
    vec4 currentPosition = position;
    vec4 currentPositionView = positionView;

    // compute fragment step amount
    float marchHorizontal = stopFrag.x - startFrag.x;
    float marchVertical = stopFrag.y - startFrag.y;
    bool shouldMarchHorizontal = abs(marchHorizontal) >= abs(marchVertical);
    float stepCount = abs(shouldMarchHorizontal ? marchHorizontal : marchVertical) * lighting.ssrrDetail;
    vec2 stepAmount = vec2(marchHorizontal, marchVertical) / max(stepCount, 0.001);

    // march fragment
    float currentProgressA = 0.0;
    float currentProgressB = 0.0;
    float currentDepthView = 0.0;
    for (int i = 0; i < stepCount && currentTexCoords.x >= 0.0 && currentTexCoords.x < 1.0 && currentTexCoords.y >= 0.0 && currentTexCoords.y < 1.0; ++i)
    {
        // advance frag values
        currentFrag += stepAmount;
        currentTexCoords = currentFrag / texSize;
        currentDepth = texture(sampler2D(depthTexture, filteredSampler), currentTexCoords).r;
        currentPosition = depthToPosition(currentDepth, currentTexCoords);
        currentPositionView = eye.view * currentPosition;
        currentProgressB = length(currentFrag - startFrag) / lengthFrag;
        currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth.

        // compute depth delta and thickness based on view state
        float depthDelta = currentDepthView - -currentPositionView.z;
        float thickness = max(pow(-currentPositionView.z, 32.0) * lighting.ssrrRayThickness, lighting.ssrrRayThickness);

        // determine whether we hit geometry within acceptable thickness
        if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
        {
            // perform refinements within walk
            currentProgressB = currentProgressA + (currentProgressB - currentProgressA) * 0.5;
            for (int j = 0; j < lighting.ssrrRefinementsMax; ++j)
            {
                // advance frag values
                currentFrag = mix(startFrag, stopFrag, currentProgressB);
                currentTexCoords = currentFrag / texSize;
                currentDepth = texture(sampler2D(depthTexture, filteredSampler), currentTexCoords).r;
                currentPosition = depthToPosition(currentDepth, currentTexCoords);
                currentPositionView = eye.view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(pow(-currentPositionView.z, 32.0) * lighting.ssrrRayThickness, lighting.ssrrRayThickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // compute screen-space diffuse color
                    diffuseScreen = texture(sampler2D(colorTexture, unfilteredSampler), currentTexCoords).rgb * lighting.ssrrIntensity;

                    // compute diffuse surface weight
                    diffuseSurfaceWeight =
                        smoothstep(1.0 - subsurfaceCutoffMargin, 1.0, abs(currentPositionView.z - positionView.z) / subsurfaceCutoff) * // weight toward surface as penetration nears max depth
                        (subsurfaceCutoff == 0.0 ? 0.0 : 1.0); // disable when cutoff is zero
                    diffuseSurfaceWeight = clamp(diffuseSurfaceWeight, 0.0, 1.0);

                    // compute diffuse screen-space weight
                    diffuseScreenWeight =
                        (1.0 - smoothstep(1.0 - lighting.ssrrDistanceCutoffMargin, 1.0, length(currentPositionView - positionView) / lighting.ssrrDistanceCutoff)) * // filter out as reflection point reaches max distance from fragment
                        smoothstep(0.0, 0.5, eyeDistanceFromPlane) * // filter out as eye nears plane
                        smoothstep(0.0, lighting.ssrrEdgeHorizontalMargin, min(currentTexCoords.x, 1.0 - currentTexCoords.x)) *
                        smoothstep(0.0, lighting.ssrrEdgeVerticalMargin, min(currentTexCoords.y, 1.0 - currentTexCoords.y));
                    diffuseScreenWeight = clamp(diffuseScreenWeight, 0.0, 1.0);
                    break;
                }

                // continue in the same direction
                float temp = currentProgressB;
                currentProgressB = currentProgressB + (currentProgressB - currentProgressA) * 0.5;
                currentProgressA = temp;
            }

            // fin
            break;
        }
    }

    // otherwise loop
    currentProgressA = currentProgressB;
}

void main()
{
    // discard when depth out of range
    float depthCutoff = heightPlus.z;
    float depth = gl_FragCoord.z / gl_FragCoord.w;
    if (depthCutoff >= 0.0) { if (depth > depthCutoff) discard; }
    else if (depth <= -depthCutoff) discard;

    // compute basic fragment data
    vec3 normal = normalize(normal);
    float distance = length(position.xyz - eye.center);

    // compute spatial converters
    vec3 q1 = dFdx(position.xyz);
    vec3 q2 = dFdy(position.xyz);
    vec2 st1 = dFdx(texCoords);
    vec2 st2 = dFdy(texCoords);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    tangent = normalize(tangent - normal * dot(normal, tangent));
    binormal = cross(normal, tangent);
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax occlusion space
    vec3 eyeCenterTangent = toTangent * eye.center;
    vec3 positionTangent = toTangent * position.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(sampler2D(heightTexture, materialSampler), texCoords).x * heightPlus.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoords - parallax;

    // compute albedo with alpha sample
    float opaqueDistance = heightPlus.w;
    vec4 albedoSample = texture(sampler2D(albedoTexture, materialSampler), texCoords);
    vec4 albedoPlus =
        vec4(
            pow(albedoSample.rgb, vec3(GAMMA)) * albedo.rgb,
            mix(albedoSample.a, 1.0, smoothstep(opaqueDistance * 0.667, opaqueDistance, distance)));

    // compute normal
    vec3 n = normalize(toWorld * decodeNormal(texture(sampler2D(normalTexture, materialSampler), texCoords).xy));

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = texture(sampler2D(roughnessTexture, materialSampler), texCoords).r * material.r;
    vec3 du = dFdx(n);
    vec3 dv = dFdy(n);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));

    // compute remaining material properties
    float metallic = texture(sampler2D(metallicTexture, materialSampler), texCoords).g * material.g;
    float ambientOcclusion = texture(sampler2D(ambientOcclusionTexture, materialSampler), texCoords).b * material.b;
    vec3 emission = vec3(texture(sampler2D(emissionTexture, materialSampler), texCoords).r * material.a);

    // compute ignore light maps
    bool ignoreLightMaps = heightPlus.y != 0.0;

    // compute subsurface properties
    float subsurfaceCutoff = subsurfacePlus.x;
    float subsurfaceCutoffMargin = subsurfacePlus.y;
    float specularScalar = subsurfacePlus.z;
    float refractiveIndex = subsurfacePlus.w;

    // accumulate light and fog
    vec3 v = normalize(eye.center - position.xyz);
    float nDotV = saturate(dot(n, v));
    vec3 f0 = mix(vec3(0.04), albedoPlus.rgb, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedoPlus color as f0.
    vec3 lightAccumDiffuse = vec3(0.0);
    vec3 lightAccumSpecular = vec3(0.0);
    vec3 fogAccum = vec3(0.0);
    for (int i = 0; i < lightsGeneral.lightsCount; ++i)
    {
        // per-light radiance
        LightStruct light = lights[i];
        bool lightPoint = light.lightType == 0;
        bool lightSpot = light.lightType == 1;
        float hDotV, intensity;
        vec3 l, h, radiance;
        if (lightPoint || lightSpot)
        {
            vec3 d = light.origin - position.xyz;
            l = normalize(d);
            h = normalize(v + l);
            hDotV = saturate(dot(h, v));
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(light.cutoff * (1.0 - lighting.lightCutoffMargin), light.cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.attenuationLinear * distance + light.attenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -light.direction));
            float halfConeInner = light.coneInner * 0.5;
            float halfConeOuter = light.coneOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            intensity = attenuation * halfConeScalar * cutoffScalar;
            radiance = light.color * light.brightness * intensity;
        }
        else
        {
            l = -light.direction;
            h = normalize(v + l);
            hDotV = saturate(dot(h, v));
            intensity = 1.0;
            radiance = light.color * light.brightness;
        }

        // accumulate light
        if (intensity > 0.0)
        {
            // shadow scalar
            int shadowIndex = light.shadowIndex;
            float shadowScalar = 1.0f;
            if (shadowIndex >= 0)
            {
                switch (light.lightType)
                {
                    case 0: { shadowScalar = computeShadowScalarPoint(position, light.origin, shadowIndex); break; } // point
                    case 1: { shadowScalar = computeShadowScalarSpot(position, light.coneOuter, shadowIndex); break; } // spot
                    case 2: { shadowScalar = computeShadowScalarDirectional(position, shadowIndex); break; } // directional
                    default: { shadowScalar = computeShadowScalarCascaded(position, light.cutoff, shadowIndex); break; } // cascaded
                }
            }

            // cook-torrance brdf
            float ndf = distributionGGX(n, h, roughness);
            float g = geometrySchlick(n, v, l, roughness);
            vec3 f = fresnelSchlick(hDotV, f0);

            // compute specularity
            vec3 numerator = ndf * g * f;
            float nDotL = saturate(dot(n, l));
            float denominator = 4.0 * nDotV * nDotL + 0.0001; // add epsilon to prevent division by zero
            vec3 specular = clamp(numerator / denominator, 0.0, 10000.0);

            // compute diffusion
            vec3 kS = f;
            vec3 kD = vec3(1.0) - kS;
            kD *= 1.0 - metallic;

            // compute burley diffusion approximation (unlike lambert, this is NOT energy-preserving!)
            float lDotH = saturate(dot(l, h));
            float f90 = 0.5 + 2.0 * roughness * lDotH * lDotH; // retroreflection term
            float lightScatter = pow(1.0 - nDotL, 5.0) * (f90 - 1.0) + 1.0;
            float viewScatter  = pow(1.0 - nDotV, 5.0) * (f90 - 1.0) + 1.0;
            float burley = lightScatter * viewScatter;

            // add to outgoing lightAccums
            vec3 lightScalar = radiance * nDotL * shadowScalar;
            lightAccumDiffuse += (kD * albedoPlus.rgb / PI * burley) * lightScalar;
            lightAccumSpecular += specular * lightScalar;
        }

        // accumulate fog
        if (lighting.ssvfEnabled == 1 && light.desireFog == 1)
        {
            switch (light.lightType)
            {
                case 0: { fogAccum += computeFogAccumPoint(position, light); break; } // point
                case 1: { fogAccum += computeFogAccumSpot(position, light); break; } // spot
                case 2: { fogAccum += computeFogAccumDirectional(position, light); break; } // directional
                default: { fogAccum += computeFogAccumCascaded(position, light); break; } // cascaded
            }
        }
    }

    // determine light map indices, including their validity
    int lm1 = lightsGeneral.lightMapsCount > 0 && !ignoreLightMaps ? 0 : -1;
    int lm2 = lightsGeneral.lightMapsCount > 1 && !ignoreLightMaps ? 1 : -1;
    LightMapStruct lightMap1 = lightMaps[lm1];
    LightMapStruct lightMap2 = lightMaps[lm2];
    if (lm2 != -1 && !inBounds(position.xyz, lightMap2.min, lightMap2.size)) lm2 = -1;
    if (lm1 != -1 && !inBounds(position.xyz, lightMap1.min, lightMap1.size)) lm1 = lm2;
    lightMap1 = lightMaps[lm1];
    lightMap2 = lightMaps[lm2];

    // compute light mapping terms
    vec3 ambientColor = vec3(0.0);
    float ambientBrightness = 0.0;
    vec3 irradiance = vec3(0.0);
    vec3 environmentFilter = vec3(0.0);
    bool ssrrDesired = lighting.ssrrEnabled == 1 && refractiveIndex != 1.0;
    vec3 irradianceRefracted = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        ambientColor = lighting.lightAmbientColor;
        ambientBrightness = lighting.lightAmbientBrightness;
        irradiance = texture(samplerCube(irradianceMap, filteredSampler), n).rgb;
        vec3 r = reflect(-v, n);
        environmentFilter = textureLod(samplerCube(environmentFilterMap, filteredSampler), r, roughness * REFLECTION_LOD_MAX).rgb;
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr = k >= 0.0 ? refract(-v, n, refractiveIndex) : r;
        irradianceRefracted = ssrrDesired ? textureLod(samplerCube(irradianceMap, filteredSampler), rfr, 0).rgb : vec3(1.0);
    }
    else if (lm2 == -1)
    {
        // compute blending
        vec3 min1 = lightMap1.min;
        vec3 size1 = lightMap1.size;
        float distance = distanceToOutside(position.xyz, min1, size1);
        float ratio = 1.0 - smoothstep(0.0, lightsGeneral.lightMapSingletonBlendMargin, distance);

        // compute blended ambient values
        vec3 ambientColor1 = lightMap1.ambientColor;
        vec3 ambientColor2 = lighting.lightAmbientColor;
        float ambientBrightness1 = lightMap1.ambientBrightness;
        float ambientBrightness2 = lighting.lightAmbientBrightness;
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(samplerCube(irradianceMaps[lm1], filteredSampler), n).rgb;
        vec3 irradiance2 = texture(samplerCube(irradianceMap, filteredSampler), n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMap1, position.xyz, n);
        vec3 r2 = reflect(-v, n);

        vec3 environmentFilter1 = textureLod(samplerCube(environmentFilterMaps[lm1], filteredSampler), r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(samplerCube(environmentFilterMap, filteredSampler), r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);

        // compute blended environment filter refracted
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr1 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r1;
        vec3 rfr2 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r2;
        vec3 irradianceRefracted1 = ssrrDesired ? textureLod(samplerCube(irradianceMaps[lm1], filteredSampler), rfr1, 0).rgb : vec3(1.0);
        vec3 irradianceRefracted2 = ssrrDesired ? textureLod(samplerCube(irradianceMap, filteredSampler), rfr2, 0).rgb : vec3(1.0);
        irradianceRefracted = mix(irradianceRefracted1, irradianceRefracted2, ratio);
    }
    else
    {
        // compute blending
        float ratio = computeDepthRatio(lightMap1.min, lightMap1.size, lightMap2.min, lightMap2.size, position.xyz, n);

        // compute blended ambient values
        vec3 ambientColor1 = lightMap1.ambientColor;
        vec3 ambientColor2 = lightMap2.ambientColor;
        float ambientBrightness1 = lightMap1.ambientBrightness;
        float ambientBrightness2 = lightMap2.ambientBrightness;
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(samplerCube(irradianceMaps[lm1], filteredSampler), n).rgb;
        vec3 irradiance2 = texture(samplerCube(irradianceMaps[lm2], filteredSampler), n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMap1, position.xyz, n);
        vec3 r2 = parallaxCorrection(lightMap2, position.xyz, n);
        vec3 environmentFilter1 = textureLod(samplerCube(environmentFilterMaps[lm1], filteredSampler), r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(samplerCube(environmentFilterMaps[lm2], filteredSampler), r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);

        // compute blended environment filter refracted
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr1 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r1;
        vec3 rfr2 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r2;
        vec3 irradianceRefracted1 = ssrrDesired ? textureLod(samplerCube(irradianceMaps[lm1], filteredSampler), rfr1, 0).rgb : vec3(1.0);
        vec3 irradianceRefracted2 = ssrrDesired ? textureLod(samplerCube(irradianceMaps[lm2], filteredSampler), rfr2, 0).rgb : vec3(1.0);
        irradianceRefracted = mix(irradianceRefracted1, irradianceRefracted2, ratio);
    }

    // compute ambient terms
    float ambientBoostFactor = smoothstep(1.0 - lighting.lightAmbientBoostCutoff, 1.0, 1.0 - roughness);
    float ambientBoost = 1.0 + ambientBoostFactor * lighting.lightAmbientBoostScalar;
    vec3 ambientDiffuse = ambientColor * ambientBrightness * ambientBoost * ambientOcclusion;
    vec3 ambientSpecular = ambientDiffuse * ambientOcclusion;
    vec3 irradianceRefractedSaturated = saturate(irradianceRefracted, ENVIRONMENT_FILTER_REFRACTED_SATURATION);
    vec3 ambientColorRefracted = irradianceRefractedSaturated * ambientBrightness * lighting.ssrrIntensity;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(nDotV, f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedoPlus.rgb * ambientDiffuse;
    if (ssrrDesired)
    {
        vec3 diffuseScreen = vec3(0.0);
        float diffuseSurfaceWeight = 0.0;
        float diffuseScreenWeight = 0.0;
        computeSsrr(depth, position, normal, refractiveIndex, subsurfaceCutoff, subsurfaceCutoffMargin, diffuseScreen, diffuseSurfaceWeight, diffuseScreenWeight);
        diffuse = mix(diffuseScreen, diffuse, diffuseSurfaceWeight);
        diffuse = mix(ambientColorRefracted, diffuse, diffuseScreenWeight);
    }

    // compute specular term
    vec2 environmentBrdf = texture(sampler2D(brdfTexture, filteredSampler), vec2(nDotV, roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * ambientSpecular;

    // compute alpha term
    float alpha = albedoPlus.a * albedo.a;

    // since alpha only affects diffuse, increase accumulated specular light in proportion to alpha's color reduction.
    // after, apply specular scalar.
    lightAccumSpecular *= 1.0 / max(alpha, 0.0001) * specularScalar;

    // compute color composition
    vec3 color = lightAccumDiffuse + diffuse + emission * albedoPlus.rgb + lightAccumSpecular + specular + fogAccum;

    // compute and apply distance fog when enabled
    if (lighting.fogEnabled == 1)
    {
        switch (lighting.fogType)
        {
            case 0: // linear
            {
                float fogFactor = smoothstep(lighting.fogStart / lighting.fogFinish, 1.0, min(1.0, distance / lighting.fogFinish)) * lighting.fogColor.a;
                color = color * (1.0 - fogFactor) + lighting.fogColor.rgb * fogFactor;
                break;
            }
            case 1: // exponential
            {
                float fogFactor = (1.0 - exp(-lighting.fogDensity * distance)) * lighting.fogColor.a;
                color = color * (1.0 - fogFactor) + lighting.fogColor.rgb * fogFactor;
                break;
            }
            default: // exponential squared
            {
                float fogFactor = (1.0 - exp(-lighting.fogDensity * lighting.fogDensity * distance * distance)) * lighting.fogColor.a;
                color = color * (1.0 - fogFactor) + lighting.fogColor.rgb * fogFactor;
                break;
            }
        }
    }

    // write fragment
    frag = vec4(color, alpha);
}
