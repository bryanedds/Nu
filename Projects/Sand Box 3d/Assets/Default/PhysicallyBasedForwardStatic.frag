#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

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

struct Transform
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

struct Common
{
    vec3 eyeCenter;
    mat4 viewInverse;
    mat4 projectionInverse;
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
    float shadowNear; // NOTE: currently unused.
};

struct LightMap
{
    vec3 lightMapOrigins;
    vec3 lightMapMins;
    vec3 lightMapSizes;
    vec3 lightMapAmbientColors;
    float lightMapAmbientBrightnesses;
};

struct LightsGeneral
{
    int lightMapsCount;
    float lightMapSingletonBlendMargin;
    int lightsCount;
};

struct Light
{
    vec3 lightOrigins;
    vec3 lightDirections;
    vec3 lightColors;
    float lightBrightnesses;
    float lightAttenuationLinears;
    float lightAttenuationQuadratics;
    float lightCutoffs;
    int lightTypes;
    float lightConeInners;
    float lightConeOuters;
    int lightDesireFogs;
    int lightShadowIndices;
};

struct ShadowMatrix
{
    mat4 shadowMatrix;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
    int grpId;
};

layout(binding = 0) uniform TransformBlock
{
    Transform transform;
} transform[];

layout(binding = 1) uniform CommonBlock
{
    // TODO: DJL: reform name.
    Common commonData; // common is reserved
} commonData[];

layout(binding = 2) uniform sampler2D depthTexture[];
layout(binding = 3) uniform sampler2D colorTexture[];
layout(binding = 4) uniform sampler2D brdfTexture[];
layout(binding = 5) uniform samplerCube irradianceMap[];
layout(binding = 6) uniform samplerCube environmentFilterMap[];

layout(set = 1, binding = 1) uniform LightMapBlock
{
    LightMap lightMap;
} lightMaps[];

layout(set = 1, binding = 2) uniform LightsGeneralBlock
{
    LightsGeneral lightsGeneral;
} lightsGeneral[];

layout(set = 1, binding = 3) uniform LightBlock
{
    Light light;
} lights[];

layout(set = 1, binding = 4) uniform ShadowMatrixBlock
{
    ShadowMatrix shadowMatrix;
} shadowMatrices[];

layout(set = 1, binding = 5) uniform sampler2D albedoTexture[];
layout(set = 1, binding = 6) uniform sampler2D roughnessTexture[];
layout(set = 1, binding = 7) uniform sampler2D metallicTexture[];
layout(set = 1, binding = 8) uniform sampler2D ambientOcclusionTexture[];
layout(set = 1, binding = 9) uniform sampler2D emissionTexture[];
layout(set = 1, binding = 10) uniform sampler2D normalTexture[];
layout(set = 1, binding = 11) uniform sampler2D heightTexture[];
layout(set = 1, binding = 18) uniform samplerCube irradianceMaps[];
layout(set = 1, binding = 19) uniform samplerCube environmentFilterMaps[];
layout(set = 1, binding = 20) uniform sampler2DArray shadowTextures[];
layout(set = 1, binding = 21) uniform samplerCube shadowMaps[];
layout(set = 1, binding = 22) uniform sampler2DArray shadowCascades[];

layout(location = 0) in vec4 positionOut;
layout(location = 1) in vec2 texCoordsOut;
layout(location = 2) in vec3 normalOut;
flat layout(location = 3) in vec4 albedoOut;
flat layout(location = 4) in vec4 materialOut;
flat layout(location = 5) in vec4 heightPlusOut;
flat layout(location = 6) in vec4 subsurfacePlusOut;

layout(location = 0) out vec4 frag;

mat4 view = transform[grpId].transform.view;
mat4 projection = transform[grpId].transform.projection;
vec3 eyeCenter = commonData[grpId].commonData.eyeCenter;
mat4 viewInverse = commonData[grpId].commonData.viewInverse;
mat4 projectionInverse = commonData[grpId].commonData.projectionInverse;
float lightCutoffMargin = commonData[grpId].commonData.lightCutoffMargin;
vec3 lightAmbientColor = commonData[grpId].commonData.lightAmbientColor;
float lightAmbientBrightness = commonData[grpId].commonData.lightAmbientBrightness;
float lightAmbientBoostCutoff = commonData[grpId].commonData.lightAmbientBoostCutoff;
float lightAmbientBoostScalar = commonData[grpId].commonData.lightAmbientBoostScalar;
int lightShadowSamples = commonData[grpId].commonData.lightShadowSamples;
float lightShadowBias = commonData[grpId].commonData.lightShadowBias;
float lightShadowSampleScalar = commonData[grpId].commonData.lightShadowSampleScalar;
float lightShadowExponent = commonData[grpId].commonData.lightShadowExponent;
float lightShadowDensity = commonData[grpId].commonData.lightShadowDensity;
int fogEnabled = commonData[grpId].commonData.fogEnabled;
int fogType = commonData[grpId].commonData.fogType;
float fogStart = commonData[grpId].commonData.fogStart;
float fogFinish = commonData[grpId].commonData.fogFinish;
float fogDensity = commonData[grpId].commonData.fogDensity;
vec4 fogColor = commonData[grpId].commonData.fogColor;
int ssvfEnabled = commonData[grpId].commonData.ssvfEnabled;
float ssvfIntensity = commonData[grpId].commonData.ssvfIntensity;
int ssvfSteps = commonData[grpId].commonData.ssvfSteps;
float ssvfAsymmetry = commonData[grpId].commonData.ssvfAsymmetry;
int ssrrEnabled = commonData[grpId].commonData.ssrrEnabled;
float ssrrIntensity = commonData[grpId].commonData.ssrrIntensity;
float ssrrDetail = commonData[grpId].commonData.ssrrDetail;
int ssrrRefinementsMax = commonData[grpId].commonData.ssrrRefinementsMax;
float ssrrRayThickness = commonData[grpId].commonData.ssrrRayThickness;
float ssrrDistanceCutoff = commonData[grpId].commonData.ssrrDistanceCutoff;
float ssrrDistanceCutoffMargin = commonData[grpId].commonData.ssrrDistanceCutoffMargin;
float ssrrEdgeHorizontalMargin = commonData[grpId].commonData.ssrrEdgeHorizontalMargin;
float ssrrEdgeVerticalMargin = commonData[grpId].commonData.ssrrEdgeVerticalMargin;
int lightMapsCount = lightsGeneral[drawId].lightsGeneral.lightMapsCount;
float lightMapSingletonBlendMargin = lightsGeneral[drawId].lightsGeneral.lightMapSingletonBlendMargin;
int lightsCount = lightsGeneral[drawId].lightsGeneral.lightsCount;

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
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = projectionInverse * positionClip;
    positionView /= positionView.w;
    return viewInverse * positionView;
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

vec3 parallaxCorrection(vec3 lightMapOrigin, vec3 lightMapMin, vec3 lightMapSize, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eyeCenter;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMapMin + lightMapSize - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMapMin - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMapOrigin;
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
    for (int i = 0; i < lightShadowSamples; ++i)
    {
        for (int j = 0; j < lightShadowSamples; ++j)
        {
            for (int k = 0; k < lightShadowSamples; ++k)
            {
                vec3 offset = (vec3(i, j, k) - vec3(lightShadowSamples / 2.0)) * (lightShadowSampleScalar / lightShadowSamples);
                shadowHits += shadowZ - lightShadowBias > texture(shadowMaps[drawId * SHADOW_MAPS_MAX + (shadowIndex - SHADOW_TEXTURES_MAX)], positionShadow + offset).x ? 1.0 : 0.0;
            }
        }
    }
    return 1.0 - shadowHits / (lightShadowSamples * lightShadowSamples * lightShadowSamples);
}

float computeShadowScalarSpot(vec4 position, float lightConeOuter, int shadowIndex)
{
    mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + shadowIndex].shadowMatrix.shadowMatrix;
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
        shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
        shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
    {
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZ = shadowTexCoords.z;
        float shadowZExp = exp(-lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(shadowTextures[drawId], vec3(shadowTexCoords.xy, float(shadowIndex))).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lightShadowDensity);
        shadowScalar = lightConeOuter > SHADOW_FOV_MAX ? fadeShadowScalar(shadowTexCoords.xy, shadowScalar) : shadowScalar;
        return shadowScalar;
    }
    return 1.0;
}

float computeShadowScalarDirectional(vec4 position, int shadowIndex)
{
    mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + shadowIndex].shadowMatrix.shadowMatrix;
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.x < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET &&
        shadowTexCoordsProj.y >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.y < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET &&
        shadowTexCoordsProj.z >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET)
    {
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZ = shadowTexCoords.z;
        float shadowZExp = exp(-lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(shadowTextures[drawId], vec3(shadowTexCoords.xy, float(shadowIndex))).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lightShadowDensity);
        return shadowScalar;
    }
    return 1.0;
}

float computeShadowScalarCascaded(vec4 position, float shadowCutoff, int shadowIndex)
{
    for (int i = 0; i < SHADOW_CASCADE_LEVELS; ++i)
    {
        mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + (SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + i)].shadowMatrix.shadowMatrix;
        vec4 positionShadowClip = shadowMatrix * position;
        vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
        if (shadowTexCoordsProj.x >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.x < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.y >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.y < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.z >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_CASCADE_SEAM_INSET)
        {
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            float shadowZ = shadowTexCoords.z;
            float shadowZExp = exp(-lightShadowExponent * shadowZ);
            float shadowDepthExp = texture(shadowCascades[drawId * SHADOW_CASCADES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX)], vec3(shadowTexCoords.xy, float(i))).y;
            float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
            float densityScalar = 1.0f + float(i) * SHADOW_CASCADE_DENSITY_BONUS;
            shadowScalar = pow(shadowScalar, lightShadowDensity * densityScalar);
            return shadowScalar;
        }
    }
    return 1.0;
}

vec3 computeFogAccumPoint(vec4 position, int lightIndex)
{
    // grab light values
    Light light = lights[drawId * LIGHTS_MAX + lightIndex].light;
    vec3 lightOrigin = light.lightOrigins;
    float lightCutoff = light.lightCutoffs;
    vec3 lightDirection = light.lightDirections;
    float lightAttenuationLinear = light.lightAttenuationLinears;
    float lightAttenuationQuadratic = light.lightAttenuationQuadratics;
    float lightConeInner = light.lightConeInners;
    float lightConeOuter = light.lightConeOuters;

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 stopPosition = position.xyz;
    vec3 rayVector = stopPosition - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = light.lightShadowIndices;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scattering approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec3 positionShadow = currentPosition - lightOrigin;
            float shadowZ = length(positionShadow);
            float shadowDepth = texture(shadowMaps[drawId * SHADOW_MAPS_MAX + (shadowIndex - SHADOW_TEXTURES_MAX)], positionShadow).x;

            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scattering approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * light.lightColors * light.lightBrightnesses * ssvfIntensity;
}

vec3 computeFogAccumSpot(vec4 position, int lightIndex)
{
    // grab light values
    Light light = lights[drawId * LIGHTS_MAX + lightIndex].light;
    vec3 lightOrigin = light.lightOrigins;
    float lightCutoff = light.lightCutoffs;
    vec3 lightDirection = light.lightDirections;
    float lightAttenuationLinear = light.lightAttenuationLinears;
    float lightAttenuationQuadratic = light.lightAttenuationQuadratics;
    float lightConeInner = light.lightConeInners;
    float lightConeOuter = light.lightConeOuters;

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = light.lightShadowIndices;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + shadowIndex].shadowMatrix.shadowMatrix;
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures[drawId], vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    
    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * light.lightColors * light.lightBrightnesses * ssvfIntensity;
}

vec3 computeFogAccumDirectional(vec4 position, int lightIndex)
{
    // grab light values
    Light light = lights[drawId * LIGHTS_MAX + lightIndex].light;
    vec3 lightOrigin = light.lightOrigins;
    vec3 lightDirection = light.lightDirections;

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    int shadowIndex = light.lightShadowIndices;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment;

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + shadowIndex].shadowMatrix.shadowMatrix;
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures[drawId], vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / ssvfSteps) * light.lightColors * light.lightBrightnesses * ssvfIntensity;
}

vec3 computeFogAccumCascaded(vec4 position, int lightIndex)
{
    // grab light values
    Light light = lights[drawId * LIGHTS_MAX + lightIndex].light;
    vec3 lightOrigin = light.lightOrigins;
    vec3 lightDirection = light.lightDirections;

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    int shadowIndex = light.lightShadowIndices;
    vec3 currentPosition = startPosition + step * dithering;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // compute depths
                mat4 shadowMatrix = shadowMatrices[drawId * (SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS) + (SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + j)].shadowMatrix.shadowMatrix;
                vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
                vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
                vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
                bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
                float shadowZ = shadowTexCoords.z;
                float shadowDepth = shadowTexCoordsInRange ? texture(shadowCascades[drawId * SHADOW_CASCADES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX)], vec3(shadowTexCoords.xy, float(i))).x : 1.0;

                // step through ray, accumulating fog light moment
                if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
                {
                    // mie scaterring approximated with Henyey-Greenstein phase function
                    float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                    float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                    result += fogMoment;
                }
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / (ssvfSteps * SHADOW_CASCADE_LEVELS)) * light.lightColors * light.lightBrightnesses * ssvfIntensity;
}

void computeSsrr(float depth, vec4 position, vec3 normal, float refractiveIndex, float subsurfaceCutoff, float subsurfaceCutoffMargin, inout vec3 diffuseScreen, inout float diffuseSurfaceWeight, inout float diffuseScreenWeight)
{
    // compute view values
    vec4 positionView = view * position;
    vec3 positionViewNormal = normalize(positionView.xyz);
    vec3 normalView = mat3(view) * normal;
    vec3 refractionView = refract(positionViewNormal, normalView, refractiveIndex);
    vec4 startView = vec4(positionView.xyz, 1.0);
    vec4 stopView = vec4(positionView.xyz + refractionView * ssrrDistanceCutoff, 1.0);
    float eyeDistanceFromPlane = abs(dot(normalView, positionView.xyz));

    // compute the fragment at which to start marching
    vec2 texSize = textureSize(depthTexture[grpId], 0).xy;
    vec4 startFrag4 = projection * startView;
    vec2 startFrag = startFrag4.xy / startFrag4.w;
    startFrag = startFrag * 0.5 + 0.5;
    startFrag *= texSize;

    // compute the fragment at which to end marching as well as total length
    vec4 stopFrag4 = projection * stopView;
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
    float stepCount = abs(shouldMarchHorizontal ? marchHorizontal : marchVertical) * ssrrDetail;
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
        currentDepth = texture(depthTexture[grpId], currentTexCoords).r;
        currentPosition = depthToPosition(currentDepth, currentTexCoords);
        currentPositionView = view * currentPosition;
        currentProgressB = length(currentFrag - startFrag) / lengthFrag;
        currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth.

        // compute depth delta and thickness based on view state
        float depthDelta = currentDepthView - -currentPositionView.z;
        float thickness = max(pow(-currentPositionView.z, 32.0) * ssrrRayThickness, ssrrRayThickness);

        // determine whether we hit geometry within acceptable thickness
        if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
        {
            // perform refinements within walk
            currentProgressB = currentProgressA + (currentProgressB - currentProgressA) * 0.5;
            for (int j = 0; j < ssrrRefinementsMax; ++j)
            {
                // advance frag values
                currentFrag = mix(startFrag, stopFrag, currentProgressB);
                currentTexCoords = currentFrag / texSize;
                currentDepth = texture(depthTexture[grpId], currentTexCoords).r;
                currentPosition = depthToPosition(currentDepth, currentTexCoords);
                currentPositionView = view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(pow(-currentPositionView.z, 32.0) * ssrrRayThickness, ssrrRayThickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // compute screen-space diffuse color
                    diffuseScreen = texture(colorTexture[grpId], currentTexCoords).rgb * ssrrIntensity;

                    // compute diffuse surface weight
                    diffuseSurfaceWeight =
                        smoothstep(1.0 - subsurfaceCutoffMargin, 1.0, abs(currentPositionView.z - positionView.z) / subsurfaceCutoff) * // weight toward surface as penetration nears max depth
                        (subsurfaceCutoff == 0.0 ? 0.0 : 1.0); // disable when cutoff is zero
                    diffuseSurfaceWeight = clamp(diffuseSurfaceWeight, 0.0, 1.0);

                    // compute diffuse screen-space weight
                    diffuseScreenWeight =
                        (1.0 - smoothstep(1.0 - ssrrDistanceCutoffMargin, 1.0, length(currentPositionView - positionView) / ssrrDistanceCutoff)) * // filter out as reflection point reaches max distance from fragment
                        smoothstep(0.0, 0.5, eyeDistanceFromPlane) * // filter out as eye nears plane
                        smoothstep(0.0, ssrrEdgeHorizontalMargin, min(currentTexCoords.x, 1.0 - currentTexCoords.x)) *
                        smoothstep(0.0, ssrrEdgeVerticalMargin, min(currentTexCoords.y, 1.0 - currentTexCoords.y));
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
    float depthCutoff = heightPlusOut.z;
    float depth = gl_FragCoord.z / gl_FragCoord.w;
    if (depthCutoff >= 0.0) { if (depth > depthCutoff) discard; }
    else if (depth <= -depthCutoff) discard;

    // compute basic fragment data
    vec4 position = positionOut;
    vec3 normal = normalize(normalOut);
    float distance = length(position.xyz - eyeCenter);

    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    tangent = normalize(tangent - normal * dot(normal, tangent));
    binormal = cross(normal, tangent);
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax occlusion space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * position.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture[drawId], texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo with alpha sample
    float opaqueDistance = heightPlusOut.w;
    vec4 albedoSample = texture(albedoTexture[drawId], texCoords);
    vec4 albedo =
        vec4(
            pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb,
            mix(albedoSample.a, 1.0, smoothstep(opaqueDistance * 0.667, opaqueDistance, distance)));

    // compute normal
    vec3 n = normalize(toWorld * decodeNormal(texture(normalTexture[drawId], texCoords).xy));

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = texture(roughnessTexture[drawId], texCoords).r * materialOut.r;
    vec3 du = dFdx(n);
    vec3 dv = dFdy(n);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));

    // compute remaining material properties
    float metallic = texture(metallicTexture[drawId], texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture[drawId], texCoords).b * materialOut.b;
    vec3 emission = vec3(texture(emissionTexture[drawId], texCoords).r * materialOut.a);

    // compute ignore light maps
    bool ignoreLightMaps = heightPlusOut.y != 0.0;

    // compute subsurface properties
    float subsurfaceCutoff = subsurfacePlusOut.x;
    float subsurfaceCutoffMargin = subsurfacePlusOut.y;
    float specularScalar = subsurfacePlusOut.z;
    float refractiveIndex = subsurfacePlusOut.w;

    // accumulate light and fog
    vec3 v = normalize(eyeCenter - position.xyz);
    float nDotV = saturate(dot(n, v));
    vec3 f0 = mix(vec3(0.04), albedo.rgb, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 lightAccumDiffuse = vec3(0.0);
    vec3 lightAccumSpecular = vec3(0.0);
    vec3 fogAccum = vec3(0.0);
    for (int i = 0; i < lightsCount; ++i)
    {
        // per-light radiance
        Light light = lights[drawId * LIGHTS_MAX + i].light;
        vec3 lightOrigin = light.lightOrigins;
        float lightCutoff = light.lightCutoffs;
        int lightType = light.lightTypes;
        bool lightPoint = lightType == 0;
        bool lightSpot = lightType == 1;
        float hDotV, intensity;
        vec3 l, h, radiance;
        if (lightPoint || lightSpot)
        {
            vec3 d = lightOrigin - position.xyz;
            l = normalize(d);
            h = normalize(v + l);
            hDotV = saturate(dot(h, v));
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + light.lightAttenuationLinears * distance + light.lightAttenuationQuadratics * distanceSquared);
            float angle = acos(dot(l, -light.lightDirections));
            float halfConeInner = light.lightConeInners * 0.5;
            float halfConeOuter = light.lightConeOuters * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            intensity = attenuation * halfConeScalar * cutoffScalar;
            radiance = light.lightColors * light.lightBrightnesses * intensity;
        }
        else
        {
            l = -light.lightDirections;
            h = normalize(v + l);
            hDotV = saturate(dot(h, v));
            intensity = 1.0;
            radiance = light.lightColors * light.lightBrightnesses;
        }

        // accumulate light
        if (intensity > 0.0)
        {
            // shadow scalar
            int shadowIndex = light.lightShadowIndices;
            float shadowScalar = 1.0f;
            if (shadowIndex >= 0)
            {
                switch (lightType)
                {
                    case 0: { shadowScalar = computeShadowScalarPoint(position, lightOrigin, shadowIndex); break; } // point
                    case 1: { shadowScalar = computeShadowScalarSpot(position, light.lightConeOuters, shadowIndex); break; } // spot
                    case 2: { shadowScalar = computeShadowScalarDirectional(position, shadowIndex); break; } // directional
                    default: { shadowScalar = computeShadowScalarCascaded(position, lightCutoff, shadowIndex); break; } // cascaded
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
            lightAccumDiffuse += (kD * albedo.rgb / PI * burley) * lightScalar;
            lightAccumSpecular += specular * lightScalar;
        }

        // accumulate fog
        if (ssvfEnabled == 1 && light.lightDesireFogs == 1)
        {
            switch (lightType)
            {
                case 0: { fogAccum += computeFogAccumPoint(position, i); break; } // point
                case 1: { fogAccum += computeFogAccumSpot(position, i); break; } // spot
                case 2: { fogAccum += computeFogAccumDirectional(position, i); break; } // directional
                default: { fogAccum += computeFogAccumCascaded(position, i); break; } // cascaded
            }
        }
    }

    // determine light map indices, including their validity
    int lm1 = lightMapsCount > 0 && !ignoreLightMaps ? 0 : -1;
    int lm2 = lightMapsCount > 1 && !ignoreLightMaps ? 1 : -1;
    LightMap lightMap1 = lightMaps[drawId * LIGHT_MAPS_MAX + lm1].lightMap;
    LightMap lightMap2 = lightMaps[drawId * LIGHT_MAPS_MAX + lm2].lightMap;
    if (lm2 != -1 && !inBounds(position.xyz, lightMap2.lightMapMins, lightMap2.lightMapSizes)) lm2 = -1;
    if (lm1 != -1 && !inBounds(position.xyz, lightMap1.lightMapMins, lightMap1.lightMapSizes)) lm1 = lm2;
    lightMap1 = lightMaps[drawId * LIGHT_MAPS_MAX + lm1].lightMap;
    lightMap2 = lightMaps[drawId * LIGHT_MAPS_MAX + lm2].lightMap;

    // compute light mapping terms
    vec3 ambientColor = vec3(0.0);
    float ambientBrightness = 0.0;
    vec3 irradiance = vec3(0.0);
    vec3 environmentFilter = vec3(0.0);
    bool ssrrDesired = ssrrEnabled == 1 && refractiveIndex != 1.0;
    vec3 environmentFilterRefracted = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        ambientColor = lightAmbientColor;
        ambientBrightness = lightAmbientBrightness;
        irradiance = texture(irradianceMap[grpId], n).rgb;
        vec3 r = reflect(-v, n);
        environmentFilter = textureLod(environmentFilterMap[grpId], r, roughness * REFLECTION_LOD_MAX).rgb;
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr = k >= 0.0 ? refract(-v, n, refractiveIndex) : r;
        environmentFilterRefracted = ssrrDesired ? textureLod(environmentFilterMap[grpId], rfr, 0).rgb : vec3(1.0);
    }
    else if (lm2 == -1)
    {
        // compute blending
        vec3 min1 = lightMap1.lightMapMins;
        vec3 size1 = lightMap1.lightMapSizes;
        float distance = distanceToOutside(position.xyz, min1, size1);
        float ratio = 1.0 - smoothstep(0.0, lightMapSingletonBlendMargin, distance);

        // compute blended ambient values
        vec3 ambientColor1 = lightMap1.lightMapAmbientColors;
        vec3 ambientColor2 = lightAmbientColor;
        float ambientBrightness1 = lightMap1.lightMapAmbientBrightnesses;
        float ambientBrightness2 = lightAmbientBrightness;
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(irradianceMaps[drawId * LIGHT_MAPS_MAX + lm1], n).rgb;
        vec3 irradiance2 = texture(irradianceMap[grpId], n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMap1.lightMapOrigins, lightMap1.lightMapMins, lightMap1.lightMapSizes, position.xyz, n);
        vec3 r2 = reflect(-v, n);

        vec3 environmentFilter1 = textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm1], r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMap[grpId], r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);

        // compute blended environment filter refracted
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr1 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r1;
        vec3 rfr2 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r2;
        vec3 environmentFilterRefracted1 = ssrrDesired ? textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm1], rfr1, 0).rgb : vec3(1.0);
        vec3 environmentFilterRefracted2 = ssrrDesired ? textureLod(environmentFilterMap[grpId], rfr2, 0).rgb : vec3(1.0);
        environmentFilterRefracted = mix(environmentFilterRefracted1, environmentFilterRefracted2, ratio);
    }
    else
    {
        // compute blending
        float ratio = computeDepthRatio(lightMap1.lightMapMins, lightMap1.lightMapSizes, lightMap2.lightMapMins, lightMap2.lightMapSizes, position.xyz, n);

        // compute blended ambient values
        vec3 ambientColor1 = lightMap1.lightMapAmbientColors;
        vec3 ambientColor2 = lightMap2.lightMapAmbientColors;
        float ambientBrightness1 = lightMap1.lightMapAmbientBrightnesses;
        float ambientBrightness2 = lightMap2.lightMapAmbientBrightnesses;
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(irradianceMaps[drawId * LIGHT_MAPS_MAX + lm1], n).rgb;
        vec3 irradiance2 = texture(irradianceMaps[drawId * LIGHT_MAPS_MAX + lm2], n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMap1.lightMapOrigins, lightMap1.lightMapMins, lightMap1.lightMapSizes, position.xyz, n);
        vec3 r2 = parallaxCorrection(lightMap2.lightMapOrigins, lightMap2.lightMapMins, lightMap2.lightMapSizes, position.xyz, n);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm1], r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm2], r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);

        // compute blended environment filter refracted
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr1 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r1;
        vec3 rfr2 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r2;
        vec3 environmentFilterRefracted1 = ssrrDesired ? textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm1], rfr1, 0).rgb : vec3(1.0);
        vec3 environmentFilterRefracted2 = ssrrDesired ? textureLod(environmentFilterMaps[drawId * LIGHT_MAPS_MAX + lm2], rfr2, 0).rgb : vec3(1.0);
        environmentFilterRefracted = mix(environmentFilterRefracted1, environmentFilterRefracted2, ratio);
    }

    // compute ambient terms
    float ambientBoostFactor = smoothstep(1.0 - lightAmbientBoostCutoff, 1.0, 1.0 - roughness);
    float ambientBoost = 1.0 + ambientBoostFactor * lightAmbientBoostScalar;
    vec3 ambientDiffuse = ambientColor * ambientBrightness * ambientBoost * ambientOcclusion;
    vec3 ambientSpecular = ambientDiffuse * ambientOcclusion;
    vec3 environmentFilterRefractedSaturated = saturate(environmentFilterRefracted, ENVIRONMENT_FILTER_REFRACTED_SATURATION);
    vec3 ambientColorRefracted = environmentFilterRefractedSaturated * ambientBrightness * ssrrIntensity;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(nDotV, f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedo.rgb * ambientDiffuse;
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
    vec2 environmentBrdf = texture(brdfTexture[grpId], vec2(nDotV, roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * ambientSpecular;

    // compute alpha term
    float alpha = albedo.a * albedoOut.a;

    // since alpha only affects diffuse, increase accumulated specular light in proportion to alpha's color reduction.
    // after, apply specular scalar.
    lightAccumSpecular *= 1.0 / max(alpha, 0.0001) * specularScalar;

    // compute color composition
    vec3 color = lightAccumDiffuse + diffuse + emission * albedo.rgb + lightAccumSpecular + specular + fogAccum;

    // compute and apply distance fog when enabled
    if (fogEnabled == 1)
    {
        switch (fogType)
        {
            case 0: // linear
            {
                float fogFactor = smoothstep(fogStart / fogFinish, 1.0, min(1.0, distance / fogFinish)) * fogColor.a;
                color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
                break;
            }
            case 1: // exponential
            {
                float fogFactor = (1.0 - exp(-fogDensity * distance)) * fogColor.a;
                color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
                break;
            }
            default: // exponential squared
            {
                float fogFactor = (1.0 - exp(-fogDensity * fogDensity * distance * distance)) * fogColor.a;
                color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
                break;
            }
        }
    }

    // write fragment
    frag = vec4(color, alpha);
}
