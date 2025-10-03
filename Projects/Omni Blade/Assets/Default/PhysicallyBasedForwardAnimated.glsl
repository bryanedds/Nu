#shader vertex
#version 460 core

const int TEX_COORDS_OFFSET_VERTS = 6;
const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

const vec2 TEX_COORDS_OFFSET_FILTERS[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TEX_COORDS_OFFSET_FILTERS_2[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;
uniform mat4 bones[BONES_MAX];

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec4 boneIds;
layout(location = 4) in vec4 weights;
layout(location = 5) in mat4 model;
layout(location = 9) in vec4 texCoordsOffset;
layout(location = 10) in vec4 albedo;
layout(location = 11) in vec4 material;
layout(location = 12) in vec4 heightPlus;
layout(location = 13) in vec4 subsurfacePlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;
flat out vec4 subsurfacePlusOut;

void main()
{
    // compute blended bone influences
    mat4 boneBlended = mat4(0.0);
    for (int i = 0; i < BONES_INFLUENCE_MAX; ++i)
    {
        int boneId = int(boneIds[i]);
        if (boneId >= 0) boneBlended += bones[boneId] * weights[i];
    }

    // compute blended position and normal
    vec4 positionBlended = boneBlended * vec4(position, 1.0);
    vec4 normalBlended = boneBlended * vec4(normal, 0.0);

    // compute remaining values
    positionOut = model * positionBlended;
    positionOut /= positionOut.w; // NOTE: normalizing by w seems to fix a bug caused by weights not summing to 1.0.
    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normalBlended.xyz;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    gl_Position = viewProjection * positionOut;
}

#shader fragment
#version 460 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0f;
const float ENVIRONMENT_FILTER_REFRACTED_SATURATION = 2.0;
const int LIGHT_MAPS_MAX = 2;
const int LIGHTS_MAX = 9;
const int SHADOW_TEXTURES_MAX = 8;
const int SHADOW_MAPS_MAX = 7;
const float SHADOW_DIRECTIONAL_SEAM_INSET = 0.05; // TODO: see if this should be proportionate to shadow texel size.
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;
const float SHADOW_CASCADE_SEAM_INSET = 0.001;
const float SHADOW_CASCADE_DENSITY_BONUS = 0.5;
const float SHADOW_FOV_MAX = 2.1;

const vec4 SSVF_DITHERING[4] =
    vec4[4](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

uniform mat4 view;
uniform mat4 projection;
uniform vec3 eyeCenter;
uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform float lightCutoffMargin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform float lightAmbientBoostCutoff;
uniform float lightAmbientBoostScalar;
uniform int lightShadowSamples;
uniform float lightShadowBias;
uniform float lightShadowExponent;
uniform float lightShadowDensity;
uniform float lightShadowSampleScalar;
uniform int fogEnabled;
uniform int fogType;
uniform float fogStart;
uniform float fogFinish;
uniform float fogDensity;
uniform vec4 fogColor;
uniform int ssvfEnabled;
uniform int ssvfSteps;
uniform float ssvfAsymmetry;
uniform float ssvfIntensity;
uniform int ssrrEnabled;
uniform float ssrrIntensity;
uniform float ssrrDetail;
uniform int ssrrRefinementsMax;
uniform float ssrrRayThickness;
uniform float ssrrDistanceCutoff;
uniform float ssrrDistanceCutoffMargin;
uniform float ssrrEdgeHorizontalMargin;
uniform float ssrrEdgeVerticalMargin;
uniform sampler2D albedoTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D metallicTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D emissionTexture;
uniform sampler2D normalTexture;
uniform sampler2D heightTexture;
uniform sampler2D depthTexture;
uniform sampler2D colorTexture;
uniform sampler2D brdfTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];
uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform sampler2D shadowTextures[SHADOW_TEXTURES_MAX];
uniform samplerCube shadowMaps[SHADOW_MAPS_MAX];
uniform sampler2DArray shadowCascades[SHADOW_CASCADES_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform vec3 lightMapAmbientColors[LIGHT_MAPS_MAX];
uniform float lightMapAmbientBrightnesses[LIGHT_MAPS_MAX];
uniform int lightMapsCount;
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec3 lightDirections[LIGHTS_MAX];
uniform vec3 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightAttenuationLinears[LIGHTS_MAX];
uniform float lightAttenuationQuadratics[LIGHTS_MAX];
uniform float lightCutoffs[LIGHTS_MAX];
uniform int lightTypes[LIGHTS_MAX];
uniform float lightConeInners[LIGHTS_MAX];
uniform float lightConeOuters[LIGHTS_MAX];
uniform int lightDesireFogs[LIGHTS_MAX];
uniform int lightShadowIndices[LIGHTS_MAX];
uniform int lightsCount;
uniform float shadowNear; // NOTE: currently unused.
uniform mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS];

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;
flat in vec4 subsurfacePlusOut;

layout(location = 0) out vec4 frag;

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

vec3 saturate(vec3 color, float boost)
{
    float luma = dot(color, vec3(0.2126, 0.7152, 0.0722)); // compute perceived luminance (Rec. 709)
    return mix(vec3(luma), color, boost); // interpolate between grayscale and original color
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
    float nDotH = max(dot(normal, h), 0.0);
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
    float nDotV = max(dot(normal, v), 0.0);
    float nDotL = max(dot(normal, l), 0.0);
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
                shadowHits += shadowZ - lightShadowBias > texture(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], positionShadow + offset).x ? 1.0 : 0.0;
            }
        }
    }
    return 1.0 - shadowHits / (lightShadowSamples * lightShadowSamples * lightShadowSamples);
}

float computeShadowScalarSpot(vec4 position, float lightConeOuter, int shadowIndex)
{
    mat4 shadowMatrix = shadowMatrices[shadowIndex];
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
        shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
        shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
    {
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZ = shadowTexCoords.z;
        float shadowZExp = exp(-lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(shadowTextures[shadowIndex], shadowTexCoords.xy).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lightShadowDensity);
        shadowScalar = lightConeOuter > SHADOW_FOV_MAX ? fadeShadowScalar(shadowTexCoords.xy, shadowScalar) : shadowScalar;
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
        shadowTexCoordsProj.z >= -1.0 + SHADOW_DIRECTIONAL_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_DIRECTIONAL_SEAM_INSET)
    {
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZ = shadowTexCoords.z;
        float shadowZExp = exp(-lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(shadowTextures[shadowIndex], shadowTexCoords.xy).y;
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
        mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + i];
        vec4 positionShadowClip = shadowMatrix * position;
        vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
        if (shadowTexCoordsProj.x >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.x < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.y >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.y < 1.0 - SHADOW_CASCADE_SEAM_INSET &&
            shadowTexCoordsProj.z >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_CASCADE_SEAM_INSET)
        {
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            float shadowZ = shadowTexCoords.z;
            float shadowZExp = exp(-lightShadowExponent * shadowZ);
            float shadowDepthExp = texture(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], vec3(shadowTexCoords.xy, float(i))).y;
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
    vec3 result = vec3(0.0);
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex >= 0)
    {
        // grab light values
        vec3 lightOrigin = lightOrigins[lightIndex];
        float lightCutoff = lightCutoffs[lightIndex];
        vec3 lightDirection = lightDirections[lightIndex];
        float lightAttenuationLinear = lightAttenuationLinears[lightIndex];
        float lightAttenuationQuadratic = lightAttenuationQuadratics[lightIndex];
        float lightConeInner = lightConeInners[lightIndex];
        float lightConeOuter = lightConeOuters[lightIndex];

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

        // march over ray, accumulating fog light value
        vec3 currentPosition = startPosition + step * dithering;
        float validSteps = 0.0001; // epsilon to avoid dbz
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec3 positionShadow = currentPosition - lightOrigin;
            float shadowZ = length(positionShadow);
            float shadowDepth = texture(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], positionShadow).x;

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
        result = smoothstep(0.0, 1.0, result / validSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
    }
    return result;
}

vec3 computeFogAccumSpot(vec4 position, int lightIndex)
{
    vec3 result = vec3(0.0);
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex >= 0)
    {
        // grab light values
        vec3 lightOrigin = lightOrigins[lightIndex];
        float lightCutoff = lightCutoffs[lightIndex];
        vec3 lightDirection = lightDirections[lightIndex];
        float lightAttenuationLinear = lightAttenuationLinears[lightIndex];
        float lightAttenuationQuadratic = lightAttenuationQuadratics[lightIndex];
        float lightConeInner = lightConeInners[lightIndex];
        float lightConeOuter = lightConeOuters[lightIndex];

        // compute shadow space
        mat4 shadowMatrix = shadowMatrices[shadowIndex];

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

        // march over ray, accumulating fog light value
        vec3 currentPosition = startPosition + step * dithering;
        float validSteps = 0.0001; // epsilon to avoid dbz
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures[shadowIndex], shadowTexCoords.xy).x : 1.0;

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
        result = smoothstep(0.0, 1.0, result / validSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
    }
    return result;
}

vec3 computeFogAccumDirectional(vec4 position, int lightIndex)
{
    vec3 result = vec3(0.0);
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex >= 0)
    {
        // grab light values
        vec3 lightOrigin = lightOrigins[lightIndex];
        vec3 lightDirection = lightDirections[lightIndex];

        // compute shadow space
        mat4 shadowMatrix = shadowMatrices[shadowIndex];

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

        // march over ray, accumulating fog light value
        vec3 currentPosition = startPosition + step * dithering;
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures[shadowIndex], shadowTexCoords.xy).x : 1.0;

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
        result = smoothstep(0.0, 1.0, result / ssvfSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
    }
    return result;
}

vec3 computeFogAccumCascaded(vec4 position, int lightIndex)
{
    vec3 result = vec3(0.0);
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex >= 0)
    {
        // grab light values
        vec3 lightOrigin = lightOrigins[lightIndex];
        vec3 lightDirection = lightDirections[lightIndex];

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

        // march over ray, accumulating fog light value
        vec3 currentPosition = startPosition + step * dithering;
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // compute depths
                mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + j];
                vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
                vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
                vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
                bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
                float shadowZ = shadowTexCoords.z;
                float shadowDepth = shadowTexCoordsInRange ? texture(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], vec3(shadowTexCoords.xy, float(i))).x : 1.0;

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
        result = smoothstep(0.0, 1.0, result / (ssvfSteps * SHADOW_CASCADE_LEVELS)) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
    }
    return result;
}

void computeSsrr(float depth, vec4 position, vec3 normal, float refractiveIndex, out vec3 diffuseScreen, out float diffuseScreenWeight)
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
    vec2 texSize = textureSize(depthTexture, 0).xy;
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
        currentDepth = texture(depthTexture, currentTexCoords).r;
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
                currentDepth = texture(depthTexture, currentTexCoords).r;
                currentPosition = depthToPosition(currentDepth, currentTexCoords);
                currentPositionView = view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(pow(-currentPositionView.z, 32.0) * ssrrRayThickness, ssrrRayThickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // compute screen-space diffuse color and weight
                    diffuseScreen = texture(colorTexture, currentTexCoords).rgb * ssrrIntensity;
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
    float height = texture(heightTexture, texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo with alpha sample
    float opaqueDistance = heightPlusOut.w;
    vec4 albedoSample = texture(albedoTexture, texCoords);
    vec4 albedo =
        vec4(
            pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb,
            mix(albedoSample.a, 1.0, smoothstep(opaqueDistance * 0.667, opaqueDistance, distance)));

    // compute material properties
    float roughness = texture(roughnessTexture, texCoords).r * materialOut.r;
    float metallic = texture(metallicTexture, texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).b * materialOut.b;
    vec3 emission = vec3(texture(emissionTexture, texCoords).r * materialOut.a);

    // compute ignore light maps
    bool ignoreLightMaps = heightPlusOut.y != 0.0;

    // compute light accumulation
    vec3 n = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
    vec3 v = normalize(eyeCenter - position.xyz);
    float nDotV = max(dot(n, v), 0.0);
    vec3 f0 = mix(vec3(0.04), albedo.rgb, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    float refractiveIndex = subsurfacePlusOut.w;
    vec3 lightAccumDiffuse = vec3(0.0);
    vec3 lightAccumSpecular = vec3(0.0);
    vec3 fogAccum = vec3(0.0);
    for (int i = 0; i < lightsCount; ++i)
    {
        // per-light radiance
        vec3 lightOrigin = lightOrigins[i];
        float lightCutoff = lightCutoffs[i];
        int lightType = lightTypes[i];
        bool lightDirectional = lightType == 2;
        bool lightCascaded = lightType == 3;
        vec3 l, h, radiance;
        if (!lightDirectional && !lightCascaded)
        {
            vec3 d = lightOrigin - position.xyz;
            l = normalize(d);
            h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinears[i] * distance + lightAttenuationQuadratics[i] * distanceSquared);
            float angle = acos(dot(l, -lightDirections[i]));
            float halfConeInner = lightConeInners[i] * 0.5;
            float halfConeOuter = lightConeOuters[i] * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity;
        }
        else
        {
            l = -lightDirections[i];
            h = normalize(v + l);
            radiance = lightColors[i] * lightBrightnesses[i];
        }

        // shadow scalar
        int shadowIndex = lightShadowIndices[i];
        float shadowScalar = 1.0f;
        if (shadowIndex >= 0)
        {
            switch (lightType)
            {
                case 0: { shadowScalar = computeShadowScalarPoint(position, lightOrigin, shadowIndex); break; } // point
                case 1: { shadowScalar = computeShadowScalarSpot(position, lightConeOuters[i], shadowIndex); break; } // spot
                case 2: { shadowScalar = computeShadowScalarDirectional(position, shadowIndex); break; } // directional
                default: { shadowScalar = computeShadowScalarCascaded(position, lightCutoff, shadowIndex); break; } // cascaded
            }
        }

        // cook-torrance brdf
        float hDotV = max(dot(h, v), 0.0);
        float ndf = distributionGGX(n, h, roughness);
        float g = geometrySchlick(n, v, l, roughness);
        vec3 f = fresnelSchlick(hDotV, f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float nDotL = max(dot(n, l), 0.0);
        float denominator = 4.0 * nDotV * nDotL + 0.0001; // add epsilon to prevent division by zero
        vec3 specular = numerator / denominator;

        // compute diffusion
        vec3 kS = f;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        // compute burley diffusion approximation (unlike lambert, this is NOT energy-preserving!)
        float lDotH = max(dot(l, h), 0.0);
        float f90 = 0.5 + 2.0 * roughness * lDotH * lDotH; // retroreflection term
        float lightScatter = pow(1.0 - nDotL, 5.0) * (f90 - 1.0) + 1.0;
        float viewScatter  = pow(1.0 - nDotV, 5.0) * (f90 - 1.0) + 1.0;
        float burley = lightScatter * viewScatter;

        // add to outgoing lightAccums
        vec3 lightScalar = radiance * nDotL * shadowScalar;
        lightAccumDiffuse += (kD * albedo.rgb / PI * burley) * lightScalar;
        lightAccumSpecular += specular * lightScalar;

        // accumulate fog
        if (ssvfEnabled == 1 && lightDesireFogs[i] == 1)
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
    if (lm2 != -1 && !inBounds(position.xyz, lightMapMins[lm2], lightMapSizes[lm2])) lm2 = -1;
    if (lm1 != -1 && !inBounds(position.xyz, lightMapMins[lm1], lightMapSizes[lm1])) lm1 = lm2;

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
        irradiance = texture(irradianceMap, n).rgb;
        vec3 r = reflect(-v, n);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * REFLECTION_LOD_MAX).rgb;
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr = k >= 0.0 ? refract(-v, n, refractiveIndex) : r;
        environmentFilterRefracted = ssrrDesired ? textureLod(environmentFilterMap, rfr, 0).rgb : vec3(1.0);
    }
    else if (lm2 == -1)
    {
        ambientColor = lightMapAmbientColors[lm1];
        ambientBrightness = lightMapAmbientBrightnesses[lm1];
        irradiance = texture(irradianceMaps[lm1], n).rgb;
        vec3 r = parallaxCorrection(lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position.xyz, n);
        environmentFilter = textureLod(environmentFilterMaps[lm1], r, roughness * REFLECTION_LOD_MAX).rgb;
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr = k >= 0.0 ? refract(-v, n, refractiveIndex) : r;
        environmentFilterRefracted = ssrrDesired ? textureLod(environmentFilterMaps[lm1], rfr, 0).rgb : vec3(1.0);
    }
    else
    {
        // compute blending
        float ratio = computeDepthRatio(lightMapMins[lm1], lightMapSizes[lm1], lightMapMins[lm2], lightMapSizes[lm2], position.xyz, n);

        // compute blended ambient values
        vec3 ambientColor1 = lightMapAmbientColors[lm1];
        vec3 ambientColor2 = lightMapAmbientColors[lm2];
        float ambientBrightness1 = lightMapAmbientBrightnesses[lm1];
        float ambientBrightness2 = lightMapAmbientBrightnesses[lm2];
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(irradianceMaps[lm1], n).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2], n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position.xyz, n);
        vec3 r2 = parallaxCorrection(lightMapOrigins[lm2], lightMapMins[lm2], lightMapSizes[lm2], position.xyz, n);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1], r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2], r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);

        // compute blended environment filter refracted
        float cosNvn = dot(-v, n);
        float k = 1.0 - refractiveIndex * refractiveIndex * (1.0 - cosNvn * cosNvn);
        vec3 rfr1 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r1;
        vec3 rfr2 = k >= 0.0 ? refract(-v, n, refractiveIndex) : r2;
        vec3 environmentFilterRefracted1 = ssrrDesired ? textureLod(environmentFilterMaps[lm1], rfr1, 0).rgb : vec3(1.0);
        vec3 environmentFilterRefracted2 = ssrrDesired ? textureLod(environmentFilterMaps[lm2], rfr2, 0).rgb : vec3(1.0);
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
    vec3 diffuse = vec3(0.0);
    if (ssrrDesired)
    {
        vec3 diffuseScreen = vec3(0.0);
        float diffuseScreenWeight = 0.0;
        computeSsrr(depth, position, normal, refractiveIndex, diffuseScreen, diffuseScreenWeight);
        diffuse = (1.0f - diffuseScreenWeight) * ambientColorRefracted + diffuseScreenWeight * diffuseScreen;
    }
    else
    {
        vec3 kS = f;
        vec3 kD = 1.0 - kS;
        kD *= 1.0 - metallic;
        diffuse = kD * irradiance * albedo.rgb * ambientDiffuse;
    }

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(nDotV, roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * ambientSpecular;

    // compute color composition
    vec3 color = lightAccumDiffuse + diffuse + emission * albedo.rgb + lightAccumSpecular + specular + fogAccum;

    // compute and apply global fog when enabled
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

    // increase alpha when accumulated specular light or fog exceeds albedo alpha. Also tone map and gamma-correct
    // specular light color (doing so seems to look better). Finally, apply alpha from albedo uniform.
    float specularScalar = subsurfacePlusOut.z;
    lightAccumSpecular = lightAccumSpecular / (lightAccumSpecular + vec3(1.0));
    lightAccumSpecular = pow(lightAccumSpecular, vec3(1.0 / GAMMA));
    lightAccumSpecular = lightAccumSpecular * specularScalar;
    float lightAccumAlpha = (lightAccumSpecular.r + lightAccumSpecular.g + lightAccumSpecular.b) / 3.0;
    float fogAccumAlphaScalar = 6.0; // arbitrary scalar
    float fogAccumAlpha = (fogAccum.r + fogAccum.g + fogAccum.b) / 3.0 * fogAccumAlphaScalar;
    albedo.a = max(albedo.a, max(lightAccumAlpha, fogAccumAlpha * 2.0));
    albedo.a = albedo.a * albedoOut.a;

    // write fragment
    frag = vec4(color, albedo.a);
}
