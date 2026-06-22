#version 450 core

const float PI = 3.141592654;
const float PI_OVER_2 = PI / 2.0;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const int SHADOW_TEXTURES_MAX = 12;
const int SHADOW_MAPS_MAX = 12;
const float SHADOW_DIRECTIONAL_SEAM_INSET = 0.05; // TODO: see if this should be proportionate to shadow texel size.
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;
const float SHADOW_CASCADE_SEAM_INSET = 0.005;
const float SHADOW_CASCADE_DENSITY_BONUS = 0.5;
const float SHADOW_FOV_MAX = 2.1;
const float CLEAR_COAT_REFRACTIVE_INDEX = 1.5; // typical for automotive clear coat

struct Eye
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct Lighting
{
    float lightCutoffMargin;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
    int sssEnabled;
    int lightsCount;
    float shadowNear;
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

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) buffer readonly LightingBlock { Lighting lighting; };
layout(set = 0, binding = 2) buffer readonly LightBlock { Light lights[LIGHTS_MAX]; };
layout(set = 0, binding = 3) buffer readonly ShadowMatrixBlock { mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS]; };
layout(set = 0, binding = 4) uniform texture2D depthTexture;
layout(set = 0, binding = 5) uniform texture2D albedoTexture;
layout(set = 0, binding = 6) uniform texture2D materialTexture;
layout(set = 0, binding = 7) uniform texture2D normalPlusTexture;
layout(set = 0, binding = 8) uniform texture2D subdermalPlusTexture;
layout(set = 0, binding = 9) uniform texture2D scatterPlusTexture;
layout(set = 0, binding = 10) uniform texture2D clearCoatPlusTexture;
layout(set = 0, binding = 11) uniform texture2DArray shadowTextures;
layout(set = 0, binding = 12) uniform textureCube shadowMaps[SHADOW_MAPS_MAX];
layout(set = 0, binding = 13) uniform texture2DArray shadowCascades[SHADOW_CASCADES_MAX];

layout(set = 1, binding = 0) uniform sampler geometrySampler;
layout(set = 1, binding = 1) uniform sampler shadowSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec3 frag;

float saturate(float v)
{
    return clamp(v, 0.0f, 1.0);
}

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

vec3 rotate(vec3 axis, float angle, vec3 v)
{
    return mix(dot(axis, v) * axis, v, cos(angle)) + cross(axis, v) * sin(angle);
}

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

float depthViewToDepthBuffer(float near, float far, float depthView)
{
    return (-depthView - near) / (far - near);
}

float depthScreenToDepthView(float near, float far, float depthScreen)
{
    // for a standard OpenGL projection, compute a and b:
    float a = -(far + near) / (far - near);
    float b = -(2.0 * far * near) / (far - near);

    // convert depth from [0, 1] to normalized device coordinate (NDC) z in [-1, 1].
    float ndcZ = depthScreen * 2.0 - 1.0;

    // recover view-space z: note that view-space z is negative in front of the camera.
    // when depthScreen is 0 (near plane), ndcZ is -1 and view.z becomes -near.
    // when depthScreen is 1 (far plane), ndcZ is 1 and view.z becomes -far.
    return b / (ndcZ + a);
}

float worldToDepthView(float near, float far, mat4 viewProjection, vec4 position)
{
    // for a standard OpenGL projection, compute a and b:
    float a = -(far + near) / (far - near);
    float b = -(2.0 * far * near) / (far - near);

    // transform into light clip space using the combined shadow matrix.
    vec4 positionClip = viewProjection * position;

    // perspective division gives you the light's normalized device coordinates.
    float ndcZ = positionClip.z / positionClip.w; // in range [-1, 1]

    // invert the projection depth mapping to recover light view-space depth.
    return b / (ndcZ + a);
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

float geometrySchlick(vec3 n, vec3 v, vec3 l, float roughness)
{
    float nDotV = saturate(dot(n, v));
    float nDotL = saturate(dot(n, l));
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
                shadowHits += shadowZ - lighting.lightShadowBias > texture(samplerCube(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), positionShadow + offset).x ? 1.0 : 0.0;
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
        shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
    {
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZ = shadowTexCoords.z;
        float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy, float(shadowIndex))).y;
        float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
        shadowScalar = pow(shadowScalar, lighting.lightShadowDensity);
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
        float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
        float shadowDepthExp = texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy, float(shadowIndex))).y;
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
            shadowTexCoordsProj.z >= -1.0 + SHADOW_CASCADE_SEAM_INSET && shadowTexCoordsProj.z < 1.0 - SHADOW_CASCADE_SEAM_INSET)
        {
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            float shadowZ = shadowTexCoords.z;
            float shadowZExp = exp(-lighting.lightShadowExponent * shadowZ);
            float shadowDepthExp = texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), vec3(shadowTexCoords.xy, float(i))).y;
            float shadowScalar = clamp(shadowZExp * shadowDepthExp, 0.0, 1.0);
            float densityScalar = 1.0f + float(i) * SHADOW_CASCADE_DENSITY_BONUS;
            shadowScalar = pow(shadowScalar, lighting.lightShadowDensity * densityScalar);
            return shadowScalar;
        }
    }
    return 1.0;
}

float geometryTravelPoint(vec4 position, int lightIndex, int shadowIndex)
{
    // compute travel average in world space
    vec3 lightOrigin = lights[lightIndex].lightOrigins;
    vec3 positionShadow = position.xyz - lightOrigin;
    float shadowZ = length(positionShadow);
    float travel = 0.0;
    for (int i = -1; i <= 1; i += 2)
    {
        for (int j = -1; j <= 1; j += 2)
        {
            for (int k = -1; k <= 1; k += 2)
            {
                vec3 offset = vec3(i, j, k) * lighting.lightShadowSampleScalar;
                float shadowDepth = texture(samplerCube(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), positionShadow + offset).x;
                float delta = shadowZ - shadowDepth;
                travel += max(0.0, delta);
            }
        }
    }
    return travel / 8.0;
}

float geometryTravelSpot(vec4 position, int lightIndex, int shadowIndex)
{
    // attempt to compute travel average in view space
    mat4 shadowMatrix = shadowMatrices[shadowIndex];
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
        shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
        shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
    {
        // compute z position in view space
        float shadowFar = lights[lightIndex].lightCutoffs;
        float shadowZ = worldToDepthView(lighting.shadowNear, shadowFar, shadowMatrix, position);

        // compute light distance travel through surface (not accounting for incidental surface concavity)
        float travel = 0.0;
        vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5; // adj-ndc space
        vec2 shadowTextureSize = textureSize(sampler2DArray(shadowTextures, shadowSampler), 0).xy;
        vec2 shadowTexelSize = 1.0 / shadowTextureSize;
        for (int i = -1; i <= 1; ++i)
        {
            for (int j = -1; j <= 1; ++j)
            {
                float shadowDepthScreen = texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy + vec2(i, j) * shadowTexelSize, float(shadowIndex))).x;
                float shadowDepth = depthScreenToDepthView(lighting.shadowNear, shadowFar, shadowDepthScreen);
                float delta = shadowZ - shadowDepth;
                travel += max(0.0, delta);
            }
        }
        return travel / 9.0;
    }

    // tracing out of range, return default
    return 1.0;
}

float geometryTravelDirectional(vec4 position, int lightIndex, int shadowIndex)
{
    // attempt to compute travel average in view space
    mat4 shadowMatrix = shadowMatrices[shadowIndex];
    vec4 positionShadowClip = shadowMatrix * position;
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
        shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
        shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
    {
        // compute light distance travel through surface (not accounting for incidental surface concavity)
        vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
        float shadowZScreen = shadowTexCoords.z; // linear, screen space
        vec2 shadowTextureSize = textureSize(sampler2DArray(shadowTextures, shadowSampler), 0).xy;
        vec2 shadowTexelSize = 1.0 / shadowTextureSize;
        float shadowDepthScreen = texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy, float(shadowIndex))).x; // linear, screen space
        float delta = shadowZScreen - shadowDepthScreen;
        float shadowFar = lights[lightIndex].lightCutoffs;
        return max(0.0, delta * shadowFar);
    }

    // tracing out of range, return default
    return 1.0;
}

float geometryTravelCascaded(vec4 position, int lightIndex, int shadowIndex)
{
    for (int i = 0; i < SHADOW_CASCADE_LEVELS; ++i)
    {
        // attempt to compute travel average in view space
        mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + i];
        vec4 positionShadowClip = shadowMatrix * position;
        vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
        if (shadowTexCoordsProj.x >= -1.0 && shadowTexCoordsProj.x < 1.0 &&
            shadowTexCoordsProj.y >= -1.0 && shadowTexCoordsProj.y < 1.0 &&
            shadowTexCoordsProj.z >= -1.0 && shadowTexCoordsProj.z < 1.0)
        {
            // compute light distance travel through surface (not accounting for incidental surface concavity)
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            float shadowZScreen = shadowTexCoords.z; // linear, screen space
            vec2 shadowTextureSize = textureSize(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), 0).xy;
            vec2 shadowTexelSize = 1.0 / shadowTextureSize;
            float shadowDepthScreen = texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), vec3(shadowTexCoords.xy, float(i))).x; // linear, screen space
            float delta = shadowZScreen - shadowDepthScreen;
            float shadowFar = lights[lightIndex].lightCutoffs;
            return max(0.0, delta * shadowFar);
        }
    }

    // tracing out of range, return default
    return 1.0;
}

vec3 computeSubsurfaceScatter(vec4 position, vec3 albedo, vec4 subdermalPlus, vec4 scatterPlus, float nDotL, vec2 texCoords, int lightIndex)
{
    // retrieve light and shadow values
    Light light = lights[lightIndex];
    int lightType = light.lightTypes;
    int shadowIndex = light.lightShadowIndices;

    // compute geometry travel distance through material, defaulting to 1.0 when no shadow present for this light index
    float travel = 1.0;
    if (shadowIndex >= 0)
    {
        switch (lightType)
        {
        case 0: // point light
            travel = geometryTravelPoint(position, lightIndex, shadowIndex);
            break;
        case 1: // spot light
            travel = geometryTravelSpot(position, lightIndex, shadowIndex);
            break;
        case 2: // directional light
            travel = geometryTravelDirectional(position, lightIndex, shadowIndex);
            break;
        default: // cascaded light
            travel = geometryTravelCascaded(position, lightIndex, shadowIndex);
            break;
        }
    }

    // compute scattered color
    vec3 subdermal = subdermalPlus.rgb;
    float fineness = subdermalPlus.a;
    float finenessSquared = fineness * fineness;
    vec3 scatter = scatterPlus.rgb;
    float scatterType = scatterPlus.a;
    if (scatterType > 0.09 && scatterType < 0.11) // skin formula
    {
        const float density = 15.0;
        vec3 radii = finenessSquared * scatter.rgb * clamp(exp(-travel * density), 0.0, 1.0);
        float nDotLPos = clamp(nDotL, 0.0, 1.0);
        float nDotLNeg = clamp(-nDotL, 0.0, 1.0);
        vec3 scalar =
            0.2 *
            pow(vec3(1.0 - nDotLPos), 0.1 / (radii + 0.001)) *
            pow(vec3(1.0 - nDotLNeg), 0.1 / (radii + 0.001));
        return subdermal * radii * scalar;
    }
    if (scatterType > 0.19 && scatterType < 0.21) // foliage formula
    {
        const float density = 5.0;
        vec3 radii = finenessSquared * scatter.rgb * clamp(exp(-travel * density), 0.0, 1.0);
        vec3 scalar =
            0.2 *
            exp(-3.0 * abs(nDotL) / (radii + 0.001));
        return subdermal * radii * scalar;
    }
    if (scatterType > 0.29 && scatterType < 0.31) // wax formula
    {
        // tunable parameters
        const float density = 8.0; // absorption coefficient
        const vec3 waxTint = vec3(1.0, 0.94, 0.85); // warm tint
        const float g = 0.2; // Henyey-Greenstein anisotropy (0 = isotropic, >0 = forward bias)

        // attenuation by travel distance (Beer-Lambert law)
        vec3 attenuation = exp(-travel * density * finenessSquared * scatter.rgb);

        // Henyey-Greenstein phase function for angular dependence
        float cosTheta = clamp(nDotL, -1.0, 1.0);
        float denom = 1.0 + g * g - 2.0 * g * cosTheta;
        float phase = (1.0 - g * g) / (4.0 * PI * pow(denom, 1.5));

        // fin
        return subdermal * attenuation * phase * waxTint;
    }
    return vec3(0.0); // nop formula
}

void main()
{
    // initialize light accumulation
    vec3 lightAccum = vec3(0.0);

    // ensure fragment was written
    float depth = texture(sampler2D(depthTexture, geometrySampler), texCoordsOut).r;
    if (depth != 0.0)
    {
        // recover position from depth
        vec4 position = depthToPosition(depth, texCoordsOut);

        // retrieve remaining data from geometry buffers
        vec3 albedo = texture(sampler2D(albedoTexture, geometrySampler), texCoordsOut).rgb;
        vec4 material = texture(sampler2D(materialTexture, geometrySampler), texCoordsOut);
        vec3 normal = normalize(texture(sampler2D(normalPlusTexture, geometrySampler), texCoordsOut).xyz);
        vec4 subdermalPlus = vec4(0.0);
        vec4 scatterPlus = vec4(0.0);
        if (lighting.sssEnabled == 1)
        {
            subdermalPlus = texture(sampler2D(subdermalPlusTexture, geometrySampler), texCoordsOut);
            scatterPlus = texture(sampler2D(scatterPlusTexture, geometrySampler), texCoordsOut);
        }

        // compute materials
        float roughness = material.r;
        float metallic = material.g;

        // compute clear coat values
        vec4 clearCoatPlus = texture(sampler2D(clearCoatPlusTexture, geometrySampler), texCoordsOut);
        float clearCoat = clearCoatPlus.r;
        float clearCoatRoughness = clearCoatPlus.g;
        vec3 clearCoatNormal = decodeOctahedral(clearCoatPlus.ba);

        // accumulate light
        vec3 v = normalize(eye.center - position.xyz);
        float nDotV = saturate(dot(normal, v));
        vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
        for (int i = 0; i < lighting.lightsCount; ++i)
        {
            // compute per-light radiance
            Light light = lights[i];
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
                hDotV = saturate(dot(h,  v));
                float distanceSquared = dot(d, d);
                float distance = sqrt(distanceSquared);
                float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lighting.lightCutoffMargin), lightCutoff, distance);
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
                float ndf = distributionGGX(normal, h, roughness);
                float g = geometrySchlick(normal, v, l, roughness);
                vec3 f = fresnelSchlick(hDotV, f0);

                // compute specularity
                vec3 numerator = ndf * g * f;
                float nDotL = saturate(dot(normal, l));
                float denominator = 4.0 * nDotV * nDotL + 0.0001; // add epsilon to prevent division by zero
                vec3 specular = clamp(numerator / denominator, 0.0, 10000.0);

                // mix in specularity of clear coat when desired
                if (clearCoat > 0.0)
                {
                    // nDotV and f0 derived from clear coat specific values
                    float nDotV = saturate(dot(clearCoatNormal, v));
                    vec3 f0 = vec3(pow((CLEAR_COAT_REFRACTIVE_INDEX - 1.0) / (CLEAR_COAT_REFRACTIVE_INDEX + 1.0), 2.0));

                    // cook-torrance brdf
                    float ndf = distributionGGX(clearCoatNormal, h, clearCoatRoughness);
                    float g = geometrySchlick(clearCoatNormal, v, l, clearCoatRoughness);
                    vec3 f = fresnelSchlick(hDotV, f0);

                    // compute specularity
                    vec3 numerator = ndf * g * f;
                    float nDotL = saturate(dot(clearCoatNormal, l));
                    float denominator = 4.0 * nDotV * nDotL + 0.0001; // add epsilon to prevent division by zero
                    vec3 clearCoatSpecular = clamp(numerator / denominator, 0.0, 10000.0);

                    // mix specular
                    specular = mix(specular, clearCoatSpecular, clearCoat);
                }

                // compute diffusion
                vec3 kS = f;
                vec3 kD = vec3(1.0) - kS;
                kD *= 1.0 - metallic;

                // compute burley diffusion approximation (unlike lambert, this is NOT energy-preserving!)
                float lDotH = saturate(dot(l, h));
                float f90 = 0.5 + 2.0 * roughness * lDotH * lDotH; // retroreflection term
                float lightScatter = pow(1.0 - nDotL, 5.0) * (f90 - 1.0) + 1.0;
                float viewScatter = pow(1.0 - nDotV, 5.0) * (f90 - 1.0) + 1.0;
                float burley = lightScatter * viewScatter;

                // accumulate light
                lightAccum += (kD * albedo / PI * burley + specular) * radiance * nDotL * shadowScalar;

                // accumulate light from subsurface scattering
                float scatterType = scatterPlus.a;
                if (lighting.sssEnabled == 1 && scatterType != 0.0)
                {
                    vec3 scatter = computeSubsurfaceScatter(position, albedo, subdermalPlus, scatterPlus, nDotL, texCoordsOut, i);
                    lightAccum += kD * scatter * radiance;
                }
            }
        }
    }

    // write light accumulation
    frag = lightAccum;
}
