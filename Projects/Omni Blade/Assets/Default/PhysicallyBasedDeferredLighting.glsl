#shader vertex
#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 460 core

const float PI = 3.141592654;
const float PI_OVER_2 = PI / 2.0;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const int SHADOW_TEXTURES_MAX = 8;
const int SHADOW_MAPS_MAX = 7;
const float SHADOW_DIRECTIONAL_SEAM_INSET = 0.05; // TODO: see if this should be proportionate to shadow texel size.
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;
const float SHADOW_CASCADE_SEAM_INSET = 0.005;
const float SHADOW_CASCADE_DENSITY_BONUS = 0.5;
const float SHADOW_FOV_MAX = 2.1;

const vec4 SSVF_DITHERING[4] =
vec4[](
    vec4(0.0, 0.5, 0.125, 0.625),
    vec4(0.75, 0.22, 0.875, 0.375),
    vec4(0.1875, 0.6875, 0.0625, 0.5625),
    vec4(0.9375, 0.4375, 0.8125, 0.3125));

uniform vec3 eyeCenter;
uniform mat4 view;
uniform mat4 viewInverse;
uniform mat4 projection;
uniform mat4 projectionInverse;
uniform float lightCutoffMargin;
uniform int lightShadowSamples;
uniform float lightShadowBias;
uniform float lightShadowSampleScalar;
uniform float lightShadowExponent;
uniform float lightShadowDensity;
uniform int sssEnabled;
uniform int ssvfEnabled;
uniform int ssvfSteps;
uniform float ssvfAsymmetry;
uniform float ssvfIntensity;
uniform sampler2D depthTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalPlusTexture;
uniform sampler2D subdermalPlusTexture;
uniform sampler2D scatterPlusTexture;
uniform sampler2D shadowTextures[SHADOW_TEXTURES_MAX];
uniform samplerCube shadowMaps[SHADOW_MAPS_MAX];
uniform sampler2DArray shadowCascades[SHADOW_CASCADES_MAX];
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
uniform float shadowNear;
uniform mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS];

in vec2 texCoordsOut;

layout(location = 0) out vec4 lightAccum;
layout(location = 1) out vec4 fogAccum;

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

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = projectionInverse * positionClip;
    positionView /= positionView.w;
    return viewInverse * positionView;
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

float geometrySchlick(vec3 n, vec3 v, vec3 l, float roughness)
{
    float nDotV = max(dot(n, v), 0.0);
    float nDotL = max(dot(n, l), 0.0);
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

float geometryTravelPoint(vec4 position, int lightIndex, int shadowIndex)
{
    // compute travel average in world space
    vec3 lightOrigin = lightOrigins[lightIndex];
    vec3 positionShadow = position.xyz - lightOrigin;
    float shadowZ = length(positionShadow);
    float travel = 0.0;
    for (int i = -1; i <= 1; i += 2)
    {
        for (int j = -1; j <= 1; j += 2)
        {
            for (int k = -1; k <= 1; k += 2)
            {
                vec3 offset = vec3(i, j, k) * lightShadowSampleScalar;
                float shadowDepth = texture(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], positionShadow + offset).x;
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
        float shadowFar = lightCutoffs[lightIndex];
        float shadowZ = worldToDepthView(shadowNear, shadowFar, shadowMatrix, position);

        // compute light distance travel through surface (not accounting for incidental surface concavity)
        float travel = 0.0;
        vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5; // adj-ndc space
        vec2 shadowTextureSize = textureSize(shadowTextures[shadowIndex], 0);
        vec2 shadowTexelSize = 1.0 / shadowTextureSize;
        for (int i = -1; i <= 1; ++i)
        {
            for (int j = -1; j <= 1; ++j)
            {
                float shadowDepthScreen = texture(shadowTextures[shadowIndex], shadowTexCoords + vec2(i, j) * shadowTexelSize).x;
                float shadowDepth = depthScreenToDepthView(shadowNear, shadowFar, shadowDepthScreen);
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
        vec2 shadowTextureSize = textureSize(shadowTextures[shadowIndex], 0);
        vec2 shadowTexelSize = 1.0 / shadowTextureSize;
        float shadowDepthScreen = texture(shadowTextures[shadowIndex], shadowTexCoords.xy).x; // linear, screen space
        float delta = shadowZScreen - shadowDepthScreen;
        float shadowFar = lightCutoffs[lightIndex];
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
            vec2 shadowTextureSize = textureSize(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], 0).xy;
            vec2 shadowTexelSize = 1.0 / shadowTextureSize;
            float shadowDepthScreen = texture(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], vec3(shadowTexCoords.xy, float(i))).x; // linear, screen space
            float delta = shadowZScreen - shadowDepthScreen;
            float shadowFar = lightCutoffs[lightIndex];
            return max(0.0, delta * shadowFar);
        }
    }

    // tracing out of range, return default
    return 1.0;
}

vec3 computeSubsurfaceScatter(vec4 position, vec3 albedo, vec4 subdermalPlus, vec4 scatterPlus, float nDotL, vec2 texCoords, int lightIndex)
{
    // retrieve light and shadow values
    int lightType = lightTypes[lightIndex];
    int shadowIndex = lightShadowIndices[lightIndex];

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
        const float g = 0.2; // Henyey–Greenstein anisotropy (0 = isotropic, >0 = forward bias)

        // attenuation by travel distance (Beer–Lambert law)
        vec3 attenuation = exp(-travel * density * finenessSquared * scatter.rgb);

        // Henyey–Greenstein phase function for angular dependence
        float cosTheta = clamp(nDotL, -1.0, 1.0);
        float denom = 1.0 + g * g - 2.0 * g * cosTheta;
        float phase = (1.0 - g * g) / (4.0 * PI * pow(denom, 1.5));

        // fin
        return subdermal * attenuation * phase * waxTint;
    }
    return vec3(0.0); // nop formula
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

void main()
{
    // ensure fragment was written
    float depth = texture(depthTexture, texCoordsOut).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoordsOut);

    // retrieve remaining data from geometry buffers
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);
    vec3 normal = normalize(texture(normalPlusTexture, texCoordsOut).xyz);
    vec4 subdermalPlus = vec4(0.0);
    vec4 scatterPlus = vec4(0.0);
    if (sssEnabled == 1)
    {
        subdermalPlus = texture(subdermalPlusTexture, texCoordsOut);
        scatterPlus = texture(scatterPlusTexture, texCoordsOut);
    }

    // compute materials
    float roughness = material.r;
    float metallic = material.g;

    // clear accumulation buffers because there seems to exist a Mesa bug where glClear doesn't work on certain
    // platforms on this buffer - https://github.com/bryanedds/Nu/issues/800#issuecomment-3239861861
    lightAccum = vec4(0.0);
    fogAccum = vec4(0.0);

    // compute light accumulation
    vec3 v = normalize(eyeCenter - position.xyz);
    float nDotV = max(dot(normal, v), 0.0);
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    for (int i = 0; i < lightsCount; ++i)
    {
        // per-light radiance
        vec3 lightOrigin = lightOrigins[i];
        float lightCutoff = lightCutoffs[i];
        int lightType = lightTypes[i];
        bool lightDirectional = lightType == 2;
        bool lightCascaded = lightType == 3;
        vec3 l, h, radiance;
        float intensity = 0.0;
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
            intensity = attenuation * halfConeScalar * cutoffScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity;
        }
        else
        {
            l = -lightDirections[i];
            h = normalize(v + l);
            intensity = 1.0;
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
        float ndf = distributionGGX(normal, h, roughness);
        float g = geometrySchlick(normal, v, l, roughness);
        vec3 f = fresnelSchlick(hDotV, f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float nDotL = max(dot(normal, l), 0.0);
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

        // accumulate light, clearing on first light (HACK: seems to fix glClear not working on the respective buffer
        // on certain platforms)
        lightAccum.rgb += (kD * albedo / PI * burley + specular) * radiance * nDotL * shadowScalar;

        // accumulate light from subsurface scattering
        float scatterType = scatterPlus.a;
        if (sssEnabled == 1 && scatterType != 0.0)
        {
            vec3 scatter = computeSubsurfaceScatter(position, albedo, subdermalPlus, scatterPlus, nDotL, texCoordsOut, i);
            lightAccum.rgb += kD * scatter * radiance;
        }

        // accumulate fog, clearing on first light (HACK: seems to fix glClear not working on the respective buffer on
        // certain platforms)
        if (ssvfEnabled == 1 && lightDesireFogs[i] == 1)
        {
            switch (lightType)
            {
                case 0: { fogAccum.rgb += computeFogAccumPoint(position, i); break; } // point
                case 1: { fogAccum.rgb += computeFogAccumSpot(position, i); break; } // spot
                case 2: { fogAccum.rgb += computeFogAccumDirectional(position, i); break; } // directional
                default: { fogAccum.rgb += computeFogAccumCascaded(position, i); break; } // cascaded
            }
        }
    }
}
