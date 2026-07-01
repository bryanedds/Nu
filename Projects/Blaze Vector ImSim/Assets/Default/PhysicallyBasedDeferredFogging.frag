#version 450 core

const float PI = 3.141592654;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const int SHADOW_TEXTURES_MAX = 12;
const int SHADOW_MAPS_MAX = 12;
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;

const vec4 SSVF_DITHERING[4] =
    vec4[](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

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

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) buffer readonly LightingBlock { Lighting lighting; };
layout(set = 0, binding = 2) buffer readonly LightsGeneralBlock { LightsGeneral lightsGeneral; };
layout(set = 0, binding = 3) buffer readonly LightsBlock { Light lights[LIGHTS_MAX]; };
layout(set = 0, binding = 4) buffer readonly ShadowMatrixBlock { mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS]; };
layout(set = 0, binding = 5) uniform texture2D depthTexture;
layout(set = 0, binding = 6) uniform texture2DArray shadowTextures;
layout(set = 0, binding = 7) uniform textureCube shadowMaps[SHADOW_MAPS_MAX];
layout(set = 0, binding = 8) uniform texture2DArray shadowCascades[SHADOW_CASCADES_MAX];

layout(set = 1, binding = 0) uniform sampler colorSampler;
layout(set = 1, binding = 1) uniform sampler shadowSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec3 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

vec3 computeFogAccumPoint(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lights[lightIndex].lightOrigins;
    float lightCutoff = lights[lightIndex].lightCutoffs;
    vec3 lightDirection = lights[lightIndex].lightDirections;
    float lightAttenuationLinear = lights[lightIndex].lightAttenuationLinears;
    float lightAttenuationQuadratic = lights[lightIndex].lightAttenuationQuadratics;
    float lightConeInner = lights[lightIndex].lightConeInners;
    float lightConeOuter = lights[lightIndex].lightConeOuters;

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
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = lights[lightIndex].lightShadowIndices;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lighting.lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
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
            vec3 positionShadow = currentPosition - lightOrigin;
            float shadowZ = length(positionShadow);
            float shadowDepth = texture(samplerCube(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), positionShadow).x;

            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lighting.lightCutoffMargin), lightCutoff, distance);
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
    return smoothstep(0.0, 1.0, result / validSteps) * lights[lightIndex].lightColors * lights[lightIndex].lightBrightnesses * lighting.ssvfIntensity;
}

vec3 computeFogAccumSpot(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lights[lightIndex].lightOrigins;
    float lightCutoff = lights[lightIndex].lightCutoffs;
    vec3 lightDirection = lights[lightIndex].lightDirections;
    float lightAttenuationLinear = lights[lightIndex].lightAttenuationLinears;
    float lightAttenuationQuadratic = lights[lightIndex].lightAttenuationQuadratics;
    float lightConeInner = lights[lightIndex].lightConeInners;
    float lightConeOuter = lights[lightIndex].lightConeOuters;

    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = lights[lightIndex].lightShadowIndices;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < lighting.ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lighting.lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
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
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

            // compute intensity inside light volume
            vec3 v = normalize(eye.center - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lighting.lightCutoffMargin), lightCutoff, distance);
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
    return smoothstep(0.0, 1.0, result / validSteps) * lights[lightIndex].lightColors * lights[lightIndex].lightBrightnesses * lighting.ssvfIntensity;
}

vec3 computeFogAccumDirectional(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lights[lightIndex].lightOrigins;
    vec3 lightDirection = lights[lightIndex].lightDirections;

    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    int shadowIndex = lights[lightIndex].lightShadowIndices;
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
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowTextures, shadowSampler), vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

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
    return smoothstep(0.0, 1.0, result / lighting.ssvfSteps) * lights[lightIndex].lightColors * lights[lightIndex].lightBrightnesses * lighting.ssvfIntensity;
}

vec3 computeFogAccumCascaded(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lights[lightIndex].lightOrigins;
    vec3 lightDirection = lights[lightIndex].lightDirections;

    // compute ray info
    vec3 startPosition = eye.center;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / lighting.ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    int shadowIndex = lights[lightIndex].lightShadowIndices;
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
                vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
                bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
                float shadowZ = shadowTexCoords.z;
                float shadowDepth = shadowTexCoordsInRange ? texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], shadowSampler), vec3(shadowTexCoords.xy, float(i))).x : 1.0;

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
    return smoothstep(0.0, 1.0, result / (lighting.ssvfSteps * SHADOW_CASCADE_LEVELS)) * lights[lightIndex].lightColors * lights[lightIndex].lightBrightnesses * lighting.ssvfIntensity;
}

void main()
{
    // initialize fog accumulation
    vec3 fogAccum = vec3(0.0);

    // ensure fragment was written and ssvf is enabled
    float depth = texture(sampler2D(depthTexture, colorSampler), texCoordsOut).r;
    if (depth != 0.0 && lighting.ssvfEnabled == 1)
    {
        // recover position from depth
        vec4 position = depthToPosition(depth, texCoordsOut);

        // accumulate fog
        for (int i = 0; i < lightsGeneral.lightsCount; ++i)
        {
            if (lights[i].lightDesireFogs == 1)
            {
                switch (lights[i].lightTypes)
                {
                    case 0: { fogAccum += computeFogAccumPoint(position, i); break; } // point
                    case 1: { fogAccum += computeFogAccumSpot(position, i); break; } // spot
                    case 2: { fogAccum += computeFogAccumDirectional(position, i); break; } // directional
                    default: { fogAccum += computeFogAccumCascaded(position, i); break; } // cascaded
                }
            }
        }
    }

    // write fog accumulation
    frag = fogAccum;
}
