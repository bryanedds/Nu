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
layout(set = 0, binding = 2) uniform LightsGeneralUniform { LightsGeneralStruct lightsGeneral; };
layout(set = 0, binding = 3) uniform LightsUniform { LightStruct lights[LIGHTS_MAX]; };
layout(set = 0, binding = 4) uniform ShadowMatricesUniform { mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS]; };
layout(set = 0, binding = 5) uniform texture2D depthTexture;
layout(set = 0, binding = 6) uniform texture2DArray shadowTextures;
layout(set = 0, binding = 7) uniform textureCube shadowMaps[SHADOW_MAPS_MAX];
layout(set = 0, binding = 8) uniform texture2DArray shadowCascades[SHADOW_CASCADES_MAX];

layout(set = 1, binding = 0) uniform sampler unfilteredSampler;
layout(set = 1, binding = 1) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec3 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, depth, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
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

void main()
{
    // initialize fog accumulation
    vec3 fogAccum = vec3(0.0);

    // ensure fragment was written and ssvf is enabled
    float depth = texture(sampler2D(depthTexture, unfilteredSampler), texCoords).r;
    if (depth != 0.0 && lighting.ssvfEnabled == 1)
    {
        // recover position from depth
        vec4 position = depthToPosition(depth, texCoords);

        // accumulate fog
        for (int i = 0; i < lightsGeneral.lightsCount; ++i)
        {
            LightStruct light = lights[i];
            if (light.desireFog == 1)
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
    }

    // write fog accumulation
    frag = fogAccum;
}
