#version 450 core

const float PI = 3.141592654;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const int SHADOW_TEXTURES_MAX = 12;
const int SHADOW_MAPS_MAX = 12;
const int SHADOW_CASCADES_MAX = 1;
const int SHADOW_CASCADE_LEVELS = 3;
const float SHADOW_CASCADE_BLEND = 0.15; // fraction of a cascade's depth range over which it cross-fades into the next (must match the lighting shaders)
const float SSVF_DIRECTIONAL_SCALAR = 0.01;

const vec4 SSVF_DITHERING[4] =
    vec4[](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct Lighting3dStruct
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
    float shadowCascadeSplitLambda;
};

struct Lighting3d3Struct
{
    int lightMapsCount;
    float lightMapSingletonBlendMargin;
    int lightsCount;
};

struct Light3dStruct
{
    vec3 origin;
    vec3 direction;
    vec3 color;
    float brightness;
    float attenuationLinear;
    float attenuationQuadratic;
    float specularity;
    float cutoff;
    int lightType;
    float radius;
    float coneInner;
    float coneOuter;
    int desireFog;
    int shadowIndex;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };
layout(set = 0, binding = 1) uniform Lighting3dUniform { Lighting3dStruct lighting; };
layout(set = 0, binding = 2) uniform Lighting3d3Uniform { Lighting3d3Struct lighting3; };
layout(set = 0, binding = 3) uniform Light3dsUniform { Light3dStruct lights[LIGHTS_MAX]; };
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
    vec4 clip = vec4(texCoords * 2.0 - 1.0, 1.0, 1.0);
    vec4 rayView = eye.projectionInverse * clip;
    rayView /= rayView.w;
    vec3 positionView = rayView.xyz * (depth / -rayView.z);
    return eye.viewInverse * vec4(positionView, 1.0);
}

vec3 computeFogAccumPoint(vec4 position, Light3dStruct light)
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

vec3 computeFogAccumSpot(vec4 position, Light3dStruct light)
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

vec3 computeFogAccumDirectional(vec4 position, Light3dStruct light)
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
    float result = 0.0;
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
            result += fogMoment * stepLength;

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
                result += fogMoment * stepLength;
            }

            // step
            currentPosition += step;
        }
    }

    // scale intensity specially for directional lighting
    float intensity = lighting.ssvfIntensity * SSVF_DIRECTIONAL_SCALAR;

    // apply intensity
    result = result * intensity;

    // cap fog so it doesn't blow out everything
    result = min(intensity, result);

    // fin
    return result * light.color * light.brightness;
}

float computeShadowCascadeSplit(float shadowCutoff, int cascade)
{
    // practical split scheme - a blend of logarithmic and uniform splits
    // NOTE: must match Math.ShadowCascadeSplit.
    float t = float(cascade + 1) / float(SHADOW_CASCADE_LEVELS);
    float splitLog = lighting.shadowNear * pow(shadowCutoff / lighting.shadowNear, t);
    float splitUniform = lighting.shadowNear + (shadowCutoff - lighting.shadowNear) * t;
    return mix(splitUniform, splitLog, lighting.shadowCascadeSplitLambda);
}

float sampleFogVisibilityCascade(vec3 position, int shadowIndex, int cascade)
{
    // returns -1.0 when the position falls outside the cascade's map, otherwise whether light reaches it
    mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + cascade];
    vec4 positionShadowClip = shadowMatrix * vec4(position, 1.0);
    vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
    if (max(abs(shadowTexCoordsProj.x), abs(shadowTexCoordsProj.y)) >= 1.0 || shadowTexCoordsProj.z < 0.0) return -1.0;
    if (shadowTexCoordsProj.z >= 1.0) return 1.0;
    vec2 shadowTexCoords = shadowTexCoordsProj.xy * 0.5 + 0.5;
    float shadowDepth = texture(sampler2DArray(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], filteredSampler), vec3(shadowTexCoords, float(cascade))).x;
    return shadowTexCoordsProj.z <= shadowDepth ? 1.0 : 0.0;
}

float computeFogVisibilityCascaded(vec3 position, float shadowCutoff, int shadowIndex)
{
    // cascades are selected by view depth against the same splits the shadows use, and cross-faded over the same bands,
    // so the fog's light shafts don't step or double up where one cascade hands off to the next
    float depthView = -(eye.view * vec4(position, 1.0)).z;
    for (int i = 0; i < SHADOW_CASCADE_LEVELS; ++i)
    {
        float splitFar = computeShadowCascadeSplit(shadowCutoff, i);
        if (depthView < splitFar)
        {
            float visibility = sampleFogVisibilityCascade(position, shadowIndex, i);
            if (visibility >= 0.0) // otherwise outside this cascade's map, so try the next
            {
                float splitNear = i == 0 ? 0.0 : computeShadowCascadeSplit(shadowCutoff, i - 1);
                float blend = (splitFar - depthView) / ((splitFar - splitNear) * SHADOW_CASCADE_BLEND); // 0 at the split, 1 at the band's start
                if (blend < 1.0)
                {
                    // cross-fade into the next cascade over the end of this one's depth range, or toward unshadowed past the last
                    float visibilityNext = i < SHADOW_CASCADE_LEVELS - 1 ? sampleFogVisibilityCascade(position, shadowIndex, i + 1) : 1.0;
                    if (visibilityNext < 0.0) visibilityNext = visibility;
                    visibility = mix(visibilityNext, visibility, smoothstep(0.0, 1.0, blend));
                }
                return visibility;
            }
        }
    }
    return 1.0; // beyond the cascades, nothing is shadowed
}

vec3 computeFogAccumCascaded(vec4 position, Light3dStruct light)
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

    // mie scaterring approximated with Henyey-Greenstein phase function, constant along the ray
    float asymmetrySquared = lighting.ssvfAsymmetry * lighting.ssvfAsymmetry;
    float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * lighting.ssvfAsymmetry * theta, 1.5));

    // accumulate fog light once per step, as the directional variant does
    float result = 0.0;
    int shadowIndex = light.shadowIndex;
    vec3 currentPosition = startPosition + step * dithering;
    for (int i = 0; i < lighting.ssvfSteps; ++i)
    {
        float visibility = shadowIndex < 0 ? 1.0 : computeFogVisibilityCascaded(currentPosition, light.cutoff, shadowIndex);
        result += fogMoment * stepLength * visibility;
        currentPosition += step;
    }

    // scale intensity specially for directional lighting
    float intensity = lighting.ssvfIntensity * SSVF_DIRECTIONAL_SCALAR;

    // apply intensity
    result = result * intensity;

    // cap fog so it doesn't blow out everything
    result = min(intensity, result);

    // fin
    return result * light.color * light.brightness;
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
        for (int i = 0; i < lighting3.lightsCount; ++i)
        {
            Light3dStruct light = lights[i];
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
