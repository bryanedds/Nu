#version 450 core

const float PI = 3.141592654;
const float PI_OVER_2 = PI / 2.0;
const float SSRL_STEP_COUNT_MAX = 256; // TODO: P1: promote this to uniform?

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

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(set = 0, binding = 1) uniform LightingUniform { LightingStruct lighting; };
layout(set = 0, binding = 2) uniform texture2D depthTexture;
layout(set = 0, binding = 3) uniform texture2D albedoTexture;
layout(set = 0, binding = 4) uniform texture2D materialTexture;
layout(set = 0, binding = 5) uniform texture2D normalPlusTexture;
layout(set = 0, binding = 6) uniform texture2D clearCoatPlusTexture;
layout(set = 0, binding = 7) uniform texture2D lightAccumTexture;
layout(set = 0, binding = 8) uniform texture2D brdfTexture;
layout(set = 0, binding = 9) uniform texture2D ambientTexture;
layout(set = 0, binding = 10) uniform texture2D irradianceTexture;
layout(set = 0, binding = 11) uniform texture2D environmentFilterTexture;
layout(set = 0, binding = 12) uniform texture2D ssaoTexture;

layout(set = 1, binding = 0) uniform sampler unfilteredSampler;
layout(set = 1, binding = 1) uniform sampler filteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec3 color;
layout(location = 1) out float depth;

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
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, depth, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void computeSsrl(float depth, vec4 position, vec3 albedo, float roughness, float metallic, vec3 normal, float slope, inout vec3 specularScreen, inout float specularScreenWeight)
{
    // compute view values
    vec4 positionView = eye.view * position;
    vec3 positionViewNormal = normalize(positionView.xyz);
    vec3 normalView = mat3(eye.view) * normal;
    vec3 reflectionView = reflect(positionViewNormal, normalView);
    vec4 startView = vec4(positionView.xyz, 1.0);
    vec4 stopView = vec4(positionView.xyz + reflectionView * lighting.ssrlDistanceCutoff, 1.0);
    float eyeDistanceFromPlane = abs(dot(normalView, positionView.xyz));

    // compute the fragment at which to start marching
    vec2 texSize = textureSize(sampler2D(depthTexture, unfilteredSampler), 0).xy;
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
    float stepCount = min(abs(shouldMarchHorizontal ? marchHorizontal : marchVertical) * lighting.ssrlDetail, float(SSRL_STEP_COUNT_MAX));
    vec2 stepAmount = vec2(marchHorizontal, marchVertical) / max(stepCount, 0.001);

    // march fragment
    float currentProgressA = 0.0;
    float currentProgressB = 0.0;
    float currentDepthView = 0.0;
    for(int i = 0;
        i < stepCount &&
        currentTexCoords.x >= 0.0 && currentTexCoords.x < 1.0 &&
        currentTexCoords.y >= 0.0 && currentTexCoords.y < 1.0;
        ++i)
    {
        // advance frag values
        currentFrag += stepAmount;
        currentTexCoords = currentFrag / texSize;
        currentDepth = texture(sampler2D(depthTexture, unfilteredSampler), currentTexCoords).r;
        currentPosition = depthToPosition(currentDepth, currentTexCoords);
        currentPositionView = eye.view * currentPosition;
        currentProgressB = length(currentFrag - startFrag) / lengthFrag;
        currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrlDistanceCutoff increases.

        // compute depth delta and thickness based on view state
        float depthDelta = currentDepthView - -currentPositionView.z;
        float thickness = max(-currentPositionView.z * lighting.ssrlRayThickness, lighting.ssrlRayThickness);

        // determine whether we hit geometry within acceptable thickness
        if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
        {
            // perform refinements within walk
            currentProgressB = currentProgressA + (currentProgressB - currentProgressA) * 0.5;
            for (int j = 0; j < lighting.ssrlRefinementsMax; ++j)
            {
                // advance frag values
                currentFrag = mix(startFrag, stopFrag, currentProgressB);
                currentTexCoords = currentFrag / texSize;
                currentDepth = texture(sampler2D(depthTexture, unfilteredSampler), currentTexCoords).r;
                currentPosition = depthToPosition(currentDepth, currentTexCoords);
                currentPositionView = eye.view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrlDistanceCutoff increases.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(-currentPositionView.z * lighting.ssrlRayThickness, lighting.ssrlRayThickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // sample inputs
                    vec3 albedo = texture(sampler2D(albedoTexture, unfilteredSampler), currentTexCoords).rgb;
                    vec3 lightAccum = texture(sampler2D(lightAccumTexture, unfilteredSampler), currentTexCoords).rgb;
                    vec3 irradiance = texture(sampler2D(irradianceTexture, unfilteredSampler), currentTexCoords).rgb;
                    vec3 environmentFilter = texture(sampler2D(environmentFilterTexture, unfilteredSampler), currentTexCoords).rgb;

                    // compute diffuse
                    vec4 ambientColorAndBrightness = texture(sampler2D(ambientTexture, unfilteredSampler), currentTexCoords);
                    vec3 ambientColor = ambientColorAndBrightness.rgb;
                    float ambientBrightness = ambientColorAndBrightness.a;
                    float ambientBoostFactor = smoothstep(1.0 - lighting.lightAmbientBoostCutoff, 1.0, 1.0 - roughness);
                    float ambientBoost = 1.0 + ambientBoostFactor * lighting.lightAmbientBoostScalar;
                    vec3 ambientLight = ambientColor * ambientBrightness * ambientBoost;
                    vec3 diffuse = irradiance * albedo * ambientLight;

                    // compute color
                    specularScreen = (lightAccum + diffuse) * lighting.ssrlIntensity;

                    // compute weight
                    specularScreenWeight =
                        (1.0 - smoothstep(1.0 - lighting.ssrlRoughnessCutoffMargin, 1.0, roughness / lighting.ssrlRoughnessCutoff)) * // filter out as fragment reaches max roughness
                        (1.0 - smoothstep(1.0 - lighting.ssrlDepthCutoffMargin, 1.0, positionView.z / -lighting.ssrlDepthCutoff)) * // filter out as fragment reaches max depth
                        (1.0 - smoothstep(1.0 - lighting.ssrlDistanceCutoffMargin, 1.0, length(currentPositionView - positionView) / lighting.ssrlDistanceCutoff)) * // filter out as reflection point reaches max distance from fragment
                        (1.0 - smoothstep(1.0 - lighting.ssrlSlopeCutoffMargin, 1.0, slope / lighting.ssrlSlopeCutoff)) * // filter out as slope nears cutoff
                        smoothstep(0.0, 0.5, eyeDistanceFromPlane) * // filter out as eye nears plane. TODO: expose edge B as uniform!
                        smoothstep(0.0, lighting.ssrlEdgeHorizontalMargin, min(currentTexCoords.x, 1.0 - currentTexCoords.x)) *
                        smoothstep(0.0, lighting.ssrlEdgeVerticalMargin, min(currentTexCoords.y, 1.0 - currentTexCoords.y));
                    specularScreenWeight = clamp(specularScreenWeight, 0.0, 1.0);
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
    // ensure fragment was written
    float depthInput = texture(sampler2D(depthTexture, unfilteredSampler), texCoords).r;
    if (depthInput == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depthInput, texCoords);

    // retrieve remaining data from geometry buffers
    vec3 albedo = texture(sampler2D(albedoTexture, unfilteredSampler), texCoords).rgb;
    vec4 material = texture(sampler2D(materialTexture, unfilteredSampler), texCoords);
    vec3 normal = normalize(texture(sampler2D(normalPlusTexture, unfilteredSampler), texCoords).xyz);
    vec2 clearCoatPlus = texture(sampler2D(clearCoatPlusTexture, unfilteredSampler), texCoords).rg;
    float clearCoat = clearCoatPlus.r;
    float clearCoatRoughness = clearCoatPlus.g;
    vec3 lightAccum = texture(sampler2D(lightAccumTexture, unfilteredSampler), texCoords).rgb;

    // retrieve data from intermediate buffers
    vec4 ambientColorAndBrightness = texture(sampler2D(ambientTexture, unfilteredSampler), texCoords);
    vec3 irradiance = texture(sampler2D(irradianceTexture, unfilteredSampler), texCoords).rgb;
    vec3 environmentFilter = texture(sampler2D(environmentFilterTexture, unfilteredSampler), texCoords).rgb;
    float ssao = texture(sampler2D(ssaoTexture, unfilteredSampler), texCoords).r;

    // compute materials
    float roughness = material.r;
    float metallic = material.g;
    float ambientOcclusion = material.b * ssao;
    vec3 emission = vec3(material.a);

    // mix clear coat roughness with roughness value
    roughness = mix(roughness, clearCoatRoughness, clearCoat);

    // compute lighting terms
    vec3 v = normalize(eye.center - position.xyz);
    float nDotV = saturate(dot(normal, v));
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.

    // compute ambient light
    vec3 ambientColor = ambientColorAndBrightness.rgb;
    float ambientBrightness = ambientColorAndBrightness.a;
    float ambientBoostFactor = smoothstep(1.0 - lighting.lightAmbientBoostCutoff, 1.0, 1.0 - roughness);
    float ambientBoost = 1.0 + ambientBoostFactor * lighting.lightAmbientBoostScalar;
    vec3 ambientLight = ambientColor * ambientBrightness * ambientBoost * ambientOcclusion;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(nDotV, f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedo * ambientLight;

    // compute specular term and weight from screen-space
    vec3 forward = vec3(eye.view[0][2], eye.view[1][2], eye.view[2][2]);
    float towardEye = dot(forward, normal);
    float slope = 1.0 - abs(dot(normal, vec3(0.0, 1.0, 0.0)));
    vec4 positionView = eye.view * position;
    vec3 specularScreen = vec3(0.0);
    float specularScreenWeight = 0.0;
    if (lighting.ssrlEnabled == 1 && towardEye <= lighting.ssrlTowardEyeCutoff && -positionView.z <= lighting.ssrlDepthCutoff && roughness <= lighting.ssrlRoughnessCutoff && slope <= lighting.ssrlSlopeCutoff)
    {
        vec2 texSize = textureSize(sampler2D(depthTexture, unfilteredSampler), 0).xy;
        float texelHeight = 1.0 / texSize.y;
        vec2 texCoordsBelow = texCoords + vec2(0.0, texelHeight); // using tex coord below current pixel reduces 'cracks' on floor reflections
        texCoordsBelow.y = max(0.0, texCoordsBelow.y);
        float depthBelow = texture(sampler2D(depthTexture, unfilteredSampler), texCoordsBelow).r;
        vec4 positionBelow = depthToPosition(depthBelow, texCoordsBelow);
        computeSsrl(depthBelow, positionBelow, albedo, roughness, metallic, normal, slope, specularScreen, specularScreenWeight);

        // if hit failed, try again on the proper tex coord
        if (specularScreenWeight == 0.0)
            computeSsrl(depthInput, position, albedo, roughness, metallic, normal, slope, specularScreen, specularScreenWeight);
    }

    // compute specular term
    vec2 environmentBrdf = texture(sampler2D(brdfTexture, filteredSampler), vec2(nDotV, roughness)).rg;
    vec3 specularEnvironmentSubterm = f * environmentBrdf.x + environmentBrdf.y;
    vec3 specularEnvironment = environmentFilter * specularEnvironmentSubterm * ambientLight;
    vec3 specular = (1.0 - specularScreenWeight) * specularEnvironment + specularScreenWeight * specularScreen;

    // write color
    color = lightAccum + diffuse + emission * albedo + specular;

    // write depth
    depth = depthInput;
}
