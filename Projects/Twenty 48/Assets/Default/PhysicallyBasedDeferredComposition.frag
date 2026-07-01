#version 450 core

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

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) buffer readonly LightingBlock { Lighting lighting; };
layout(set = 0, binding = 2) uniform texture2D depthTexture;
layout(set = 0, binding = 3) uniform texture2D colorTexture;
layout(set = 0, binding = 4) uniform texture2D fogAccumTexture;

layout(set = 1, binding = 0) uniform sampler colorSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

void main()
{
    // ensure fragment written
    float depth = texture(sampler2D(depthTexture, colorSampler), texCoordsOut, 0).r;
    if (depth == 0.0) discard;

    // apply volumetric fog
    vec3 fogAccum = texture(sampler2D(fogAccumTexture, colorSampler), texCoordsOut, 0).xyz;
    vec3 color = texture(sampler2D(colorTexture, colorSampler), texCoordsOut, 0).xyz + fogAccum;

    // compute and apply distance fog when enabled
    vec4 position = depthToPosition(depth, texCoordsOut);
    float distance = length(position.xyz - eye.center);
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
    frag = vec4(color, 1.0);
}
