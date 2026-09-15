#version 450 core

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct DepthOfFieldStruct
{
    float nearDistance;
    float farDistance;
    int focalType;
    float focalDistance;
    vec2 focalPoint;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(set = 0, binding = 1) uniform DepthOfFieldUniform { DepthOfFieldStruct depthOfField; };
layout(set = 0, binding = 2) uniform texture2D depthTexture;
layout(set = 0, binding = 3) uniform texture2D blurredTexture;
layout(set = 0, binding = 4) uniform texture2D unblurredTexture;

layout(set = 1, binding = 0) uniform sampler unfilteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

float depthToDistance(float depth)
{
    float ndc = depth * 2.0 - 1.0;
    vec4 clip = vec4(0.0, 0.0, ndc, 1.0);
    vec4 view = eye.projectionInverse * clip;
    view /= view.w;
    return -view.z;
}

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
    // retrieve unblurred color
    vec4 unblurredColor = texture(sampler2D(unblurredTexture, unfilteredSampler), texCoords);

    // sample depth values that may be invalid when 0.0
    vec2 texelSize = vec2(1.0) / textureSize(sampler2D(depthTexture, unfilteredSampler), 0);
    float depths[] =
        float[](
            texture(sampler2D(depthTexture, unfilteredSampler), texCoords + texelSize * vec2(-1.0, -1.0)).r,
            texture(sampler2D(depthTexture, unfilteredSampler), texCoords + texelSize * vec2(1.0, -1.0)).r,
            texture(sampler2D(depthTexture, unfilteredSampler), texCoords + texelSize * vec2(-1.0, 1.0)).r,
            texture(sampler2D(depthTexture, unfilteredSampler), texCoords + texelSize * vec2(1.0, 1.0)).r);

    // compute average of valid depth values
    int depthCount = 0;
    float depth = 0.0;
    for (int i = 0; i < 4; ++i)
    {
        float depthCurrent = depths[i];
        if (depthCurrent != 0.0)
        {
            depth += depthCurrent;
            ++depthCount;
        }
    }
    depth /= float(depthCount);

    // compute frag
    if (depthCount > 0)
    {
        vec4 blurredColor = texture(sampler2D(blurredTexture, unfilteredSampler), texCoords);
        if (depthOfField.focalType == 0)
        {
            float distance = depthToDistance(depth);
            float blur =
                distance - depthOfField.focalDistance >= 0.0 ?
                smoothstep(depthOfField.focalDistance, depthOfField.farDistance, distance) :
                1.0 - smoothstep(depthOfField.nearDistance, depthOfField.focalDistance, distance);
            frag = mix(unblurredColor, blurredColor, blur);
        }
        else
        {
            float focalDistance = texture(sampler2D(depthTexture, unfilteredSampler), depthOfField.focalPoint + vec2(0.5)).r;
            if (focalDistance != 0.0)
            {
                vec2 focalTexCoords = depthOfField.focalPoint + vec2(0.5);
                float distance = length(depthToPosition(depth, texCoords).xyz - depthToPosition(focalDistance, focalTexCoords).xyz);
                float blur = smoothstep(depthOfField.nearDistance, depthOfField.farDistance, distance);
                frag = mix(unblurredColor, blurredColor, blur);
            }
            else frag = blurredColor;
        }
    }
    else frag = unblurredColor;
}
