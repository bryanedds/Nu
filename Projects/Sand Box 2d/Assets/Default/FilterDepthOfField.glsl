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

uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform float nearDistance;
uniform float farDistance;
uniform int focalType;
uniform float focalDistance;
uniform vec2 focalPoint;
uniform sampler2D depthTexture;
uniform sampler2D blurredTexture;
uniform sampler2D unblurredTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

float depthToDistance(float depth)
{
    float ndc = depth * 2.0 - 1.0;
    vec4 clip = vec4(0.0, 0.0, ndc, 1.0);
    vec4 view = projectionInverse * clip;
    view /= view.w;
    return -view.z;
}

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = projectionInverse * positionClip;
    positionView /= positionView.w;
    return viewInverse * positionView;
}

void main()
{
    vec4 unblurredColor = texture(unblurredTexture, texCoordsOut);
    vec2 texelSize = vec2(1.0) / textureSize(depthTexture, 0);
    float depth = // TODO: P1: attempt to increase efficiency here by downsampling the depth texture instead of multi-tapping.
        (texture(depthTexture, texCoordsOut + texelSize).r +
         texture(depthTexture, texCoordsOut + texelSize * -1.0).r +
         texture(depthTexture, texCoordsOut + texelSize * vec2(1.0, -1.0)).r +
         texture(depthTexture, texCoordsOut + texelSize * vec2(-1.0, -1.0)).r) *
        0.25;
    if (depth != 0.0)
    {
        vec4 blurredColor = texture(blurredTexture, texCoordsOut);
        if (focalType == 0)
        {
            float distance = depthToDistance(depth);
            float blur =
                distance - focalDistance >= 0.0 ?
                smoothstep(focalDistance, farDistance, distance) :
                1.0 - smoothstep(nearDistance, focalDistance, distance);
            frag = mix(unblurredColor, blurredColor, blur);
        }
        else
        {
            float focalDistance = texture(depthTexture, focalPoint + vec2(0.5)).r;
            if (focalDistance != 0.0)
            {
                vec2 focalTexCoords = focalPoint + vec2(0.5);
                float distance = length(depthToPosition(depth, texCoordsOut).xyz - depthToPosition(focalDistance, focalTexCoords).xyz);
                float blur = smoothstep(nearDistance, farDistance, distance);
                frag = mix(unblurredColor, blurredColor, blur);
            }
            else frag = blurredColor;
        }
    }
    else frag = unblurredColor;
}
