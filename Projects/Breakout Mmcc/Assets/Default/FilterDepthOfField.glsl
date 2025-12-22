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
uniform vec2 focalPoint;
uniform sampler2D depthTexture;
uniform sampler2D blurredTexture;
uniform sampler2D unblurredTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

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
    float depth = texture(depthTexture, texCoordsOut).r;
    if (depth != 0.0)
    {
        vec4 blurredColor = texture(blurredTexture, texCoordsOut);
        float focalDepth = texture(depthTexture, focalPoint + vec2(0.5)).r;
        if (focalDepth != 0.0)
        {
            vec2 focalTexCoords = focalPoint + vec2(0.5);
            float distance = length(depthToPosition(depth, texCoordsOut).xyz - depthToPosition(focalDepth, focalTexCoords).xyz);
            float blur = smoothstep(nearDistance, farDistance, distance);
            frag = mix(unblurredColor, blurredColor, blur);
        }
        else frag = blurredColor;
    }
    else frag = unblurredColor;
}
