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

uniform vec3 eyeCenter;
uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform int fogEnabled;
uniform float fogStart;
uniform float fogFinish;
uniform vec4 fogColor;
uniform sampler2D depthTexture;
uniform sampler2D colorTexture;
uniform sampler2D fogAccumTexture;

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
    // ensure fragment written
    float depth = texture(depthTexture, texCoordsOut, 0).r;
    if (depth == 0.0) discard;

    // apply volumetric fog
    vec3 fogAccum = texture(fogAccumTexture, texCoordsOut, 0).xyz;
    vec3 color = texture(colorTexture, texCoordsOut, 0).xyz + fogAccum;

    // compute and apply global fog when enabled
    if (fogEnabled == 1)
    {
        vec4 position = depthToPosition(depth, texCoordsOut);
        float distance = length(position.xyz - eyeCenter);
        float fogFactor = smoothstep(fogStart / fogFinish, 1.0, min(1.0, distance / fogFinish)) * fogColor.a;
        color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
    }

    // write fragment
    frag = vec4(color, 1.0);
}
