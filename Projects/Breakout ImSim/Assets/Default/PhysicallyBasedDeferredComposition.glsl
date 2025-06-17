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

const float GAMMA = 2.2;

uniform vec3 eyeCenter;
uniform int fogEnabled;
uniform float fogStart;
uniform float fogFinish;
uniform vec4 fogColor;
uniform sampler2D positionTexture;
uniform sampler2D colorTexture;
uniform sampler2D fogAccumTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec4 color = texture(colorTexture, texCoordsOut, 0);
    if (color.w == 1.0) // ensure fragment written
    {
        // apply volumetric fog
        vec3 fogAccum = texture(fogAccumTexture, texCoordsOut, 0).xyz;
        vec3 color = color.xyz + fogAccum;

        // compute and apply global fog when enabled
        if (fogEnabled == 1)
        {
            vec4 position = texture(positionTexture, texCoordsOut, 0);
            float depth = length(position.xyz - eyeCenter);
            float fogFactor = smoothstep(fogStart / fogFinish, 1.0, min(1.0, depth / fogFinish)) * fogColor.a;
            color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
        }

        // apply tone mapping and gamma correction
        color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0 / GAMMA));

        // write fragment
        frag = vec4(color, 1.0);
    }
}
