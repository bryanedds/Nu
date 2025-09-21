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

uniform float bloomStrength;
uniform sampler2D bloomFilterTexture;
uniform sampler2D compositionTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 bloomBlurColor = texture(bloomFilterTexture, texCoordsOut).rgb;
    vec3 sceneColor = texture(compositionTexture, texCoordsOut).rgb;
    frag = vec4(mix(sceneColor, bloomBlurColor, bloomStrength), 0.0);
}
