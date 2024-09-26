#shader vertex
#version 410

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410

const float GAMMA = 2.2;

uniform sampler2D colorTexture;
uniform sampler2D fogAccumTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec4 color = texture(colorTexture, texCoordsOut, 0);
    if (color.w == 1.0) // ensure fragment written
    {
        vec3 fogAccum = texture(fogAccumTexture, texCoordsOut, 0).xyz;
        vec3 color = color.xyz + fogAccum;
        color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0 / GAMMA));
        frag = vec4(color, 1.0);
    }
}
