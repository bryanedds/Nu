#shader vertex
#version 410

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410

const float GAMMA = 2.2;

uniform sampler2D diffuseLitTexture;
uniform sampler2D specularEnvironmentTexture;
uniform sampler2D specularScreenAndWeightTexture;
uniform sampler2D fogAccumTexture;

in vec2 texCoordsOut;

layout (location = 0) out vec4 frag;

void main()
{
    vec4 diffuseLit = texture(diffuseLitTexture, texCoordsOut, 0);
    if (diffuseLit.w == 1.0)
    {
        // sample composition terms
        vec3 specularEnvironment = texture(specularEnvironmentTexture, texCoordsOut, 0).xyz;
        vec4 specularScreenAndWeight = texture(specularScreenAndWeightTexture, texCoordsOut, 0);
        vec3 fogAccum = texture(fogAccumTexture, texCoordsOut, 0).xyz;

        // compute specular term
        vec3 specularScreen = specularScreenAndWeight.rgb;
        float specularScreenWeight = specularScreenAndWeight.a;
        vec3 specular = (1.0 - specularScreenWeight) * specularEnvironment + specularScreenWeight * specularScreen;

        // compute color composition with tone mapping and gamma correction
        vec3 color = diffuseLit.xyz + specular + fogAccum;
        color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0 / GAMMA));
        frag = vec4(color, 1.0);
    }
}
