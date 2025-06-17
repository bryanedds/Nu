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

const int LIGHT_MAPS_MAX = 27;

uniform sampler2D positionTexture;
uniform sampler2D lightMappingTexture;
uniform vec3 lightMapAmbientColor;
uniform float lightMapAmbientBrightness;
uniform vec3 lightMapAmbientColors[LIGHT_MAPS_MAX];
uniform float lightMapAmbientBrightnesses[LIGHT_MAPS_MAX];

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    // ensure position was written
    vec4 position = texture(positionTexture, texCoordsOut);
    if (position.w == 1.0)
    {
        // retrieve light mapping data
        vec4 lmData = texture(lightMappingTexture, texCoordsOut);
        int lm1 = int(lmData.r) - 1;
        int lm2 = int(lmData.g) - 1;
        float lmRatio = lmData.b;

        // compute ambient values
        vec3 ambientColor = vec3(0.0);
        float ambientBrightness = 0.0;
        if (lm1 == -1 && lm2 == -1)
        {
            ambientColor = lightMapAmbientColor;
            ambientBrightness = lightMapAmbientBrightness;
        }
        else if (lm2 == -1)
        {
            ambientColor = lightMapAmbientColors[lm1];
            ambientBrightness = lightMapAmbientBrightnesses[lm1];
        }
        else
        {
            // compute blended irradiance
            vec3 ambientColor1 = lightMapAmbientColors[lm1];
            vec3 ambientColor2 = lightMapAmbientColors[lm2];
            float ambientBrightness1 = lightMapAmbientBrightnesses[lm1];
            float ambientBrightness2 = lightMapAmbientBrightnesses[lm2];
            ambientColor = mix(ambientColor1, ambientColor2, lmRatio);
            ambientBrightness = mix(ambientBrightness1, ambientBrightness2, lmRatio);
        }

        // write
        frag = vec4(ambientColor, ambientBrightness);
    }
    else frag = vec4(1.0); // white ambient values
}
