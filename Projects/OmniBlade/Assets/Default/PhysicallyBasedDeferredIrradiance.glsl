#shader vertex
#version 410 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const int LIGHT_MAPS_MAX = 27;

uniform sampler2D normalAndHeightTexture;
uniform sampler2D lightMappingTexture;
uniform samplerCube irradianceMap;
uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

void main()
{
    // retrieve normal and height values first, allowing for early-out
    vec3 normal = texture(normalAndHeightTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve light mapping data
    vec4 lmData = texture(lightMappingTexture, texCoordsOut);
    int lm1 = int(lmData.r);
    int lm2 = int(lmData.g);
    float lm1Distance = lmData.b;
    float lm2Distance = lmData.a;

    // compute irradiance terms
    vec3 irradiance = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        irradiance = texture(irradianceMap, normal).rgb;
    }
    else if (lm2 == -1)
    {
        irradiance = texture(irradianceMaps[lm1], normal).rgb;
    }
    else
    {
        // compute blended irradiance
        float distanceTotal = lm1Distance + lm2Distance;
        float distanceTotalInverse = 1.0 / distanceTotal;
        float scalar1 = (distanceTotal - lm1Distance) * distanceTotalInverse;
        float scalar2 = (distanceTotal - lm2Distance) * distanceTotalInverse;
        vec3 irradiance1 = texture(irradianceMaps[lm1], normal).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2], normal).rgb;
        irradiance = irradiance1 * scalar1 + irradiance2 * scalar2;
    }

    // write
    frag = vec4(irradiance, 1.0);
}
