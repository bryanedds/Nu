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

const float PI = 3.141592654;
const int LIGHT_MAPS_MAX = 27;

uniform vec3 eyeCenter;
uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform sampler2D depthTexture;
uniform sampler2D normalPlusTexture;
uniform sampler2D lightMappingTexture;
uniform samplerCube irradianceMap;
uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];

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
    // ensure fragment was written
    float depth = texture(depthTexture, texCoordsOut).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoordsOut);

    // retrieve remaining data from geometry buffers
    vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;

    // retrieve light mapping data
    vec4 lmData = texture(lightMappingTexture, texCoordsOut);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute irradiance
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
        vec3 irradiance1 = texture(irradianceMaps[lm1], normal).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2], normal).rgb;
        irradiance = mix(irradiance1, irradiance2, lmRatio);
    }

    // write
    frag = vec4(irradiance, 1.0);
}
