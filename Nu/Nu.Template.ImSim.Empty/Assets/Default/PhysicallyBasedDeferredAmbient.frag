#version 450 core

const int LIGHT_MAPS_MAX = 26;

struct Eye
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct LightMap
{
    vec3 lightMapOrigins;
    vec3 lightMapMins;
    vec3 lightMapSizes;
    vec3 lightMapAmbientColors;
    float lightMapAmbientBrightnesses;
};

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) buffer readonly LightMapBlock { LightMap lightMap; };
layout(set = 0, binding = 2) buffer readonly LightMapsBlock { LightMap lightMaps[LIGHT_MAPS_MAX]; };
layout(set = 0, binding = 3) uniform texture2D depthTexture;
layout(set = 0, binding = 4) uniform texture2D lightMappingTexture;

layout(set = 1, binding = 0) uniform sampler colorSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = eye.projectionInverse * positionClip;
    positionView /= positionView.w;
    return eye.viewInverse * positionView;
}

void main()
{
    // ensure fragment was written
    float depth = texture(sampler2D(depthTexture, colorSampler), texCoordsOut).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoordsOut);

    // retrieve light mapping data
    vec4 lmData = texture(sampler2D(lightMappingTexture, colorSampler), texCoordsOut);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute ambient values
    vec3 ambientColor = vec3(0.0);
    float ambientBrightness = 0.0;
    if (lm1 == -1 && lm2 == -1)
    {
        ambientColor = lightMap.lightMapAmbientColors;
        ambientBrightness = lightMap.lightMapAmbientBrightnesses;
    }
    else if (lm2 == -1)
    {
        // compute blended irradiance
        vec3 ambientColor1 = lightMaps[lm1].lightMapAmbientColors;
        vec3 ambientColor2 = lightMap.lightMapAmbientColors;
        float ambientBrightness1 = lightMaps[lm1].lightMapAmbientBrightnesses;
        float ambientBrightness2 = lightMap.lightMapAmbientBrightnesses;
        ambientColor = mix(ambientColor1, ambientColor2, lmRatio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, lmRatio);
    }
    else
    {
        // compute blended irradiance
        vec3 ambientColor1 = lightMaps[lm1].lightMapAmbientColors;
        vec3 ambientColor2 = lightMaps[lm2].lightMapAmbientColors;
        float ambientBrightness1 = lightMaps[lm1].lightMapAmbientBrightnesses;
        float ambientBrightness2 = lightMaps[lm2].lightMapAmbientBrightnesses;
        ambientColor = mix(ambientColor1, ambientColor2, lmRatio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, lmRatio);
    }

    // write
    frag = vec4(ambientColor, ambientBrightness);
}
