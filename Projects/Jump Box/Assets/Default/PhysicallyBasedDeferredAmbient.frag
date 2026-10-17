#version 450 core

const int LIGHT_MAPS_MAX = 26;

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct LightMapStruct
{
    vec3 origin;
    vec3 min;
    vec3 size;
    vec3 ambientColor;
    float ambientBrightness;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };
layout(set = 0, binding = 1) uniform LightMapUniform { LightMapStruct lightMapFallback; };
layout(set = 0, binding = 2) uniform LightMapsUniform { LightMapStruct lightMaps[LIGHT_MAPS_MAX]; };
layout(set = 0, binding = 3) uniform texture2D depthTexture;
layout(set = 0, binding = 4) uniform texture2D lightMappingTexture;

layout(set = 1, binding = 0) uniform sampler unfilteredSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    vec4 clip = vec4(texCoords * 2.0 - 1.0, 1.0, 1.0);
    vec4 rayView = eye.projectionInverse * clip;
    rayView /= rayView.w;
    vec3 positionView = rayView.xyz * (depth / -rayView.z);
    return eye.viewInverse * vec4(positionView, 1.0);
}

void main()
{
    // ensure fragment was written
    float depth = texture(sampler2D(depthTexture, unfilteredSampler), texCoords).r;
    if (depth == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depth, texCoords);

    // retrieve light mapping data
    vec4 lmData = texture(sampler2D(lightMappingTexture, unfilteredSampler), texCoords);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute ambient values
    vec3 ambientColor = vec3(0.0);
    float ambientBrightness = 0.0;
    if (lm1 == -1 && lm2 == -1)
    {
        ambientColor = lightMapFallback.ambientColor;
        ambientBrightness = lightMapFallback.ambientBrightness;
    }
    else if (lm2 == -1)
    {
        // compute blended irradiance
        vec3 ambientColor1 = lightMaps[lm1].ambientColor;
        vec3 ambientColor2 = lightMapFallback.ambientColor;
        float ambientBrightness1 = lightMaps[lm1].ambientBrightness;
        float ambientBrightness2 = lightMapFallback.ambientBrightness;
        ambientColor = mix(ambientColor1, ambientColor2, lmRatio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, lmRatio);
    }
    else
    {
        // compute blended irradiance
        vec3 ambientColor1 = lightMaps[lm1].ambientColor;
        vec3 ambientColor2 = lightMaps[lm2].ambientColor;
        float ambientBrightness1 = lightMaps[lm1].ambientBrightness;
        float ambientBrightness2 = lightMaps[lm2].ambientBrightness;
        ambientColor = mix(ambientColor1, ambientColor2, lmRatio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, lmRatio);
    }

    // write
    frag = vec4(ambientColor, ambientBrightness);
}
