#version 450 core

const float PI = 3.141592654;
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

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };
layout(set = 0, binding = 1) uniform texture2D depthTexture;
layout(set = 0, binding = 2) uniform texture2D normalPlusTexture;
layout(set = 0, binding = 3) uniform texture2D lightMappingTexture;
layout(set = 0, binding = 4) uniform textureCube irradianceMap;
layout(set = 0, binding = 5) uniform textureCube irradianceMaps[LIGHT_MAPS_MAX];

layout(set = 1, binding = 0) uniform sampler colorSampler;
layout(set = 1, binding = 1) uniform sampler irradianceMapSampler;

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

    // retrieve remaining data from geometry buffers
    vec3 normal = normalize(texture(sampler2D(normalPlusTexture, colorSampler), texCoordsOut).xyz);

    // retrieve light mapping data
    vec4 lmData = texture(sampler2D(lightMappingTexture, colorSampler), texCoordsOut);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lmRatio = lmData.b;

    // compute irradiance
    vec3 irradiance = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        irradiance = texture(samplerCube(irradianceMap, irradianceMapSampler), normal).rgb;
    }
    else if (lm2 == -1)
    {
        // compute blended irradiance
        vec3 irradiance1 = texture(samplerCube(irradianceMaps[lm1], irradianceMapSampler), normal).rgb;
        vec3 irradiance2 = texture(samplerCube(irradianceMap, irradianceMapSampler), normal).rgb;
        irradiance = mix(irradiance1, irradiance2, lmRatio);
    }
    else
    {
        // compute blended irradiance
        vec3 irradiance1 = texture(samplerCube(irradianceMaps[lm1], irradianceMapSampler), normal).rgb;
        vec3 irradiance2 = texture(samplerCube(irradianceMaps[lm2], irradianceMapSampler), normal).rgb;
        irradiance = mix(irradiance1, irradiance2, lmRatio);
    }

    // write
    frag = vec4(irradiance, 1.0);
}
