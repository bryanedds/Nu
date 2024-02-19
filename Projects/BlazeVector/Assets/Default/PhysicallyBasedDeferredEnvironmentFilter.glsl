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
#extension GL_ARB_bindless_texture : require

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const int LIGHT_MAPS_MAX = 32;

uniform vec3 eyeCenter;
layout (bindless_sampler) uniform sampler2D positionTexture;
layout (bindless_sampler) uniform sampler2D materialTexture;
layout (bindless_sampler) uniform sampler2D normalPlusTexture;
layout (bindless_sampler) uniform sampler2D lightMappingTexture;
layout (bindless_sampler) uniform samplerCube environmentFilterMap;
layout (bindless_sampler) uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

vec2 rayBoxIntersectionRatios(vec3 rayOrigin, vec3 rayDirection, vec3 boxMin, vec3 boxSize)
{
    vec3 rayDirectionInv = vec3(1.0) / rayDirection;
    vec3 boxMax = boxMin + boxSize;
    vec3 t1 = (boxMin - rayOrigin) * rayDirectionInv;
    vec3 t2 = (boxMax - rayOrigin) * rayDirectionInv;
    vec3 tMin = min(t1, t2);
    vec3 tMax = max(t1, t2);
    float tEnter = max(max(tMin.x / boxSize.x, tMin.y / boxSize.y), tMin.z / boxSize.z);
    float tExit = min(min(tMax.x / boxSize.x, tMax.y / boxSize.y), tMax.z / boxSize.z);
    return tEnter < tExit ? vec2(tEnter, tExit) : vec2(0.0);
}

float computeDepthRatio(vec3 minA, vec3 sizeA, vec3 minB, vec3 sizeB, vec3 position, vec3 normal)
{
    vec3 centerA = minA + sizeA * 0.5;
    vec3 centerB = minB + sizeB * 0.5;
    vec3 direction = normalize(cross(cross(centerB - centerA, normal), normal));
    vec3 intersectionMin = max(minA, minB);
    vec3 intersectionSize = min(minA + sizeA, minB + sizeB) - intersectionMin;
    vec2 intersectionRatios = rayBoxIntersectionRatios(position, direction, intersectionMin, intersectionSize);
    return intersectionRatios != vec2(0.0) ? intersectionRatios.y / (intersectionRatios.y - intersectionRatios.x) : 0.5;
}

vec3 parallaxCorrection(samplerCube cubeMap, vec3 lightMapOrigin, vec3 lightMapMin, vec3 lightMapSize, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eyeCenter;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMapMin + lightMapSize - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMapMin - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMapOrigin;
}

void main()
{
    // retrieve normal and height values first, allowing for early-out
    vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).xyz;
    float roughness = texture(materialTexture, texCoordsOut).r;

    // retrieve light mapping data
    vec4 lmData = texture(lightMappingTexture, texCoordsOut);
    int lm1 = int(lmData.r) - 1;
    int lm2 = int(lmData.g) - 1;
    float lm1Distance = lmData.b;
    float lm2Distance = lmData.a;

    // compute environment filter term
    vec3 v = normalize(eyeCenter - position);
    vec3 environmentFilter = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        vec3 r = reflect(-v, normal);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * REFLECTION_LOD_MAX).rgb;
    }
    else if (lm2 == -1)
    {
        vec3 r = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        environmentFilter = textureLod(environmentFilterMaps[lm1], r, roughness * REFLECTION_LOD_MAX).rgb;
    }
    else
    {
        // compute blended environment filter
        float ratio = computeDepthRatio(lightMapMins[lm1], lightMapSizes[lm1], lightMapMins[lm2], lightMapSizes[lm2], position, normal);
        vec3 r1 = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        vec3 r2 = parallaxCorrection(environmentFilterMaps[lm2], lightMapOrigins[lm2], lightMapMins[lm2], lightMapSizes[lm2], position, normal);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1], r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2], r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);
    }

    // write
    frag = vec4(environmentFilter, 1.0);
}
