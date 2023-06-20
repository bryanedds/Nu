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
const float REFLECTION_LOD_MAX = 5.0;
const int LIGHT_MAPS_MAX = 24;

uniform vec3 eyeCenter;
uniform sampler2D positionTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalAndHeightTexture;
uniform sampler2D lightMappingTexture;
uniform samplerCube environmentFilterMap;
uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

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
    vec3 normal = texture(normalAndHeightTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;
    float roughness = texture(materialTexture, texCoordsOut).g;

    // retrieve light mapping data
    vec4 lmData = texture(lightMappingTexture, texCoordsOut);
    int lm1 = int(lmData.r);
    int lm2 = int(lmData.g);
    float lm1Distance = lmData.b;
    float lm2Distance = lmData.a;

    // compute environment filter term
    vec3 v = normalize(eyeCenter - position);
    vec3 environmentFilter = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        vec3 r = reflect(-v, normal);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else if (lm2 == -1)
    {
        vec3 r = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        environmentFilter = textureLod(environmentFilterMaps[lm1], r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else
    {
        // compute blended environment filter
        float distanceTotal = lm1Distance + lm2Distance;
        float distanceTotalInverse = 1.0 / distanceTotal;
        float scalar1 = (distanceTotal - lm1Distance) * distanceTotalInverse;
        float scalar2 = (distanceTotal - lm2Distance) * distanceTotalInverse;
        vec3 r1 = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        vec3 r2 = parallaxCorrection(environmentFilterMaps[lm2], lightMapOrigins[lm2], lightMapMins[lm2], lightMapSizes[lm2], position, normal);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1], r1, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2], r2, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        environmentFilter = environmentFilter1 * scalar1 + environmentFilter2 * scalar2;
    }

    // write
    frag = vec4(environmentFilter, 1.0);
}
