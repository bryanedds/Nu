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
const float FLOAT_MAX = 3.402823466e+38;
const int LIGHT_MAPS_MAX = 32;

layout (bindless_sampler) uniform sampler2D positionTexture;
layout (bindless_sampler) uniform sampler2D normalPlusTexture;
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform int lightMapsCount;

in vec2 texCoordsOut;

layout (location = 0) out vec4 frag;

bool inBounds(vec3 point, vec3 min, vec3 size)
{
    return
        all(greaterThanEqual(point, min)) &&
        all(lessThanEqual(point, min + size));
}

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

void main()
{
    // retrieve normal value first, allowing for early-out
    vec4 normalPlus = texture(normalPlusTexture, texCoordsOut);
    vec3 normal = normalPlus.xyz;
    bool ignoreLightMaps = normalPlus.w == 1.0;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).xyz;

    // compute nearest light map indices
    int lm1 = -1;
    int lm2 = -1;
    float lm1DistanceSquared = FLOAT_MAX;
    float lm2DistanceSquared = FLOAT_MAX;
    if (!ignoreLightMaps)
    {
        for (int i = 0; i < lightMapsCount; ++i)
        {
            if (inBounds(position, lightMapMins[i], lightMapSizes[i]))
            {
                vec3 delta = lightMapOrigins[i] - position;
                float distanceSquared = dot(delta, delta);
                if (distanceSquared < lm1DistanceSquared)
                {
                    lm2 = lm1;
                    lm1 = i;
                    lm2DistanceSquared = lm1DistanceSquared;
                    lm1DistanceSquared = distanceSquared;
                }
                else if (distanceSquared < lm2DistanceSquared)
                {
                    lm2 = i;
                    lm2DistanceSquared = distanceSquared;
                }
            }
        }
    }

    // compute light map blending ratio
    float ratio =
        lm1 != -1 && lm2 != -1 ?
        computeDepthRatio(lightMapMins[lm1], lightMapSizes[lm1], lightMapMins[lm2], lightMapSizes[lm2], position, normal) :
        0.0f;

    // write with indices starting at 0.0 rather than -1.0 so that a black texture can be passed in for no light mapping
    frag = vec4(float(lm1 + 1), float(lm2 + 1), ratio, 0.0f);
}
