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
const float FLOAT_MAX = 3.402823466e+38;
const int LIGHT_MAPS_MAX = 27;

uniform sampler2D positionTexture;
uniform sampler2D normalAndHeightTexture;
uniform int lightMapEnableds[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform int lightMapsCount;

in vec2 texCoordsOut;

out vec4 frag;

bool inBounds(vec3 point, vec3 min, vec3 size)
{
    return
        all(greaterThanEqual(point, min)) &&
        all(lessThanEqual(point, min + size));
}

void main()
{
    // retrieve normal value first, allowing for early-out
    vec3 normal = texture(normalAndHeightTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;

    // compute nearest light map indices
    int lm1 = -1;
    int lm2 = -1;
    float lm1DistanceSquared = FLOAT_MAX;
    float lm2DistanceSquared = FLOAT_MAX;
    for (int i = 0; i < lightMapsCount; ++i)
    {
        if (lightMapEnableds[i] != 0 && inBounds(position, lightMapMins[i], lightMapSizes[i]))
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

    // write with indices starting at 0.0 rather than -1.0 so that a black texture can be passed in for no light mapping
    frag = vec4(float(lm1 + 1), float(lm2 + 1), sqrt(lm1DistanceSquared), sqrt(lm2DistanceSquared));
}
