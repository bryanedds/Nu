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

    // write with indices starting at 0.0 rather than -1.0 so that a black texture can be passed in for no light mapping
    frag = vec4(float(lm1 + 1), float(lm2 + 1), sqrt(lm1DistanceSquared), sqrt(lm2DistanceSquared));
}
