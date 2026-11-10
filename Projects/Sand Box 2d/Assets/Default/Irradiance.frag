#version 450 core

const float PI = 3.14159265359;

struct IrradianceStruct
{
    int flipped;
    int sampleResolution;
    float sampleLod;
};

layout(set = 0, binding = 1) uniform IrradianceUniform { IrradianceStruct irradiance; };

layout(set = 1, binding = 0) uniform textureCube inputCubeMap;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) out vec4 frag;

// The direction through a point of a cube face, its coordinates each in [-1, 1].
vec3 getFaceDirection(int face, vec2 uv)
{
    switch (face)
    {
        case 0: return vec3(1.0, -uv.y, -uv.x); // +x
        case 1: return vec3(-1.0, -uv.y, uv.x); // -x
        case 2: return vec3(uv.x, 1.0, uv.y); // +y
        case 3: return vec3(uv.x, -1.0, -uv.y); // -y
        case 4: return vec3(uv.x, -uv.y, 1.0); // +z
        default: return vec3(-uv.x, -uv.y, -1.0); // -z
    }
}

// The direction a flipped cube map holds what's in a direction - each face's image mirrored top to bottom, as when it
// was rendered with a vulkan-flipped projection.
vec3 getFlippedDirection(vec3 direction)
{
    vec3 magnitude = abs(direction);
    if (magnitude.y >= magnitude.x && magnitude.y >= magnitude.z) return vec3(direction.x, direction.y, -direction.z);
    return vec3(direction.x, -direction.y, direction.z);
}

// The real spherical harmonic basis function of an index (bands 0 through 2) in a direction.
float getBasis(int index, vec3 d)
{
    switch (index)
    {
        case 0: return 0.282095;
        case 1: return 0.488603 * d.y;
        case 2: return 0.488603 * d.z;
        case 3: return 0.488603 * d.x;
        case 4: return 1.092548 * d.x * d.y;
        case 5: return 1.092548 * d.y * d.z;
        case 6: return 0.315392 * (3.0 * d.z * d.z - 1.0);
        case 7: return 1.092548 * d.x * d.z;
        default: return 0.546274 * (d.x * d.x - d.y * d.y);
    }
}

void main()
{
    // the coefficient this fragment computes, and its band's share of the cosine lobe's convolution, divided by pi so
    // that the coefficients evaluate to irradiance / pi, as the deferred and forward shaders use it
    int index = int(gl_FragCoord.x);
    float convolution = index == 0 ? 1.0 : index < 4 ? 2.0 / 3.0 : 0.25;

    // project the input's radiance onto the coefficient's basis function, weighting each sample by the solid angle of
    // the cube face texel it stands for
    vec3 coefficient = vec3(0.0);
    float weightTotal = 0.0;
    for (int face = 0; face < 6; ++face)
    {
        for (int j = 0; j < irradiance.sampleResolution; ++j)
        {
            for (int i = 0; i < irradiance.sampleResolution; ++i)
            {
                vec2 uv = (vec2(i, j) + 0.5) / float(irradiance.sampleResolution) * 2.0 - 1.0;
                vec3 direction = getFaceDirection(face, uv);
                float weight = 1.0 / pow(dot(direction, direction), 1.5); // solid angle of the texel, up to a constant
                direction = normalize(direction);
                vec3 lookup = irradiance.flipped == 1 ? getFlippedDirection(direction) : direction;
                vec3 radiance = textureLod(samplerCube(inputCubeMap, inputSampler), lookup, irradiance.sampleLod).rgb;
                if (!any(isnan(radiance))) // TODO: understand why NaN can come from this sample and try to apply a more appropriate fix.
                {
                    coefficient += radiance * getBasis(index, direction) * weight;
                    weightTotal += weight;
                }
            }
        }
    }

    // normalize the weights to the sphere's solid angle and apply the convolution
    coefficient *= 4.0 * PI / weightTotal * convolution;
    frag = vec4(coefficient, 1.0);
}
