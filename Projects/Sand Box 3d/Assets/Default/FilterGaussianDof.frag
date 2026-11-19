#version 450 core

const float GAMMA = 2.2;

struct GaussianDofStruct
{
    vec2 scale;
    float radius;
};

layout(set = 0, binding = 0) uniform GaussianDofUniform { GaussianDofStruct gaussianDof; };

layout(set = 1, binding = 0) uniform texture2D inputTexture;

layout(set = 2, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

const float KERNEL[5] = float[](1.0 / 16.0, 4.0 / 16.0, 6.0 / 16.0, 4.0 / 16.0, 1.0 / 16.0);

vec3 powVec3(vec3 v, float p)
{
    return vec3(pow(v.x, p), pow(v.y, p), pow(v.z, p));
}

vec3 toSrgb(vec3 v)
{
    return powVec3(v, 1.0 / GAMMA);
}

float sRGBToLuma(vec3 col)
{
    return dot(col, vec3(0.299f, 0.587f, 0.114f));
}

float karisAverage(vec3 col)
{
    float luma = sRGBToLuma(toSrgb(col)) * 0.25f;
    return 1.0f / (1.0f + luma);
}

void main()
{
    // karis weights each sample down by its brightness (karis average) so that a few very bright texels, such as an
    // hdr specular highlight, don't dominate the blur and smear across their surroundings
    vec2 scalar = gaussianDof.scale * gaussianDof.radius;
    vec4 colorSum = vec4(0.0);
    float weightSum = 0.0;
    for (int i = 0; i < 5; ++i)
    {
        vec4 color = texture(sampler2D(inputTexture, inputSampler), texCoords + vec2(float(i - 2)) * scalar);
        float weight = KERNEL[i] * karisAverage(color.rgb);
        colorSum += color * weight;
        weightSum += weight;
    }
    frag = colorSum / weightSum;
}
