#version 450 core

const float GAMMA = 2.2;

struct BloomDownSampleStruct
{
    int karisAverageEnabled;
    int sampleLevel;
    vec2 sourceResolution;
};

layout(set = 0, binding = 0) uniform BloomDownSampleUniform { BloomDownSampleStruct bloomDownSample; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec3 frag;

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

// NOTE: This is the readable version of this shader. It will be optimized!
void main()
{
    vec2 srcTexelSize = 1.0 / bloomDownSample.sourceResolution;
    float x = srcTexelSize.x;
    float y = srcTexelSize.y;

    // Take 13 samples around current texel (e):
    // a - b - c
    // - j - k -
    // d - e - f
    // - l - m -
    // g - h - i
    vec3 a = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - 2*x, texCoordsOut.y + 2*y)).rgb;
    vec3 b = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,       texCoordsOut.y + 2*y)).rgb;
    vec3 c = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + 2*x, texCoordsOut.y + 2*y)).rgb;

    vec3 d = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - 2*x, texCoordsOut.y)).rgb;
    vec3 e = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,       texCoordsOut.y)).rgb;
    vec3 f = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + 2*x, texCoordsOut.y)).rgb;

    vec3 g = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - 2*x, texCoordsOut.y - 2*y)).rgb;
    vec3 h = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,       texCoordsOut.y - 2*y)).rgb;
    vec3 i = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + 2*x, texCoordsOut.y - 2*y)).rgb;

    vec3 j = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - x,   texCoordsOut.y + y)).rgb;
    vec3 k = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + x,   texCoordsOut.y + y)).rgb;
    vec3 l = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - x,   texCoordsOut.y - y)).rgb;
    vec3 m = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + x,   texCoordsOut.y - y)).rgb;

    // Apply weighted distribution:
    // 0.5 + 0.125 + 0.125 + 0.125 + 0.125 = 1
    // a,b,d,e * 0.125
    // b,c,e,f * 0.125
    // d,e,g,h * 0.125
    // e,f,h,i * 0.125
    // j,k,l,m * 0.5
    // This shows 5 square areas that are being sampled. But some of them overlap,
    // so to have an energy preserving downsample we need to make some adjustments.
    // The weights are the distributed, so that the sum of j,k,l,m (e.g.)
    // contribute 0.5 to the final color output. The code below is written
    // to effectively yield this sum. We get:
    // 0.125*5 + 0.03125*4 + 0.0625*4 = 1

    // Check if we need to perform Karis average on each block of 4 samples.
    // For additional efficacy, karis average is applied at all sample levels.
    vec3 groups[5];
    if (bloomDownSample.karisAverageEnabled == 1 && bloomDownSample.sampleLevel == 0)
    {
        // We are writing to level 0, so we need to apply Karis average to each block
        // of 4 samples to prevent fireflies (very bright subpixels, leads to pulsating
        // artifacts).
        groups[0] = (a+b+d+e) * (0.125f/4.0f);
        groups[1] = (b+c+e+f) * (0.125f/4.0f);
        groups[2] = (d+e+g+h) * (0.125f/4.0f);
        groups[3] = (e+f+h+i) * (0.125f/4.0f);
        groups[4] = (j+k+l+m) * (0.5f/4.0f);
        groups[0] *= karisAverage(groups[0]);
        groups[1] *= karisAverage(groups[1]);
        groups[2] *= karisAverage(groups[2]);
        groups[3] *= karisAverage(groups[3]);
        groups[4] *= karisAverage(groups[4]);
        frag = groups[0]+groups[1]+groups[2]+groups[3]+groups[4];
        frag = max(frag, 0.0001f);
    }
    else
    {
        // No Karis average, just a weighted downsample.
        frag = e*0.125;
        frag += (a+c+g+i)*0.03125;
        frag += (b+d+f+h)*0.0625;
        frag += (j+k+l+m)*0.125;
    }
}
