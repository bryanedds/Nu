#version 450 core

// This shader performs upsampling on a texture, as taken from Call Of Duty method, presented at ACM Siggraph 2014.

struct BloomUpSampleStruct
{
    float radius;
};

layout(set = 0, binding = 0) uniform BloomUpSampleUniform { BloomUpSampleStruct bloomUpSample; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec3 frag;

void main()
{
    // The filter kernel is applied with a radius, specified in texture
    // coordinates, so that the radius will vary across mip resolutions.
    float x = bloomUpSample.radius;
    float y = bloomUpSample.radius;

    // Take 9 samples around current texel:
    // a - b - c
    // d - e - f
    // g - h - i
    // === ('e' is the current texel) ===
    vec3 a = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - x, texCoordsOut.y + y)).rgb;
    vec3 b = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,     texCoordsOut.y + y)).rgb;
    vec3 c = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + x, texCoordsOut.y + y)).rgb;

    vec3 d = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - x, texCoordsOut.y)).rgb;
    vec3 e = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,     texCoordsOut.y)).rgb;
    vec3 f = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + x, texCoordsOut.y)).rgb;

    vec3 g = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x - x, texCoordsOut.y - y)).rgb;
    vec3 h = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x,     texCoordsOut.y - y)).rgb;
    vec3 i = texture(sampler2D(inputTexture, inputSampler), vec2(texCoordsOut.x + x, texCoordsOut.y - y)).rgb;

    // Apply weighted distribution, by using a 3x3 tent filter:
    //  1   | 1 2 1 |
    // -- * | 2 4 2 |
    // 16   | 1 2 1 |
    frag = e*4.0;
    frag += (b+d+f+h)*2.0;
    frag += (a+c+g+i);
    frag *= 1.0 / 16.0;
}
