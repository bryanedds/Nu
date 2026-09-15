#version 450 core

struct BloomExtractStruct
{
    float threshold;
};

layout(set = 0, binding = 0) uniform BloomExtractUniform { BloomExtractStruct bloomExtract; };
layout(set = 0, binding = 1) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 color = texture(sampler2D(inputTexture, inputSampler), texCoordsOut).rgb;
    float brightness = dot(color, vec3(0.2126, 0.7152, 0.0722));
    frag = vec4(brightness >= bloomExtract.threshold ? color : vec3(0.0), 1.0);
}
