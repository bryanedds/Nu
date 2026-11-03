#version 450 core

const float BLOOM_KNEE = ; // half the width of the soft knee about the threshold, relative to the threshold

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
    // sample color
    vec3 color = texture(sampler2D(inputTexture, inputSampler), texCoordsOut).rgb;

    // compute brightness as perceived by the human eye, which is more sensitive to green than red or blue
    float brightness = dot(color, vec3(0.2126, 0.7152, 0.0722));

    // extract through a soft knee rather than a hard cut, so that where a smooth gradient crosses the threshold (as on
    // broad surfaces lit by falloff or IBL) its bloom eases in instead of starting at a contour that the blur only softens
    float knee = max(bloomExtract.threshold * 0.5, 0.0001);
    float soft = clamp(brightness - bloomExtract.threshold + knee, 0.0, 2.0 * knee);
    soft = soft * soft / (4.0 * knee);
    float contribution = max(soft, brightness - bloomExtract.threshold) / max(brightness, 0.0001);
    frag = vec4(color * contribution, 1.0);
}
