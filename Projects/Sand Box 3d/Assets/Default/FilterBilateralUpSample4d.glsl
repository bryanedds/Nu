#shader vertex
#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 460 core

const float SHARPNESS = 0.05;

uniform sampler2D colorDownSampledTexture;
uniform sampler2D depthDownSampledTexture;
uniform sampler2D depthTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    // compute up-sampled texture size
    vec2 texelSize = vec2(1.0) / textureSize(depthTexture, 0).xy;
    vec2 offsets[4] =
        vec2[4](
            vec2(-texelSize.x, texelSize.y),
            texelSize,
            vec2(texelSize.x, -texelSize.y),
            -texelSize);

    // compute up-sampled color sum and weight
    float depth = texture(depthTexture, texCoordsOut, 0).x;
    vec4 colorSum = vec4(0.0);
    float weight = 0.0;
    for (int i = 0; i < 4; ++i)
    {
        vec4 colorDownSampled = texture(colorDownSampledTexture, texCoordsOut + offsets[i], 0);
        float depthDownSampled = texture(depthDownSampledTexture, texCoordsOut + offsets[i], 0).x;
        float weightDownSampled = max(0.0, 1.0 - abs(depthDownSampled - depth) * SHARPNESS);
        colorSum += colorDownSampled * weightDownSampled;
        weight += weightDownSampled;
    }

    // write color when weighted
    if (weight > 0.0) frag = colorSum / weight;
}
