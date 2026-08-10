#version 450 core

layout(set = 0, binding = 0) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out float frag;

void main()
{
    vec2 texelSize = 1.0 / vec2(textureSize(sampler2D(inputTexture, inputSampler), 0));
    float result = 0.0;
    for (int i = -2; i < 3; ++i) 
    {
        float x = float(i);
        for (int j = -2; j < 3; ++j) 
        {
            float y = float(j);
            vec2 offset = vec2(x, y) * texelSize;
            result += texture(sampler2D(inputTexture, inputSampler), texCoords + offset).r;
        }
    }
    frag = result / 25.0f;
}
