#version 450 core

const float GAMMA = 2.2;

layout(set = 0, binding = 0) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 color = texture(sampler2D(inputTexture, inputSampler), texCoords, 0).rgb;
    color = pow(color, vec3(1.0 / GAMMA));
    frag = vec4(color, 1.0);
}
