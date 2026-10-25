#version 450 core

const float PI = 3.141592654;
const float GAMMA = 2.2;
const float DITHER_ENTROPY = 409.0;
const float DITHER_SIN_SCALAR = 1500.0;
const float DITHER_STRENGTH = 0.005;

layout(set = 0, binding = 0) uniform texture2D inputTexture;

layout(set = 1, binding = 0) uniform sampler inputSampler;

layout(location = 0) in vec2 texCoords;

layout(location = 0) out vec4 frag;

float computeDither()
{
    float seed = texCoords.x + texCoords.y * DITHER_ENTROPY;
    return fract(DITHER_SIN_SCALAR * sin(seed)) * DITHER_STRENGTH;
}

void main()
{
    // sample color
    vec3 color = texture(sampler2D(inputTexture, inputSampler), texCoords, 0).rgb;

    // apply gamma correction
    color = pow(color, vec3(1.0 / GAMMA));

    // apply dithering to make up for precision reduction from gamma correction
    // sourced from - https://lettier.github.io/3d-game-shaders-for-beginners/film-grain.html
    color.rgb += vec3(computeDither());

    // write fragment
    frag = vec4(color, 1.0);
}
