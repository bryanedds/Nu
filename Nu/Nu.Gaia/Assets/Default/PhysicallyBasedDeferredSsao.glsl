#shader vertex
#version 410 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const int SSAO_SAMPLES_MAX = 64;
const vec3[SSAO_SAMPLES_MAX] SSAO_SAMPLING_DIRECTIONS = vec3[](
    vec3(0.935, -0.0272, -0.351),   vec3(0.821, -0.568, 0.0404),    vec3(-0.553, -0.247, -0.795),   vec3(-0.968, -0.114, -0.224),
    vec3(0.276, 0.950, -0.145),     vec3(-0.069, 0.994, -0.0841),   vec3(0.417, -0.884, -0.206),    vec3(0.108, -0.986, 0.115),
    vec3(0.382, 0.501, 0.776),      vec3(-0.520, 0.655, -0.547),    vec3(-0.821, -0.537, -0.191),   vec3(-0.058, 0.972, 0.227),
    vec3(0.059, -0.800, -0.595),    vec3(-0.885, -0.432, -0.171),   vec3(0.613, 0.758, -0.219),     vec3(0.381, 0.699, 0.604),
    vec3(-0.660, -0.526, 0.535),    vec3(0.466, -0.814, -0.343),    vec3(-0.524, 0.078, -0.847),    vec3(0.352, 0.932, 0.082),
    vec3(-0.431, -0.870, -0.235),   vec3(0.931, -0.041, -0.360),    vec3(0.907, 0.308, 0.283),      vec3(0.260, -0.723, -0.639),
    vec3(-0.496, -0.825, -0.270),   vec3(-0.954, -0.284, -0.091),   vec3(0.136, 0.722, 0.677),      vec3(0.397, -0.203, -0.894),
    vec3(-0.779, 0.343, 0.525),     vec3(-0.284, -0.917, -0.277),   vec3(0.441, 0.225, 0.869),      vec3(0.524, -0.5704, 0.631),
    vec3(-0.329, 0.201, 0.922),     vec3(-0.408, -0.810, 0.420),    vec3(-0.196, 0.803, 0.562),     vec3(0.850, -0.388, -0.355),
    vec3(0.766, 0.439, 0.469),      vec3(0.164, -0.555, 0.815),     vec3(-0.848, 0.363, -0.384),    vec3(0.350, -0.886, -0.303),
    vec3(-0.557, -0.179, 0.811),    vec3(-0.240, 0.709, -0.663),    vec3(-0.542, -0.699, -0.466),   vec3(0.450, -0.722, -0.525),
    vec3(0.729, -0.621, 0.286),     vec3(0.071, -0.798, 0.598),     vec3(0.318, -0.792, -0.521),    vec3(-0.173, 0.091, -0.980),
    vec3(-0.659, 0.540, -0.523),    vec3(0.864, -0.501, -0.044),    vec3(0.746, 0.375, 0.549),      vec3(-0.973, 0.180, -0.143),
    vec3(0.383, -0.925, 0.003),     vec3(-0.120, -0.967, -0.224),   vec3(0.911, 0.178, -0.373),     vec3(-0.319, 0.638, 0.700),
    vec3(0.588, -0.556, 0.587),     vec3(0.358, -0.062, -0.931),    vec3(-0.485, -0.161, 0.860),    vec3(0.165, -0.415, -0.895),
    vec3(-0.905, 0.391, 0.162),     vec3(0.684, -0.493, -0.538),    vec3(-0.733, -0.673, 0.087),    vec3(0.431, 0.847, -0.309));

uniform mat4 view;
uniform mat4 projection;
uniform sampler2D positionTexture;
uniform sampler2D normalAndHeightTexture;
uniform float ssaoIntensity;
uniform float ssaoBias;
uniform float ssaoRadius;
uniform int ssaoSampleCount;

in vec2 texCoordsOut;

out float frag;

void main()
{
    // retrieve normal value first, allowing for early-out
    vec3 normal = texture(normalAndHeightTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;

    // ensure sample count is in range and pre-compute sample count inverse
    int ssaoSampleCountCeil = max(0, min(SSAO_SAMPLES_MAX, ssaoSampleCount));
    float ssaoSampleCountInverse = 1.0 / float(ssaoSampleCountCeil);

    // compute screen space ambient occlusion
    float ssao = 0.0;
    vec3 positionView = (view * vec4(position, 1.0)).xyz;
    for (int i = 0; i < ssaoSampleCountCeil; ++i)
    {
        // compute sampling direction in world space
        vec3 samplingDirection = SSAO_SAMPLING_DIRECTIONS[i];
        samplingDirection *= ssaoRadius; // scale by radius
        samplingDirection *= mix(ssaoSampleCountInverse, 1.0f, i * ssaoSampleCountInverse); // linearly increase sampling distance from origin
        samplingDirection = dot(samplingDirection, normal) > 0.0f ? samplingDirection : -samplingDirection; // only sampling upper hemisphere

        // compute sampling position in screen space
        vec3 samplingPosition = position + samplingDirection;
        vec3 samplingPositionView = (view * vec4(samplingPosition, 1.0)).xyz;
        vec4 samplingPositionClip = projection * vec4(samplingPositionView, 1.0);
        vec2 samplingPositionScreen = samplingPositionClip.xy / samplingPositionClip.w * 0.5 + 0.5;

        // sample position in view space
        vec3 samplePosition = texture(positionTexture, samplingPositionScreen).rgb;
        vec3 samplePositionView = (view * vec4(samplePosition, 1.0)).xyz;

        // perform range check and accumulate if occluded
        float rangeCheck = smoothstep(0.0, 1.0, ssaoRadius / abs(positionView.z - samplePositionView.z));
        ssao += samplePositionView.z >= samplingPositionView.z + ssaoBias ? rangeCheck : 0.0;
    }
    ssao *= ssaoSampleCountInverse;
    ssao *= ssaoIntensity;
    ssao = 1.0 - ssao;
    ssao = max(0.0, ssao);

    // write
    frag = ssao;
}
