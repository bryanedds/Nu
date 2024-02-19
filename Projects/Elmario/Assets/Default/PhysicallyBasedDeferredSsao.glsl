#shader vertex
#version 410

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require

const float PI = 3.141592654;
const int SSAO_SAMPLES_MAX = 128;

const float SSAO_SAMPLING_ANGLES[25] = float[](
    4.4969575, 5.2307380, 2.4965059, 1.0937309, 3.9373241,
    5.8075777, 2.1917731, 3.5731674, 5.9864803, 1.5461461,
    0.5736624, 5.0692194, 2.7529615, 4.4798282, 4.1854371,
    3.4072130, 3.0481559, 4.7628750, 1.1246019, 0.3490351,
    5.8329164, 5.1977364, 4.3651479, 1.6525684, 2.9340809);

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
    vec3(-0.905, 0.391, 0.162),     vec3(0.684, -0.493, -0.538),    vec3(-0.733, -0.673, 0.087),    vec3(0.431, 0.847, -0.309),
    vec3(0.435, 0.899, -0.054),     vec3(0.764, -0.280, 0.581),     vec3(-0.674, -0.713, -0.194),   vec3(-0.390, -0.390, -0.834),
    vec3(-0.074, -0.367, 0.927),    vec3(-0.347, 0.378, -0.858),    vec3(-0.753, 0.626, 0.201),     vec3(-0.727, 0.644, -0.235),
    vec3(-0.941, 0.297, -0.159),    vec3(0.187, -0.692, 0.697),     vec3(0.292, 0.564, -0.772),     vec3(0.275, 0.961, 0.000),
    vec3(0.738, 0.580, 0.347),      vec3(-0.725, -0.442, 0.529),    vec3(0.894, -0.006, 0.447),     vec3(0.934, -0.060, 0.351),
    vec3(-0.162, -0.275, 0.947),    vec3(-0.838, 0.018, 0.545),     vec3(0.446, -0.675, -0.588),    vec3(0.134, 0.605, 0.784),
    vec3(0.839, -0.543, -0.034),    vec3(0.511, 0.494, 0.703),      vec3(-0.440, -0.572, 0.692),    vec3(-0.625, 0.253, 0.739),
    vec3(0.094, 0.975, 0.200),      vec3(0.277, -0.924, -0.264),    vec3(0.645, -0.246, -0.724),    vec3(0.262, -0.674, -0.691),
    vec3(-0.150, -0.242, -0.958),   vec3(0.419, 0.062, -0.906),     vec3(-0.462, -0.546, -0.699),   vec3(0.105, -0.718, 0.688),
    vec3(0.689, -0.724, 0.026),     vec3(-0.124, 0.897, -0.426),    vec3(-0.563, 0.256, 0.787),     vec3(-0.952, -0.268, -0.149),
    vec3(-0.399, -0.835, -0.378),   vec3(-0.767, -0.569, 0.295),    vec3(0.512, 0.460, -0.725),     vec3(0.898, -0.105, -0.428),
    vec3(0.197, -0.161, 0.967),     vec3(-0.109, 0.763, -0.637),    vec3(-0.352, -0.780, 0.518),    vec3(0.225, -0.401, -0.888),
    vec3(0.960, 0.258, 0.107),      vec3(0.516, -0.744, -0.423),    vec3(0.675, -0.459, 0.577),     vec3(-0.844, 0.403, -0.351),
    vec3(0.342, -0.882, -0.324),    vec3(0.308, -0.860, 0.406),     vec3(-0.293, -0.902, 0.315),    vec3(-0.651, -0.289, -0.702),
    vec3(0.937, 0.232, -0.262),     vec3(-0.064, -0.693, -0.718),   vec3(0.780, 0.620, -0.084),     vec3(-0.305, 0.784, -0.541),
    vec3(0.430, -0.194, -0.881),    vec3(-0.568, -0.537, -0.623),   vec3(-0.598, 0.707, -0.377),    vec3(0.366, -0.804, 0.469),
    vec3(0.062, 0.981, -0.184),     vec3(0.211, -0.936, 0.281),     vec3(0.151, -0.988, -0.027),    vec3(-0.949, -0.169, 0.266));

uniform mat4 view;
uniform mat4 projection;
layout (bindless_sampler) uniform sampler2D positionTexture;
layout (bindless_sampler) uniform sampler2D normalPlusTexture;
uniform ivec2 ssaoResolution;
uniform float ssaoIntensity;
uniform float ssaoBias;
uniform float ssaoRadius;
uniform float ssaoDistanceMax;
uniform int ssaoSampleCount;

in vec2 texCoordsOut;

out float frag;

float randomAngle()
{
    float x = gl_FragCoord.x;
    float xlow = floor(x);
    float xhigh = ceil(x);
    float y = gl_FragCoord.y;
    float ylow = floor(y);
    float yhigh = ceil(y);
    float result =
        mix(
            SSAO_SAMPLING_ANGLES[int(xlow) % 5 + int(ylow) % 5 * 5],
            SSAO_SAMPLING_ANGLES[int(xhigh) % 5 + int(yhigh) % 5 * 5],
            length(vec2(x, y) - vec2(xlow, ylow)));
    return result;
}

void main()
{
    // retrieve normal value first, allowing for early-out
    vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).xyz;

    // pre-compute resolution inverse
    vec2 ssaoResolutionInverse = vec2(1.0) / vec2(ssaoResolution);

    // ensure sample count is in range and pre-compute sample count inverse
    int ssaoSampleCountCeil = max(0, min(SSAO_SAMPLES_MAX, ssaoSampleCount));
    float ssaoSampleCountInverse = 1.0 / float(ssaoSampleCountCeil);

    // contrive sampling rotation
    float samplingAngle = randomAngle();
    mat3 samplingRotation =
        mat3(
            cos(samplingAngle), -sin(samplingAngle), 0.0,
            sin(samplingAngle), cos(samplingAngle), 0.0,
            0.0, 0.0, 1.0);

    // compute screen space ambient occlusion
    float ssao = 0.0;
    vec3 positionView = (view * vec4(position, 1.0)).xyz;
    vec3 normalView = mat3(view) * normal;
    for (int i = 0; i < ssaoSampleCountCeil; ++i)
    {
        // compute sampling direction in view space
        vec3 samplingDirectionView = samplingRotation * SSAO_SAMPLING_DIRECTIONS[i];
        samplingDirectionView *= ssaoRadius; // scale by radius
        samplingDirectionView *= mix(ssaoSampleCountInverse, 1.0f, i * ssaoSampleCountInverse); // linearly increase sampling distance from origin
        samplingDirectionView = dot(samplingDirectionView, normalView) > 0.0f ? samplingDirectionView : -samplingDirectionView; // only sampling upper hemisphere

        // compute position and sampling position in screen space along with distance from origin
        vec2 positionScreen = gl_FragCoord.xy * ssaoResolutionInverse;
        vec3 samplingPositionView = positionView + samplingDirectionView;
        vec4 samplingPositionClip = projection * vec4(samplingPositionView, 1.0);
        vec2 samplingPositionScreen = samplingPositionClip.xy / samplingPositionClip.w * 0.5 + 0.5;
        float distanceScreen = length(samplingPositionScreen - positionScreen);

        // ensure we're not sampling too far from origin and thus blowing the texture cache and that we're not using
        // empty space as indicated by normal sample
        if (distanceScreen < ssaoDistanceMax && texture(normalPlusTexture, samplingPositionScreen).xyz != vec3(1.0))
        {
            // sample position in view space
            vec3 samplePosition = texture(positionTexture, samplingPositionScreen).xyz;
            vec3 samplePositionView = (view * vec4(samplePosition, 1.0)).xyz;

            // perform range check and accumulate if occluded
            float rangeCheck = smoothstep(0.0, 1.0, ssaoRadius / abs(positionView.z - samplePositionView.z));
            ssao += samplePositionView.z >= samplingPositionView.z + ssaoBias ? rangeCheck : 0.0;
        }
    }
    ssao *= ssaoSampleCountInverse;
    ssao *= ssaoIntensity;
    ssao = 1.0 - ssao;
    ssao = max(0.0, ssao);

    // write
    frag = ssao;
}
