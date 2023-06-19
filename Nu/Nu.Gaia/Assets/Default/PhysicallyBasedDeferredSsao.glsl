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
const float SSAO = 1.4;
const float SSAO_BIAS = 0.01;
const float SSAO_RADIUS = 0.25;
const int SSAO_SAMPLES = 32;
const float SSAO_SAMPLES_INVERSE = 1.0 / float(SSAO_SAMPLES);
const vec3[SSAO_SAMPLES] SSAO_SAMPLING_DIRECTIONS = vec3[](
    vec3(0.9359231, -0.02727632, -0.35178102), vec3(0.8210779, -0.56898764, 0.04045432), vec3(-0.55349622, -0.24700827, -0.79513649), vec3(-0.96813618, -0.11483972, -0.22411245),
    vec3(0.27629185, 0.9501148, -0.1451443 ), vec3(-0.06949984, 0.99414051, -0.08414021), vec3(0.41758068, -0.88436643, -0.20611643), vec3(0.1080913, -0.98697543, 0.11580234),
    vec3(0.38256372, 0.50147269, 0.776086), vec3(-0.52093203, 0.65556912, -0.54715596), vec3(-0.82166309, -0.53712316, -0.19158611), vec3(-0.05843224, 0.97212569, 0.2273377),
    vec3(0.05989327, -0.80092543, -0.59577847), vec3(-0.88521547, -0.43258009, -0.17180302), vec3(0.61371228, 0.75813784, -0.21964663), vec3(0.38108761, 0.6994238, 0.60440729),
    vec3(-0.6606408, -0.52617907, 0.53501332), vec3(0.46617216, -0.8148892, -0.34363632), vec3(-0.52490061, 0.07865526, -0.84734162), vec3(0.35224499, 0.93218048, 0.0823843 ),
    vec3(-0.43173574, -0.87088326, -0.23551558), vec3(0.93189228, -0.04152137, -0.36085101), vec3(0.90774428, 0.30885884, 0.28341675), vec3(0.26065878, -0.7230991, -0.63901384),
    vec3(-0.49612224, -0.82538313, -0.27002421), vec3(-0.9541399, -0.28490529, -0.09172737), vec3(0.13630145, 0.7227553, 0.67767699), vec3(0.39768804, -0.20326345, -0.89493027),
    vec3(-0.7792292, 0.34389984, 0.52529078), vec3(-0.2842151, -0.91795119, -0.27790138), vec3(0.44169986, 0.22560122, 0.86954608), vec3(0.52438351, -0.57048116, 0.63150796));

uniform mat4 view;
uniform mat4 projection;
uniform sampler2D positionTexture;
uniform sampler2D normalAndHeightTexture;

in vec2 texCoordsOut;

out vec4 frag;

void main()
{
    // retrieve normal value first, allowing for early-out
    vec3 normal = texture(normalAndHeightTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;

    // compute screen space ambient occlusion
    float ambientOcclusionScreen = 0.0;
    vec3 positionView = (view * vec4(position, 1.0)).xyz;
    for (int i = 0; i < SSAO_SAMPLES; ++i)
    {
        // compute sampling direction in world space
        vec3 samplingDirection = SSAO_SAMPLING_DIRECTIONS[i];
        samplingDirection *= SSAO_RADIUS; // scale by radius
        samplingDirection *= mix(SSAO_SAMPLES_INVERSE, 1.0f, i * SSAO_SAMPLES_INVERSE); // linearly increase sampling distance from origin
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
        float rangeCheck = smoothstep(0.0, 1.0, SSAO_RADIUS / abs(positionView.z - samplePositionView.z));
        ambientOcclusionScreen += samplePositionView.z >= samplingPositionView.z + SSAO_BIAS ? rangeCheck : 0.0;
    }
    ambientOcclusionScreen *= SSAO_SAMPLES_INVERSE;
    ambientOcclusionScreen *= SSAO;
    ambientOcclusionScreen = 1.0 - ambientOcclusionScreen;
    ambientOcclusionScreen = max(0.0, ambientOcclusionScreen);

    // write
    frag = vec4(ambientOcclusionScreen, 0.0, 0.0, 0.0);
}
