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
const float FLOAT_MAX = 3.402823466e+38;
const float REFLECTION_LOD_MAX = 5.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0;
const float PARALLAX_CORRECTION_SCALE = 0.02;
const int LIGHT_MAPS_MAX = 8;
const int LIGHTS_MAX = 64;
const float SSAO = 1.25;
const float SSAO_RADIUS = 0.5;
const float SSAO_BIAS = 0.01;
const int SSAO_SAMPLES = 96;
const float SSAO_SAMPLES_INVERSE = 1.0 / float(SSAO_SAMPLES);
const vec3[SSAO_SAMPLES] SSAO_SAMPLING_DIRECTIONS = vec3[](
    vec3(0.16628033, 0.98031127, 0.1090987), vec3(-0.73178808, -0.30931247, 0.60528424), vec3(0.05797729, -0.95963732, -0.27573171), vec3(0.83240714, -0.48937448, 0.25896714),
    vec3(-0.54424764, 0.29322405, 0.78570088), vec3(0.7682875, -0.6353843, -0.07468267), vec3(-0.88601684, 0.31203889, -0.34169098), vec3(-0.88316705, -0.29226478, -0.36708327),
    vec3(-0.75806223, -0.57716967, -0.30456295), vec3(-0.27128172, -0.86209868, 0.42658131), vec3(-0.22086532, -0.53516115, -0.81472868), vec3(0.42703992, 0.71746574, -0.55026034),
    vec3(0.9359231, -0.02727632, -0.35178102), vec3(0.8210779, -0.56898764, 0.04045432), vec3(-0.55349622, -0.24700827, -0.79513649), vec3(-0.96813618, -0.11483972, -0.22411245),
    vec3(-0.90443412, 0.23467092, 0.35696694), vec3(-0.67700226, -0.47685315, -0.55906321), vec3(0.96360764, -0.12112319, -0.23834743), vec3(-0.13367438, -0.83547148, -0.53240409),
    vec3(0.27629185, 0.9501148, -0.1451443 ), vec3(-0.06949984, 0.99414051, -0.08414021), vec3(0.41758068, -0.88436643, -0.20611643), vec3(0.1080913, -0.98697543, 0.11580234),
    vec3(0.92614116, -0.03146595, 0.37537357), vec3(0.1953031, -0.97656169, -0.08620548), vec3(-0.2363298, -0.93043551, -0.2803014 ), vec3(-0.18945285, -0.98097892, 0.03954832),
    vec3(0.33922152, 0.80777186, 0.48003408), vec3(-0.77157015, -0.51573158, -0.37127888), vec3(0.49419218, -0.6539731, 0.57275781), vec3(-0.30255045, -0.94479081, -0.12463052),
    vec3(0.38256372, 0.50147269, 0.776086), vec3(-0.52093203, 0.65556912, -0.54715596), vec3(-0.82166309, -0.53712316, -0.19158611), vec3(-0.05843224, 0.97212569, 0.2273377 ),
    vec3(-0.72557985, -0.3508935, 0.59223811), vec3(0.07456534, 0.66294589, 0.74435879), vec3(-0.18699833, -0.48022118, -0.85606789), vec3(-0.97348632, -0.09776054, -0.20878801),
    vec3(-0.52524914, 0.45615706, -0.71871028), vec3(0.09170391, -0.03913828, 0.99474815), vec3(-0.2468046, 0.82528938, 0.50736308), vec3(0.25914143, -0.31174538, -0.91317047),
    vec3(-0.78617654, 0.48280039, 0.38333033), vec3(0.23595569, -0.24462351, -0.94020334), vec3(0.83675803, -0.20701828, -0.50659673), vec3(0.72094744, 0.63442062, 0.28012598),
    vec3(0.65056219, -0.75502587, -0.08558727), vec3(0.32967887, -0.76845654, 0.54831097), vec3(-0.16275214, -0.98351949, -0.07592411), vec3(-0.21233449, -0.03247453, -0.97642807),
    vec3(0.50823303, -0.72087949, 0.47158727), vec3(-0.6916743, -0.60842421, -0.39119645), vec3(0.74586961, -0.04063843, -0.66464078), vec3(-0.12465777, 0.48092895, -0.86795433),
    vec3(0.05989327, -0.80092543, -0.59577847), vec3(-0.88521547, -0.43258009, -0.17180302), vec3(0.61371228, 0.75813784, -0.21964663), vec3(0.38108761, 0.6994238, 0.60440729),
    vec3(-0.28849762, -0.08917015, -0.95319779), vec3(-0.37106589, 0.8725433, 0.31802352), vec3(0.66272525, -0.7201678, -0.20427001), vec3(-0.3991032, 0.87828506, 0.26523621),
    vec3(-0.55767943, -0.16777643, -0.81325633), vec3(-0.76462043, -0.56213702, -0.31512292), vec3(-0.36184022, -0.13197954, 0.9227112 ), vec3(-0.57229405, -0.67560978, -0.46430111),
    vec3(-0.37616852, -0.78392198, -0.49496419), vec3(0.63794469, -0.53653562, 0.55273611), vec3(0.9472977, 0.14148769, -0.28704415), vec3(0.74634471, -0.54525999, -0.38130754),
    vec3(-0.30417697, 0.92931925, -0.20922226), vec3(-0.2077227, -0.36127262, 0.90901584), vec3(-0.17062094, -0.41011599, -0.89600709), vec3(0.87642291, -0.22738558, 0.4240449 ),
    vec3(-0.6606408, -0.52617907, 0.53501332), vec3(0.46617216, -0.8148892, -0.34363632), vec3(-0.52490061, 0.07865526, -0.84734162), vec3(0.35224499, 0.93218048, 0.0823843 ),
    vec3(0.6711024, 0.5717819, 0.47277569), vec3(0.30415247, -0.85081069, -0.42913759), vec3(-0.75866344, -0.38170469, -0.52773738), vec3(0.4388384, 0.89759687, 0.03937445),
    vec3(-0.77581787, -0.62154051, -0.10982123), vec3(-0.63908292, 0.49880337, -0.58407057), vec3(-0.33957551, 0.93914362, 0.04858199), vec3(0.1108422, 0.65754083, -0.74541667),
    vec3(-0.90282783, -0.2951247, -0.31190677), vec3(-0.21754245, 0.96337104, 0.15405321), vec3(-0.21695763, 0.71202469, -0.66876785), vec3(0.1464786, 0.31270654, 0.93883212),
    vec3(-0.43173574, -0.87088326, -0.23551558), vec3(0.93189228, -0.04152137, -0.36085101), vec3(0.90774428, 0.30885884, 0.28341675), vec3(0.26065878, -0.7230991, -0.63901384));

uniform mat4 view;
uniform mat4 projection;
uniform vec3 eyeCenter;
uniform int lightMap;
uniform vec3 lightMapMin;
uniform vec3 lightMapSize;
uniform vec3 lightMapOrigin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform sampler2D positionTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalAndHeightTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform sampler2D brdfTexture;
uniform int lightMaps[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];
uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec3 lightDirections[LIGHTS_MAX];
uniform vec3 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightAttenuationLinears[LIGHTS_MAX];
uniform float lightAttenuationQuadratics[LIGHTS_MAX];
uniform int lightDirectionals[LIGHTS_MAX];
uniform float lightConeInners[LIGHTS_MAX];
uniform float lightConeOuters[LIGHTS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

vec3 parallaxCorrection(samplerCube cubeMap, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eyeCenter;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMapMin + lightMapSize - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMapMin - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMapOrigin;
}

float distributionGGX(vec3 normal, vec3 h, float roughness)
{
    float a = roughness * roughness;
    float aPow2 = a * a;
    float nDotH = max(dot(normal, h), 0.0);
    float nDotHPow2 = nDotH * nDotH;
    float nom = aPow2;
    float denom = nDotHPow2 * (aPow2 - 1.0) + 1.0;
    denom = PI * denom * denom;
    return nom / denom;
}

float geometrySchlickGGX(float nDotV, float roughness)
{
    float r = roughness + 1.0;
    float k = r * r / 8.0;
    float nom = nDotV;
    float denom = nDotV * (1.0 - k) + k;
    return nom / denom;
}

float geometrySchlick(vec3 n, vec3 v, vec3 l, float roughness)
{
    float nDotV = max(dot(n, v), 0.0);
    float nDotL = max(dot(n, l), 0.0);
    float ggx2 = geometrySchlickGGX(nDotV, roughness);
    float ggx1 = geometrySchlickGGX(nDotL, roughness);
    return ggx1 * ggx2;
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

void main()
{
    // retrieve normal and height values first, allowing for early-out
    vec4 normalAndHeight = texture(normalAndHeightTexture, texCoordsOut);
    vec3 normal = normalAndHeight.rgb;
    if (normal == vec3(1.0, 1.0, 1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)
    float height = normalAndHeight.a;

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);

    // compute materials
    float metallic = material.r;
    float ambientOcclusion = material.g;
    float roughness = material.b;
    vec3 emission = vec3(material.a);

    // compute lightAccum term
    vec3 v = normalize(eyeCenter - position);
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 lightAccum = vec3(0.0);
    for (int i = 0; i < LIGHTS_MAX; ++i)
    {
        // per-light radiance
        vec3 d, l, h;
        vec3 radiance;
        if (lightDirectionals[i] == 0)
        {
            d = lightOrigins[i] - position;
            l = normalize(d);
            h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float attenuation = 1.0f / (ATTENUATION_CONSTANT + lightAttenuationLinears[i] * distance + lightAttenuationQuadratics[i] * distanceSquared);
            float angle = acos(dot(lightDirections[i], l));
            float coneDelta = lightConeOuters[i] - lightConeInners[i];
            float coneBetween = angle - lightConeInners[i];
            float coneScalar = clamp(1.0f - coneBetween / coneDelta, 0.0f, 1.0f);
            float intensity = attenuation * coneScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity;
        }
        else
        {
            d = lightDirections[i];
            l = d;
            h = normalize(v + l);
            radiance = lightColors[i] * lightBrightnesses[i];
        }

        // cook-torrance brdf
        float ndf = distributionGGX(normal, h, roughness);
        float g = geometrySchlick(normal, v, l, roughness);
        vec3 f = fresnelSchlick(max(dot(h, v), 0.0), f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float denominator = 4.0 * max(dot(normal, v), 0.0) * max(dot(normal, l), 0.0) + 0.0001; // add epsilon to prevent division by zero
        vec3 specular = numerator / denominator;

        // compute diffusion
        vec3 kS = f;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        // compute light scalar
        float nDotL = max(dot(normal, l), 0.0);

        // add to outgoing lightAccum
        lightAccum += (kD * albedo / PI + specular) * radiance * nDotL;
    }

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
    ambientOcclusionScreen /= float(SSAO_SAMPLES);
    ambientOcclusionScreen *= SSAO;
    ambientOcclusionScreen = 1.0 - ambientOcclusionScreen;
    ambientOcclusionScreen = max(0.0, ambientOcclusionScreen);

    // compute first light map index
    int lm1Index = -1;
    float lm1DistanceSquared = FLOAT_MAX;
    for (int i = 0; i < LIGHT_MAPS_MAX; ++i)
    {
        if (lightMaps[i] != 0)
        {
            vec3 delta = lightMapOrigins[i] - position;
            float distanceSquared = dot(delta, delta);
            if (distanceSquared < lm1DistanceSquared)
            {
                lm1Index = i;
                lm1DistanceSquared = distanceSquared;
            }
        }
    }

    // compute second light map index
    int lm2Index = -1;
    float lm2DistanceSquared = FLOAT_MAX;
    for (int i = 0; i < LIGHT_MAPS_MAX; ++i)
    {
        if (lightMaps[i] != 0)
        {
            vec3 delta = lightMapOrigins[i] - position;
            float distanceSquared = dot(delta, delta);
            if (distanceSquared < lm2DistanceSquared && i != lm1Index)
            {
                lm2Index = i;
                lm2DistanceSquared = distanceSquared;
            }
        }
    }

    //int lm1Index = -1;
    //int lm2Index = -1;
    //float lm1DistanceSquared = FLOAT_MAX;
    //float lm2DistanceSquared = FLOAT_MAX;
    //for (int i = 0; i < LIGHT_MAPS_MAX; ++i)
    //{
    //    if (lightMaps[i] != 0)
    //    {
    //        vec3 delta = lightMapOrigins[i] - position;
    //        float distanceSquared = dot(delta, delta);
    //        if (distanceSquared < lm1DistanceSquared)
    //        {
    //            lm2Index = lm1Index;
    //            lm2DistanceSquared = lm1DistanceSquared;
    //            lm1Index = i;
    //            lm1DistanceSquared = distanceSquared;
    //        }
    //        else if (distanceSquared < lm2DistanceSquared && i != lm1Index)
    //        {
    //            lm2Index = i;
    //            lm2DistanceSquared = distanceSquared;
    //        }
    //    }
    //}

    // compute irradiance and environment filter terms
    vec3 irradiance = vec3(0.0);
    vec3 environmentFilter = vec3(0.0);
    if (lm1Index == -1 && lm2Index == -1)
    {
        irradiance = texture(irradianceMap, normal).rgb;
        vec3 r = lightMap != 0 ? parallaxCorrection(environmentFilterMap, position, normal) : reflect(-v, normal);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else if (lm2Index == -1)
    {
        irradiance = texture(irradianceMaps[lm1Index], normal).rgb;
        vec3 r = parallaxCorrection(environmentFilterMaps[lm1Index], position, normal);
        environmentFilter = textureLod(environmentFilterMaps[lm1Index], r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else
    {
        // compute blended irradiance
        float distance1 = sqrt(lm1DistanceSquared);
        float distance2 = sqrt(lm2DistanceSquared);
        float distanceTotal = distance1 + distance2;
        vec3 irradiance1 = texture(irradianceMaps[lm1Index], normal).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2Index], normal).rgb;
        irradiance = irradiance1 * (distance1 / distanceTotal) + irradiance2 * (distance2 / distanceTotal);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(environmentFilterMaps[lm1Index], position, normal);
        vec3 r2 = parallaxCorrection(environmentFilterMaps[lm2Index], position, normal);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1Index], r1, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2Index], r2, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        environmentFilter = environmentFilter1 * (distance1 / distanceTotal) + environmentFilter2 * (distance2 / distanceTotal);
    }

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = irradiance * albedo * lightAmbientColor * lightAmbientBrightness;

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * lightAmbientColor * lightAmbientBrightness;

    // compute ambient term
    vec3 ambient = (kD * diffuse + specular) * ambientOcclusion * ambientOcclusionScreen;

    // compute color w/ tone mapping, gamma correction, and emission
    vec3 color = lightAccum + ambient;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    color = color + emission * albedo.rgb;

    // write
    frag = vec4(color, 1.0);
}
