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
const float REFLECTION_LOD_MAX = 5.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 96;
const float SSAO = 1.5;
const float SSAO_RADIUS = 1.0;
const float SSAO_BIAS = 0.01;
const int SSAO_SAMPLES = 96;
const int SSAO_ITERATIONS = 4;
const int SSAO_SUBSAMPLES = SSAO_SAMPLES / SSAO_ITERATIONS;
const float SSAO_SUBSAMPLES_RECIPRICOL = 1.0 / float(SSAO_SUBSAMPLES);
const vec3 SSAO_TANGENTS[SSAO_ITERATIONS] = vec3[SSAO_ITERATIONS](
    vec3(1.0, 0.0, 0.0),
    vec3(-1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, -1.0, 0.0));

uniform mat4 view;
uniform mat4 projection;
uniform vec3 eyeCenter;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform sampler2D positionTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalAndDepthTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform sampler2D brdfTexture;
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

float hash(float n)
{
    return fract(sin(n) * 43758.5453123);
}

float random(float seedX, float seedY)
{
    return fract(sin(dot(vec2(seedX, seedY), vec2(12.9898, 78.233))) * 43758.5453);
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
    // first, retrieve normal and depth values, allowing for early-out
    vec4 normalAndDepth = texture(normalAndDepthTexture, texCoordsOut);
    vec3 normal = normalAndDepth.rgb;
    if (normal == vec3(1.0, 1.0, 1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)
    float depth = normalAndDepth.a;

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);

    // compute materials
    float metallic = material.r;
    float ambientOcclusion = material.g;
    float roughness = material.b;
    vec3 emission = vec3(material.a);

    // compute lighting profile
    vec3 v = normalize(eyeCenter - position);
    vec3 r = reflect(-v, normal);

    // compute lightAccum term
    // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 f0 = mix(vec3(0.04), albedo, metallic);
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

    // compute screen-space ambient occlusion
    float ambientOcclusionScreen = 0.0;
    vec3 positionView = (view * vec4(position, 1.0)).xyz;
    for (int i = 0; i < SSAO_ITERATIONS; ++i)
    {
        // compute tangent basis for ssao operations
        vec3 normalView = normalize(transpose(inverse(mat3(view))) * normal);
        vec3 tangentView = normalize(SSAO_TANGENTS[i] - dot(SSAO_TANGENTS[i], normalView) * normalView);
        vec3 bitangentView = cross(normalView, tangentView);
        mat3 tangentToView = mat3(tangentView, bitangentView, normalView);

        // iterate over the sample kernel and calculate occlusion factor
        for (int j = 0; j < SSAO_SUBSAMPLES; ++j)
        {
            // get sample position in view space
            float s = float(j) * 3.0;
            vec3 sampleDirection = normalize(vec3(hash(s), abs(hash(s+1.0)), hash(s+2.0))) * 2.0 - 1.0;
            sampleDirection *= mix(SSAO_SUBSAMPLES_RECIPRICOL, 1.0f, j * SSAO_SUBSAMPLES_RECIPRICOL);
            vec3 samplePositionView = tangentToView * sampleDirection; // from tangent to view-space
            samplePositionView = positionView + samplePositionView * SSAO_RADIUS;

            // project sample position from view space to clip space
            vec4 offset = vec4(samplePositionView, 1.0);
            offset = projection * offset; // from view to clip-space
            offset.xyz /= offset.w; // perspective divide
            offset.xyz = offset.xyz * 0.5 + 0.5; // transform to range 0.0 - 1.0

            // get sample depth, perform range check, then accumulate
            float sampleDepth = ((view * texture(positionTexture, offset.xy)).rgb).z;
            float rangeCheck = smoothstep(0.0, 1.0, SSAO_RADIUS / abs(positionView.z - sampleDepth));
            ambientOcclusionScreen += (sampleDepth >= samplePositionView.z + SSAO_BIAS ? 1.0 : 0.0) * rangeCheck;
        }
    }
    ambientOcclusionScreen = 1.0 - ambientOcclusionScreen / float(SSAO_SAMPLES) * SSAO;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 irradiance = texture(irradianceMap, normal).rgb * lightAmbientColor * lightAmbientBrightness;
    vec3 diffuse = irradiance * albedo;

    // compute specular term
    vec3 environmentFilter = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb * lightAmbientColor * lightAmbientBrightness;
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y);

    // compute ambient term
    vec3 ambient = kD * diffuse * ambientOcclusion * ambientOcclusionScreen + specular;

    // compute color w/ tone mapping, gamma correction, and emission
    vec3 color = lightAccum + ambient;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    color = color + emission * albedo.rgb;

    // write
    frag = vec4(color, 1.0);
}
