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
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const float SHADOW_FOV_MAX = 2.1;
const int SHADOWS_MAX = 16;

uniform vec3 eyeCenter;
uniform float lightCutoffMargin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform float lightShadowBiasAcne;
uniform float lightShadowBiasBleed;
layout (bindless_sampler) uniform sampler2D positionTexture;
layout (bindless_sampler) uniform sampler2D albedoTexture;
layout (bindless_sampler) uniform sampler2D materialTexture;
layout (bindless_sampler) uniform sampler2D normalPlusTexture;
layout (bindless_sampler) uniform sampler2D brdfTexture;
layout (bindless_sampler) uniform sampler2D irradianceTexture;
layout (bindless_sampler) uniform sampler2D environmentFilterTexture;
layout (bindless_sampler) uniform sampler2D ssaoTexture;
layout (bindless_sampler) uniform sampler2D shadowTextures[SHADOWS_MAX];
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec3 lightDirections[LIGHTS_MAX];
uniform vec3 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightAttenuationLinears[LIGHTS_MAX];
uniform float lightAttenuationQuadratics[LIGHTS_MAX];
uniform float lightCutoffs[LIGHTS_MAX];
uniform int lightDirectionals[LIGHTS_MAX];
uniform float lightConeInners[LIGHTS_MAX];
uniform float lightConeOuters[LIGHTS_MAX];
uniform int lightShadowIndices[LIGHTS_MAX];
uniform int lightsCount;
uniform mat4 shadowMatrices[SHADOWS_MAX];

in vec2 texCoordsOut;

layout (location = 0) out vec4 frag;

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

float computeShadowScalar(sampler2D shadowMap, vec2 shadowTexCoords, float shadowZ, float varianceMin, float lightBleedFilter)
{
    vec2 moments = texture(shadowMap, shadowTexCoords).xy;
    float p = step(shadowZ, moments.x);
    float variance = max(moments.y - moments.x * moments.x, varianceMin);
    float delta = shadowZ - moments.x;
    float pMax = linstep(lightBleedFilter, 1.0, variance / (variance + delta * delta));
    return max(p, pMax);
}

float fadeShadowScalar(vec2 shadowTexCoords, float shadowScalar)
{
    vec2 normalized = abs(shadowTexCoords * 2.0 - 1.0);
    float fadeScalar =
        max(
            smoothstep(0.85, 1.0, normalized.x),
            smoothstep(0.85, 1.0, normalized.y));
    return 1.0 - (1.0 - shadowScalar) * (1.0 - fadeScalar);
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
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void main()
{
    // retrieve normal value first, allowing for early-out
    vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).xyz;
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);

    // retrieve data from intermediate buffers
    vec3 irradiance = texture(irradianceTexture, texCoordsOut).rgb;
    vec3 environmentFilter = texture(environmentFilterTexture, texCoordsOut).rgb;
    float ssao = texture(ssaoTexture, texCoordsOut).r;

    // compute materials
    float roughness = material.r;
    float metallic = material.g;
    float ambientOcclusion = material.b * ssao;
    vec3 emission = vec3(material.a);

    // compute lightAccum term
    vec3 v = normalize(eyeCenter - position);
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 lightAccum = vec3(0.0);
    for (int i = 0; i < lightsCount; ++i)
    {
        // per-light radiance
        vec3 l, h, radiance;
        if (lightDirectionals[i] == 0)
        {
            vec3 d = lightOrigins[i] - position;
            l = normalize(d);
            h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoff = lightCutoffs[i];
            float cutoffScalar = 1.0 - smoothstep(cutoff * (1.0 - lightCutoffMargin), cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinears[i] * distance + lightAttenuationQuadratics[i] * distanceSquared);
            float angle = acos(dot(l, -lightDirections[i]));
            float halfConeInner = lightConeInners[i] * 0.5;
            float halfConeOuter = lightConeOuters[i] * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity * cutoffScalar;
        }
        else
        {
            l = -lightDirections[i];
            h = normalize(v + l);
            radiance = lightColors[i] * lightBrightnesses[i];
        }

        // shadow scalar
        int shadowIndex = lightShadowIndices[i];
        float shadowScalar = 1.0;
        if (shadowIndex >= 0)
        {
            vec4 positionShadow = shadowMatrices[shadowIndex] * vec4(position, 1.0);
            vec3 shadowTexCoordsProj = positionShadow.xyz / positionShadow.w;
            vec2 shadowTexCoords = vec2(shadowTexCoordsProj.x, shadowTexCoordsProj.y) * 0.5 + 0.5;
            float shadowZ = shadowTexCoordsProj.z * 0.5 + 0.5;
            if (shadowZ < 1.0f && shadowTexCoords.x >= 0.0 && shadowTexCoords.x <= 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y <= 1.0)
            {
                shadowScalar = computeShadowScalar(shadowTextures[shadowIndex], shadowTexCoords, shadowZ, lightShadowBiasAcne, lightShadowBiasBleed);
                if (lightConeOuters[i] > SHADOW_FOV_MAX) shadowScalar = fadeShadowScalar(shadowTexCoords, shadowScalar);
            }
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
        lightAccum += (kD * albedo / PI + specular) * radiance * nDotL * shadowScalar;
    }

    // compute light ambient terms
    // NOTE: lightAmbientSpecular gets an additional ao multiply for some specular occlusion.
    // TODO: use a better means of computing specular occlusion as this one isn't very effective.
    vec3 lightAmbientDiffuse = lightAmbientColor * lightAmbientBrightness * ambientOcclusion;
    vec3 lightAmbientSpecular = lightAmbientDiffuse * ambientOcclusion;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedo * lightAmbientDiffuse;

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * lightAmbientSpecular;

    // compute ambient term
    vec3 ambient = diffuse + specular;

    // compute color w/ tone mapping, gamma correction, and emission
    vec3 color = lightAccum + ambient;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    color = color + emission * albedo.rgb;

    // write
    frag = vec4(color, 1.0);
}
