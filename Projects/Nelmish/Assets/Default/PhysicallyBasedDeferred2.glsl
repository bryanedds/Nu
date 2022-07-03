#shader vertex
#version 410 core

layout (location = 0) in vec3 position;
layout (location = 2) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const int LIGHTS_MAX = 32;

uniform vec3 eyePosition;
uniform sampler2D positionTexture;
uniform sampler2D normalTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform sampler2D brdfTexture;
uniform vec3 lightPositions[LIGHTS_MAX];
uniform vec4 lightColors[LIGHTS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

float distributionGGX(vec3 normal, vec3 h, float roughness)
{
    float rPow2 = roughness * roughness;
    float rPow4 = rPow2 * rPow2;
    float nDotH = max(dot(normal, h), 0.0);
    float nDotHPow2 = nDotH * nDotH;
    float nom = rPow4;
    float denom = nDotHPow2 * (rPow4 - 1.0) + 1.0;
    return nom / (PI * denom * denom);
}

float geometrySchlickGGX(float nDotV, float roughness)
{
    float r = roughness + 1.0;
    float k = r * r / 8.0;
    float nom = nDotV;
    float denom = nDotV * (1.0 - k) + k;
    return nom / denom;
}

float geometrySchlick(vec3 normal, vec3 v, vec3 l, float roughness)
{
    float nDotV = max(dot(normal, v), 0.0);
    float nDotL = max(dot(normal, l), 0.0);
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
    // discard if geometry pixel was not written
    vec3 normal = texture(normalTexture, texCoordsOut).rgb;
    if (normal == vec3(1.0, 1.0, 1.0)) discard;

    // retrieve remaining data from geometry buffer
    vec3 position = texture(positionTexture, texCoordsOut).rgb;
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec3 material = texture(materialTexture, texCoordsOut).rgb;

    // compute materials
    float metalness = material.r;
    float roughness = material.g;
    float ambientOcclusion = material.b;

    // compute lighting profile
    vec3 v = normalize(eyePosition - position);
    vec3 r = reflect(-v, normal);

    // compute light ouput term
    // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 f0 = mix(vec3(0.04), albedo, metalness);
    vec3 lightOutput = vec3(0.0);
    for (int i = 0; i < LIGHTS_MAX; ++i)
    {
        // per-light radiance
        vec3 l = normalize(lightPositions[i] - position);
        vec3 h = normalize(v + l);
        float distance = length(lightPositions[i] - position);
        float attenuation = 1.0 / (distance * distance);
        vec3 radiance = lightColors[i].rgb * attenuation;

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
        kD *= 1.0 - metalness;

        // compute light scalar
        float nDotL = max(dot(normal, l), 0.0);

        // add to outgoing lightOutput
        lightOutput += (kD * albedo / PI + specular) * radiance * nDotL;
    }

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metalness;
    vec3 irradiance = texture(irradianceMap, normal).rgb;
    vec3 diffuse = irradiance * albedo;

    // compute specular term
    vec3 environmentFilterColor = textureLod(environmentFilterMap, r, roughness * REFLECTION_LOD_MAX).rgb;
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilterColor * (f * environmentBrdf.x + environmentBrdf.y);

    // compute ambient term
    vec3 ambient = (kD * diffuse + specular) * ambientOcclusion;

    // compute color w/ tone mapping and gamma correction
    vec3 color = ambient + lightOutput;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));

    // write
    frag = vec4(color, 1.0);
}
