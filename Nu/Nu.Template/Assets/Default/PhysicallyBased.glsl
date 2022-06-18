#shader vertex
#version 330 core

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;

out vec3 positionOut;
out vec3 normalOut;
out vec2 texCoordsOut;

void main()
{
    positionOut = vec3(model * vec4(position, 1.0));
    normalOut = mat3(model) * normal;
    texCoordsOut = texCoords;
    gl_Position = projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 330 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 4.0;
const int LIGHTS_MAX = 4;

uniform vec3 eyePosition;
uniform sampler2D albedoMap;
uniform sampler2D metallicMap;
uniform sampler2D roughnessMap;
uniform sampler2D normalMap;
uniform sampler2D ambientOcclusionMap;
uniform samplerCube irradianceCubemap;
uniform samplerCube prefilterCubemap;
uniform sampler2D brdfMap;
uniform vec3 lightPositions[4];
uniform vec3 lightColors[4];

in vec2 texCoordsOut;
in vec3 positionOut;
in vec3 normalOut;

out vec4 frag;

vec3 getNormal()
{
    vec3 tangentNormal = texture(normalMap, texCoordsOut).xyz * 2.0 - 1.0;
    vec3 q1 = dFdx(positionOut);
    vec3 q2 = dFdy(positionOut);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1*st2.t - q2*st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 tbn = mat3(tangent, binormal, normal);
    return normalize(tbn * tangentNormal);
}

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
    // compute material properties
    vec3 albedo = pow(texture(albedoMap, texCoordsOut).rgb, vec3(2.2));
    float metallic = texture(metallicMap, texCoordsOut).r;
    float roughness = texture(roughnessMap, texCoordsOut).r;
    float ambientOcclusion = texture(ambientOcclusionMap, texCoordsOut).r;

    // compute lighting profile
    vec3 n = getNormal();
    vec3 v = normalize(eyePosition - positionOut);
    vec3 r = reflect(-v, n);

    // compute reflectance term
    // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 f0 = mix(vec3(0.04), albedo, metallic);
    vec3 reflectance = vec3(0.0);
    for (int i = 0; i < LIGHTS_MAX; ++i)
    {
        // per-light radiance
        vec3 l = normalize(lightPositions[i] - positionOut);
        vec3 h = normalize(v + l);
        float distance = length(lightPositions[i] - positionOut);
        float attenuation = 1.0 / (distance * distance);
        vec3 radiance = lightColors[i] * attenuation;

        // cook-torrance brdf
        float ndf = distributionGGX(n, h, roughness);
        float g = geometrySchlick(n, v, l, roughness);
        vec3 f = fresnelSchlick(max(dot(h, v), 0.0), f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float denominator = 4.0 * max(dot(n, v), 0.0) * max(dot(n, l), 0.0) + 0.0001; // add epsilon to prevent division by zero
        vec3 specular = numerator / denominator;

        // compute diffusion
        vec3 kS = f;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        // compute light scalar
        float nDotL = max(dot(n, l), 0.0);

        // add to outgoing reflectance
        reflectance += (kD * albedo / PI + specular) * radiance * nDotL;
    }

    // compute ambient lighting
    vec3 f = fresnelSchlickRoughness(max(dot(n, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;

    // compute diffuse term
    vec3 irradiance = texture(irradianceCubemap, n).rgb;
    vec3 diffuse = irradiance * albedo;

    // compute specular term.
    vec3 prefilteredColor = textureLod(prefilterCubemap, r, roughness * REFLECTION_LOD_MAX).rgb;
    vec2 brdf = texture(brdfMap, vec2(max(dot(n, v), 0.0), roughness)).rg;
    vec3 specular = prefilteredColor * (f * brdf.x + brdf.y);

    // compute ambient term
    vec3 ambient = (kD * diffuse + specular) * ambientOcclusion;

    // compute color w/ tone mapping and gamma correction
    vec3 color = ambient + reflectance;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / 2.2));

    // write
    frag = vec4(color, 1.0);
}