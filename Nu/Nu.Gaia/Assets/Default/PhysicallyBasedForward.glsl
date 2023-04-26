#shader vertex
#version 410 core

const int TexCoordsOffsetVerts = 6;

const vec2 TexCoordsOffsetFilters[TexCoordsOffsetVerts] =
    vec2[TexCoordsOffsetVerts](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TexCoordsOffsetFilters2[TexCoordsOffsetVerts] =
    vec2[TexCoordsOffsetVerts](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 2) in vec3 normal;
layout (location = 3) in mat4 model;
layout (location = 7) in vec4 texCoordsOffset;
layout (location = 8) in vec4 albedo;
layout (location = 9) in vec4 material;
layout (location = 10) in float height;
layout (location = 11) in int invertRoughness;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out float heightOut;
flat out int invertRoughnessOut;

void main()
{
    positionOut = model * vec4(position, 1.0);
    int texCoordsOffsetIndex = gl_VertexID % TexCoordsOffsetVerts;
    vec2 texCoordsOffsetFilter = TexCoordsOffsetFilters[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TexCoordsOffsetFilters2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = mat3(model) * normal;
    heightOut = height;
    invertRoughnessOut = invertRoughness;
    gl_Position = projection * view * positionOut;
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 5.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0f;
const int LIGHT_MAPS_MAX = 2;
const int LIGHTS_MAX = 32;

uniform vec3 eyeCenter;
uniform int lightMap;
uniform vec3 lightMapMin;
uniform vec3 lightMapSize;
uniform vec3 lightMapOrigin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform sampler2D albedoTexture;
uniform sampler2D metallicTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D emissionTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D normalTexture;
uniform sampler2D heightTexture;
uniform samplerCube irradianceMap;
uniform samplerCube irradianceMapLocal;
uniform samplerCube irradianceMapLocal2;
uniform samplerCube irradianceMapLocal3;
uniform samplerCube irradianceMapLocal4;
uniform samplerCube irradianceMapLocal5;
uniform samplerCube irradianceMapLocal6;
uniform samplerCube irradianceMapLocal7;
uniform samplerCube environmentFilterMap;
uniform samplerCube environmentFilterMapLocal;
uniform samplerCube environmentFilterMapLocal2;
uniform samplerCube environmentFilterMapLocal3;
uniform samplerCube environmentFilterMapLocal4;
uniform samplerCube environmentFilterMapLocal5;
uniform samplerCube environmentFilterMapLocal6;
uniform samplerCube environmentFilterMapLocal7;
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

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in float heightOut;
flat in int invertRoughnessOut;

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
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

void main()
{
    // compute basic fragment data
    vec3 position = positionOut.xyz;
    vec3 normal = normalize(normalOut);

    // compute spatial converters
    vec3 q1 = dFdx(position);
    vec3 q2 = dFdy(position);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax occlusion space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * position;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).r;
    vec2 parallax = toEyeTangent.xy * height * heightOut;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo with alpha
    vec4 albedoSample = texture(albedoTexture, texCoords);
    vec4 albedo;
    albedo.rgb = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;
    albedo.a = albedoSample.a * albedoOut.a;
    if (albedo.a == 0.0f) discard;

    // compute material properties
    float metallic = texture(metallicTexture, texCoords).r * materialOut.r;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).g * materialOut.g;
    vec4 roughnessSample = texture(roughnessTexture, texCoords);
    float roughness = roughnessSample.a == 1.0f ? roughnessSample.b : roughnessSample.a;
    roughness = (invertRoughnessOut == 0 ? roughness : 1.0f - roughness) * materialOut.b;
    vec3 emission = vec3(texture(emissionTexture, texCoords).r * materialOut.a);

    // compute lighting profile

    // compute lightAccum term
    vec3 n = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
    vec3 v = normalize(eyeCenter - position);
    vec3 f0 = mix(vec3(0.04), albedo.rgb, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
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

        // add to outgoing lightAccum
        lightAccum += (kD * albedo.rgb / PI + specular) * radiance * nDotL;
    }

    // compute irradiance
    vec3 irradiance = texture(irradianceMap, normal).rgb;

    // compute environment filter
    vec3 r = lightMap != 0 ? parallaxCorrection(environmentFilterMap, position, normal) : reflect(-v, normal);
    vec3 environmentFilter = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(n, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = irradiance * albedo.rgb * lightAmbientColor * lightAmbientBrightness;
    float alpha = albedo.a;

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(n, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * lightAmbientColor * lightAmbientBrightness;

    // compute ambient term
    vec3 ambient = (kD * diffuse + specular) * ambientOcclusion;

    // compute color w/ tone mapping, gamma correction, and emission
    vec3 color = lightAccum + ambient;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    color = color + emission * albedo.rgb;

    // write
    frag = vec4(color, alpha);
}