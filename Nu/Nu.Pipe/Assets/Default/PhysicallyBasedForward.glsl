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
layout (location = 9) in vec3 material;

out vec3 positionOut;
out vec2 texCoordsOut;
out vec4 albedoOut;
out vec3 materialOut;
out vec3 normalOut;

void main()
{
    positionOut = vec3(model * vec4(position, 1.0));
    int texCoordsOffsetIndex = gl_VertexID % TexCoordsOffsetVerts;
    vec2 texCoordsOffsetFilter = TexCoordsOffsetFilters[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TexCoordsOffsetFilters2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = mat3(model) * normal;
    gl_Position = projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 5.0;
const float GAMMA = 2.2;
const int LIGHTS_MAX = 8;

uniform vec3 eyeCenter;
uniform sampler2D albedoTexture;
uniform sampler2D metalnessTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D normalTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform sampler2D brdfTexture;
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec4 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightIntensities[LIGHTS_MAX];

in vec3 positionOut;
in vec2 texCoordsOut;
in vec4 albedoOut;
in vec3 materialOut;
in vec3 normalOut;

out vec4 frag;

vec3 getNormal()
{
    vec3 tangentNormal = texture(normalTexture, texCoordsOut).xyz * 2.0 - 1.0;
    vec3 q1 = dFdx(positionOut);
    vec3 q2 = dFdy(positionOut);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 tbn = mat3(tangent, binormal, normal);
    return normalize(tbn * tangentNormal);
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
    // compute albedo with alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut);
    vec4 albedo;
    albedo.rgb = pow(albedoSample.rgb * albedoOut.rgb, vec3(GAMMA));
    albedo.a = albedoSample.a * albedoOut.a;

    // compute material properties
    float metalness = texture(metalnessTexture, texCoordsOut).r * materialOut.r;
    float roughness = texture(roughnessTexture, texCoordsOut).r * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoordsOut).r * materialOut.b;

    // compute lighting profile
    vec3 n = getNormal();
    vec3 v = normalize(eyeCenter - positionOut);
    vec3 r = reflect(-v, n);

    // compute lightAccum term
    // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 f0 = mix(vec3(0.04), albedo.rgb, metalness);
    vec3 lightAccum = vec3(0.0);
    for (int i = 0; i < LIGHTS_MAX; ++i)
    {
        // per-light radiance
        vec3 l = normalize(lightOrigins[i] - positionOut);
        vec3 h = normalize(v + l);
        vec3 d = lightOrigins[i] - positionOut;
        float distanceSquared = dot(d, d);
        float attenuation = 1.0 / distanceSquared;
        float intensity =
            // TODO: 3D: figure out an algorithm for intensity that doesn't create a black hole at origin like this one -
            // pow(max(attenuation, 0.0001), 1.0 / lightIntensities[i]);
            attenuation;
        vec3 radiance = lightColors[i].rgb * lightBrightnesses[i] * intensity;

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
        kD *= 1.0 - metalness;

        // compute light scalar
        float nDotL = max(dot(n, l), 0.0);

        // add to outgoing lightAccum
        lightAccum += (kD * albedo.rgb / PI + specular) * radiance * nDotL;
    }

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(n, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metalness;
    vec3 irradiance = texture(irradianceMap, n).rgb;
    vec3 diffuse = irradiance * albedo.rgb;
    float alpha = albedo.a;

    // compute specular term
    vec3 environmentFilterColor = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(n, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilterColor * (f * environmentBrdf.x + environmentBrdf.y);

    // compute ambient term
    vec3 ambient = (kD * diffuse.rgb + specular) * ambientOcclusion;

    // compute color w/ tone mapping and gamma correction
    vec3 color = ambient + lightAccum;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));

    // write
    frag = vec4(color, alpha);
}
