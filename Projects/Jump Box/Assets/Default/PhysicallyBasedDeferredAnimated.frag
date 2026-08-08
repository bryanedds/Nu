#version 450 core

const float GAMMA = 2.2;
const float ALBEDO_ALPHA_MIN = 0.3;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as lighting config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as lighting config property.

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };

layout(set = 1, binding = 0) uniform texture2D albedoTexture;
layout(set = 1, binding = 1) uniform texture2D roughnessTexture;
layout(set = 1, binding = 2) uniform texture2D metallicTexture;
layout(set = 1, binding = 3) uniform texture2D ambientOcclusionTexture;
layout(set = 1, binding = 4) uniform texture2D emissionTexture;
layout(set = 1, binding = 5) uniform texture2D normalTexture;
layout(set = 1, binding = 6) uniform texture2D heightTexture;
layout(set = 1, binding = 7) uniform texture2D subdermalTexture;
layout(set = 1, binding = 8) uniform texture2D finenessTexture;
layout(set = 1, binding = 9) uniform texture2D scatterTexture;
layout(set = 1, binding = 10) uniform texture2D clearCoatTexture;
layout(set = 1, binding = 11) uniform texture2D clearCoatRoughnessTexture;
layout(set = 1, binding = 12) uniform texture2D clearCoatNormalTexture;

layout(set = 3, binding = 0) uniform sampler materialSampler;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) flat in vec4 albedo;
layout(location = 4) flat in vec4 material;
layout(location = 5) flat in vec4 heightPlus;
layout(location = 6) flat in vec4 subsurfacePlus;
layout(location = 7) flat in vec4 clearCoatPlus;

layout(location = 0) out float depthOut;
layout(location = 1) out vec3 albedoOut;
layout(location = 2) out vec4 materialOut;
layout(location = 3) out vec4 normalPlusOut;
layout(location = 4) out vec4 subdermalPlusOut;
layout(location = 5) out vec4 scatterPlusOut;
layout(location = 6) out vec4 clearCoatPlusOut;

// NOTE: algorithm from Chapter 16 of OpenGL Shading Language.
vec3 saturate(vec3 rgb, float adjustment)
{
    const vec3 w = vec3(0.2125, 0.7154, 0.0721);
    vec3 intensity = vec3(dot(rgb, w));
    return mix(intensity, rgb, adjustment);
}

vec3 decodeNormal(vec2 normalEncoded)
{
    vec2 xy = normalEncoded * 2.0 - 1.0;
    float z = sqrt(max(0.0, 1.0 - dot(xy, xy)));
    return normalize(vec3(xy, z));
}

float signNotZero(float f)
{
    return f >= 0.0 ? 1.0 : -1.0;
}

vec2 signNotZero(vec2 v)
{
    return vec2(signNotZero(v.x), signNotZero(v.y));
}

vec2 encodeOctahedral(vec3 v)
{
    float l1norm = abs(v.x) + abs(v.y) + abs(v.z);
    vec2 result = v.xy * (1.0 / l1norm);
    if (v.z < 0.0)
    {
        result = (1.0 - abs(result.yx)) * signNotZero(result.xy);
    }
    return result;
}

void main()
{
    // write depth
    depthOut = gl_FragCoord.z;

    // compute spatial converters
    vec3 q1 = dFdx(position.xyz);
    vec3 q2 = dFdy(position.xyz);
    vec2 st1 = dFdx(texCoords);
    vec2 st2 = dFdy(texCoords);
    vec3 normal = normalize(normal);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    tangent = normalize(tangent - normal * dot(normal, tangent));
    binormal = cross(normal, tangent);
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eye.center;
    vec3 positionTangent = toTangent * position.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(sampler2D(heightTexture, materialSampler), texCoords).x * heightPlus.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoords - parallax;

    // compute albedo
    vec4 albedoSample = texture(sampler2D(albedoTexture, materialSampler), texCoords);
    if (albedoSample.a < ALBEDO_ALPHA_MIN) discard;
    albedoOut = pow(albedoSample.rgb, vec3(GAMMA)) * albedo.rgb;

    // compute normal and ignore local height maps
    normalPlusOut.xyz = normalize(toWorld * decodeNormal(texture(sampler2D(normalTexture, materialSampler), texCoords).xy));
    normalPlusOut.w = heightPlus.y;

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = texture(sampler2D(roughnessTexture, materialSampler), texCoords).r * material.r;
    vec3 du = dFdx(normalPlusOut.xyz);
    vec3 dv = dFdy(normalPlusOut.xyz);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));

    // compute remaining materialOut properties
    float metallic = texture(sampler2D(metallicTexture, materialSampler), texCoords).g * material.g;
    float ambientOcclusion = texture(sampler2D(ambientOcclusionTexture, materialSampler), texCoords).b * material.b;
    float emission = texture(sampler2D(emissionTexture, materialSampler), texCoords).r * material.a;
    materialOut = vec4(roughness, metallic, ambientOcclusion, emission);

    // compute subsurface scattering properties
    float scatterType = subsurfacePlus.g;
    if (scatterType != 0.0) // not no scatter
    {
        vec4 subdermal = texture(sampler2D(subdermalTexture, materialSampler), texCoords);
        float finenessOffset = subsurfacePlus.r;
        float fineness = texture(sampler2D(finenessTexture, materialSampler), texCoords).r;
        subdermalPlusOut.rgb = subdermal.a == 0.0 ? saturate(albedoOut, 1.5) : subdermal.rgb;
        subdermalPlusOut.a = clamp(fineness + finenessOffset, 0.0, 1.5);
        vec4 scatter = texture(sampler2D(scatterTexture, materialSampler), texCoords);
        if (scatter.a == 0.0)
            scatterPlusOut.rgb =
                scatterType > 0.09 && scatterType < 0.11 ?
                vec3(1, 0.25, 0.04) : // skin scatter
                vec3(0.6, 1, 0.06); // foliage scatter
        else scatterPlusOut.rgb = scatter.rgb;
        scatterPlusOut.a = scatterType;
    }
    else
    {
        subdermalPlusOut = vec4(0.0);
        scatterPlusOut = vec4(0.0);
    }

    // compute clear coat properties
    float clearCoat = texture(sampler2D(clearCoatTexture, materialSampler), texCoords).r * clearCoatPlus.r;
    if (clearCoat > 0.0)
    {
        float clearCoatRoughness = clamp(texture(sampler2D(clearCoatRoughnessTexture, materialSampler), texCoords).r * clearCoatPlus.g, 0.0, 1.0);
        vec3 clearCoatNormal = normalize(toWorld * decodeNormal(texture(sampler2D(clearCoatNormalTexture, materialSampler), texCoords).rg));
        clearCoatPlusOut.r = clearCoat;
        clearCoatPlusOut.g = clearCoatRoughness;
        clearCoatPlusOut.ba = encodeOctahedral(clearCoatNormal);
    }
    else clearCoatPlusOut = vec4(0.0);
}
