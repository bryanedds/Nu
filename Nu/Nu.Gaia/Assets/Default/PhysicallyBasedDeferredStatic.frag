#version 450 core

const float GAMMA = 2.2;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as lighting config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as lighting config property.

struct Common
{
    vec3 eyeCenter; // only this used in this shader!
    mat4 viewInverse;
    mat4 projectionInverse;
    float lightCutoffMargin;
    vec3 lightAmbientColor;
    float lightAmbientBrightness;
    float lightAmbientBoostCutoff;
    float lightAmbientBoostScalar;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
    int fogEnabled;
    int fogType;
    float fogStart;
    float fogFinish;
    float fogDensity;
    vec4 fogColor;
    int ssvfEnabled;
    float ssvfIntensity;
    int ssvfSteps;
    float ssvfAsymmetry;
    int ssrrEnabled;
    float ssrrIntensity;
    float ssrrDetail;
    int ssrrRefinementsMax;
    float ssrrRayThickness;
    float ssrrDistanceCutoff;
    float ssrrDistanceCutoffMargin;
    float ssrrEdgeHorizontalMargin;
    float ssrrEdgeVerticalMargin;
    float shadowNear; // NOTE: currently unused.
};

layout(binding = 1) buffer readonly CommonBlock
{
    // TODO: DJL: reform name.
    Common commonData; // common is reserved
};

layout(set = 1, binding = 5) uniform texture2D albedoTexture;
layout(set = 1, binding = 6) uniform texture2D roughnessTexture;
layout(set = 1, binding = 7) uniform texture2D metallicTexture;
layout(set = 1, binding = 8) uniform texture2D ambientOcclusionTexture;
layout(set = 1, binding = 9) uniform texture2D emissionTexture;
layout(set = 1, binding = 10) uniform texture2D normalTexture;
layout(set = 1, binding = 11) uniform texture2D heightTexture;
layout(set = 1, binding = 12) uniform texture2D subdermalTexture;
layout(set = 1, binding = 13) uniform texture2D finenessTexture;
layout(set = 1, binding = 14) uniform texture2D scatterTexture;
layout(set = 1, binding = 15) uniform texture2D clearCoatTexture;
layout(set = 1, binding = 16) uniform texture2D clearCoatRoughnessTexture;
layout(set = 1, binding = 17) uniform texture2D clearCoatNormalTexture;

layout(location = 0) in vec4 positionOut;
layout(location = 1) in vec2 texCoordsOut;
layout(location = 2) in vec3 normalOut;
flat layout(location = 3) in vec4 albedoOut;
flat layout(location = 4) in vec4 materialOut;
flat layout(location = 5) in vec4 heightPlusOut;
flat layout(location = 6) in vec4 subsurfacePlusOut;
flat layout(location = 7) in vec4 clearCoatPlusOut;

layout(location = 0) out float depth;
layout(location = 1) out vec3 albedo;
layout(location = 2) out vec4 material;
layout(location = 3) out vec4 normalPlus;
layout(location = 4) out vec4 subdermalPlus;
layout(location = 5) out vec4 scatterPlus;
layout(location = 6) out vec4 clearCoatPlus;

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
    depth = gl_FragCoord.z;

    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    tangent = normalize(tangent - normal * dot(normal, tangent));
    binormal = cross(normal, tangent);
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * commonData.eyeCenter;
    vec3 positionTangent = toTangent * positionOut.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo
    vec4 albedoSample = texture(albedoTexture, texCoords);
    albedo = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;

    // compute normal and ignore local height maps
    normalPlus.xyz = normalize(toWorld * decodeNormal(texture(normalTexture, texCoords).xy));
    normalPlus.w = heightPlusOut.y;

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = texture(roughnessTexture, texCoords).r * materialOut.r;
    vec3 du = dFdx(normalPlus.xyz);
    vec3 dv = dFdy(normalPlus.xyz);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));

    // compute remaining material properties
    float metallic = texture(metallicTexture, texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).b * materialOut.b;
    float emission = texture(emissionTexture, texCoords).r * materialOut.a;
    material = vec4(roughness, metallic, ambientOcclusion, emission);

    // compute subsurface scattering properties
    float scatterType = subsurfacePlusOut.g;
    if (scatterType != 0.0) // not no scatter
    {
        vec4 subdermal = texture(subdermalTexture, texCoords);
        float finenessOffset = subsurfacePlusOut.r;
        float fineness = texture(finenessTexture, texCoords).r;
        subdermalPlus.rgb = subdermal.a == 0.0 ? saturate(albedo, 1.5) : subdermal.rgb;
        subdermalPlus.a = clamp(fineness + finenessOffset, 0.0, 1.5);
        vec4 scatter = texture(scatterTexture, texCoords);
        if (scatter.a == 0.0)
            scatterPlus.rgb =
                scatterType > 0.09 && scatterType < 0.11 ?
                vec3(1, 0.25, 0.04) : // skin scatter
                vec3(0.6, 1, 0.06); // foliage scatter
        else scatterPlus.rgb = scatter.rgb;
        scatterPlus.a = scatterType;
    }
    else scatterPlus = vec4(0.0);

    // compute clear coat properties
    float clearCoat = texture(clearCoatTexture, texCoords).r * clearCoatPlusOut.r;
    if (clearCoat > 0.0)
    {
        float clearCoatRoughness = clamp(texture(clearCoatRoughnessTexture, texCoords).r * clearCoatPlusOut.g, 0.0, 1.0);
        vec3 clearCoatNormal = normalize(toWorld * decodeNormal(texture(clearCoatNormalTexture, texCoords).rg));
        clearCoatPlus.r = clearCoat;
        clearCoatPlus.g = clearCoatRoughness;
        clearCoatPlus.ba = encodeOctahedral(clearCoatNormal);
    }
    else clearCoatPlus = vec4(0.0);
}
