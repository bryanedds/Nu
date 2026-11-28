#version 450 core

// Foliage fragment shader, derived from PhysicallyBasedDeferredStaticClipped.frag. Unlike the original,
// vertex color tints albedo and its brightness scales ambient occlusion, and surfaces approach full roughness as they
// near the draw distance. For speed, it samples only the albedo, normal, and roughness textures, taking the engine's
// default values for the rest (no metallic, emission, or clear coat, full ambient occlusion and fineness, and the
// default subdermal and scatter colors), so foliage materials that need those textures aren't supported.
// NOTE: PhysicallyBasedDeferredFoliageClipped.frag is this shader with an alpha test, for foliage whose shape comes
// from its albedo alpha (such as a tree's leaf cards). Lacking the alpha test lets the GPU test and write depth before
// shading, so prefer this shader where the geometry gives the shape. Keep the two in sync.

const float GAMMA = 2.2;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as lighting config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as lighting config property.

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };

layout(set = 1, binding = 0) uniform texture2D albedoTexture;
layout(set = 1, binding = 1) uniform texture2D roughnessTexture;
layout(set = 1, binding = 2) uniform texture2D metallicTexture;
layout(set = 1, binding = 3) uniform texture2D ambientOcclusionTexture;
layout(set = 1, binding = 4) uniform texture2D emissionTexture;
layout(set = 1, binding = 5) uniform texture2D normalTexture;
layout(set = 1, binding = 6) uniform texture2D subdermalTexture;
layout(set = 1, binding = 7) uniform texture2D finenessTexture;
layout(set = 1, binding = 8) uniform texture2D scatterTexture;
layout(set = 1, binding = 9) uniform texture2D sharedImage0Texture;
layout(set = 1, binding = 10) uniform texture2D sharedImage1Texture;
layout(set = 1, binding = 11) uniform texture2D sharedImage2Texture;
layout(set = 1, binding = 12) uniform texture2D userDefinedImage0Texture;
layout(set = 1, binding = 13) uniform texture2D userDefinedImage1Texture;
layout(set = 1, binding = 14) uniform texture2D userDefinedImage2Texture;

layout(set = 3, binding = 0) uniform sampler unfilteredSampler;
layout(set = 3, binding = 1) uniform sampler filteredSampler;
layout(set = 3, binding = 2) uniform sampler materialRepeatSampler;
layout(set = 3, binding = 3) uniform sampler materialRepeatMirrorSampler;
layout(set = 3, binding = 4) uniform sampler materialClampToEdge;
layout(set = 3, binding = 5) uniform sampler materialClampToBorder;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 tangent;
layout(location = 4) flat in vec4 albedo;
layout(location = 5) flat in vec4 material;
layout(location = 6) flat in vec4 material2;
layout(location = 7) flat in vec4 subsurfacePlus;
layout(location = 8) flat in vec4 sharedSettings[2];
layout(location = 10) flat in vec4 userDefinedSettings[2];
layout(location = 12) in vec3 tint;
layout(location = 13) in float roughen;

layout(location = 0) out float depthOut;
layout(location = 1) out vec3 albedoOut;
layout(location = 2) out vec4 materialOut;
layout(location = 3) out vec4 normalPlusOut;
layout(location = 4) out vec4 subdermalPlusOut;
layout(location = 5) out vec4 scatterPlusOut;
layout(location = 6) out vec4 clearCoatPlusOut;
layout(location = 7) out vec4 userDefinedBufferOut;

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
    depthOut = -(eye.view * position).z;

    // compute spatial converters
    vec3 normal = normalize(normal);
    vec3 tangent = normalize(tangent);
    vec3 binormal = -normalize(cross(normal, tangent));
    if (!gl_FrontFacing)
    {
        // a back face is the other side of the same surface, so it faces the opposite way
        normal = -normal;
        tangent = -tangent;
        binormal = -binormal;
    }
    mat3 toWorld = mat3(tangent, binormal, normal);

    // compute albedo
    vec4 albedoSample = texture(sampler2D(albedoTexture, materialRepeatSampler), texCoords);
    albedoOut = pow(albedoSample.rgb, vec3(GAMMA)) * albedo.rgb * tint;

    // compute normal and ignore local light maps
    normalPlusOut.xyz = normalize(toWorld * decodeNormal(texture(sampler2D(normalTexture, materialRepeatSampler), texCoords).xy));
    normalPlusOut.w = material2.x;

    // compute roughness with specular anti-aliasing (Tokuyoshi & Kaplanyan 2019)
    // NOTE: the SAA algo also includes derivative scalars that are currently not utilized here due to lack of need -
    // https://github.com/google/filament/blob/d7b44a2585a7ce19615dbe226501acc3fe3f0c16/shaders/src/surface_shading_lit.fs#L41-L42
    float roughness = texture(sampler2D(roughnessTexture, materialRepeatSampler), texCoords).r * material.r;
    vec3 du = dFdx(normalPlusOut.xyz);
    vec3 dv = dFdy(normalPlusOut.xyz);
    float variance = SAA_VARIANCE * (dot(du, du) + dot(dv, dv));
    float roughnessKernal = min(2.0 * variance, SAA_THRESHOLD);
    float roughnessPerceptual = roughness * roughness;
    float roughnessPerceptualSquared = clamp(roughnessPerceptual * roughnessPerceptual + roughnessKernal, 0.0, 1.0);
    roughness = sqrt(sqrt(roughnessPerceptualSquared));
    roughness = mix(roughness, 1.0, roughen);

    // compute remaining material properties
    float ambientOcclusion = material.b * (tint.r + tint.g + tint.b) / 3.0;
    materialOut = vec4(roughness, 0.0, ambientOcclusion, 0.0);

    // compute subsurface scattering properties
    float scatterType = subsurfacePlus.g;
    if (scatterType != 0.0) // not no scatter
    {
        float finenessOffset = subsurfacePlus.r;
        subdermalPlusOut.rgb = saturate(albedoOut, 1.5);
        subdermalPlusOut.a = clamp(1.0 + finenessOffset, 0.0, 1.5);
        scatterPlusOut.rgb =
            scatterType > 0.09 && scatterType < 0.11 ?
            vec3(1, 0.25, 0.04) : // skin scatter
            vec3(0.6, 1, 0.06); // foliage scatter
        scatterPlusOut.a = scatterType;
    }
    else
    {
        subdermalPlusOut = vec4(0.0);
        scatterPlusOut = vec4(0.0);
    }

    // write zero to clear coat buffer
    clearCoatPlusOut = vec4(0.0);

    // write zero to user-defined buffer
    userDefinedBufferOut = vec4(0.0);
}
