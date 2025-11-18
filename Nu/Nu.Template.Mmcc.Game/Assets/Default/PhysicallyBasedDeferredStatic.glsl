#shader vertex
#version 460 core

const int TEX_COORDS_OFFSET_VERTS = 6;

const vec2 TEX_COORDS_OFFSET_FILTERS[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TEX_COORDS_OFFSET_FILTERS_2[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in mat4 model;
layout(location = 7) in vec4 texCoordsOffset;
layout(location = 8) in vec4 albedo;
layout(location = 9) in vec4 material;
layout(location = 10) in vec4 heightPlus;
layout(location = 11) in vec4 subsurfacePlus;
layout(location = 12) in vec4 clearCoatPlus; // NOTE: z and w are free for additional parameters.

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;
flat out vec4 subsurfacePlusOut;
flat out vec4 clearCoatPlusOut;

void main()
{
    positionOut = model * vec4(position, 1.0);
    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normal;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    clearCoatPlusOut = clearCoatPlus;
    gl_Position = viewProjection * positionOut;
}

#shader fragment
#version 460 core

const float GAMMA = 2.2;
const float SAA_VARIANCE = 0.1; // TODO: consider exposing as lighting config property.
const float SAA_THRESHOLD = 0.1; // TODO: consider exposing as lighting config property.

uniform vec3 eyeCenter;
uniform sampler2D albedoTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D metallicTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D emissionTexture;
uniform sampler2D normalTexture;
uniform sampler2D heightTexture;
uniform sampler2D subdermalTexture;
uniform sampler2D finenessTexture;
uniform sampler2D scatterTexture;
uniform sampler2D clearCoatTexture;
uniform sampler2D clearCoatRoughnessTexture;
uniform sampler2D clearCoatNormalTexture;

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;
flat in vec4 subsurfacePlusOut;
flat in vec4 clearCoatPlusOut;

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
    vec3 eyeCenterTangent = toTangent * eyeCenter;
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
    vec4 subdermal = texture(subdermalTexture, texCoords);
    float fineness = texture(finenessTexture, texCoords).r;
    float finenessOffset = subsurfacePlusOut.r;
    subdermalPlus.rgb = subdermal.a == 0.0 ? saturate(albedo, 1.5) : subdermal.rgb;
    subdermalPlus.a = clamp(fineness + finenessOffset, 0.0, 1.5);
    vec4 scatter = texture(scatterTexture, texCoords);
    float scatterType = subsurfacePlusOut.g;
    if (scatter.a == 0.0)
        scatterPlus.rgb =
            scatterType > 0.09 && scatterType < 0.11 ?
            vec3(1, 0.25, 0.04) : // skin scatter
            vec3(0.6, 1, 0.06); // foliage scatter
    else scatterPlus.rgb = scatter.rgb;
    scatterPlus.a = scatterType;

    // compute clear coat properties
    clearCoatPlus.r = texture(clearCoatTexture, texCoords).r * clearCoatPlusOut.r;
    if (clearCoatPlus.r > 0.0)
    {
        float clearCoatRoughness = texture(clearCoatRoughnessTexture, texCoords).r * max(0.0, clearCoatPlusOut.g);
        vec3 clearCoatNormal = normalize(toWorld * decodeNormal(texture(clearCoatNormalTexture, texCoords).rg));
        clearCoatPlus.g = clearCoatRoughness;
        clearCoatPlus.ba = encodeOctahedral(clearCoatNormal);
    }
}
