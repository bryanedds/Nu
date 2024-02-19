#shader vertex
#version 410

const int TEX_COORDS_OFFSET_VERTS = 6;
const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

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
uniform mat4 bones[BONES_MAX];

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 2) in vec3 normal;
layout (location = 3) in vec4 boneIds;
layout (location = 4) in vec4 weights;
layout (location = 5) in mat4 model;
layout (location = 9) in vec4 texCoordsOffset;
layout (location = 10) in vec4 albedo;
layout (location = 11) in vec4 material;
layout (location = 12) in vec4 heightPlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;

void main()
{
    // compute blended bone influences
    mat4 boneBlended = mat4(0.0);
    for (int i = 0; i < BONES_INFLUENCE_MAX; ++i)
    {
        int boneId = int(boneIds[i]);
        if (boneId >= 0) boneBlended += bones[boneId] * weights[i];
    }

    // compute blended position and normal
    vec4 positionBlended = boneBlended * vec4(position, 1.0);
    vec4 normalBlended = boneBlended * vec4(normal, 0.0);

    // compute remaining values
    positionOut = model * positionBlended;
    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normalBlended.xyz;
    heightPlusOut = heightPlus;
    gl_Position = projection * view * positionOut;
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require

const float GAMMA = 2.2;

uniform vec3 eyeCenter;
layout (bindless_sampler) uniform sampler2D albedoTexture;
layout (bindless_sampler) uniform sampler2D roughnessTexture;
layout (bindless_sampler) uniform sampler2D metallicTexture;
layout (bindless_sampler) uniform sampler2D emissionTexture;
layout (bindless_sampler) uniform sampler2D ambientOcclusionTexture;
layout (bindless_sampler) uniform sampler2D normalTexture;
layout (bindless_sampler) uniform sampler2D heightTexture;

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;

layout (location = 0) out vec4 position;
layout (location = 1) out vec3 albedo;
layout (location = 2) out vec4 material;
layout (location = 3) out vec4 normalPlus;

void main()
{
    // discard if depth out of range
    float depthCutoff = heightPlusOut.z;
    if (depthCutoff >= 0.0)
    {
        if (gl_FragCoord.z / gl_FragCoord.w > depthCutoff) discard;
    }
    else
    {
        if (gl_FragCoord.z / gl_FragCoord.w <= -depthCutoff) discard;
    }

    // forward position
    position = positionOut;

    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * positionOut.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoords);
    if (albedoSample.a == 0.0f) discard;
    albedo = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;

    // compute material properties
    float roughness = texture(roughnessTexture, texCoords).r * materialOut.r;
    float metallic = texture(metallicTexture, texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).b * materialOut.b;
    float emission = texture(emissionTexture, texCoords).r * materialOut.a;
    material = vec4(roughness, metallic, ambientOcclusion, emission);

    // compute normal and ignore local light maps
    normalPlus.xyz = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
    normalPlus.w = heightPlusOut.y;
}
