#shader vertex
#version 410

const int TEX_COORDS_OFFSET_VERTS = 6;
const int TERRAIN_LAYERS_MAX = 8;

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

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 2) in vec3 normal;
layout (location = 3) in vec3 tint;
layout (location = 4) in vec4 blends[2];
layout (location = 6) in mat4 model;
layout (location = 10) in vec4 texCoordsOffset;
layout (location = 11) in vec4 albedo;
layout (location = 12) in vec4 material;
layout (location = 13) in vec4 heightPlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
out vec4 blendsOut[2];
out vec3 tintOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;

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
    blendsOut[0] = blends[0];
    blendsOut[1] = blends[1];
    tintOut = tint;
    gl_Position = projection * view * positionOut;
}

#shader fragment
#version 410

const float GAMMA = 2.2;
const int TERRAIN_LAYERS_MAX = 8;

uniform vec3 eyeCenter;
uniform int layersCount;
uniform sampler2D albedoTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D roughnessTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D ambientOcclusionTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D normalTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D heightTextures[TERRAIN_LAYERS_MAX];

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
in vec4 blendsOut[2];
in vec3 tintOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;

layout (location = 0) out vec4 position;
layout (location = 1) out vec3 albedo;
layout (location = 2) out vec4 material;
layout (location = 3) out vec4 normalPlus;

void main()
{
    // ensure layers count is in range
    float layersCountCeil = max(min(layersCount, TERRAIN_LAYERS_MAX), 0);

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

    // compute height blend, height, and ignore local light maps
    float heightBlend = 0.0;
    for (int i = 0; i < layersCountCeil; ++i) heightBlend += texture(heightTextures[i], texCoordsOut).r * blendsOut[i/4][i%4];
    float height = heightBlend * heightPlusOut.x;

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * positionOut.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo and material blends
    vec4 albedoBlend = vec4(0.0);
    float roughnessBlend = 0.0;
    float ambientOcclusionBlend = 0.0;
    vec3 normalBlend = vec3(0.0);
    for (int i = 0; i < layersCountCeil; ++i)
    {
        float blend = blendsOut[i/4][i%4];
        albedoBlend += texture(albedoTextures[i], texCoords) * blend;
        vec4 roughness = texture(roughnessTextures[i], texCoords);
        roughnessBlend += (roughness.a == 1.0f ? roughness.r : roughness.a) * blend;
        ambientOcclusionBlend += texture(ambientOcclusionTextures[i], texCoords).b * blend;
        normalBlend += (texture(normalTextures[i], texCoords).xyz * 2.0 - 1.0) * blend;
    }

    // discard on zero alpha
    if (albedoBlend.a == 0.0f) discard;

    // populate albedo, material, and normalPlus
    albedo = pow(albedoBlend.rgb, vec3(GAMMA)) * tintOut * albedoOut.rgb;
    material = vec4(roughnessBlend * materialOut.g, 0.0, ambientOcclusionBlend * materialOut.b, 0.0);
    normalPlus.xyz = normalize(toWorld * normalize(normalBlend));
    normalPlus.w = heightPlusOut.y;
}
