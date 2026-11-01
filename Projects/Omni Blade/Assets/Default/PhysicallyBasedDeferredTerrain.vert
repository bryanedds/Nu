#version 450 core

const int TEX_COORDS_OFFSET_VERTS = 6;
const int TERRAIN_LAYERS_MAX = 6;

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

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct TerrainStruct
{
    int layersCount;
    int lightShadowSamples;
    float lightShadowBias;
    float lightShadowSampleScalar;
    float lightShadowExponent;
    float lightShadowDensity;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };
layout(set = 0, binding = 1) uniform TerrainUniform { TerrainStruct terrain; };

layout(set = 1, binding = 4) uniform texture2D userDefinedImageTextures[TERRAIN_LAYERS_MAX];
layout(set = 1, binding = 5) uniform texture2D userDefinedImage0Texture;
layout(set = 1, binding = 6) uniform texture2D userDefinedImage1Texture;

layout(set = 2, binding = 0) uniform sampler unfilteredSampler;
layout(set = 2, binding = 1) uniform sampler filteredSampler;
layout(set = 2, binding = 2) uniform sampler materialRepeatSampler;
layout(set = 2, binding = 3) uniform sampler materialRepeatMirrorSampler;
layout(set = 2, binding = 4) uniform sampler materialClampToEdge;
layout(set = 2, binding = 5) uniform sampler materialClampToBorder;

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 tint;
layout(location = 4) in vec4 blends[2];
layout(location = 6) in mat4 model;
layout(location = 10) in vec4 texCoordsOffset;
layout(location = 11) in vec4 albedo;
layout(location = 12) in vec4 material;
layout(location = 13) in vec4 material2;
layout(location = 16) in vec4 reservedSettings;
layout(location = 17) in vec4 userDefinedSettings[2];

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec2 texCoordsOut;
layout(location = 2) out vec3 normalOut;
layout(location = 3) out vec4 blendsOut[2];
layout(location = 5) out vec3 tintOut;
layout(location = 6) flat out vec4 albedoOut;
layout(location = 7) flat out vec4 materialOut;
layout(location = 8) flat out vec4 material2Out;
layout(location = 9) flat out vec4 userDefinedSettingsOut[2];

void main()
{
    positionOut = model * vec4(position, 1.0);
    int texCoordsOffsetIndex = gl_VertexIndex % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    normalOut = transpose(inverse(mat3(model))) * normal;
    blendsOut = blends;
    tintOut = tint;
    albedoOut = albedo;
    materialOut = material;
    material2Out = material2;
    userDefinedSettingsOut = userDefinedSettings;
    gl_Position = eye.viewProjection * positionOut;
}
