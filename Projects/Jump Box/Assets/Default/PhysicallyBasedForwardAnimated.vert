#version 450 core

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

layout(set = 2, binding = 0) uniform BonesUniform { mat4 bones[BONES_MAX]; };

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec2 texCoords2;
layout(location = 3) in vec2 texCoords3;
layout(location = 4) in vec3 normal;
layout(location = 5) in vec3 tangent;
layout(location = 6) in vec4 color;
layout(location = 7) in vec4 boneIds;
layout(location = 8) in vec4 weights;
layout(location = 9) in mat4 model;
layout(location = 13) in vec4 texCoordsOffset;
layout(location = 14) in vec4 albedo;
layout(location = 15) in vec4 material;
layout(location = 16) in vec4 material2;
layout(location = 17) in vec4 subsurfacePlus;
layout(location = 18) in vec4 clearCoatPlus; // NOTE: z and w are reserved for additional engine parameters.
layout(location = 19) in vec4 reservedSettings;
layout(location = 20) in vec4 userDefinedSettings[2];

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec2 texCoordsOut;
layout(location = 2) out vec3 normalOut;
layout(location = 3) out vec3 tangentOut;
layout(location = 4) flat out vec4 albedoOut;
layout(location = 5) flat out vec4 materialOut;
layout(location = 6) flat out vec4 material2Out;
layout(location = 7) flat out vec4 subsurfacePlusOut;

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
    vec4 tangentBlended = boneBlended * vec4(tangent, 0.0);

    // compute remaining values
    positionOut = model * positionBlended;
    positionOut /= positionOut.w; // NOTE: normalizing by w seems to fix a bug caused by weights not summing to 1.0.
    int texCoordsOffsetIndex = gl_VertexIndex % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    normalOut = transpose(inverse(mat3(model))) * normalBlended.xyz;
    tangentOut = transpose(inverse(mat3(model))) * tangentBlended.xyz;
    albedoOut = albedo;
    materialOut = material;
    material2Out = material2;
    subsurfacePlusOut = subsurfacePlus;
    gl_Position = eye.viewProjection * positionOut;
}
