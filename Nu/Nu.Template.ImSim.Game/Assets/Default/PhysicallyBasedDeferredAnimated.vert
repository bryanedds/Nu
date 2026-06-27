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

struct Eye
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) buffer readonly EyeBlock { Eye eye; };

layout(set = 2, binding = 0) buffer readonly BonesBlock { mat4 bones[BONES_MAX]; };

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec4 boneIds;
layout(location = 4) in vec4 weights;
layout(location = 5) in mat4 model;
layout(location = 9) in vec4 texCoordsOffset;
layout(location = 10) in vec4 albedo;
layout(location = 11) in vec4 material;
layout(location = 12) in vec4 heightPlus;
layout(location = 13) in vec4 subsurfacePlus;
layout(location = 14) in vec4 clearCoatPlus; // NOTE: z and w are free for additional parameters.

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec2 texCoordsOut;
layout(location = 2) out vec3 normalOut;
flat layout(location = 3) out vec4 albedoOut;
flat layout(location = 4) out vec4 materialOut;
flat layout(location = 5) out vec4 heightPlusOut;
flat layout(location = 6) out vec4 subsurfacePlusOut;
flat layout(location = 7) out vec4 clearCoatPlusOut;

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
    positionOut /= positionOut.w; // NOTE: normalizing by w seems to fix a bug caused by weights not summing to 1.0.
    int texCoordsOffsetIndex = gl_VertexIndex % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normalBlended.xyz;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    clearCoatPlusOut = clearCoatPlus;
    gl_Position = eye.viewProjection * positionOut;
}
