#version 450 core

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

struct Transform
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

layout(binding = 0) uniform TransformBlock
{
    Transform transform;
};

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in mat4 model;
layout(location = 7) in vec4 texCoordsOffset;
layout(location = 8) in vec4 albedo;
layout(location = 9) in vec4 material;
layout(location = 10) in vec4 heightPlus;
layout(location = 11) in vec4 subsurfacePlus;

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec2 texCoordsOut;
layout(location = 2) out vec3 normalOut;
flat layout(location = 3) out vec4 albedoOut;
flat layout(location = 4) out vec4 materialOut;
flat layout(location = 5) out vec4 heightPlusOut;
flat layout(location = 6) out vec4 subsurfacePlusOut;

void main()
{
    positionOut = model * vec4(position, 1.0);
    int texCoordsOffsetIndex = gl_VertexIndex % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normal;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    gl_Position = transform.viewProjection * positionOut;
}
