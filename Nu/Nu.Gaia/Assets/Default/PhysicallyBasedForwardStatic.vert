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

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;
flat out vec4 subsurfacePlusOut;

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
    gl_Position = viewProjection * positionOut;
}
