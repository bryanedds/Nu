#shader vertex
#version 410 core

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
layout (location = 12) in float height;

out vec2 texCoordsOut;
flat out vec4 albedoOut;

void main()
{
    // compute blended bone influences
    mat4 boneBlended = mat4(0.0);
    for (int i = 0; i < BONES_INFLUENCE_MAX; ++i)
    {
        int boneId = int(boneIds[i]);
        if (boneId >= 0) boneBlended += bones[boneId] * weights[i];
    }

    // compute output values
    texCoordsOut = texCoords;
    albedoOut = albedo;
    vec4 positionBlended = boneBlended * vec4(position, 1.0);
    gl_Position = projection * view * model * positionBlended;
}

#shader fragment
#version 410 core

uniform sampler2D albedoTexture;

in vec2 texCoordsOut;
flat in vec4 albedoOut;

void main()
{
    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut) * albedoOut;
    if (albedoSample.a == 0.0f) discard;
    gl_FragDepth = gl_FragCoord.z;
}
