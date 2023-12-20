#shader vertex
#version 410 core

const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;


uniform mat4 view;
uniform mat4 projection;
uniform mat4 bones[BONES_MAX];

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 3) in vec4 boneIds;
layout (location = 4) in vec4 weights;
layout (location = 5) in mat4 model;
layout (location = 10) in vec4 albedo;

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
#extension GL_ARB_bindless_texture : require

layout(bindless_sampler) uniform sampler2D albedoTexture;

in vec2 texCoordsOut;
flat in vec4 albedoOut;

void main()
{
    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut) * albedoOut;
    if (albedoSample.a == 0.0f) discard;
    gl_FragDepth = gl_FragCoord.z;
}
