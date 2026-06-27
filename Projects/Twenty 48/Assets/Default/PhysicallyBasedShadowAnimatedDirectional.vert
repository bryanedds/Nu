#version 450 core

const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

struct ShadowVert
{
    mat4 viewProjection;
};

layout(set = 0, binding = 0) buffer readonly ShadowVertBlock { ShadowVert shadowVert; };

layout(set = 1, binding = 0) buffer readonly BonesBlock { mat4 bones[BONES_MAX]; };

layout(location = 0) in vec3 position;
layout(location = 3) in vec4 boneIds;
layout(location = 4) in vec4 weights;
layout(location = 5) in mat4 model;

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
    vec4 positionBlended = boneBlended * vec4(position, 1.0);
    vec4 positionOut = model * positionBlended;
    positionOut /= positionOut.w; // NOTE: normalizing by w seems to fix a bug caused by weights not summing to 1.0.
    gl_Position = shadowVert.viewProjection * positionOut;
}
