#shader vertex
#version 410 core

const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 bones[BONES_MAX];

layout (location = 0) in vec3 position;
layout (location = 3) in vec4 boneIds;
layout (location = 4) in vec4 weights;
layout (location = 5) in mat4 model;

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
    gl_Position = projection * view * model * positionBlended;
}

#shader fragment
#version 410 core

void main()
{
    gl_FragDepth = gl_FragCoord.z;
}
