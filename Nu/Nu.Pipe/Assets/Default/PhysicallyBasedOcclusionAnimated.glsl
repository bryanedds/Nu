#shader vertex
#version 410

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
    vec4 positionIntermediate = model * positionBlended;
    gl_Position = projection * view * positionIntermediate;
}

#shader fragment
#version 410

void main()
{
    // discard if depth out of range
    float depthCutoff = heightPlusOut.z;
    if (depthCutoff >= 0.0)
    {
        if (gl_FragCoord.z / gl_FragCoord.w > depthCutoff) discard;
    }
    else
    {
        if (gl_FragCoord.z / gl_FragCoord.w <= -depthCutoff) discard;
    }
}
