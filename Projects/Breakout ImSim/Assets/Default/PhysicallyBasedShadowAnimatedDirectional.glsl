#shader vertex
#version 460 core

const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;
uniform mat4 bones[BONES_MAX];

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
    gl_Position = viewProjection * positionOut;
}

#shader fragment
#version 460 core

uniform float lightShadowExponent;

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // linear, screen space depth
	depths.y = exp(lightShadowExponent * depths.x);
}
