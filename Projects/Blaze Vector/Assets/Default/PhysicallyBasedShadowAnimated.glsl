#shader vertex
#version 410

const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 bones[BONES_MAX];

layout(location = 0) in vec3 position;
layout(location = 3) in vec4 boneIds;
layout(location = 4) in vec4 weights;
layout(location = 5) in mat4 model;

out vec4 positionOut;
out float depthDirectionalOut;

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
    positionOut = model * positionBlended;
    gl_Position = projection * view * positionOut;
	depthDirectionalOut = gl_Position.z / gl_Position.w;
}

#shader fragment
#version 410

uniform vec3 eyeCenter;
uniform int lightType;
uniform float lightShadowExponent;

layout(location = 0) out vec2 depths;

in vec4 positionOut;
in float depthDirectionalOut;

void main()
{
	switch (lightType)
	{
	case 0: // point light
		depths.x = length(positionOut.xyz - eyeCenter);
		break;
	case 1: // spot light
		depths.x = gl_FragCoord.z;
		depths.y = exp(lightShadowExponent * depths.x);
		break;
	case 2: // directional light
		depths.x = gl_FragCoord.z;
		depths.y = exp(lightShadowExponent * depthDirectionalOut);
		break;
	}
}
