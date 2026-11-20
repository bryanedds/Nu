#version 450 core

struct ShadowVertStruct
{
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform ShadowVertUniform { ShadowVertStruct shadowVert; };

layout(location = 0) in vec3 position;
layout(location = 7) in mat4 model;

void main()
{
	vec4 positionOut = model * vec4(position, 1.0);
	gl_Position = shadowVert.viewProjection * positionOut;
	gl_Position.z = max(gl_Position.z, 0.0); // pancake casters nearer the light than the near plane so they still cast (orthographic, so w = 1)
}
