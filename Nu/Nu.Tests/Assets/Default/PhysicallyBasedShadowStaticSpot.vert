#version 450 core

struct ShadowVert
{
    mat4 viewProjection;
};

layout(set = 0, binding = 0) buffer readonly ShadowVertBlock { ShadowVert shadowVert; };

layout(location = 0) in vec3 position;
layout(location = 3) in mat4 model;

void main()
{
	vec4 positionOut = model * vec4(position, 1.0);
	gl_Position = shadowVert.viewProjection * positionOut;
}
