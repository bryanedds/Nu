#version 450 core

struct ShadowVertStruct
{
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform ShadowVertUniform { ShadowVertStruct shadowVert; };

layout(location = 0) in vec3 position;
layout(location = 3) in mat4 model;

layout(location = 0) out vec4 positionOut;

void main()
{
	positionOut = model * vec4(position, 1.0);
	gl_Position = shadowVert.viewProjection * positionOut;
}
