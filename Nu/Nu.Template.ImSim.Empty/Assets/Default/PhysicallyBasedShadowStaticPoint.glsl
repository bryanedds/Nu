#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;

layout(location = 0) in vec3 position;
layout(location = 3) in mat4 model;

out vec4 positionOut;

void main()
{
	positionOut = model * vec4(position, 1.0);
	gl_Position = projection * view * positionOut;
}

#shader fragment
#version 460 core

uniform vec3 eyeCenter;

layout(location = 0) out float depth;

in vec4 positionOut;

void main()
{
	depth = length(positionOut.xyz - eyeCenter); // linear, world space depth
}
