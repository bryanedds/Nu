#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;

layout(location = 0) in vec3 position;
layout(location = 6) in mat4 model;

void main()
{
	vec4 positionOut = model * vec4(position, 1.0);
	gl_Position = projection * view * positionOut;
}

#shader fragment
#version 460 core

uniform float lightShadowExponent;

layout(location = 0) out vec2 depths;

void main()
{
	depths.x = gl_FragCoord.z; // non-linear, screen space depth
	depths.y = exp(lightShadowExponent * depths.x);
}
