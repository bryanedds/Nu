#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 6) in mat4 model;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    gl_Position = projection * view * positionWorld;
}

#shader fragment
#version 410

layout (location = 0) out vec2 moments;

void main()
{
	float depth = gl_FragCoord.z;
	moments.x = depth;
	float dx = dFdx(depth);
	float dy = dFdy(depth);
	moments.y = depth * depth + 0.25 * (dx * dx + dy * dy);
}
