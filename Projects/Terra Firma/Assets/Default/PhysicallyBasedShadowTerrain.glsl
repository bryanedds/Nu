#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 6) in mat4 model;

out float depthOut;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    gl_Position = projection * view * positionWorld;
	depthOut = gl_Position.z / gl_Position.w;
}

#shader fragment
#version 410

layout (location = 0) out vec2 moments;

in float depthOut;

float linearizeDepth(float z, float n, float f)
{
	return -f * n / (f * z - n * z - f);
}

void main()
{
	float depth = depthOut;//linearizeDepth(depthOut, 0.125, 4096.0);
	float e_cz = exp(100.0 * depth);
	moments.x = e_cz;
}
