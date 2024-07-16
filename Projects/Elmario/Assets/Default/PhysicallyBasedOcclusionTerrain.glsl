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

void main()
{
	// nothing to do since terrain doesn't discard when out of range
}
