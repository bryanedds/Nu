#shader vertex
#version 410 core

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 6) in mat4 model;

void main()
{
    vec4 positionModel = model * vec4(position, 1.0);
    gl_Position = projection * view * positionModel;
}

#shader fragment
#version 410 core

void main()
{
    gl_FragDepth = gl_FragCoord.z;
}
