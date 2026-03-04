#version 450 core
layout (location = 0) in vec4 fragColor;
layout (location = 0) out vec4 frag;

void main()
{
    frag = fragColor;
}
