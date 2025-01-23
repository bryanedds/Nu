#shader fragment
#version 410
uniform sampler2D tex;
in vec2 texCoords;
in vec4 color;
layout (location = 0) out vec4 frag;
void main()
{
    frag = color * texture(tex, texCoords);
}
