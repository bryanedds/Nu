#shader fragment
#version 410
layout (binding = 2) uniform sampler2D tex;

layout (binding = 3) uniform C {
    vec4 color;
} c;

layout (location = 0) in vec2 texCoords;
layout (location = 0) out vec4 frag;
void main()
{
    frag = c.color * texture(tex, texCoords);
}
