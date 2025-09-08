#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable
layout (push_constant) uniform pc { int drawId; };
layout (binding = 2) uniform sampler2D tex[];
layout (binding = 3) uniform a { vec4 color; } color[];
layout (location = 0) in vec2 texCoords;
layout (location = 0) out vec4 frag;
void main()
{
    frag = color[drawId].color * texture(tex[drawId], texCoords);
}
