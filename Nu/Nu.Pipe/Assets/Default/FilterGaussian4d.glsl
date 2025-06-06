#shader vertex
#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 460 core

uniform vec2 scale;
uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    frag =
        texture(inputTexture, texCoordsOut + vec2(-1.0) * scale) * (1.0 / 8.0) +
        texture(inputTexture, texCoordsOut + vec2(0.0) * scale) * (6.0 / 8.0) +
        texture(inputTexture, texCoordsOut + vec2(1.0) * scale) * (1.0 / 8.0);
}
