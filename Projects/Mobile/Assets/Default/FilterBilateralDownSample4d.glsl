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

uniform sampler2D colorTexture;
uniform sampler2D depthTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 color;
layout(location = 1) out float depth;

void main()
{
    color = texture(colorTexture, texCoordsOut);
    depth = texture(depthTexture, texCoordsOut).x;
}
