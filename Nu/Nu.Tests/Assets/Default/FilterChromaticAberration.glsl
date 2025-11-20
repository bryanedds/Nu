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

uniform vec3 channelOffsets;
uniform vec2 focalPoint;
uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec2 direction = texCoordsOut - focalPoint;
    frag = texture(inputTexture, texCoordsOut);
    frag.r = texture(inputTexture, texCoordsOut + direction * vec2(channelOffsets.r)).r;
    frag.g = texture(inputTexture, texCoordsOut + direction * vec2(channelOffsets.g)).g;
    frag.b = texture(inputTexture, texCoordsOut + direction * vec2(channelOffsets.b)).b;
}
