#shader vertex
#version 410

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410

uniform vec2 scale;
uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout (location = 0) out vec2 frag;

void main()
{
    vec2 moments =
        texture(inputTexture, texCoordsOut + vec2(-3.0) * scale).xy * (1.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(-2.0) * scale).xy * (6.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(-1.0) * scale).xy * (15.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(0.0) * scale).xy * (20.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(1.0) * scale).xy * (15.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(2.0) * scale).xy * (6.0 / 64.0) +
        texture(inputTexture, texCoordsOut + vec2(3.0) * scale).xy * (1.0 / 64.0);
    frag = moments;
}
