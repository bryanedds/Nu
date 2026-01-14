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
uniform int inputIndex;
uniform sampler2DArray inputTextureArray;

in vec2 texCoordsOut;

layout(location = 0) out vec2 frag;

void main()
{
    frag =
        texture(inputTextureArray, vec3(texCoordsOut + vec2(-3.0) * scale, float(inputIndex))).xy * (1.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(-2.0) * scale, float(inputIndex))).xy * (6.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(-1.0) * scale, float(inputIndex))).xy * (15.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(0.0) * scale, float(inputIndex))).xy * (20.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(1.0) * scale, float(inputIndex))).xy * (15.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(2.0) * scale, float(inputIndex))).xy * (6.0 / 64.0) +
        texture(inputTextureArray, vec3(texCoordsOut + vec2(3.0) * scale, float(inputIndex))).xy * (1.0 / 64.0);
}
