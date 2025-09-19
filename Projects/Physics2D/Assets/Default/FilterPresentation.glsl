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

const float GAMMA = 2.2;

uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    // apply tone mapping and gamma correction
    vec3 color = texture(inputTexture, texCoordsOut, 0).xyz;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    frag = vec4(color, 1.0);
}
