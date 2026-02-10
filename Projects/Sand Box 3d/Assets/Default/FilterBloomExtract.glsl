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

uniform float threshold;
uniform sampler2D colorTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec3 color = texture(colorTexture, texCoordsOut).rgb;
    float brightness = dot(color, vec3(0.2126, 0.7152, 0.0722));
    frag = vec4(brightness >= threshold ? color : vec3(0.0), 1.0);
}
