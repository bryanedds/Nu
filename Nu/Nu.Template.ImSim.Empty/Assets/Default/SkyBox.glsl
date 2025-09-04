#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;

layout(location = 0) in vec3 position;

out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (viewProjection * vec4(position, 1.0)).xyww;
}

#shader fragment
#version 460 core

uniform vec3 color;
uniform float brightness;
uniform samplerCube cubeMap;

in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    vec4 color4 = vec4(color, 1.0);
    frag = texture(cubeMap, texCoordsOut) * color4 * brightness;
}
