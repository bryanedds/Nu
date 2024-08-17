#shader vertex
#version 460

layout(binding = 0) uniform UniformBuffer
{
    mat4 view;
    mat4 projection;
} ub;

layout(location = 0) in vec3 position;

out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (ub.projection * ub.view * vec4(position, 1.0)).xyww;
}

#shader fragment
#version 460

layout(binding = 1) uniform UniformBuffer
{
    vec3 color;
    float brightness;
    samplerCube cubeMap;
} ub;

in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    frag = texture(ub.cubeMap, texCoordsOut) * vec4(ub.color, 1.0f) * ub.brightness;
}
