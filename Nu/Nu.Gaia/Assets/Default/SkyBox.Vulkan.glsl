#shader vertex
#version 460

layout (binding = 0) uniform UniformBufferVertex
{
    mat4 View;
    mat4 Projection;
} ubv;

layout (location = 0) in vec3 position;

out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (ubv.Projection * ubv.View * vec4(position, 1.0)).xyww;
}

#shader fragment
#version 460

layout (binding = 1) uniform UniformBufferFragment
{
    vec3 Color;
    float Brightness;
    samplerCube CubeMap;
} ubf;

in vec3 texCoordsOut;

layout (location = 0) out vec4 frag;

void main()
{
    frag = texture(ubf.CubeMap, texCoordsOut) * vec4(ubf.Color, 1.0f) * ubf.Brightness;
}
