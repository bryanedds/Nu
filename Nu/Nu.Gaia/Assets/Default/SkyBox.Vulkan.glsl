#shader vertex
#version 460

layout(binding = 0) uniform Uniforms
{
    mat4 View;
    mat4 Projection;
} unis;

layout(location = 0) in vec3 position;

out vec3 texCoordsOut;

void main()
{
    texCoordsOut = position;
    gl_Position = (unis.Projection * unis.View * vec4(position, 1.0)).xyww;
}

#shader fragment
#version 460

layout(binding = 1) uniform Uniforms
{
    vec3 Color;
    float Brightness;
    samplerCube CubeMap;
} unis;

in vec3 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    frag = texture(unis.CubeMap, texCoordsOut) * vec4(unis.Color, 1.0f) * unis.Brightness;
}
