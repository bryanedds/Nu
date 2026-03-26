#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

struct VectorPath
{
    mat4 modelViewProjection;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
};

layout(binding = 0) buffer readonly VectorPathBlock
{
    VectorPath vectorPath;
} vectorPath[];

layout (location = 0) in vec2 position;
layout (location = 1) in vec4 color;
layout (location = 0) out vec4 fragColor;

void main()
{
    gl_Position = vectorPath[drawId].vectorPath.modelViewProjection * vec4(position.x, position.y, 0, 1);
    fragColor = color;
}
