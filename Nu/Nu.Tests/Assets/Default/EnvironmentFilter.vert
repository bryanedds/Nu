#version 450 core
#extension GL_EXT_nonuniform_qualifier : enable

struct Transform
{
    mat4 view;
    mat4 projection;
    mat4 viewProjection;
};

layout(push_constant) uniform PushConstant
{
    int drawId;
};

layout(binding = 0) buffer readonly TransformBlock
{
    Transform transform;
} transforms[];

layout(location = 0) in vec3 position;

layout(location = 0) out vec3 positionOut;

void main()
{
    positionOut = position;
    gl_Position = transforms[drawId].transform.viewProjection * vec4(positionOut, 1.0);
}
