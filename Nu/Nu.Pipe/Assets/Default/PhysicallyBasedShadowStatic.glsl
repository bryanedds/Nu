#shader vertex
#version 410 core

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 3) in mat4 model;
layout (location = 8) in vec4 albedo;

out vec4 positionOut;
out vec2 texCoordsOut;
flat out vec4 albedoOut;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    texCoordsOut = texCoords;
    albedoOut = albedo;
    gl_Position = projection * view * positionWorld;
}

#shader fragment
#version 410 core
#extension GL_ARB_bindless_texture : require

layout(bindless_sampler) uniform sampler2D albedoTexture;

in vec2 texCoordsOut;
flat in vec4 albedoOut;

void main()
{
    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut) * albedoOut;
    if (albedoSample.a == 0.0f) discard;
}
