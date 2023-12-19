#shader vertex
#version 410 core

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 3) in mat4 model;

out vec4 positionOut;
out vec2 texCoordsOut;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    texCoordsOut = texCoords;
    gl_Position = projection * view * positionWorld;
}

#shader fragment
#version 410 core

uniform vec4 albedo;
uniform sampler2D albedoTexture;

in vec2 texCoordsOut;

void main()
{
    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut) * albedo;
    if (albedoSample.a == 0.0f) discard;
}
