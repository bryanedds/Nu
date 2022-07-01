#shader vertex
#version 410 core

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;
layout (location = 3) in mat4 model;

out vec3 positionOut;
out vec3 normalOut;
out vec2 texCoordsOut;

void main()
{
    positionOut = vec3(model * vec4(position, 1.0));
    normalOut = transpose(inverse(mat3(model))) * normal;
    texCoordsOut = texCoords;
    gl_Position = projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 410 core

const float GAMMA_SQRT = 1.5;
const float TONE = GAMMA_SQRT * GAMMA_SQRT;

uniform sampler2D albedoTexture;
uniform sampler2D metalnessTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D normalTexture;
uniform sampler2D ambientOcclusionTexture;

in vec3 positionOut;
in vec3 normalOut;
in vec2 texCoordsOut;

layout (location = 0) out vec3 position;
layout (location = 1) out vec3 normal;
layout (location = 2) out vec3 albedo;
layout (location = 3) out vec3 material;

void main()
{
    // forward position
    position = positionOut;

    // forward normal
    normal = normalize(normalOut);

    // compute albedo
    albedo = pow(texture(albedoTexture, texCoordsOut).rgb, vec3(TONE));

    // compute material properties
    float metalness = texture(metalnessTexture, texCoordsOut).r;
    float roughness = texture(roughnessTexture, texCoordsOut).r;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoordsOut).r;
    material = vec3(metalness, roughness, ambientOcclusion);
}
