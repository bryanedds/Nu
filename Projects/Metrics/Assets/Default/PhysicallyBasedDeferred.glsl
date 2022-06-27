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

const int LIGHTS_MAX = 4;

uniform vec3 eyePosition;
uniform sampler2D albedoTexture;
uniform sampler2D metalnessTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D normalTexture;
uniform sampler2D ambientOcclusionTexture;
uniform vec3 lightPositions[LIGHTS_MAX]; // NOTE: unused, but kept in to preserve interface.
uniform vec3 lightColors[LIGHTS_MAX]; // NOTE: unused, but kept in to preserve interface.

in vec3 positionOut;
in vec3 normalOut;
in vec2 texCoordsOut;

layout (location = 0) out vec3 position;
layout (location = 1) out vec3 normal;
layout (location = 2) out vec3 albedo;
layout (location = 3) out vec4 material;

vec3 getNormal()
{
    vec3 tangentNormal = texture(normalTexture, texCoordsOut).xyz * 2.0 - 1.0;
    vec3 q1 = dFdx(positionOut);
    vec3 q2 = dFdy(positionOut);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1*st2.t - q2*st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 tbn = mat3(tangent, binormal, normal);
    return normalize(tbn * tangentNormal);
}

void main()
{
    // forward position
    position = positionOut;

    // compute normal
    normal = getNormal();

    // compute albedo
    albedo = pow(texture(albedoTexture, texCoordsOut).rgb, vec3(2.2));

    // compute material properties
    float metalness = texture(metalnessTexture, texCoordsOut).r;
    float roughness = texture(roughnessTexture, texCoordsOut).r;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoordsOut).r;
    float uniformPreservationHack = eyePosition.x + lightPositions[0].x + lightColors[0].r; // HACK: utilizes eyePosition to keep the uniform from being optimized away.
    material = vec4(metalness, roughness, ambientOcclusion, uniformPreservationHack);
}
