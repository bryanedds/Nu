#shader vertex
#version 410 core

const int TexCoordsOffsetVerts = 6;

const vec2 TexCoordsOffsetFilters[TexCoordsOffsetVerts] =
    vec2[TexCoordsOffsetVerts](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TexCoordsOffsetFilters2[TexCoordsOffsetVerts] =
    vec2[TexCoordsOffsetVerts](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 2) in vec3 normal;
layout (location = 3) in mat4 model;
layout (location = 7) in vec4 texCoordsOffset;
layout (location = 8) in vec4 albedo;
layout (location = 9) in vec4 material;
layout (location = 10) in float height;
layout (location = 11) in int invertRoughness;

out vec3 positionOut;
out vec2 texCoordsOut;
out vec4 albedoOut;
out vec4 materialOut;
out vec3 normalOut;
out float heightOut;
flat out int invertRoughnessOut;

void main()
{
    positionOut = vec3(model * vec4(position, 1.0));
    int texCoordsOffsetIndex = gl_VertexID % TexCoordsOffsetVerts;
    vec2 texCoordsOffsetFilter = TexCoordsOffsetFilters[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TexCoordsOffsetFilters2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normal;
    heightOut = height;
    invertRoughnessOut = invertRoughness;
    gl_Position = projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 410 core

const float GAMMA = 2.2;

uniform vec3 eyeCenter;
uniform sampler2D albedoTexture;
uniform sampler2D metallicTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D emissionTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D normalTexture;
uniform sampler2D heightTexture;

in vec3 positionOut;
in vec2 texCoordsOut;
in vec4 albedoOut;
in vec4 materialOut;
in vec3 normalOut;
in float heightOut;
flat in int invertRoughnessOut;

layout (location = 0) out vec3 position;
layout (location = 1) out vec3 albedo;
layout (location = 2) out vec4 material;
layout (location = 3) out vec3 normal;

void main()
{
    // forward position
    position = positionOut;

    // compute spatial converters
    vec3 q1 = dFdx(positionOut);
    vec3 q2 = dFdy(positionOut);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 n = normalize(normalOut);
    vec3 t = normalize(q1 * st2.t - q2 * st1.t);
    vec3 b = -normalize(cross(n, t));
    mat3 toWorld = mat3(t, b, n);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * positionOut;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).r;
    vec2 parallax = toEyeTangent.xy * height * heightOut;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo, discarding on zero alpha
    vec4 albedoSample = texture(albedoTexture, texCoords);
    if (albedoSample.a == 0.0f) discard;
    albedo = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;

    // compute material properties
    float metallic = texture(metallicTexture, texCoords).r * materialOut.r;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).g * materialOut.g;
    vec4 roughnessSample = texture(roughnessTexture, texCoords);
    float roughness = roughnessSample.a == 1.0f ? roughnessSample.b : roughnessSample.a;
    roughness = (invertRoughnessOut == 0 ? roughness : 1.0f - roughness) * materialOut.b;
    float emission = texture(emissionTexture, texCoords).r * materialOut.a;
    material = vec4(metallic, ambientOcclusion, roughness, emission);

    // compute normal
    normal = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
}
