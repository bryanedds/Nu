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
layout (location = 9) in vec3 material;

out vec3 positionOut;
out vec2 texCoordsOut;
out vec4 albedoOut;
out vec3 materialOut;
out vec3 normalOut;

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
    gl_Position = projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 410 core

const float GAMMA = 2.2;

uniform sampler2D albedoTexture;
uniform sampler2D metalnessTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D normalTexture;

in vec3 positionOut;
in vec2 texCoordsOut;
in vec4 albedoOut;
in vec3 materialOut;
in vec3 normalOut;

layout (location = 0) out vec3 position;
layout (location = 1) out vec3 albedo;
layout (location = 2) out vec3 material;
layout (location = 3) out vec3 normal;

vec3 getNormal()
{
    vec3 tangentNormal = texture(normalTexture, texCoordsOut).xyz * 2.0 - 1.0;
    vec3 q1 = dFdx(positionOut);
    vec3 q2 = dFdy(positionOut);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 tbn = mat3(tangent, binormal, normal);
    return normalize(tbn * tangentNormal);
}

void main()
{
    // forward position
    position = positionOut;

    // compute albedo without alpha
    vec4 albedoSample = texture(albedoTexture, texCoordsOut);
    albedo = pow(albedoSample.rgb * albedoOut.rgb, vec3(GAMMA));

    // discard fragment if alpha is zero
    float alpha = albedoSample.a * albedoOut.a;
    if (alpha == 0.0f) discard;

    // compute material properties
    float metalness = texture(metalnessTexture, texCoordsOut).r * materialOut.r;
    float roughness = texture(roughnessTexture, texCoordsOut).r * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoordsOut).r * materialOut.b;
    material = vec3(metalness, roughness, ambientOcclusion);

    // compute normal
    normal = getNormal();
}
