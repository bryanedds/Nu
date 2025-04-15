#shader vertex
#version 410

const int TEX_COORDS_OFFSET_VERTS = 6;

const vec2 TEX_COORDS_OFFSET_FILTERS[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TEX_COORDS_OFFSET_FILTERS_2[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in mat4 model;
layout(location = 7) in vec4 texCoordsOffset;
layout(location = 8) in vec4 albedo;
layout(location = 9) in vec4 material;
layout(location = 10) in vec4 heightPlus;
layout(location = 11) in vec4 subsurfacePlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;
flat out vec4 subsurfacePlusOut;

void main()
{
    positionOut = model * vec4(position, 1.0);
    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normal;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    gl_Position = projection * view * positionOut;
}

#shader fragment
#version 410

const float GAMMA = 2.2;

uniform vec3 eyeCenter;
uniform sampler2D albedoTexture;
uniform sampler2D roughnessTexture;
uniform sampler2D metallicTexture;
uniform sampler2D ambientOcclusionTexture;
uniform sampler2D emissionTexture;
uniform sampler2D normalTexture;
uniform sampler2D heightTexture;
uniform sampler2D subdermalTexture;
uniform sampler2D finenessTexture;
uniform sampler2D scatterTexture;

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;
flat in vec4 subsurfacePlusOut;

layout(location = 0) out vec4 position;
layout(location = 1) out vec3 albedo;
layout(location = 2) out vec4 material;
layout(location = 3) out vec4 normalPlus;
layout(location = 4) out vec4 subdermalPlus;
layout(location = 5) out vec4 scatterPlus;

// NOTE: algorithm from Chapter 16 of OpenGL Shading Language
vec3 saturate(vec3 rgb, float adjustment)
{
    const vec3 w = vec3(0.2125, 0.7154, 0.0721);
    vec3 intensity = vec3(dot(rgb, w));
    return mix(intensity, rgb, adjustment);
}

void main()
{
    // discard when depth out of range
    float depthCutoff = heightPlusOut.z;
    float depth = gl_FragCoord.z / gl_FragCoord.w;
    if (depthCutoff >= 0.0) { if (depth > depthCutoff) discard; }
    else if (depth <= -depthCutoff) discard;

    // forward position, marking w for written
    position.xyz = positionOut.xyz;
    position.w = 1.0;

    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * positionOut.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo, discading if even partly transparent
    vec4 albedoSample = texture(albedoTexture, texCoords);
    if (albedoSample.w < 0.5) discard;
    albedo = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;

    // compute material properties
    float roughness = texture(roughnessTexture, texCoords).r * materialOut.r;
    float metallic = texture(metallicTexture, texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).b * materialOut.b;
    float emission = texture(emissionTexture, texCoords).r * materialOut.a;
    material = vec4(roughness, metallic, ambientOcclusion, emission);

    // compute normal and ignore local height maps
    normalPlus.xyz = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
    normalPlus.w = heightPlusOut.y;

    // compute subsurface scattering properties
    vec4 subdermal = texture(subdermalTexture, texCoords);
    float fineness = texture(finenessTexture, texCoords).r;
    float finenessOffset = subsurfacePlusOut.r;
    subdermalPlus.rgb = subdermal.a == 0.0 ? saturate(albedo, 1.5) : subdermal.rgb;
    subdermalPlus.a = clamp(fineness + finenessOffset, 0.0, 1.5);
    vec4 scatter = texture(scatterTexture, texCoords);
    float scatterType = subsurfacePlusOut.g;
    if (scatter.a == 0.0)
        scatterPlus.rgb =
            scatterType == 1.0 ?
            vec3(1, 0.25, 0.04) : // skin scatter
            vec3(0.6, 1, 0.06); // foliage scatter
    else scatterPlus.rgb = scatter.rgb;
    scatterPlus.a = scatterType;
}
