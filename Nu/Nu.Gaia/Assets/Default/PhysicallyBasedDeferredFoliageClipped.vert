#version 450 core

// Foliage vertex shader, derived from PhysicallyBasedDeferredStaticClipped.vert. It sways each instance with two
// layers of wind noise read from sharedImage0Texture (R = gust, G = flutter), collapses each instance toward its root
// as it nears the draw distance, and has the fragment shader roughen each instance with distance, reaching full
// roughness at the roughen end, driven by sharedSettings -
//   [0].x  = wind cycle phase in [0, 1)
//   [0].y  = wind strength
//   [0].zw = gust scroll in noise tiles per wind cycle (whole numbers so that the cycle wraps seamlessly)
//   [1].x  = distance at which instances reach full roughness
//   [1].y  = distance at which instances begin collapsing
//   [1].z  = draw distance, at which instances are fully collapsed
//   [1].w  = flutter strength
// The model is expected to stand on y = 0 with a height of about 1, so local y doubles as the bend weight. Any
// shading particular to a model, such as normals bent for soft lighting or occlusion toward its roots or interior,
// belongs in its normals and vertex colors, where vertex color multiplies albedo and its brightness multiplies
// ambient occlusion (a model without vertex colors is left unshaded).

// NOTE: PhysicallyBasedDeferredFoliageClipped.vert must be an exact copy of this shader.

const float TAU = 6.28318530718;
const float GUST_BACKSWING = 0.3; // gust noise below this bends instances slightly against the wind
const float GUST_SCALE = 1.0 / 24.0; // gust noise tiles per meter (must match Constants.Foliage.GustTileSize)
const float FLUTTER_SCALE = 0.25; // flutter noise tiles per meter
const float FLUTTER_OSCILLATIONS = 90.0; // flutter oscillations per wind cycle (a whole number for a seamless wrap)

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

struct Eye3dStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform Eye3dUniform { Eye3dStruct eye; };

layout(set = 1, binding = 9) uniform texture2D sharedImage0Texture;
layout(set = 1, binding = 10) uniform texture2D sharedImage1Texture;
layout(set = 1, binding = 11) uniform texture2D sharedImage2Texture;
layout(set = 1, binding = 12) uniform texture2D userDefinedImage0Texture;
layout(set = 1, binding = 13) uniform texture2D userDefinedImage1Texture;
layout(set = 1, binding = 14) uniform texture2D userDefinedImage2Texture;

layout(set = 3, binding = 0) uniform sampler unfilteredSampler;
layout(set = 3, binding = 1) uniform sampler filteredSampler;
layout(set = 3, binding = 2) uniform sampler materialRepeatSampler;
layout(set = 3, binding = 3) uniform sampler materialRepeatMirrorSampler;
layout(set = 3, binding = 4) uniform sampler materialClampToEdge;
layout(set = 3, binding = 5) uniform sampler materialClampToBorder;

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec2 texCoords2;
layout(location = 3) in vec2 texCoords3;
layout(location = 4) in vec3 normal;
layout(location = 5) in vec3 tangent;
layout(location = 6) in vec4 color;
layout(location = 7) in mat4 model;
layout(location = 11) in vec4 texCoordsOffset;
layout(location = 12) in vec4 albedo;
layout(location = 13) in vec4 material;
layout(location = 14) in vec4 material2;
layout(location = 15) in vec4 subsurfacePlus;
layout(location = 16) in vec4 sharedSettings[2];
layout(location = 18) in vec4 userDefinedSettings[2];

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec2 texCoordsOut;
layout(location = 2) out vec3 normalOut;
layout(location = 3) out vec3 tangentOut;
layout(location = 4) flat out vec4 albedoOut;
layout(location = 5) flat out vec4 materialOut;
layout(location = 6) flat out vec4 material2Out;
layout(location = 7) flat out vec4 subsurfacePlusOut;
layout(location = 8) flat out vec4 sharedSettingsOut[2];
layout(location = 10) flat out vec4 userDefinedSettingsOut[2];
layout(location = 12) out vec3 tintOut;
layout(location = 13) out float roughenOut;

void main()
{
    // compute instance root and axis
    vec4 root = model * vec4(0.0, 0.0, 0.0, 1.0);
    vec3 axis = mat3(model) * vec3(0.0, 1.0, 0.0);
    float instanceHeight = length(axis);

    // extract wind settings
    float phase = sharedSettings[0].x;
    float strength = sharedSettings[0].y;
    vec2 gustScroll = sharedSettings[0].zw;
    float roughenEnd = sharedSettings[1].x;
    float collapseBegin = sharedSettings[1].y;
    float drawDistance = sharedSettings[1].z;
    float flutterStrength = sharedSettings[1].w;
    vec2 windDirection = dot(gustScroll, gustScroll) > 0.0 ? normalize(gustScroll) : vec2(1.0, 0.0);
    vec2 windSide = vec2(-windDirection.y, windDirection.x);

    // compute gust, sampled at the root so that the whole instance sways together as gusts travel down-wind
    vec2 gustCoords = root.xz * GUST_SCALE - gustScroll * phase;
    float gust = textureLod(sampler2D(sharedImage0Texture, materialRepeatSampler), gustCoords, 0.0).r;
    float gustBend = (gust - GUST_BACKSWING) * strength;

    // compute flutter, sampled per vertex so that the parts of an instance fall out of step
    positionOut = model * vec4(position, 1.0);
    float instanceHash = fract(sin(dot(root.xz, vec2(12.9898, 78.233))) * 43758.5453);
    vec2 flutterCoords = positionOut.xz * FLUTTER_SCALE - gustScroll * 2.0 * phase;
    float flutterNoise = textureLod(sampler2D(sharedImage0Texture, materialRepeatSampler), flutterCoords, 0.0).g;
    float flutterWave = sin(TAU * (phase * FLUTTER_OSCILLATIONS + instanceHash + position.y * 0.5));
    float flutter = flutterWave * flutterNoise * flutterStrength * (0.5 + gust);

    // bend the instance, more toward its tip, lowering it to roughly preserve its length
    float heightLocal = clamp(position.y, 0.0, 1.0);
    float bendWeight = heightLocal * heightLocal;
    vec2 bend = (windDirection * gustBend + windSide * flutter) * bendWeight * instanceHeight;
    positionOut.xz += bend;
    positionOut.y -= dot(bend, bend) / max(2.0 * instanceHeight, 0.001);

    // collapse the instance toward its root as it nears the draw distance
    float rootDistance = distance(root.xyz, eye.center);
    float collapse = 1.0 - smoothstep(collapseBegin, max(drawDistance, collapseBegin + 0.001), rootDistance);
    positionOut.xyz = root.xyz + (positionOut.xyz - root.xyz) * collapse;

    // compute remaining outputs
    int texCoordsOffsetIndex = gl_VertexIndex % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    // transform normals by the model's rotation alone rather than by its inverse transpose. Foliage is usually scaled
    // non-uniformly (wider than it is tall, or the reverse), and its normals are often authored as directions (such as
    // grass normals bent toward up), which the inverse transpose would bend further toward or away from up.
    mat3 rotation = mat3(normalize(model[0].xyz), normalize(model[1].xyz), normalize(model[2].xyz));
    normalOut = normalize(rotation * normal);
    tangentOut = rotation * tangent;
    albedoOut = albedo;
    materialOut = material;
    material2Out = material2;
    subsurfacePlusOut = subsurfacePlus;
    sharedSettingsOut = sharedSettings;
    userDefinedSettingsOut = userDefinedSettings;
    tintOut = color.a > 0.0 ? color.rgb : vec3(1.0); // alpha is zero only when the model has no vertex colors
    roughenOut = smoothstep(0.0, max(roughenEnd, 0.001), rootDistance);
    gl_Position = eye.viewProjection * positionOut;
}
