#shader vertex
#version 410

const int TEX_COORDS_OFFSET_VERTS = 6;
const int BONES_MAX = 128;
const int BONES_INFLUENCE_MAX = 4;

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
uniform mat4 bones[BONES_MAX];

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec4 boneIds;
layout(location = 4) in vec4 weights;
layout(location = 5) in mat4 model;
layout(location = 9) in vec4 texCoordsOffset;
layout(location = 10) in vec4 albedo;
layout(location = 11) in vec4 material;
layout(location = 12) in vec4 heightPlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;

void main()
{
    // compute blended bone influences
    mat4 boneBlended = mat4(0.0);
    for (int i = 0; i < BONES_INFLUENCE_MAX; ++i)
    {
        int boneId = int(boneIds[i]);
        if (boneId >= 0) boneBlended += bones[boneId] * weights[i];
    }

    // compute blended position and normal
    vec4 positionBlended = boneBlended * vec4(position, 1.0);
    vec4 normalBlended = boneBlended * vec4(normal, 0.0);

    // compute remaining values
    positionOut = model * positionBlended;
    positionOut /= positionOut.w; // NOTE: normalizing by w seems to fix a bug caused by weights not summing to 1.0.
    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;
    albedoOut = albedo;
    materialOut = material;
    normalOut = transpose(inverse(mat3(model))) * normalBlended.xyz;
    heightPlusOut = heightPlus;
    gl_Position = projection * view * positionOut;
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0f;
const int LIGHT_MAPS_MAX = 2;
const int LIGHTS_MAX = 8;

const vec4 SSVF_DITHERING[4] =
    vec4[4](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

uniform vec3 eyeCenter;
uniform float lightCutoffMargin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform int fogEnabled;
uniform float fogStart;
uniform float fogFinish;
uniform vec4 fogColor;
uniform int ssvfEnabled;
uniform int ssvfSteps;
uniform float ssvfAsymmetry;
uniform float ssvfIntensity;
layout(bindless_sampler) uniform sampler2D albedoTexture;
layout(bindless_sampler) uniform sampler2D roughnessTexture;
layout(bindless_sampler) uniform sampler2D metallicTexture;
layout(bindless_sampler) uniform sampler2D ambientOcclusionTexture;
layout(bindless_sampler) uniform sampler2D emissionTexture;
layout(bindless_sampler) uniform sampler2D normalTexture;
layout(bindless_sampler) uniform sampler2D heightTexture;
layout(bindless_sampler) uniform sampler2D brdfTexture;
layout(bindless_sampler) uniform samplerCube irradianceMap;
layout(bindless_sampler) uniform samplerCube environmentFilterMap;
layout(bindless_sampler) uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];
layout(bindless_sampler) uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform vec3 lightMapAmbientColors[LIGHT_MAPS_MAX];
uniform float lightMapAmbientBrightnesses[LIGHT_MAPS_MAX];
uniform int lightMapsCount;
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec3 lightDirections[LIGHTS_MAX];
uniform vec3 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightAttenuationLinears[LIGHTS_MAX];
uniform float lightAttenuationQuadratics[LIGHTS_MAX];
uniform float lightCutoffs[LIGHTS_MAX];
uniform int lightTypes[LIGHTS_MAX];
uniform float lightConeInners[LIGHTS_MAX];
uniform float lightConeOuters[LIGHTS_MAX];
uniform int lightDesireFogs[LIGHTS_MAX];
uniform int lightsCount;

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;

layout(location = 0) out vec4 frag;

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

bool inBounds(vec3 point, vec3 min, vec3 size)
{
    return
        all(greaterThanEqual(point, min)) &&
        all(lessThanEqual(point, min + size));
}

vec2 rayBoxIntersectionRatios(vec3 rayOrigin, vec3 rayDirection, vec3 boxMin, vec3 boxSize)
{
    vec3 rayDirectionInv = vec3(1.0) / rayDirection;
    vec3 boxMax = boxMin + boxSize;
    vec3 t1 = (boxMin - rayOrigin) * rayDirectionInv;
    vec3 t2 = (boxMax - rayOrigin) * rayDirectionInv;
    vec3 tMin = min(t1, t2);
    vec3 tMax = max(t1, t2);
    float tEnter = max(max(tMin.x / boxSize.x, tMin.y / boxSize.y), tMin.z / boxSize.z);
    float tExit = min(min(tMax.x / boxSize.x, tMax.y / boxSize.y), tMax.z / boxSize.z);
    return tEnter < tExit ? vec2(tEnter, tExit) : vec2(0.0);
}

float computeDepthRatio(vec3 minA, vec3 sizeA, vec3 minB, vec3 sizeB, vec3 position, vec3 normal)
{
    vec3 centerA = minA + sizeA * 0.5;
    vec3 centerB = minB + sizeB * 0.5;
    vec3 direction = normalize(cross(cross(centerB - centerA, normal), normal));
    vec3 intersectionMin = max(minA, minB);
    vec3 intersectionSize = min(minA + sizeA, minB + sizeB) - intersectionMin;
    vec2 intersectionRatios = rayBoxIntersectionRatios(position, direction, intersectionMin, intersectionSize);
    return intersectionRatios != vec2(0.0) ? intersectionRatios.y / (intersectionRatios.y - intersectionRatios.x) : 0.5;
}

vec3 parallaxCorrection(samplerCube cubeMap, vec3 lightMapOrigin, vec3 lightMapMin, vec3 lightMapSize, vec3 positionWorld, vec3 normalWorld)
{
    vec3 directionWorld = positionWorld - eyeCenter;
    vec3 reflectionWorld = reflect(directionWorld, normalWorld);
    vec3 firstPlaneIntersect = (lightMapMin + lightMapSize - positionWorld) / reflectionWorld;
    vec3 secondPlaneIntersect = (lightMapMin - positionWorld) / reflectionWorld;
    vec3 furthestPlane = max(firstPlaneIntersect, secondPlaneIntersect);
    float distance = min(min(furthestPlane.x, furthestPlane.y), furthestPlane.z);
    vec3 intersectPositionWorld = positionWorld + reflectionWorld * distance;
    return intersectPositionWorld - lightMapOrigin;
}

float distributionGGX(vec3 normal, vec3 h, float roughness)
{
    float a = roughness * roughness;
    float aPow2 = a * a;
    float nDotH = max(dot(normal, h), 0.0);
    float nDotHPow2 = nDotH * nDotH;
    float nom = aPow2;
    float denom = nDotHPow2 * (aPow2 - 1.0) + 1.0;
    denom = PI * denom * denom;
    return nom / denom;
}

float geometrySchlickGGX(float nDotV, float roughness)
{
    float r = roughness + 1.0;
    float k = r * r / 8.0;
    float nom = nDotV;
    float denom = nDotV * (1.0 - k) + k;
    return nom / denom;
}

float geometrySchlick(vec3 normal, vec3 v, vec3 l, float roughness)
{
    float nDotV = max(dot(normal, v), 0.0);
    float nDotL = max(dot(normal, l), 0.0);
    float ggx2 = geometrySchlickGGX(nDotV, roughness);
    float ggx1 = geometrySchlickGGX(nDotL, roughness);
    return ggx1 * ggx2;
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void main()
{
    // discard when depth out of range
    float depthCutoff = heightPlusOut.z;
    float depth = gl_FragCoord.z / gl_FragCoord.w;
    if (depthCutoff >= 0.0) { if (depth > depthCutoff) discard; }
    else if (depth <= -depthCutoff) discard;

    // compute basic fragment data
    vec4 position = positionOut;
    vec3 normal = normalize(normalOut);
    float distance = length(position.xyz - eyeCenter);

    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute tex coords in parallax occlusion space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * position.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    float height = texture(heightTexture, texCoordsOut).x * heightPlusOut.x;
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo with alpha
    vec4 albedoSample = texture(albedoTexture, texCoords);
    vec4 albedo;
    albedo.rgb = pow(albedoSample.rgb, vec3(GAMMA)) * albedoOut.rgb;
    albedo.a = albedoSample.a * albedoOut.a;
    float opaqueDistance = heightPlusOut.w;
    albedo.a = mix(albedo.a, 1.0, smoothstep(opaqueDistance * 0.667, opaqueDistance, distance));

    // compute material properties
    float roughness = texture(roughnessTexture, texCoords).r * materialOut.r;
    float metallic = texture(metallicTexture, texCoords).g * materialOut.g;
    float ambientOcclusion = texture(ambientOcclusionTexture, texCoords).b * materialOut.b;
    vec3 emission = vec3(texture(emissionTexture, texCoords).r * materialOut.a);

    // compute ignore light maps
    bool ignoreLightMaps = heightPlusOut.y != 0.0;

    // compute light accumulation
    vec3 n = normalize(toWorld * (texture(normalTexture, texCoords).xyz * 2.0 - 1.0));
    vec3 v = normalize(eyeCenter - position.xyz);
    float nDotV = max(dot(n, v), 0.0);
    vec3 f0 = mix(vec3(0.04), albedo.rgb, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 lightAccum = vec3(0.0);
    for (int i = 0; i < lightsCount; ++i)
    {
        // per-light radiance
        vec3 lightOrigin = lightOrigins[i];
        int lightType = lightTypes[i];
        bool lightDirectional = lightType == 2;
        vec3 l, h, radiance;
        if (!lightDirectional)
        {
            vec3 d = lightOrigin - position.xyz;
            l = normalize(d);
            h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoff = lightCutoffs[i];
            float cutoffScalar = 1.0 - smoothstep(cutoff * (1.0 - lightCutoffMargin), cutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinears[i] * distance + lightAttenuationQuadratics[i] * distanceSquared);
            float angle = acos(dot(l, -lightDirections[i]));
            float halfConeInner = lightConeInners[i] * 0.5;
            float halfConeOuter = lightConeOuters[i] * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity;
        }
        else
        {
            l = -lightDirections[i];
            h = normalize(v + l);
            radiance = lightColors[i] * lightBrightnesses[i];
        }

        // shadow scalar
        float shadowScalar = 1.0f;

        // cook-torrance brdf
        float hDotV = max(dot(h, v), 0.0);
        float ndf = distributionGGX(n, h, roughness);
        float g = geometrySchlick(n, v, l, roughness);
        vec3 f = fresnelSchlick(hDotV, f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float nDotL = max(dot(n, l), 0.0);
        float denominator = 4.0 * nDotV * nDotL + 0.0001; // add epsilon to prevent division by zero
        vec3 specular = numerator / denominator;

        // compute diffusion
        vec3 kS = f;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        // add to outgoing lightAccum
        lightAccum += (kD * albedo.rgb / PI + specular) * radiance * nDotL * shadowScalar;
    }

    // determine light map indices, including their validity
    int lm1 = lightMapsCount > 0 && !ignoreLightMaps ? 0 : -1;
    int lm2 = lightMapsCount > 1 && !ignoreLightMaps ? 1 : -1;
    if (lm2 != -1 && !inBounds(position.xyz, lightMapMins[lm2], lightMapSizes[lm2])) lm2 = -1;
    if (lm1 != -1 && !inBounds(position.xyz, lightMapMins[lm1], lightMapSizes[lm1])) lm1 = lm2;

    // compute light mapping terms
    vec3 ambientColor = vec3(0.0);
    float ambientBrightness = 0.0;
    vec3 irradiance = vec3(0.0);
    vec3 environmentFilter = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        ambientColor = lightAmbientColor;
        ambientBrightness = lightAmbientBrightness;
        irradiance = texture(irradianceMap, n).rgb;
        vec3 r = reflect(-v, n);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * REFLECTION_LOD_MAX).rgb;
    }
    else if (lm2 == -1)
    {
        ambientColor = lightMapAmbientColors[lm1];
        ambientBrightness = lightMapAmbientBrightnesses[lm1];
        irradiance = texture(irradianceMaps[lm1], n).rgb;
        vec3 r = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position.xyz, n);
        environmentFilter = textureLod(environmentFilterMaps[lm1], r, roughness * REFLECTION_LOD_MAX).rgb;
    }
    else
    {
        // compute blending
        float ratio = computeDepthRatio(lightMapMins[lm1], lightMapSizes[lm1], lightMapMins[lm2], lightMapSizes[lm2], position.xyz, n);

        // compute blended ambient values
        vec3 ambientColor1 = lightMapAmbientColors[lm1];
        vec3 ambientColor2 = lightMapAmbientColors[lm2];
        float ambientBrightness1 = lightMapAmbientBrightnesses[lm1];
        float ambientBrightness2 = lightMapAmbientBrightnesses[lm2];
        ambientColor = mix(ambientColor1, ambientColor2, ratio);
        ambientBrightness = mix(ambientBrightness1, ambientBrightness2, ratio);

        // compute blended irradiance
        vec3 irradiance1 = texture(irradianceMaps[lm1], n).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2], n).rgb;
        irradiance = mix(irradiance1, irradiance2, ratio);

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position.xyz, n);
        vec3 r2 = parallaxCorrection(environmentFilterMaps[lm2], lightMapOrigins[lm2], lightMapMins[lm2], lightMapSizes[lm2], position.xyz, n);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1], r1, roughness * REFLECTION_LOD_MAX).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2], r2, roughness * REFLECTION_LOD_MAX).rgb;
        environmentFilter = mix(environmentFilter1, environmentFilter2, ratio);
    }

    // compute ambient terms
    vec3 ambientDiffuse = ambientColor * ambientBrightness * ambientOcclusion;
    vec3 ambientSpecular = ambientDiffuse * ambientOcclusion;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(nDotV, f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedo.rgb * ambientDiffuse;

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(nDotV, roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * ambientSpecular;

    // compute color composition
    vec3 color = lightAccum + diffuse + emission * albedo.rgb + specular;

    // compute and apply global fog when enabled
    if (fogEnabled == 1)
    {
        float depth = length(position.xyz - eyeCenter);
        float fogFactor = smoothstep(fogStart / fogFinish, 1.0, min(1.0, depth / fogFinish)) * fogColor.a;
        color = color * (1.0 - fogFactor) + fogColor.rgb * fogFactor;
    }

    // apply tone mapping and gamma correction
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));

    // write fragment
    frag = vec4(color, albedo.a);
}
