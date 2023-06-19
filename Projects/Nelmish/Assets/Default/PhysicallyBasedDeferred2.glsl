#shader vertex
#version 410 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410 core

const float PI = 3.141592654;
const float FLOAT_MAX = 3.402823466e+38;
const float REFLECTION_LOD_MAX = 5.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHT_MAPS_MAX = 12;
const int LIGHTS_MAX = 64;

uniform mat4 view;
uniform mat4 projection;
uniform vec3 eyeCenter;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform sampler2D positionTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalAndHeightTexture;
uniform sampler2D ssaoTexture;
uniform samplerCube irradianceMap;
uniform samplerCube environmentFilterMap;
uniform sampler2D brdfTexture;
uniform int lightMapEnableds[LIGHT_MAPS_MAX];
uniform vec3 lightMapOrigins[LIGHT_MAPS_MAX];
uniform vec3 lightMapMins[LIGHT_MAPS_MAX];
uniform vec3 lightMapSizes[LIGHT_MAPS_MAX];
uniform samplerCube irradianceMaps[LIGHT_MAPS_MAX];
uniform samplerCube environmentFilterMaps[LIGHT_MAPS_MAX];
uniform vec3 lightOrigins[LIGHTS_MAX];
uniform vec3 lightDirections[LIGHTS_MAX];
uniform vec3 lightColors[LIGHTS_MAX];
uniform float lightBrightnesses[LIGHTS_MAX];
uniform float lightAttenuationLinears[LIGHTS_MAX];
uniform float lightAttenuationQuadratics[LIGHTS_MAX];
uniform float lightCutoffs[LIGHTS_MAX];
uniform int lightDirectionals[LIGHTS_MAX];
uniform float lightConeInners[LIGHTS_MAX];
uniform float lightConeOuters[LIGHTS_MAX];

in vec2 texCoordsOut;

out vec4 frag;

bool inBounds(vec3 point, vec3 min, vec3 size)
{
    return
        all(greaterThanEqual(point, min)) &&
        all(lessThanEqual(point, min + size));
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

float geometrySchlick(vec3 n, vec3 v, vec3 l, float roughness)
{
    float nDotV = max(dot(n, v), 0.0);
    float nDotL = max(dot(n, l), 0.0);
    float ggx2 = geometrySchlickGGX(nDotV, roughness);
    float ggx1 = geometrySchlickGGX(nDotL, roughness);
    return ggx1 * ggx2;
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), REFLECTION_LOD_MAX);
}

void main()
{
    // retrieve normal and height values first, allowing for early-out
    vec4 normalAndHeight = texture(normalAndHeightTexture, texCoordsOut);
    vec3 normal = normalAndHeight.rgb;
    if (normal == vec3(1.0)) discard; // discard if geometry pixel was not written (equal to the buffer clearing color of white)
    float height = normalAndHeight.a;

    // retrieve remaining data from geometry buffers
    vec3 position = texture(positionTexture, texCoordsOut).rgb;
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);

    // retrieve data from ssao buffer, smoothing in the process
    float ambientOcclusionScreen = textureLod(ssaoTexture, texCoordsOut, 0.5).r;

    // compute materials
    float metallic = material.r;
    float roughness = material.g;
    float ambientOcclusion = material.b;
    vec3 emission = vec3(material.a);

    // compute lightAccum term
    vec3 v = normalize(eyeCenter - position);
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
    vec3 lightAccum = vec3(0.0);
    for (int i = 0; i < LIGHTS_MAX; ++i)
    {
        // per-light radiance
        vec3 d = lightOrigins[i] - position;
        float distanceSquared = dot(d, d);
        float distance = sqrt(distanceSquared);
        float inRange = distance < lightCutoffs[i] ? 1.0 : 0.0;
        vec3 l, h, radiance;
        if (lightDirectionals[i] == 0)
        {
            l = normalize(d);
            h = normalize(v + l);
            float attenuation = 1.0f / (ATTENUATION_CONSTANT + lightAttenuationLinears[i] * distance + lightAttenuationQuadratics[i] * distanceSquared);
            float angle = acos(dot(lightDirections[i], l));
            float halfConeInner = lightConeInners[i] * 0.5f;
            float halfConeOuter = lightConeOuters[i] * 0.5f;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0f - halfConeBetween / halfConeDelta, 0.0f, 1.0);
            float intensity = attenuation * halfConeScalar;
            radiance = lightColors[i] * lightBrightnesses[i] * intensity * inRange;
        }
        else
        {
            l = lightDirections[i];
            h = normalize(v + l);
            radiance = lightColors[i] * lightBrightnesses[i] * inRange;
        }

        // cook-torrance brdf
        float ndf = distributionGGX(normal, h, roughness);
        float g = geometrySchlick(normal, v, l, roughness);
        vec3 f = fresnelSchlick(max(dot(h, v), 0.0), f0);

        // compute specularity
        vec3 numerator = ndf * g * f;
        float denominator = 4.0 * max(dot(normal, v), 0.0) * max(dot(normal, l), 0.0) + 0.0001; // add epsilon to prevent division by zero
        vec3 specular = numerator / denominator;

        // compute diffusion
        vec3 kS = f;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        // compute light scalar
        float nDotL = max(dot(normal, l), 0.0);

        // add to outgoing lightAccum
        lightAccum += (kD * albedo / PI + specular) * radiance * nDotL;
    }

    // compute nearest light map indices
    int lm1 = -1;
    int lm2 = -1;
    float lmDistanceSquared1 = FLOAT_MAX;
    float lmDistanceSquared2 = FLOAT_MAX;
    for (int i = 0; i < LIGHT_MAPS_MAX; ++i)
    {
        if (lightMapEnableds[i] != 0 && inBounds(position, lightMapMins[i], lightMapSizes[i]))
        {
            vec3 delta = lightMapOrigins[i] - position;
            float distanceSquared = dot(delta, delta);
            if (distanceSquared < lmDistanceSquared1)
            {
                lm2 = lm1;
                lm1 = i;
                lmDistanceSquared2 = lmDistanceSquared1;
                lmDistanceSquared1 = distanceSquared;
            }
            else if (distanceSquared < lmDistanceSquared2)
            {
                lm2 = i;
                lmDistanceSquared2 = distanceSquared;
            }
        }
    }

    // compute irradiance and environment filter terms
    vec3 irradiance = vec3(0.0);
    vec3 environmentFilter = vec3(0.0);
    if (lm1 == -1 && lm2 == -1)
    {
        irradiance = texture(irradianceMap, normal).rgb;
        vec3 r = reflect(-v, normal);
        environmentFilter = textureLod(environmentFilterMap, r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else if (lm2 == -1)
    {
        irradiance = texture(irradianceMaps[lm1], normal).rgb;
        vec3 r = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        environmentFilter = textureLod(environmentFilterMaps[lm1], r, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
    }
    else
    {
        // compute blended irradiance
        float distance1 = sqrt(lmDistanceSquared1);
        float distance2 = sqrt(lmDistanceSquared2);
        float distanceTotal = distance1 + distance2;
        float distanceTotalInverse = 1.0 / distanceTotal;
        float scalar1 = (distanceTotal - distance1) * distanceTotalInverse;
        float scalar2 = (distanceTotal - distance2) * distanceTotalInverse;
        vec3 irradiance1 = texture(irradianceMaps[lm1], normal).rgb;
        vec3 irradiance2 = texture(irradianceMaps[lm2], normal).rgb;
        irradiance = irradiance1 * scalar1 + irradiance2 * scalar2;

        // compute blended environment filter
        vec3 r1 = parallaxCorrection(environmentFilterMaps[lm1], lightMapOrigins[lm1], lightMapMins[lm1], lightMapSizes[lm1], position, normal);
        vec3 r2 = parallaxCorrection(environmentFilterMaps[lm2], lightMapOrigins[lm2], lightMapMins[lm2], lightMapSizes[lm2], position, normal);
        vec3 environmentFilter1 = textureLod(environmentFilterMaps[lm1], r1, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        vec3 environmentFilter2 = textureLod(environmentFilterMaps[lm2], r2, roughness * (REFLECTION_LOD_MAX - 1.0)).rgb;
        environmentFilter = environmentFilter1 * scalar1 + environmentFilter2 * scalar2;
    }

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = irradiance * albedo * lightAmbientColor * lightAmbientBrightness;

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
    vec3 specular = environmentFilter * (f * environmentBrdf.x + environmentBrdf.y) * lightAmbientColor * lightAmbientBrightness;

    // compute ambient term
    vec3 ambient = (kD * diffuse + specular) * ambientOcclusion * ambientOcclusionScreen;

    // compute color w/ tone mapping, gamma correction, and emission
    vec3 color = lightAccum + ambient;
    color = color / (color + vec3(1.0));
    color = pow(color, vec3(1.0 / GAMMA));
    color = color + emission * albedo.rgb;

    // write
    frag = vec4(color, 1.0);
}
