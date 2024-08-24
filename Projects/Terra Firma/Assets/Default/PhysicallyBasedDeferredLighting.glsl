#shader vertex
#version 410

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410

const float PI = 3.141592654;
const float REFLECTION_LOD_MAX = 7.0;
const float GAMMA = 2.2;
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const float SHADOW_FOV_MAX = 2.1;
const int SHADOWS_MAX = 16;

uniform vec3 eyeCenter;
uniform mat4 view;
uniform mat4 projection;
uniform float lightCutoffMargin;
uniform vec3 lightAmbientColor;
uniform float lightAmbientBrightness;
uniform float lightShadowBiasAcne;
uniform float lightShadowBiasBleed;
uniform int ssrEnabled;
uniform float ssrDetail;
uniform int ssrRefinementsMax;
uniform float ssrRayThickness;
uniform float ssrDepthCutoff;
uniform float ssrDistanceCutoff;
uniform float ssrRoughnessCutoff;
uniform float ssrSlopeCutoff;
uniform float ssrRoughnessCutoffMargin;
uniform float ssrDepthCutoffMargin;
uniform float ssrDistanceCutoffMargin;
uniform float ssrSlopeCutoffMargin;
uniform float ssrEdgeHorizontalMargin;
uniform float ssrEdgeVerticalMargin;
uniform vec3 ssrLightColor;
uniform float ssrLightBrightness;
uniform sampler2D positionTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalPlusTexture;
uniform sampler2D brdfTexture;
uniform sampler2D irradianceTexture;
uniform sampler2D environmentFilterTexture;
uniform sampler2D ssaoTexture;
uniform sampler2D shadowTextures[SHADOWS_MAX];
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
uniform int lightShadowIndices[LIGHTS_MAX];
uniform int lightsCount;
uniform mat4 shadowMatrices[SHADOWS_MAX];

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

vec3 rotate(vec3 axis, float angle, vec3 v)
{
    return mix(dot(axis, v) * axis, v, cos(angle)) + cross(axis, v) * sin(angle);
}

float computeShadowScalar(sampler2D shadowMap, vec2 shadowTexCoords, float shadowZ, float varianceMin, float lightBleedFilter)
{
    vec2 moments = texture(shadowMap, shadowTexCoords).xy;
    float p = step(shadowZ, moments.x);
    float variance = max(moments.y - moments.x * moments.x, varianceMin);
    float stepLength = shadowZ - moments.x;
    float pMax = linstep(lightBleedFilter, 1.0, variance / (variance + stepLength * stepLength));
    return max(p, pMax);
}

float fadeShadowScalar(vec2 shadowTexCoords, float shadowScalar)
{
    vec2 normalized = abs(shadowTexCoords * 2.0 - 1.0);
    float fadeScalar =
        max(
            smoothstep(0.85, 1.0, normalized.x),
            smoothstep(0.85, 1.0, normalized.y));
    return 1.0 - (1.0 - shadowScalar) * (1.0 - fadeScalar);
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
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void ssr(vec4 position, vec3 albedo, float roughness, float metallic, vec3 normal, float slope, out vec3 specularSS, out float specularWeight)
{
    // compute view values
    vec4 positionView = view * position;
    vec3 positionViewNormal = normalize(positionView.xyz);
    vec3 normalView = mat3(view) * normal;
    vec3 reflectionView = reflect(positionViewNormal, normalView);
    vec4 startView = vec4(positionView.xyz, 1.0);
    vec4 stopView = vec4(positionView.xyz + reflectionView * ssrDistanceCutoff, 1.0);
    float eyeDistanceFromPlane = abs(dot(normalView, positionView.xyz));

    // compute the fragment at which to start marching
    vec2 texSize = textureSize(positionTexture, 0).xy;
    vec4 startFrag4 = projection * startView;
    vec2 startFrag = startFrag4.xy / startFrag4.w;
    startFrag = startFrag * 0.5 + 0.5;
    startFrag *= texSize;

    // compute the fragment at which to end marching as well as total length
    vec4 stopFrag4 = projection * stopView;
    vec2 stopFrag = stopFrag4.xy / stopFrag4.w;
    stopFrag = stopFrag * 0.5 + 0.5;
    stopFrag *= texSize;
    float lengthFrag = length(stopFrag - startFrag);

    // initialize current fragment
    vec2 currentFrag = startFrag;
    vec2 currentUV = currentFrag / texSize;
    vec4 currentPosition = position;
    vec4 currentPositionView = positionView;

    // compute fragment step amount
    float marchHorizontal = stopFrag.x - startFrag.x;
    float marchVertical = stopFrag.y - startFrag.y;
    bool shouldMarchHorizontal = abs(marchHorizontal) >= abs(marchVertical);
    float stepCount = abs(shouldMarchHorizontal ? marchHorizontal : marchVertical) * ssrDetail;
    vec2 stepAmount = vec2(marchHorizontal, marchVertical) / max(stepCount, 0.001);

    // march fragment
    float currentProgressA = 0.0;
    float currentProgressB = 0.0;
    float currentDepthView = 0.0;
    for (int i = 0; i < stepCount && currentUV.x >= 0.0 && currentUV.x <= 1.0 && currentUV.y >= 0.0 && currentUV.y <= 1.0; ++i)
    {
        // advance frag values
        currentFrag += stepAmount;
        currentUV = currentFrag / texSize;
        currentPosition = texture(positionTexture, currentUV);
        currentPositionView = view * currentPosition;
        currentProgressB = length(currentFrag - startFrag) / lengthFrag;
        currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrDistanceCutoff increases.

        // compute depth delta and thickness based on view state
        float depthDelta = currentDepthView - -currentPositionView.z;
        float thickness = max(-currentPositionView.z * ssrRayThickness, ssrRayThickness);
        if (-currentPositionView.z > 3.0 && eyeDistanceFromPlane < 1.0)
            thickness = max(1.0 - eyeDistanceFromPlane + ssrRayThickness, thickness);

        // determine whether we hit geometry within acceptable thickness
        if (currentPosition.w == 1.0 && depthDelta >= 0.0 && depthDelta <= thickness)
        {
            // perform refinements within walk
            currentProgressB = currentProgressA + (currentProgressB - currentProgressA) * 0.5;
            for (int j = 0; j < ssrRefinementsMax; ++j)
            {
                // advance frag values
                currentFrag = mix(startFrag, stopFrag, currentProgressB);
                currentUV = currentFrag / texSize;
                currentPosition = texture(positionTexture, currentUV);
                currentPositionView = view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrDistanceCutoff increases.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(-currentPositionView.z * ssrRayThickness, ssrRayThickness);
                if (-currentPositionView.z > 3.0 && eyeDistanceFromPlane < 1.0)
                    thickness = max(1.0 - eyeDistanceFromPlane + ssrRayThickness, thickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentPosition.w == 1.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // compute screen-space specular color and weight
                    vec3 f0 = mix(vec3(0.04), albedo, metallic);
                    vec3 v = normalize(-positionView.xyz);
                    vec3 h = normalize(v + normal);
                    vec3 f = fresnelSchlick(max(dot(h, v), 0.0), f0);
                    vec3 specularIntensity = f * (1.0 - roughness);
                    specularSS = vec3(texture(albedoTexture, currentUV).rgb * ssrLightColor * ssrLightBrightness * specularIntensity);
                    specularWeight =
                        (1.0 - smoothstep(1.0 - ssrRoughnessCutoffMargin, 1.0, roughness / ssrRoughnessCutoff)) * // filter out as fragment reaches max roughness
                        (1.0 - smoothstep(1.0 - ssrDepthCutoffMargin, 1.0, positionView.z / -ssrDepthCutoff)) * // filter out as fragment reaches max depth
                        (1.0 - smoothstep(1.0 - ssrDistanceCutoffMargin, 1.0, length(currentPositionView - positionView) / ssrDistanceCutoff)) * // filter out as reflection point reaches max distance from fragment
                        (1.0 - smoothstep(1.0 - ssrSlopeCutoffMargin, 1.0, slope / ssrSlopeCutoff)) *
                        smoothstep(0.0, 1.0, eyeDistanceFromPlane) * // filter out as eye nears plane
                        smoothstep(0.0, ssrEdgeHorizontalMargin, min(currentUV.x, 1.0 - currentUV.x)) *
                        smoothstep(0.0, ssrEdgeVerticalMargin, min(currentUV.y, 1.0 - currentUV.y));
                    specularWeight = clamp(specularWeight, 0.0, 1.0);
                    break;
                }

                // continue in the same direction
                float temp = currentProgressB;
                currentProgressB = currentProgressB + (currentProgressB - currentProgressA) * 0.5;
                currentProgressA = temp;
            }

            // fin
            break;
        }
    }

    // otherwise loop
    currentProgressA = currentProgressB;
}

void main()
{
    // ensure position was written
    vec4 position = texture(positionTexture, texCoordsOut);
    if (position.w == 1.0)
    {
        // retrieve remaining data from geometry buffers
        vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
        vec4 material = texture(materialTexture, texCoordsOut);
        vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;

        // retrieve data from intermediate buffers
        vec3 irradiance = texture(irradianceTexture, texCoordsOut).rgb;
        vec3 environmentFilter = texture(environmentFilterTexture, texCoordsOut).rgb;
        float ssao = texture(ssaoTexture, texCoordsOut).r;

        // compute materials
        float roughness = material.r;
        float metallic = material.g;
        float ambientOcclusion = material.b * ssao;
        vec3 emission = vec3(material.a);

        // compute lightAccum term
        vec3 v = normalize(eyeCenter - position.xyz);
        vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.
        vec3 lightAccum = vec3(0.0);
        for (int i = 0; i < lightsCount; ++i)
        {
            // per-light radiance
            vec3 l, h, radiance;
            if (lightDirectionals[i] == 0)
            {
                vec3 d = lightOrigins[i] - position.xyz;
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
                float intensity = attenuation * halfConeScalar;
                radiance = lightColors[i] * lightBrightnesses[i] * intensity * cutoffScalar;
            }
            else
            {
                l = -lightDirections[i];
                h = normalize(v + l);
                radiance = lightColors[i] * lightBrightnesses[i];
            }

            // shadow scalar
            int shadowIndex = lightShadowIndices[i];
            float shadowScalar = 1.0;
            if (shadowIndex >= 0)
            {
                vec4 positionShadow = shadowMatrices[shadowIndex] * position;
                vec3 shadowTexCoordsProj = positionShadow.xyz / positionShadow.w;
                vec2 shadowTexCoords = vec2(shadowTexCoordsProj.x, shadowTexCoordsProj.y) * 0.5 + 0.5;
                float shadowZ = shadowTexCoordsProj.z * 0.5 + 0.5;
                if (shadowZ < 1.0f && shadowTexCoords.x >= 0.0 && shadowTexCoords.x <= 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y <= 1.0)
                {
                    shadowScalar = computeShadowScalar(shadowTextures[shadowIndex], shadowTexCoords, shadowZ, lightShadowBiasAcne, lightShadowBiasBleed);
                    if (lightConeOuters[i] > SHADOW_FOV_MAX) shadowScalar = fadeShadowScalar(shadowTexCoords, shadowScalar);
                }
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
            lightAccum += (kD * albedo / PI + specular) * radiance * nDotL * shadowScalar;
        }

        // compute light ambient terms
        // NOTE: lightAmbientSpecular gets an additional ao multiply for some specular occlusion.
        // TODO: use a better means of computing specular occlusion as this one isn't very effective.
        vec3 lightAmbientDiffuse = lightAmbientColor * lightAmbientBrightness * ambientOcclusion;
        vec3 lightAmbientSpecular = lightAmbientDiffuse * ambientOcclusion;

        // compute diffuse term
        vec3 f = fresnelSchlickRoughness(max(dot(normal, v), 0.0), f0, roughness);
        vec3 kS = f;
        vec3 kD = 1.0 - kS;
        kD *= 1.0 - metallic;
        vec3 diffuse = kD * irradiance * albedo * lightAmbientDiffuse;

        // compute specular term from light map
        vec2 environmentBrdf = texture(brdfTexture, vec2(max(dot(normal, v), 0.0), roughness)).rg;
        vec3 specularSubterm = f * environmentBrdf.x + environmentBrdf.y;
        vec3 specularLM = environmentFilter * specularSubterm * lightAmbientSpecular;

        // compute specular term and weight from screen-space
        vec3 specularSS = vec3(0.0);
        float specularWeight = 0.0;
        float slope = 1.0 - abs(dot(normal, vec3(0.0, 1.0, 0.0)));
        vec4 positionView = view * position;
        if (ssrEnabled == 1 && roughness <= ssrRoughnessCutoff && slope <= ssrSlopeCutoff && -positionView.z <= ssrDepthCutoff)
        {
            vec2 texSize = textureSize(positionTexture, 0).xy;
            float texelHeight = 1.0 / texSize.y;
            vec2 texCoordsBelow = texCoordsOut + vec2(0.0, -texelHeight); // using tex coord below current pixel reduces 'cracks' on floor reflections
            texCoordsBelow.y = max(0.0, texCoordsBelow.y);
            vec4 positionBelow = texture(positionTexture, texCoordsBelow);
            ssr(positionBelow, albedo, roughness, metallic, normal, slope, specularSS, specularWeight);
        }

        // compute specular term
        vec3 specular = (1.0 - specularWeight) * specularLM + specularWeight * specularSS;

        // compute ambient term
        vec3 ambient = diffuse + specular;

        // compute color w/ tone mapping, gamma correction, and emission
        vec3 color = lightAccum + ambient;
        color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0 / GAMMA));
        color = color + emission * albedo;

        // write color
        frag = vec4(color, 1.0);
    }
    else frag = vec4(0.0); // write zero
}
