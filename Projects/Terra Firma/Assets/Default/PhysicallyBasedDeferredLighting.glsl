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

void main()
{
    // ensure position was written
    vec4 position = texture(positionTexture, texCoordsOut);
    if (position.w == 1.0)
    {
        // retrieve remaining data from geometry buffers
        vec3 normal = texture(normalPlusTexture, texCoordsOut).xyz;
        vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
        vec4 material = texture(materialTexture, texCoordsOut);

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

        // uniform values
        float reflectionFineness = 0.34;
        float reflectionDepthMax = 24.0;
        float reflectionDistanceMax = 32.0;
        int reflectionStepsMax = 320;
        int reflectionRefinements = 9;
        float reflectionSurfaceSlopeMax = 0.1;
        float reflectionRayThickness = 0.5;
        float reflectionFilterCutoff = 0.25;
        float reflectionEdgeCutoffHorizontal = 0.05;
        float reflectionEdgeCutoffVertical = 0.25;
        reflectionFineness = clamp(reflectionFineness, 0.0, 1.0); // clamp user-defined values

        // apply screen-space reflection fragment when isn't too deep and surface slope isn't too great
        mat3 view3 = mat3(view);
        vec4 positionView = view * position;
        float surfaceSlope = 1.0 - abs(dot(normal, vec3(0.0, 1.0, 0.0)));
        if (positionView.z >= -reflectionDepthMax && surfaceSlope <= reflectionSurfaceSlopeMax)
        {
            // compute view values
            vec2 texSize = textureSize(positionTexture, 0).xy;
            vec3 positionViewNormal = normalize(positionView.xyz);
            vec3 normalView = normalize(view3 * normal);
            vec3 reflectionView = reflect(positionViewNormal, normalView);
            vec4 startView = vec4(positionView.xyz, 1.0);
            vec4 stopView = vec4(positionView.xyz + reflectionView * reflectionDistanceMax, 1.0);

            // compute the fragment at which to start marching
            vec4 startFrag4 = projection * startView;
            vec2 startFrag = startFrag4.xy / startFrag4.w;
            startFrag = startFrag * 0.5 + 0.5;
            startFrag *= texSize;

            // compute the fragment at which to end marching
            vec4 stopFrag4 = projection * stopView;
            vec2 stopFrag = stopFrag4.xy / stopFrag4.w;
            stopFrag = stopFrag * 0.5 + 0.5;
            stopFrag *= texSize;

            // initialize current fragment
            vec2 currentFrag = startFrag;
            vec2 currentUV = currentFrag / texSize;
            vec4 currentPositionView = positionView;

            // compute fragment step amount
            float marchHorizonal = stopFrag.x - startFrag.x;
            float marchVertical = stopFrag.y - startFrag.y;
            float shouldMarchHorizontal = abs(marchHorizonal) >= abs(marchVertical) ? 1.0 : 0.0;
            float stepLength = mix(abs(marchVertical), abs(marchHorizonal), shouldMarchHorizontal) * reflectionFineness;
            vec2 stepAmount = vec2(marchHorizonal, marchVertical) / max(stepLength, 0.001);

            // march fragment
            int hit0 = 0;
            int hit1 = 0;
            float search0 = 0.0;
            float search1 = 0.0;
            float currentDistanceView = 0.0;
            float currentDepthView = 0.0;
            for (int i = 0; i < min(int(stepLength), reflectionStepsMax); ++i)
            {
                // step fragment
                currentFrag += stepAmount;
                currentUV = currentFrag / texSize;

                // determine whether we're on geometry (not sky box)
                vec4 currentPosition = texture(positionTexture, currentUV);
                if (currentPosition.w == 1.0)
                {
                    // determine whether we hit geometry within acceptable thickness
                    currentPositionView = view * texture(positionTexture, currentUV);
                    search1 = clamp(mix((currentFrag.y - startFrag.y) / marchVertical, (currentFrag.x - startFrag.x) / marchHorizonal, shouldMarchHorizontal), 0.0, 1.0);
                    currentDistanceView = startView.z * stopView.z / mix(stopView.z, startView.z, search1); // uses perspective correct interpolation for depth
                    currentDepthView = currentDistanceView - currentPositionView.z;
                    if (currentDepthView < 0.0 && currentDepthView > -reflectionRayThickness)
                    {
                        hit0 = 1;
                        break;
                    }
                }

                // otherwise loop
                search0 = search1;
            }

            // refine when hit occurred
            if (hit0 == 1)
            {
                // perform refinements within last walk
                search1 = search0 + (search1 - search0) * 0.5;
                for (int i = 0; i < reflectionRefinements; ++i)
                {
                    // refine fragment
                    currentFrag = mix(startFrag, stopFrag, search1);
                    currentUV = currentFrag / texSize;

                    // determine whether we're on geometry (not sky box)
                    vec4 currentPosition = texture(positionTexture, currentUV);
                    if (currentPosition.w == 1.0)
                    {
                        // determine whether we hit geometry within acceptable thickness
                        currentPositionView = view * texture(positionTexture, currentUV);
                        currentDistanceView = startView.z * stopView.z / mix(stopView.z, startView.z, search1); // uses perspective correct interpolation for depth
                        currentDepthView = currentDistanceView - currentPositionView.z;
                        if (currentDepthView < 0.0 && currentDepthView > -reflectionRayThickness)
                        {
                            hit1 = 1;
                            search1 = search0 + (search1 - search0) * 0.5;
                            continue;
                        }
                    }

                    // otherwise continue in the same direction
                    float temp = search1;
                    search1 = search1 + (search1 - search0) * 0.5;
                    search0 = temp;
                }
            }

            // compute specular weight in favor of screen-space reflection
            specularWeight =
                hit1 * // filter out when refinement hit not found
                (1.0 - surfaceSlope) * // filter out as slope increases
                (1.0 - smoothstep(reflectionFilterCutoff, 1.0, max(dot(-positionViewNormal, reflectionView), 0.0))) * // filter out as reflection angles toward eye
                (1.0 - smoothstep(reflectionFilterCutoff, 1.0, positionView.z / -reflectionDepthMax)) * // filter out as fragment reaches max depth
                (1.0 - smoothstep(reflectionFilterCutoff, 1.0, length(currentPositionView - positionView) / reflectionDistanceMax)) * // filter out as reflection point reaches max distance from fragment
                smoothstep(0.0, reflectionEdgeCutoffHorizontal, min(currentUV.x, 1.0 - currentUV.x)) *
                smoothstep(0.0, reflectionEdgeCutoffVertical, min(currentUV.y, 1.0 - currentUV.y));
            specularWeight = clamp(specularWeight, 0.0, 1.0);

            // compute specular color
            float specularPower = (1.0 - roughness); // TODO: figure out how to make this the proper specular power (and give it its proper name).
            specularSS = vec3(texture(albedoTexture, currentUV).rgb * specularPower);
        }

        // compute specular term
        vec3 specular = (1.0 - specularWeight) * specularLM + specularWeight * specularSS;

        // compute ambient term
        vec3 ambient = diffuse + specular;

        // compute color w/ tone mapping, gamma correction, and emission
        vec3 color = lightAccum + ambient;
        color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0 / GAMMA));
        color = color + emission * albedo.rgb;

        // write color
        frag = vec4(color, 1.0);
    }
    else frag = vec4(0.0); // write zero
}
