#shader vertex
#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 460 core

const float PI = 3.141592654;
const float PI_OVER_2 = PI / 2.0;

uniform vec3 eyeCenter;
uniform mat4 view;
uniform mat4 viewInverse;
uniform mat4 projection;
uniform mat4 projectionInverse;
uniform float lightAmbientBoostCutoff;
uniform float lightAmbientBoostScalar;
uniform int ssrlEnabled;
uniform float ssrlIntensity;
uniform float ssrlDetail;
uniform int ssrlRefinementsMax;
uniform float ssrlRayThickness;
uniform float ssrlTowardEyeCutoff;
uniform float ssrlDepthCutoff;
uniform float ssrlDepthCutoffMargin;
uniform float ssrlDistanceCutoff;
uniform float ssrlDistanceCutoffMargin;
uniform float ssrlRoughnessCutoff;
uniform float ssrlRoughnessCutoffMargin;
uniform float ssrlSlopeCutoff;
uniform float ssrlSlopeCutoffMargin;
uniform float ssrlEdgeHorizontalMargin;
uniform float ssrlEdgeVerticalMargin;
uniform sampler2D depthTexture;
uniform sampler2D albedoTexture;
uniform sampler2D materialTexture;
uniform sampler2D normalPlusTexture;
uniform sampler2D lightAccumTexture;
uniform sampler2D brdfTexture;
uniform sampler2D ambientTexture;
uniform sampler2D irradianceTexture;
uniform sampler2D environmentFilterTexture;
uniform sampler2D ssaoTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 color;
layout(location = 1) out float depth;

float saturate(float v)
{
    return clamp(v, 0.0f, 1.0);
}

float linstep(float low, float high, float v)
{
    return clamp((v - low) / (high - low), 0.0, 1.0);
}

vec3 rotate(vec3 axis, float angle, vec3 v)
{
    return mix(dot(axis, v) * axis, v, cos(angle)) + cross(axis, v) * sin(angle);
}

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = projectionInverse * positionClip;
    positionView /= positionView.w;
    return viewInverse * positionView;
}

float depthViewToDepthBuffer(float near, float far, float depthView)
{
    return (-depthView - near) / (far - near);
}

float depthScreenToDepthView(float near, float far, float depthScreen)
{
    // for a standard OpenGL projection, compute a and b:
    float a = -(far + near) / (far - near);
    float b = -(2.0 * far * near) / (far - near);

    // convert depth from [0, 1] to normalized device coordinate (NDC) z in [-1, 1].
    float ndcZ = depthScreen * 2.0 - 1.0;

    // recover view-space z: note that view-space z is negative in front of the camera.
    // when depthScreen is 0 (near plane), ndcZ is -1 and view.z becomes -near.
    // when depthScreen is 1 (far plane), ndcZ is 1 and view.z becomes -far.
    return b / (ndcZ + a);
}

vec3 fresnelSchlick(float cosTheta, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 f0, float roughness)
{
    return f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void computeSsr(float depth, vec4 position, vec3 albedo, float roughness, float metallic, vec3 normal, float slope, out vec3 specularScreen, out float specularScreenWeight)
{
    // compute view values
    vec4 positionView = view * position;
    vec3 positionViewNormal = normalize(positionView.xyz);
    vec3 normalView = mat3(view) * normal;
    vec3 reflectionView = reflect(positionViewNormal, normalView);
    vec4 startView = vec4(positionView.xyz, 1.0);
    vec4 stopView = vec4(positionView.xyz + reflectionView * ssrlDistanceCutoff, 1.0);
    float eyeDistanceFromPlane = abs(dot(normalView, positionView.xyz));

    // compute the fragment at which to start marching
    vec2 texSize = textureSize(depthTexture, 0).xy;
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
    vec2 currentTexCoords = currentFrag / texSize;
    float currentDepth = depth;
    vec4 currentPosition = position;
    vec4 currentPositionView = positionView;

    // compute fragment step amount
    float marchHorizontal = stopFrag.x - startFrag.x;
    float marchVertical = stopFrag.y - startFrag.y;
    bool shouldMarchHorizontal = abs(marchHorizontal) >= abs(marchVertical);
    float stepCount = abs(shouldMarchHorizontal ? marchHorizontal : marchVertical) * ssrlDetail;
    vec2 stepAmount = vec2(marchHorizontal, marchVertical) / max(stepCount, 0.001);

    // march fragment
    float currentProgressA = 0.0;
    float currentProgressB = 0.0;
    float currentDepthView = 0.0;
    for (int i = 0; i < stepCount && currentTexCoords.x >= 0.0 && currentTexCoords.x < 1.0 && currentTexCoords.y >= 0.0 && currentTexCoords.y < 1.0; ++i)
    {
        // advance frag values
        currentFrag += stepAmount;
        currentTexCoords = currentFrag / texSize;
        currentDepth = texture(depthTexture, currentTexCoords).r;
        currentPosition = depthToPosition(currentDepth, currentTexCoords);
        currentPositionView = view * currentPosition;
        currentProgressB = length(currentFrag - startFrag) / lengthFrag;
        currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrlDistanceCutoff increases.

        // compute depth delta and thickness based on view state
        float depthDelta = currentDepthView - -currentPositionView.z;
        float thickness = max(-currentPositionView.z * ssrlRayThickness, ssrlRayThickness);

        // determine whether we hit geometry within acceptable thickness
        if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
        {
            // perform refinements within walk
            currentProgressB = currentProgressA + (currentProgressB - currentProgressA) * 0.5;
            for (int j = 0; j < ssrlRefinementsMax; ++j)
            {
                // advance frag values
                currentFrag = mix(startFrag, stopFrag, currentProgressB);
                currentTexCoords = currentFrag / texSize;
                currentDepth = texture(depthTexture, currentTexCoords).r;
                currentPosition = depthToPosition(currentDepth, currentTexCoords);
                currentPositionView = view * currentPosition;
                currentDepthView = -startView.z * -stopView.z / max(0.00001, mix(-stopView.z, -startView.z, currentProgressB)); // NOTE: uses perspective correct interpolation for depth, but causes precision issues as ssrlDistanceCutoff increases.

                // compute depth delta and thickness based on view state
                float depthDelta = currentDepthView - -currentPositionView.z;
                float thickness = max(-currentPositionView.z * ssrlRayThickness, ssrlRayThickness);

                // determine whether we hit geometry within acceptable thickness
                if (currentDepth != 0.0 && depthDelta >= 0.0 && depthDelta <= thickness)
                {
                    // compute screen-space specular color and weight
                    vec3 f0 = mix(vec3(0.04), albedo, metallic);
                    vec3 v = normalize(-positionView.xyz);
                    vec3 h = normalize(v + normal);
                    vec3 f = fresnelSchlick(max(dot(h, v), 0.0), f0);
                    vec3 specularIntensity = f * (1.0 - roughness);
                    specularScreen = texture(lightAccumTexture, currentTexCoords).rgb * specularIntensity * ssrlIntensity;
                    specularScreenWeight =
                        (1.0 - smoothstep(1.0 - ssrlRoughnessCutoffMargin, 1.0, roughness / ssrlRoughnessCutoff)) * // filter out as fragment reaches max roughness
                        (1.0 - smoothstep(1.0 - ssrlDepthCutoffMargin, 1.0, positionView.z / -ssrlDepthCutoff)) * // filter out as fragment reaches max depth
                        (1.0 - smoothstep(1.0 - ssrlDistanceCutoffMargin, 1.0, length(currentPositionView - positionView) / ssrlDistanceCutoff)) * // filter out as reflection point reaches max distance from fragment
                        (1.0 - smoothstep(1.0 - ssrlSlopeCutoffMargin, 1.0, slope / ssrlSlopeCutoff)) * // filter out as slope nears cutoff
                        smoothstep(0.0, 1.0, eyeDistanceFromPlane) * // filter out as eye nears plane
                        smoothstep(0.0, ssrlEdgeHorizontalMargin, min(currentTexCoords.x, 1.0 - currentTexCoords.x)) *
                        smoothstep(0.0, ssrlEdgeVerticalMargin, min(currentTexCoords.y, 1.0 - currentTexCoords.y));
                    specularScreenWeight = clamp(specularScreenWeight, 0.0, 1.0);
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
    // ensure fragment was written
    float depthInput = texture(depthTexture, texCoordsOut).r;
    if (depthInput == 0.0) discard;

    // recover position from depth
    vec4 position = depthToPosition(depthInput, texCoordsOut);

    // retrieve remaining data from geometry buffers
    vec3 albedo = texture(albedoTexture, texCoordsOut).rgb;
    vec4 material = texture(materialTexture, texCoordsOut);
    vec3 normal = normalize(texture(normalPlusTexture, texCoordsOut).xyz);
    vec3 lightAccum = texture(lightAccumTexture, texCoordsOut).rgb;

    // retrieve data from intermediate buffers
    vec4 ambientColorAndBrightness = texture(ambientTexture, texCoordsOut);
    vec3 irradiance = texture(irradianceTexture, texCoordsOut).rgb;
    vec3 environmentFilter = texture(environmentFilterTexture, texCoordsOut).rgb;
    float ssao = texture(ssaoTexture, texCoordsOut).r;

    // compute materials
    float roughness = material.r;
    float metallic = material.g;
    float ambientOcclusion = material.b * ssao;
    vec3 emission = vec3(material.a);

    // compute lighting terms
    vec3 v = normalize(eyeCenter - position.xyz);
    float nDotV = max(dot(normal, v), 0.0);
    vec3 f0 = mix(vec3(0.04), albedo, metallic); // if dia-electric (plastic) use f0 of 0.04f and if metal, use the albedo color as f0.

    // compute ambient light
    vec3 ambientColor = ambientColorAndBrightness.rgb;
    float ambientBrightness = ambientColorAndBrightness.a;
    float ambientBoostFactor = smoothstep(1.0 - lightAmbientBoostCutoff, 1.0, 1.0 - roughness);
    float ambientBoost = 1.0 + ambientBoostFactor * lightAmbientBoostScalar;
    vec3 ambientLight = ambientColor * ambientBrightness * ambientBoost * ambientOcclusion;

    // compute diffuse term
    vec3 f = fresnelSchlickRoughness(nDotV, f0, roughness);
    vec3 kS = f;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;
    vec3 diffuse = kD * irradiance * albedo * ambientLight;

    // compute specular term and weight from screen-space
    vec3 forward = vec3(view[0][2], view[1][2], view[2][2]);
    float towardEye = dot(forward, normal);
    float slope = 1.0 - abs(dot(normal, vec3(0.0, 1.0, 0.0)));
    vec4 positionView = view * position;
    vec3 specularScreen = vec3(0.0);
    float specularScreenWeight = 0.0;
    if (ssrlEnabled == 1 && towardEye <= ssrlTowardEyeCutoff && -positionView.z <= ssrlDepthCutoff && roughness <= ssrlRoughnessCutoff && slope <= ssrlSlopeCutoff)
    {
        vec2 texSize = textureSize(depthTexture, 0).xy;
        float texelHeight = 1.0 / texSize.y;
        vec2 texCoordsBelow = texCoordsOut + vec2(0.0, -texelHeight); // using tex coord below current pixel reduces 'cracks' on floor reflections
        texCoordsBelow.y = max(0.0, texCoordsBelow.y);
        float depthBelow = texture(depthTexture, texCoordsBelow).r;
        vec4 positionBelow = depthToPosition(depthBelow, texCoordsBelow);
        computeSsr(depthBelow, positionBelow, albedo, roughness, metallic, normal, slope, specularScreen, specularScreenWeight);

        // if hit failed, try again on the proper tex coord
        if (specularScreenWeight == 0.0)
        {
            vec2 texCoords = texCoordsOut;
            texCoords.y = max(0.0, texCoords.y);
            computeSsr(depthInput, position, albedo, roughness, metallic, normal, slope, specularScreen, specularScreenWeight);
        }
    }

    // compute specular term
    vec2 environmentBrdf = texture(brdfTexture, vec2(nDotV, roughness)).rg;
    vec3 specularEnvironmentSubterm = f * environmentBrdf.x + environmentBrdf.y;
    vec3 specularEnvironment = environmentFilter * specularEnvironmentSubterm * ambientLight;
    vec3 specular = (1.0 - specularScreenWeight) * specularEnvironment + specularScreenWeight * specularScreen;

    // write color
    color = vec4(lightAccum + diffuse + emission * albedo + specular, 1.0);

    // write depth
    depth = depthInput;
}
