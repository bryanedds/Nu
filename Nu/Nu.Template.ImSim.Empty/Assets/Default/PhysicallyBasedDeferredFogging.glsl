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
const float ATTENUATION_CONSTANT = 1.0;
const int LIGHTS_MAX = 64;
const int SHADOW_TEXTURES_MAX = 12;
const int SHADOW_MAPS_MAX = 12;
const int SHADOW_CASCADES_MAX = 2;
const int SHADOW_CASCADE_LEVELS = 3;

const vec4 SSVF_DITHERING[4] =
    vec4[](
        vec4(0.0, 0.5, 0.125, 0.625),
        vec4(0.75, 0.22, 0.875, 0.375),
        vec4(0.1875, 0.6875, 0.0625, 0.5625),
        vec4(0.9375, 0.4375, 0.8125, 0.3125));

uniform vec3 eyeCenter;
uniform mat4 view;
uniform mat4 viewInverse;
uniform mat4 projection;
uniform mat4 projectionInverse;
uniform float lightCutoffMargin;
uniform int ssvfEnabled;
uniform float ssvfIntensity;
uniform int ssvfSteps;
uniform float ssvfAsymmetry;
uniform sampler2D depthTexture;
uniform sampler2DArray shadowTextures;
uniform samplerCube shadowMaps[SHADOW_MAPS_MAX];
uniform sampler2DArray shadowCascades[SHADOW_CASCADES_MAX];
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
uniform int lightShadowIndices[LIGHTS_MAX];
uniform int lightsCount;
uniform mat4 shadowMatrices[SHADOW_TEXTURES_MAX + SHADOW_CASCADES_MAX * SHADOW_CASCADE_LEVELS];

in vec2 texCoordsOut;

layout(location = 0) out vec4 fogAccum;

vec4 depthToPosition(float depth, vec2 texCoords)
{
    float z = depth * 2.0 - 1.0;
    vec4 positionClip = vec4(texCoords * 2.0 - 1.0, z, 1.0);
    vec4 positionView = projectionInverse * positionClip;
    positionView /= positionView.w;
    return viewInverse * positionView;
}

vec3 computeFogAccumPoint(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lightOrigins[lightIndex];
    float lightCutoff = lightCutoffs[lightIndex];
    vec3 lightDirection = lightDirections[lightIndex];
    float lightAttenuationLinear = lightAttenuationLinears[lightIndex];
    float lightAttenuationQuadratic = lightAttenuationQuadratics[lightIndex];
    float lightConeInner = lightConeInners[lightIndex];
    float lightConeOuter = lightConeOuters[lightIndex];

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 stopPosition = position.xyz;
    vec3 rayVector = stopPosition - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scattering approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec3 positionShadow = currentPosition - lightOrigin;
            float shadowZ = length(positionShadow);
            float shadowDepth = texture(shadowMaps[shadowIndex - SHADOW_TEXTURES_MAX], positionShadow).x;

            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scattering approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }
            
            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
}

vec3 computeFogAccumSpot(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lightOrigins[lightIndex];
    float lightCutoff = lightCutoffs[lightIndex];
    vec3 lightDirection = lightDirections[lightIndex];
    float lightAttenuationLinear = lightAttenuationLinears[lightIndex];
    float lightAttenuationQuadratic = lightAttenuationQuadratics[lightIndex];
    float lightConeInner = lightConeInners[lightIndex];
    float lightConeOuter = lightConeOuters[lightIndex];

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    float validSteps = 0.0001; // epsilon to avoid dbz
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment * intensity;

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[shadowIndex];
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures, vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

            // compute intensity inside light volume
            vec3 v = normalize(eyeCenter - currentPosition);
            vec3 d = lightOrigin - currentPosition;
            vec3 l = normalize(d);
            vec3 h = normalize(v + l);
            float distanceSquared = dot(d, d);
            float distance = sqrt(distanceSquared);
            float cutoffScalar = 1.0 - smoothstep(lightCutoff * (1.0 - lightCutoffMargin), lightCutoff, distance);
            float attenuation = 1.0 / (ATTENUATION_CONSTANT + lightAttenuationLinear * distance + lightAttenuationQuadratic * distanceSquared);
            float angle = acos(dot(l, -lightDirection));
            float halfConeInner = lightConeInner * 0.5;
            float halfConeOuter = lightConeOuter * 0.5;
            float halfConeDelta = halfConeOuter - halfConeInner;
            float halfConeBetween = angle - halfConeInner;
            float halfConeScalar = clamp(1.0 - halfConeBetween / halfConeDelta, 0.0, 1.0);
            float intensity = attenuation * halfConeScalar * cutoffScalar;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowDepth == 0.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment * intensity;
            }

            // step
            validSteps += intensity > 0.0 ? 1.0 : 0.0;
            currentPosition += step;
        }
    }
    
    // fin
    return smoothstep(0.0, 1.0, result / validSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
}

vec3 computeFogAccumDirectional(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lightOrigins[lightIndex];
    vec3 lightDirection = lightDirections[lightIndex];

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    vec3 currentPosition = startPosition + step * dithering;
    int shadowIndex = lightShadowIndices[lightIndex];
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // mie scaterring approximated with Henyey-Greenstein phase function
            float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
            float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
            result += fogMoment;

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        mat4 shadowMatrix = shadowMatrices[shadowIndex];
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // compute depths
            vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
            vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
            vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
            bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
            float shadowZ = shadowTexCoords.z;
            float shadowDepth = shadowTexCoordsInRange ? texture(shadowTextures, vec3(shadowTexCoords.xy, float(shadowIndex))).x : 1.0;

            // step through ray, accumulating fog light moment
            if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / ssvfSteps) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
}

vec3 computeFogAccumCascaded(vec4 position, int lightIndex)
{
    // grab light values
    vec3 lightOrigin = lightOrigins[lightIndex];
    vec3 lightDirection = lightDirections[lightIndex];

    // compute ray info
    vec3 startPosition = eyeCenter;
    vec3 rayVector = position.xyz - startPosition;
    float rayLength = length(rayVector);
    vec3 rayDirection = rayVector / rayLength;

    // compute step info
    float stepLength = rayLength / ssvfSteps;
    vec3 step = rayDirection * stepLength;

    // compute light view term
    float theta = dot(-rayDirection, lightDirection);

    // compute dithering
    float dithering = SSVF_DITHERING[int(gl_FragCoord.x) % 4][int(gl_FragCoord.y) % 4];

    // accumulate fog light
    vec3 result = vec3(0.0);
    int shadowIndex = lightShadowIndices[lightIndex];
    vec3 currentPosition = startPosition + step * dithering;
    if (shadowIndex < 0)
    {
        // march over ray, accumulating fog light value without shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // mie scaterring approximated with Henyey-Greenstein phase function
                float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                result += fogMoment;
            }

            // step
            currentPosition += step;
        }
    }
    else
    {
        // march over ray, accumulating fog light value with shadows
        for (int i = 0; i < ssvfSteps; ++i)
        {
            // use the nearest available cascade for this step
            for (int j = 0; j < SHADOW_CASCADE_LEVELS; ++j)
            {
                // compute depths
                mat4 shadowMatrix = shadowMatrices[SHADOW_TEXTURES_MAX + (shadowIndex - SHADOW_TEXTURES_MAX) * SHADOW_CASCADE_LEVELS + j];
                vec4 positionShadowClip = shadowMatrix * vec4(currentPosition, 1.0);
                vec3 shadowTexCoordsProj = positionShadowClip.xyz / positionShadowClip.w; // ndc space
                vec3 shadowTexCoords = shadowTexCoordsProj * 0.5 + 0.5;
                bool shadowTexCoordsInRange = shadowTexCoords.x >= 0.0 && shadowTexCoords.x < 1.0 && shadowTexCoords.y >= 0.0 && shadowTexCoords.y < 1.0;
                float shadowZ = shadowTexCoords.z;
                float shadowDepth = shadowTexCoordsInRange ? texture(shadowCascades[shadowIndex - SHADOW_TEXTURES_MAX], vec3(shadowTexCoords.xy, float(i))).x : 1.0;

                // step through ray, accumulating fog light moment
                if (shadowZ <= shadowDepth || shadowZ >= 1.0f)
                {
                    // mie scaterring approximated with Henyey-Greenstein phase function
                    float asymmetrySquared = ssvfAsymmetry * ssvfAsymmetry;
                    float fogMoment = (1.0 - asymmetrySquared) / (4.0 * PI * pow(1.0 + asymmetrySquared - 2.0 * ssvfAsymmetry * theta, 1.5));
                    result += fogMoment;
                }
            }

            // step
            currentPosition += step;
        }
    }

    // fin
    return smoothstep(0.0, 1.0, result / (ssvfSteps * SHADOW_CASCADE_LEVELS)) * lightColors[lightIndex] * lightBrightnesses[lightIndex] * ssvfIntensity;
}

void main()
{
    // clear accumulation buffer because there seems to exist a Mesa bug where glClear doesn't work on certain
    // platforms on this buffer - https://github.com/bryanedds/Nu/issues/800#issuecomment-3239861861
    fogAccum = vec4(0.0);

    // ensure fragment was written
    float depth = texture(depthTexture, texCoordsOut).r;
    if (depth != 0.0 && ssvfEnabled == 1)
    {
        // recover position from depth
        vec4 position = depthToPosition(depth, texCoordsOut);

        // accumulate fog
        for (int i = 0; i < lightsCount; ++i)
        {
            if (lightDesireFogs[i] == 1)
            {
                switch (lightTypes[i])
                {
                    case 0: { fogAccum.rgb += computeFogAccumPoint(position, i); break; } // point
                    case 1: { fogAccum.rgb += computeFogAccumSpot(position, i); break; } // spot
                    case 2: { fogAccum.rgb += computeFogAccumDirectional(position, i); break; } // directional
                    default: { fogAccum.rgb += computeFogAccumCascaded(position, i); break; } // cascaded
                }
            }
        }
    }
}
