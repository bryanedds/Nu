#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;

layout(location = 0) in vec3 position;

out vec3 positionOut;

void main()
{
    positionOut = position;
    gl_Position = viewProjection * vec4(positionOut, 1.0);
}

#shader fragment
#version 460 core

const float PI = 3.14159265359;
const float SAMPLE_DELTA = 0.025;

uniform samplerCube cubeMap;

in vec3 positionOut;

layout(location = 0) out vec4 frag;

void main()
{
    // compute normal
    vec3 normal = normalize(positionOut);

    // calculate tangent space
    vec3 up = vec3(0.0, 1.0, 0.0);
    vec3 right = normalize(cross(up, normal));
    up = normalize(cross(normal, right));

    // compute irradiance
    float sampleCount = 0.0;
    vec3 irradiance = vec3(0.0);
    for (float phi = 0.0; phi < 2.0 * PI; phi += SAMPLE_DELTA)
    {
        for (float theta = 0.0; theta < 0.5 * PI; theta += SAMPLE_DELTA)
        {
            vec3 sampleTangent = vec3(sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta));
            vec3 sampleVector = sampleTangent.x * right + sampleTangent.y * up + sampleTangent.z * normal;
            vec3 sampleColor = texture(cubeMap, sampleVector).rgb;
            if (!any(isnan(sampleColor))) // TODO: understand why NaN can come from this sample and try to apply a more appropriate fix.
            {
                irradiance += sampleColor * cos(theta) * sin(theta);
                ++sampleCount;
            }
        }
    }

    // normalize irradiance
    irradiance = PI * irradiance * (1.0 / sampleCount);
    frag = vec4(irradiance, 1.0);
}
