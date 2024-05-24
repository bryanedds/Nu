#shader vertex
#version 410

layout (location = 0) in vec3 position;

out vec3 positionOut;

uniform mat4 view;
uniform mat4 projection;

void main()
{
    positionOut = position;  
    gl_Position =  projection * view * vec4(positionOut, 1.0);
}

#shader fragment
#version 410

const float PI = 3.141592654;
const uint SAMPLE_COUNT = 1024u;
const float TONE_UNMAP_SCALAR = 1.6225;

uniform float roughness;
uniform float resolution; // resolution of cube map face
uniform samplerCube cubeMap;

in vec3 positionOut;

layout (location = 0) out vec4 frag;

float distributionGGX(vec3 normal, vec3 h, float roughness)
{
    float rPow2 = roughness * roughness;
    float rPow4 = rPow2 * rPow2;
    float nDotH = max(dot(normal, h), 0.0);
    float nDotHPow2 = nDotH * nDotH;
    float nom = rPow4;
    float denom = nDotHPow2 * (rPow4 - 1.0) + 1.0;
    return nom / (PI * denom * denom);
}

float radicalInverse(uint bits) 
{
     bits = (bits << 16u) | (bits >> 16u);
     bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
     bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
     bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
     bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
     return float(bits) * 2.3283064365386963e-10;
}

vec2 hammersley(uint i, uint normal)
{
	return vec2(float(i) / float(normal), radicalInverse(i));
}

vec3 importanceSampleGGX(vec2 xi, vec3 normal, float roughness)
{
    // compute 
	float rPow2 = roughness * roughness;
	float phi = 2.0 * PI * xi.x;
	float cosTheta = sqrt((1.0 - xi.y) / (1.0 + (rPow2 * rPow2 - 1.0) * xi.y));
	float sinTheta = sqrt(1.0 - cosTheta * cosTheta);
	
	// convert from spherical coordinates to cartesian coordinates - halfway vector
	vec3 h;
	h.x = cos(phi) * sinTheta;
	h.y = sin(phi) * sinTheta;
	h.z = cosTheta;
	
	// convert from tangent-space h vector to world-space sample vector
	vec3 up = abs(normal.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);
	vec3 tangent = normalize(cross(up, normal));
	vec3 bitangent = cross(normal, tangent);
	
    // compute sample
	vec3 sample_ = tangent * h.x + bitangent * h.y + normal * h.z;
	return normalize(sample_);
}

void main()
{
    // compute normal
    vec3 normal = normalize(positionOut);

    // make the simplifying assumption that v equals r equals the normal
    vec3 r = normal;
    vec3 v = r;

    // compute filter color and total weight
    vec3 filterColor = vec3(0.0);
    float totalWeight = 0.0;
    for (uint i = 0u; i < SAMPLE_COUNT; ++i)
    {
        // generate a sample vector that's biased towards the preferred alignment direction (importance sampling)
        vec2 xi = hammersley(i, SAMPLE_COUNT);
        vec3 h = importanceSampleGGX(xi, normal, roughness);
        vec3 l = normalize(2.0 * dot(v, h) * h - v);

        // accumulate filter color and total weight
        float nDotL = max(dot(normal, l), 0.0);
        if (nDotL > 0.0)
        {
            // sample from the environment filter's mip level based on roughness / pdf
            float d = distributionGGX(normal, h, roughness);
            float nDotH = max(dot(normal, h), 0.0);
            float hDotV = max(dot(h, v), 0.0);
            float pdf = d * nDotH / (4.0 * hDotV) + 0.0001;
            float saTexel = 4.0 * PI / (6.0 * resolution * resolution);
            float saSample = 1.0 / (float(SAMPLE_COUNT) * pdf + 0.0001);
            float mipLevel = roughness == 0.0 ? 0.0 : 0.5 * log2(saSample / saTexel);
            vec3 sampleColor = textureLod(cubeMap, l, mipLevel).rgb;
            vec3 sampleScaled = sampleColor * TONE_UNMAP_SCALAR;
            vec3 sampleSquared = sampleScaled * sampleScaled;
            filterColor += sampleSquared * nDotL;
            totalWeight += nDotL;
        }
    }

    // normalize filtered color
    filterColor = filterColor / totalWeight;

    // fin
    frag = vec4(filterColor, 1.0);
}
