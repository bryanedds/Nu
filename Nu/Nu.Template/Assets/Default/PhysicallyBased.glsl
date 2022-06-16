#shader vertex
#version 330 core

layout(location = 0) in vec4 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec2 uv;
layout(location = 3) in vec3 binormal;
layout(location = 4) in vec3 tangent;

uniform mat4 sys_ProjectionMatrix;
uniform mat4 sys_ViewMatrix;
uniform mat4 sys_ModelMatrix;
uniform vec3 sys_CameraPosition;

uniform mat4 u_DepthBiasMVP;

out DATA
{
	vec4 position;
	vec3 normal;
	vec2 uv;
	vec3 binormal;
	vec3 tangent;
	vec3 color;
	vec4 shadowCoord;
	vec3 cameraPos;
} vs_out;

void main()
{
	vec4 pos = sys_ModelMatrix * position;
	vs_out.position = pos;
	gl_Position = sys_ProjectionMatrix * sys_ViewMatrix * pos;

	mat3 model = mat3(sys_ModelMatrix);
	vs_out.normal = model * normal;
	vs_out.binormal = model * binormal;
	vs_out.tangent = model * tangent;
	vs_out.uv = uv;
	vs_out.color = vec3(1.0);
	vs_out.cameraPos = sys_CameraPosition;

	vs_out.shadowCoord = u_DepthBiasMVP * pos;
}

#shader fragment
#version 330 core

layout(location = 0) out vec4 color;

in DATA
{
	vec4 position;
	vec3 normal;
	vec2 uv;
	vec3 binormal;
	vec3 tangent;
	vec3 color;
	vec4 shadowCoord;
	vec3 cameraPos;
} fs_in;

struct Light
{
	vec4 color;
	vec3 position;
	float p0;
	vec3 direction;
	float p1;
	vec3 lightVector;
	float intensity;
};

struct Material
{
	vec4 albedo;
	vec3 specular;
	float roughness;
	vec3 normal;
};

struct Attributes
{
	vec3 position;
	vec3 normal;
	vec3 binormal;
	vec3 tangent;
};

uniform Light sys_Light;

// PBR Inputs
uniform sampler2D u_PreintegratedFG;
uniform samplerCube u_EnvironmentMap;

// PBR Map Inputs
uniform sampler2D u_AlbedoMap;
uniform sampler2D u_SpecularMap;
uniform sampler2D u_GlossMap;
uniform sampler2D u_NormalMap;

uniform sampler2DShadow u_ShadowMap;

// PBR Static Inputs
uniform vec4 u_AlbedoColor;
uniform vec3 u_SpecularColor;
uniform float u_GlossColor;
uniform vec3 u_NormalColor;

// PBR Modes
uniform float u_UsingAlbedoMap;
uniform float u_UsingSpecularMap;
uniform float u_UsingGlossMap;
uniform float u_UsingNormalMap;

#define PI 3.1415926535897932384626433832795
#define GAMMA 2.2

vec2 poissonDisk[16] = vec2[](
	vec2(-0.94201624, -0.39906216),
	vec2(0.94558609, -0.76890725),
	vec2(-0.094184101, -0.92938870),
	vec2(0.34495938, 0.29387760),
	vec2(-0.91588581, 0.45771432),
	vec2(-0.81544232, -0.87912464),
	vec2(-0.38277543, 0.27676845),
	vec2(0.97484398, 0.75648379),
	vec2(0.44323325, -0.97511554),
	vec2(0.53742981, -0.47373420),
	vec2(-0.26496911, -0.41893023),
	vec2(0.79197514, 0.19090188),
	vec2(-0.24188840, 0.99706507),
	vec2(-0.81409955, 0.91437590),
	vec2(0.19984126, 0.78641367),
	vec2(0.14383161, -0.14100790)
	);

Attributes g_Attributes;

vec4 GammaCorrectTexture(sampler2D tex, vec2 uv)
{
	vec4 samp = texture(tex, uv);
	return vec4(pow(samp.rgb, vec3(GAMMA)), samp.a);
}

vec3 GammaCorrectTextureRGB(sampler2D tex, vec2 uv)
{
	vec4 samp = texture(tex, uv);
	return vec3(pow(samp.rgb, vec3(GAMMA)));
}

vec4 GetAlbedo()
{
	return (1.0 - u_UsingAlbedoMap) * u_AlbedoColor + u_UsingAlbedoMap * GammaCorrectTexture(u_AlbedoMap, fs_in.uv);
}

vec3 GetSpecular()
{
	return (1.0 - u_UsingSpecularMap) * u_SpecularColor + u_UsingSpecularMap * GammaCorrectTextureRGB(u_SpecularMap, fs_in.uv);
}

float GetGloss()
{
	return (1.0 - u_UsingGlossMap) * u_GlossColor + u_UsingGlossMap * GammaCorrectTextureRGB(u_GlossMap, fs_in.uv).r;
}

float GetRoughness()
{
	return 1.0 - GetGloss();
}

vec3 FinalGamma(vec3 color)
{
	return pow(color, vec3(1.0 / GAMMA));
}

float FresnelSchlick(float f0, float fd90, float view)
{
	return f0 + (fd90 - f0) * pow(max(1.0 - view, 0.1), 5.0);
}

float Disney(Light light, Material material, vec3 eye)
{
	vec3 halfVector = normalize(light.lightVector + eye);

	float NdotL = max(dot(g_Attributes.normal, light.lightVector), 0.0);
	float LdotH = max(dot(light.lightVector, halfVector), 0.0);
	float NdotV = max(dot(g_Attributes.normal, eye), 0.0);

	float energyBias = mix(0.0, 0.5, material.roughness);
	float energyFactor = mix(1.0, 1.0 / 1.51, material.roughness);
	float fd90 = energyBias + 2.0 * (LdotH * LdotH) * material.roughness;
	float f0 = 1.0;

	float lightScatter = FresnelSchlick(f0, fd90, NdotL);
	float viewScatter = FresnelSchlick(f0, fd90, NdotV);

	return lightScatter * viewScatter * energyFactor;
}

vec3 GGX(Light light, Material material, vec3 eye)
{
	vec3 h = normalize(light.lightVector + eye);
	float NdotH = max(dot(g_Attributes.normal, h), 0.0);

	float rough2 = max(material.roughness * material.roughness, 2.0e-3); // capped so spec highlights don't disappear
	float rough4 = rough2 * rough2;

	float d = (NdotH * rough4 - NdotH) * NdotH + 1.0;
	float D = rough4 / (PI * (d * d));

	// Fresnel
	vec3 reflectivity = material.specular;
	float fresnel = 1.0;
	float NdotL = clamp(dot(g_Attributes.normal, light.lightVector), 0.0, 1.0);
	float LdotH = clamp(dot(light.lightVector, h), 0.0, 1.0);
	float NdotV = clamp(dot(g_Attributes.normal, eye), 0.0, 1.0);
	vec3 F = reflectivity + (fresnel - fresnel * reflectivity) * exp2((-5.55473 * LdotH - 6.98316) * LdotH);

	// geometric / visibility
	float k = rough2 * 0.5;
	float G_SmithL = NdotL * (1.0 - k) + k;
	float G_SmithV = NdotV * (1.0 - k) + k;
	float G = 0.25 / (G_SmithL * G_SmithV);

	return G * D * F;
}

vec3 RadianceIBLIntegration(float NdotV, float roughness, vec3 specular)
{
	vec2 preintegratedFG = texture(u_PreintegratedFG, vec2(roughness, 1.0 - NdotV)).rg;
	return specular * preintegratedFG.r + preintegratedFG.g;
}

vec3 IBL(Light light, Material material, vec3 eye)
{
	float NdotV = max(dot(g_Attributes.normal, eye), 0.0);

	vec3 reflectionVector = normalize(reflect(-eye, g_Attributes.normal));
	float smoothness = 1.0 - material.roughness;
	float mipLevel = (1.0 - smoothness * smoothness) * 10.0;
	vec4 cs = textureLod(u_EnvironmentMap, reflectionVector, mipLevel);
	vec3 result = pow(cs.xyz, vec3(GAMMA)) * RadianceIBLIntegration(NdotV, material.roughness, material.specular);

	vec3 diffuseDominantDirection = g_Attributes.normal;
	float diffuseLowMip = 9.6;
	vec3 diffuseImageLighting = textureLod(u_EnvironmentMap, diffuseDominantDirection, diffuseLowMip).rgb;
	diffuseImageLighting = pow(diffuseImageLighting, vec3(GAMMA));

	return result + diffuseImageLighting * material.albedo.rgb;
}

float Diffuse(Light light, Material material, vec3 eye)
{
	return Disney(light, material, eye);
}

vec3 Specular(Light light, Material material, vec3 eye)
{
	return GGX(light, material, eye);
}

// TODO: Windowing
float Attenuate(Light light)
{
	vec3 direction = light.position - vec3(fs_in.position);
	return light.intensity * 1.0 / (dot(direction, direction) + 0.01); // TODO: 4pi
}

float random(vec3 seed, int i)
{
	vec4 seed4 = vec4(seed, i);
	float dot_product = dot(seed4, vec4(12.9898, 78.233, 45.164, 94.673));
	return fract(sin(dot_product) * 43758.5453);
}

vec3 NormalMap()
{
	mat3 toWorld = mat3(g_Attributes.binormal, g_Attributes.tangent, g_Attributes.normal);
	vec3 normalMap = texture(u_NormalMap, fs_in.uv).rgb * 2.0 - 1.0;
	normalMap = toWorld * normalMap.rgb;
	normalMap = normalize(normalMap);
	return normalMap;
}

void main()
{
	g_Attributes.position = fs_in.position.xyz;
	// Normalize inputs
	g_Attributes.normal = normalize(fs_in.normal);
	g_Attributes.tangent = normalize(fs_in.tangent);
	g_Attributes.binormal = normalize(fs_in.binormal);

	if (u_UsingNormalMap > 0.0)
		g_Attributes.normal = NormalMap();

	vec3 eye = normalize(fs_in.cameraPos - g_Attributes.position);

	Light light = sys_Light;
	light.intensity = light.intensity; // Attenuate(light);
	light.lightVector = light.direction; // normalize(light.position - vec3(fs_in.position));

	Material material;
	material.albedo = GetAlbedo();
	material.specular = GetSpecular();
	material.roughness = GetRoughness();

	vec4 diffuse = vec4(0.0);
	vec3 specular = vec3(0.0);

	// TODO: Multiple lights
	for (int i = 0; i < 1; i++)
	{
		float NdotL = clamp(dot(g_Attributes.normal, light.lightVector), 0.0, 1.0);

		// Diffuse Calculation
		diffuse += NdotL * Diffuse(light, material, eye) * light.color * light.intensity;
		// Specular Calculation
		specular += NdotL * Specular(light, material, eye) * light.color.xyz * light.intensity;
		light.intensity /= 2.0;
		light.lightVector = -light.lightVector;
	}
	// Shadows
	float bias = 0.005;
	float visibility = 1.0;
	for (int i = 0; i < 1; i++)
	{
		int index = int(16.0 * random(floor(fs_in.position.xyz * 1000.0), i)) % 16;
		visibility -= (1.0 / 4.0) * (1.0 - texture(u_ShadowMap, vec3(fs_in.shadowCoord.xy + poissonDisk[index] / 700.0, (fs_in.shadowCoord.z - bias) / fs_in.shadowCoord.w)));
	}

	vec3 finalColor = material.albedo.rgb * diffuse.rgb * visibility + (specular + IBL(light, material, eye)) * visibility;
	finalColor = FinalGamma(finalColor);
	color = vec4(finalColor, material.albedo.a);
}