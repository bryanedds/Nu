#shader vertex
#version 460 core

// FXAA algorithm from NVIDIA, C# implementation by Jasper Flick, GLSL port by Dave Hoskins.
//
// References:
// 
// http://developer.download.nvidia.com/assets/gamedev/files/sdk/11/FXAA_WhitePaper.pdf
// https://catlikecoding.com/unity/tutorials/advanced-rendering/fxaa/

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

#define EDGE_STEP_COUNT 6
#define EDGE_GUESS 8.0
#define EDGE_STEPS 1.0, 1.5, 2.0, 2.0, 2.0, 4.0

const float EdgeSteps[EDGE_STEP_COUNT] = float[EDGE_STEP_COUNT](EDGE_STEPS);
const float ContrastThreshold = 0.0312;
const float RelativeThreshold = 0.063;
const float SubpixelBlending = 1.0;

struct LuminanceData
{
	float m, n, e, s, w;
	float ne, nw, se, sw;
	float highest, lowest, contrast;
};

struct EdgeData
{
	bool isHorizontal;
	float pixelStep;
	float oppositeLuminance, gradient;
};

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

uniform sampler2D inputTexture;

float sampleLuminance(sampler2D tex2D, vec2 uv)
{
	return dot(texture(tex2D, uv).rgb, vec3(0.3, 0.59, 0.11));
}

float sampleLuminance(sampler2D tex2D, vec2 texSize, vec2 uv, float uOffset, float vOffset)
{
	uv += texSize * vec2(uOffset, vOffset);
	return sampleLuminance(tex2D, uv);
}

LuminanceData sampleLuminanceNeighborhood(sampler2D tex2D, vec2 texSize, vec2 uv)
{
	LuminanceData l;
	l.m = sampleLuminance(tex2D, uv);
	l.n = sampleLuminance(tex2D, texSize, uv,  0.0,  1.0);
	l.e = sampleLuminance(tex2D, texSize, uv,  1.0,  0.0);
	l.s = sampleLuminance(tex2D, texSize, uv,  0.0, -1.0);
	l.w = sampleLuminance(tex2D, texSize, uv, -1.0,  0.0);
	l.ne = sampleLuminance(tex2D, texSize, uv,  1.0,  1.0);
	l.nw = sampleLuminance(tex2D, texSize, uv, -1.0,  1.0);
	l.se = sampleLuminance(tex2D, texSize, uv,  1.0, -1.0);
	l.sw = sampleLuminance(tex2D, texSize, uv, -1.0, -1.0);
	l.highest = max(max(max(max(l.n, l.e), l.s), l.w), l.m);
	l.lowest = min(min(min(min(l.n, l.e), l.s), l.w), l.m);
	l.contrast = l.highest - l.lowest;
	return l;
}

bool shouldSkipPixel(LuminanceData l)
{
	float threshold = max(ContrastThreshold, RelativeThreshold * l.highest);
	return l.contrast < threshold;
}

float determinePixelBlendFactor(LuminanceData l)
{
	float f = 2.0 * (l.n + l.e + l.s + l.w);
	f += l.ne + l.nw + l.se + l.sw;
	f *= 1.0 / 12.0;
	f = abs(f - l.m);
	f = clamp(f / l.contrast, 0.0, 1.0);
	float blendFactor = smoothstep(0.0, 1.0, f);
	return blendFactor * blendFactor * SubpixelBlending;
}

EdgeData determineEdge(vec2 texSize, LuminanceData l)
{
	EdgeData e;
	float horizontal =
		abs(l.n + l.s - 2.0 * l.m) * 2.0 +
		abs(l.ne + l.se - 2.0 * l.e) +
		abs(l.nw + l.sw - 2.0 * l.w);
	float vertical =
		abs(l.e + l.w - 2.0 * l.m) * 2.0 +
		abs(l.ne + l.nw - 2.0 * l.n) +
		abs(l.se + l.sw - 2.0 * l.s);
	e.isHorizontal = horizontal >= vertical;

	float pLuminance = e.isHorizontal ? l.n : l.e;
	float nLuminance = e.isHorizontal ? l.s : l.w;
	float pGradient = abs(pLuminance - l.m);
	float nGradient = abs(nLuminance - l.m);

	e.pixelStep = e.isHorizontal ? texSize.y : texSize.x;

	if (pGradient < nGradient)
	{
		e.pixelStep = -e.pixelStep;
		e.oppositeLuminance = nLuminance;
		e.gradient = nGradient;
	}
	else
	{
		e.oppositeLuminance = pLuminance;
		e.gradient = pGradient;
	}

	return e;
}

float determineEdgeBlendFactor(sampler2D  tex2D, vec2 texSize, LuminanceData l, EdgeData e, vec2 uv)
{
	vec2 uvEdge = uv;
	vec2 edgeStep;
	if (e.isHorizontal)
	{
		uvEdge.y += e.pixelStep * 0.5;
		edgeStep = vec2(texSize.x, 0.0);
	}
	else
	{
		uvEdge.x += e.pixelStep * 0.5;
		edgeStep = vec2(0.0, texSize.y);
	}

	float edgeLuminance = (l.m + e.oppositeLuminance) * 0.5;
	float gradientThreshold = e.gradient * 0.25;

	vec2 puv = uvEdge + edgeStep * EdgeSteps[0];
	float pLuminanceDelta = sampleLuminance(tex2D, puv) - edgeLuminance;
	bool pAtEnd = abs(pLuminanceDelta) >= gradientThreshold;

	for (int i = 1; i < EDGE_STEP_COUNT && !pAtEnd; i++)
	{
		puv += edgeStep * EdgeSteps[i];
		pLuminanceDelta = sampleLuminance(tex2D, puv) - edgeLuminance;
		pAtEnd = abs(pLuminanceDelta) >= gradientThreshold;
	}

	if (!pAtEnd) puv += edgeStep * EDGE_GUESS;

	vec2 nuv = uvEdge - edgeStep * EdgeSteps[0];
	float nLuminanceDelta = sampleLuminance(tex2D, nuv) - edgeLuminance;
	bool nAtEnd = abs(nLuminanceDelta) >= gradientThreshold;

	for (int i = 1; i < EDGE_STEP_COUNT && !nAtEnd; i++)
	{
		nuv -= edgeStep * EdgeSteps[i];
		nLuminanceDelta = sampleLuminance(tex2D, nuv) - edgeLuminance;
		nAtEnd = abs(nLuminanceDelta) >= gradientThreshold;
	}

	if (!nAtEnd) nuv -= edgeStep * EDGE_GUESS;

	float pDistance, nDistance;
	if (e.isHorizontal)
	{
		pDistance = puv.x - uv.x;
		nDistance = uv.x - nuv.x;
	}
	else
	{
		pDistance = puv.y - uv.y;
		nDistance = uv.y - nuv.y;
	}

	float shortestDistance;
	bool deltaSign;
	if (pDistance <= nDistance)
	{
		shortestDistance = pDistance;
		deltaSign = pLuminanceDelta >= 0.0;
	}
	else
	{
		shortestDistance = nDistance;
		deltaSign = nLuminanceDelta >= 0.0;
	}

	if (deltaSign == (l.m - edgeLuminance >= 0.0)) return 0.0;

	return 0.5 - shortestDistance / (pDistance + nDistance);
}

vec4 applyFxaa(sampler2D tex2D, vec2 texSize, vec2 uv)
{
	LuminanceData luminance = sampleLuminanceNeighborhood(tex2D, texSize, uv);
	if (shouldSkipPixel(luminance)) return texture(tex2D, uv);

	float pixelBlend = determinePixelBlendFactor(luminance);
	EdgeData edge = determineEdge(texSize, luminance);
	float edgeBlend = determineEdgeBlendFactor(tex2D, texSize, luminance, edge, uv);
	float finalBlend = max(pixelBlend, edgeBlend);

	if (edge.isHorizontal) uv.y += edge.pixelStep * finalBlend;
	else uv.x += edge.pixelStep * finalBlend;

	return texture(tex2D, uv);
}

void main()
{
	frag = applyFxaa(inputTexture, vec2(1.0) / textureSize(inputTexture, 0).xy, texCoordsOut);
}
