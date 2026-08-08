#version 450 core

// Slug analytic coverage fragment shader.
// Faithful port of https://github.com/EricLengyel/Slug/blob/main/SlugPixelShader.hlsl
//   - 0x2E74 lookup table for root eligibility
//   - Dual horizontal + vertical ray coverage
//   - Band-based curve culling with early exit
//   - Reference epsilon 1.0/65536.0 for nearly-linear curves
//   - Even-odd / nonzero fill rule from CPU flag
//
// Stroke is rendered as a separate Slug contour pass; no per-pixel
// distance approximation is used here.

layout(binding = 0, std140) uniform ShapeDataUniform
{
    mat4 modelViewProjection;
    vec4 color;             // single draw color (r, g, b, a)
    uint curveCount;        // number of quadratic bezier curves
    uint flags;             // bit 0: fillMode (0=nonzero, 1=evenOdd)
    uint hBands;            // number of horizontal bands
    uint vBands;            // number of vertical bands
    vec2 bboxMin;           // bounding box minimum in local space
    vec2 bboxSize;          // bounding box size in local space
    vec4 bandTransform;     // (scaleX, scaleY, offsetX, offsetY) -> band index
} shapeData;

layout(binding = 1, std430) readonly buffer CurveDataUniform
{
    vec4 curves[];
};

// Band data SSBO is a flat uint array.
// Layout:
//   For band b in [0, hBands):   entry[2*b]     = curveCount, entry[2*b+1]     = curveOffset
//   For band b in [0, vBands):   entry[2*(hBands+b)]   = curveCount, entry[2*(hBands+b)+1] = curveOffset
//   After all entries: flat uint[] of curve indices.
layout(binding = 2, std430) readonly buffer BandDataBuffer
{
    uint bandData[];
};

layout(location = 0) in vec2 fragTexcoord;

layout(location = 0) out vec4 fragColor;

// ---- Constants ----

// Reference epsilon from SlugPixelShader.hlsl.
const float kSlugEpsilon = 1.0 / 65536.0;
// Small epsilon to avoid division by zero in derivative guard.
const float kDerivEpsilon = 1.0e-10;

// ---- Root eligibility (0x2E74 lookup) ----

// Reference: CalcRootCode in SlugPixelShader.hlsl
uint calcRootCode(float y1, float y2, float y3)
{
    uint i1 = floatBitsToUint(y1) >> 31u;
    uint i2 = floatBitsToUint(y2) >> 30u;
    uint i3 = floatBitsToUint(y3) >> 29u;

    uint shift = (i2 & 2u) | (i1 & ~2u);
    shift = (i3 & 4u) | (shift & ~4u);

    return (0x2E74u >> int(shift)) & 0x0101u;
}

// ---- Ray solving ----

// Reference: SolveHorizPoly in SlugPixelShader.hlsl.
// Solve a t^2 - 2b t + c = 0 for y-crossings, return x at those t values.
vec2 solveHorizRay(vec4 p12, vec2 p3)
{
    vec2 a = p12.xy - p12.zw * 2.0 + p3;
    vec2 b = p12.xy - p12.zw;

    float ra = 1.0 / a.y;
    float rb = 0.5 / b.y;

    float d = sqrt(max(b.y * b.y - a.y * p12.y, 0.0));
    float t1 = (b.y - d) * ra;
    float t2 = (b.y + d) * ra;

    if (abs(a.y) < kSlugEpsilon)
        t1 = t2 = p12.y * rb;

    return vec2((a.x * t1 - b.x * 2.0) * t1 + p12.x,
                (a.x * t2 - b.x * 2.0) * t2 + p12.x);
}

// Reference: SolveVertPoly in SlugPixelShader.hlsl.
// Solve a t^2 - 2b t + c = 0 for x-crossings, return y at those t values.
vec2 solveVertRay(vec4 p12, vec2 p3)
{
    vec2 a = p12.xy - p12.zw * 2.0 + p3;
    vec2 b = p12.xy - p12.zw;

    float ra = 1.0 / a.x;
    float rb = 0.5 / b.x;

    float d = sqrt(max(b.x * b.x - a.x * p12.x, 0.0));
    float t1 = (b.x - d) * ra;
    float t2 = (b.x + d) * ra;

    if (abs(a.x) < kSlugEpsilon)
        t1 = t2 = p12.x * rb;

    return vec2((a.y * t1 - b.y * 2.0) * t1 + p12.y,
                (a.y * t2 - b.y * 2.0) * t2 + p12.y);
}

// ---- Band-accelerated coverage accumulation ----

// Compute the offset into bandData[] where the flat curve-index array starts.
uint bandIndexArrayOffset()
{
    return (shapeData.hBands + shapeData.vBands) * 2u;
}

// Accumulate horizontal ray coverage using band culling.
// Returns vec2(xcov, xwgt).
vec2 accumulateHorizCoverage(vec2 texcoord, float pixelsPerEmX)
{
    // Determine which horizontal band this pixel falls in.
    int bandI = clamp(int(texcoord.y * shapeData.bandTransform.y + shapeData.bandTransform.w),
                      0, int(shapeData.hBands) - 1);

    uint entryBase = uint(bandI) * 2u;
    uint hCount = bandData[entryBase];
    uint hOffset = bandData[entryBase + 1u];
    uint indexBase = bandIndexArrayOffset() + hOffset;

    float xcov = 0.0;
    float xwgt = 0.0;

    for (uint i = 0u; i < hCount; i++)
    {
        uint ci = bandData[indexBase + i];
        vec4 p12 = curves[ci * 2u] - vec4(texcoord, texcoord);
        vec2 p3 = curves[ci * 2u + 1u].xy - texcoord;

        // Early exit: curves within a band are sorted descending by max x.
        // If the largest x of this curve is left of the pixel, remaining curves
        // in this band are also left and cannot contribute.
        if (max(max(p12.x, p12.z), p3.x) * pixelsPerEmX < -0.5) break;

        uint code = calcRootCode(p12.y, p12.w, p3.y);
        if (code != 0u)
        {
            vec2 r = solveHorizRay(p12, p3) * pixelsPerEmX;

            if ((code & 1u) != 0u)
            {
                xcov += clamp(r.x + 0.5, 0.0, 1.0);
                xwgt = max(xwgt, clamp(1.0 - abs(r.x) * 2.0, 0.0, 1.0));
            }
            if (code > 1u) // bit 8 set (code & 0x100u)
            {
                xcov -= clamp(r.y + 0.5, 0.0, 1.0);
                xwgt = max(xwgt, clamp(1.0 - abs(r.y) * 2.0, 0.0, 1.0));
            }
        }
    }

    return vec2(xcov, xwgt);
}

// Accumulate vertical ray coverage using band culling.
// Returns vec2(ycov, ywgt).
vec2 accumulateVertCoverage(vec2 texcoord, float pixelsPerEmY)
{
    // Determine which vertical band this pixel falls in.
    int bandI = clamp(int(texcoord.x * shapeData.bandTransform.x + shapeData.bandTransform.z),
                      0, int(shapeData.vBands) - 1);

    // Vertical band entries start after the hBands horizontal entries.
    uint entryBase = (shapeData.hBands + uint(bandI)) * 2u;
    uint vCount = bandData[entryBase];
    uint vOffset = bandData[entryBase + 1u];
    uint indexBase = bandIndexArrayOffset() + vOffset;

    float ycov = 0.0;
    float ywgt = 0.0;

    for (uint i = 0u; i < vCount; i++)
    {
        uint ci = bandData[indexBase + i];
        vec4 p12 = curves[ci * 2u] - vec4(texcoord, texcoord);
        vec2 p3 = curves[ci * 2u + 1u].xy - texcoord;

        // Early exit: curves within a vertical band are sorted descending by max y.
        if (max(max(p12.y, p12.w), p3.y) * pixelsPerEmY < -0.5) break;

        uint code = calcRootCode(p12.x, p12.z, p3.x);
        if (code != 0u)
        {
            vec2 r = solveVertRay(p12, p3) * pixelsPerEmY;

            if ((code & 1u) != 0u)
            {
                ycov -= clamp(r.x + 0.5, 0.0, 1.0);
                ywgt = max(ywgt, clamp(1.0 - abs(r.x) * 2.0, 0.0, 1.0));
            }
            if (code > 1u) // bit 8 set
            {
                ycov += clamp(r.y + 0.5, 0.0, 1.0);
                ywgt = max(ywgt, clamp(1.0 - abs(r.y) * 2.0, 0.0, 1.0));
            }
        }
    }

    return vec2(ycov, ywgt);
}

// ---- Coverage combination (reference: CalcCoverage) ----

float combineCoverage(float xcov, float ycov, float xwgt, float ywgt)
{
    float coverage = max(abs(xcov * xwgt + ycov * ywgt) / max(xwgt + ywgt, kSlugEpsilon),
                         min(abs(xcov), abs(ycov)));

    if ((shapeData.flags & 1u) != 0u)
    {
        // Even-odd fill rule (from Slug: E flag in tex.w).
        coverage = 1.0 - abs(1.0 - fract(coverage * 0.5) * 2.0);
    }
    else
    {
        // Non-zero fill rule.
        coverage = clamp(coverage, 0.0, 1.0);
    }

    return coverage;
}

// ---- Main ----

void main()
{
    // Pixel size in em-space from derivatives.
    vec2 emsPerPixel = fwidth(fragTexcoord);
    vec2 pixelsPerEm = 1.0 / max(emsPerPixel, kDerivEpsilon);

    // Fill coverage via Slug band-accelerated ray casting.
    vec2 h = accumulateHorizCoverage(fragTexcoord, pixelsPerEm.x);
    vec2 v = accumulateVertCoverage(fragTexcoord, pixelsPerEm.y);

    float coverage = combineCoverage(h.x, v.x, h.y, v.y);
    fragColor = shapeData.color * coverage;
}
