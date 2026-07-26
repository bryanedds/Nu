#version 450 core

// Adapted version of https://github.com/EricLengyel/Slug/blob/main/SlugVertexShader.hlsl
// Takes a unit-quad position in [0,1] range and maps it to the
// contour's bounding box via  bboxMin + position * bboxSize.
// The resulting local-space position is passed as em-space
// texcoord to the fragment shader for coverage computation.

layout(binding = 0, std140) uniform ShapeDataUniform
{
    mat4 modelViewProjection;   // mvp
    vec4 color;                 // single draw color (r, g, b, a)
    uint curveCount;            // number of quadratic bezier curves
    uint flags;                 // bit 0: fillMode (0=nonzero, 1=evenOdd)
    uint hBands;                // number of horizontal bands
    uint vBands;                // number of vertical bands
    vec2 bboxMin;               // bounding box minimum in local space
    vec2 bboxSize;              // bounding box size in local space
    vec4 bandTransform;         // (scaleX, scaleY, offsetX, offsetY) to map renderCoord -> band index
} shapeData;

layout(location = 0) in vec2 position; // unit quad: (0,0) to (1,1)

layout(location = 0) out vec2 fragTexcoord;

void main()
{
    // Map unit quad [0,1] to bounding box [bboxMin, bboxMin+bboxSize]
    vec2 localPos = position * shapeData.bboxSize + shapeData.bboxMin;
    
    // Transform to clip space
    gl_Position = shapeData.modelViewProjection * vec4(localPos, 0.0, 1.0);
    
    // Pass local-space position as em-space texcoord
    fragTexcoord = localPos;
}
