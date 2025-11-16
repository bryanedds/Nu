#shader compute
#version 460 core

// Work group size - process 256 instances per work group
layout(local_size_x = 256, local_size_y = 1, local_size_z = 1) in;

// Frustum planes (6 planes: left, right, bottom, top, near, far)
// Each plane is represented as vec4(nx, ny, nz, d) where nx*x + ny*y + nz*z + d = 0
layout(std430, binding = 0) readonly buffer FrustumPlanes {
    vec4 frustumPlanes[6];
};

// Input: all instance data (36 floats per instance matching Constants.Render.InstanceFieldCount)
layout(std430, binding = 1) readonly buffer InputInstances {
    float inputInstanceData[];
};

// Input: bounds for each instance
// Format: vec4(centerX, centerY, centerZ, radius)
layout(std430, binding = 2) readonly buffer InstanceBounds {
    vec4 boundsData[];
};

// Input: flags for each instance (bit 0: castsShadow)
layout(std430, binding = 3) readonly buffer InstanceFlags {
    uint instanceFlags[];
};

// Output: culled instance data (same format as input, but only visible instances)
layout(std430, binding = 4) writeonly buffer OutputInstances {
    float outputInstanceData[];
};

// Output: atomic counter for number of visible instances
layout(std430, binding = 5) buffer InstanceCounter {
    uint visibleCount;
};

// Uniforms
uniform uint totalInstances;

// Test if a bounding sphere intersects the frustum
bool sphereIntersectsFrustum(vec3 center, float radius) {
    // Test against all 6 frustum planes
    for (int i = 0; i < 6; i++) {
        vec4 plane = frustumPlanes[i];
        // Distance from sphere center to plane
        float distance = dot(plane.xyz, center) + plane.w;
        // If sphere is completely outside this plane, it's not visible
        if (distance < -radius) {
            return false;
        }
    }
    return true;
}

void main() {
    uint instanceIndex = gl_GlobalInvocationID.x;
    
    // Bounds check
    if (instanceIndex >= totalInstances) {
        return;
    }
    
    // Check if this instance casts shadows (bit 0 of flags)
    uint flags = instanceFlags[instanceIndex];
    if ((flags & 1u) == 0u) {
        return;  // Doesn't cast shadows, skip
    }
    
    // Get bounding sphere data
    vec4 bounds = boundsData[instanceIndex];
    vec3 center = bounds.xyz;
    float radius = bounds.w;
    
    // Perform frustum culling
    if (sphereIntersectsFrustum(center, radius)) {
        // This instance is visible - atomically get output index
        uint outputIndex = atomicAdd(visibleCount, 1u);
        
        // Copy instance data to output buffer
        // Each instance has 36 floats (Constants.Render.InstanceFieldCount)
        uint inputOffset = instanceIndex * 36u;
        uint outputOffset = outputIndex * 36u;
        
        for (uint i = 0u; i < 36u; i++) {
            outputInstanceData[outputOffset + i] = inputInstanceData[inputOffset + i];
        }
    }
}
