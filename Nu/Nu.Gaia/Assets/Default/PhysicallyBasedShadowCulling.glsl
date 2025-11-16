#shader compute
#version 460 core

layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

// Frustum planes in world space (equation: ax + by + cz + d = 0)
uniform vec4 frustumPlanes[6];

// Input: instance data (model matrices + bounds)
struct InstanceData
{
    mat4 model;           // 16 floats (64 bytes)
    vec3 boundsMin;       // 3 floats
    float padding0;
    vec3 boundsMax;       // 3 floats  
    float padding1;
};

layout(std430, binding = 0) readonly buffer InstanceBuffer
{
    InstanceData instances[];
};

// Output: culled instance count (first uint) and culled instance data
layout(std430, binding = 1) buffer CulledInstanceBuffer
{
    uint culledCount;
    mat4 culledModels[];
};

// Test if an AABB intersects the frustum
bool isAABBInsideFrustum(vec3 boundsMin, vec3 boundsMax, mat4 model)
{
    // Transform AABB corners by model matrix
    vec3 corners[8];
    corners[0] = (model * vec4(boundsMin.x, boundsMin.y, boundsMin.z, 1.0)).xyz;
    corners[1] = (model * vec4(boundsMax.x, boundsMin.y, boundsMin.z, 1.0)).xyz;
    corners[2] = (model * vec4(boundsMin.x, boundsMax.y, boundsMin.z, 1.0)).xyz;
    corners[3] = (model * vec4(boundsMax.x, boundsMax.y, boundsMin.z, 1.0)).xyz;
    corners[4] = (model * vec4(boundsMin.x, boundsMin.y, boundsMax.z, 1.0)).xyz;
    corners[5] = (model * vec4(boundsMax.x, boundsMin.y, boundsMax.z, 1.0)).xyz;
    corners[6] = (model * vec4(boundsMin.x, boundsMax.y, boundsMax.z, 1.0)).xyz;
    corners[7] = (model * vec4(boundsMax.x, boundsMax.y, boundsMax.z, 1.0)).xyz;
    
    // Test each frustum plane
    for (int i = 0; i < 6; i++)
    {
        bool allOutside = true;
        for (int j = 0; j < 8; j++)
        {
            float distance = dot(frustumPlanes[i].xyz, corners[j]) + frustumPlanes[i].w;
            if (distance >= 0.0)
            {
                allOutside = false;
                break;
            }
        }
        // If all corners are outside this plane, the box is outside the frustum
        if (allOutside)
            return false;
    }
    return true;
}

void main()
{
    uint instanceIndex = gl_GlobalInvocationID.x;
    
    // Bounds check
    if (instanceIndex >= instances.length())
        return;
    
    InstanceData instance = instances[instanceIndex];
    
    // Perform frustum culling
    bool visible = isAABBInsideFrustum(instance.boundsMin, instance.boundsMax, instance.model);
    
    if (visible)
    {
        // Atomically increment culled count and get the write position
        uint writeIndex = atomicAdd(culledCount, 1);
        // Write the model matrix to the output buffer
        culledModels[writeIndex] = instance.model;
    }
}
