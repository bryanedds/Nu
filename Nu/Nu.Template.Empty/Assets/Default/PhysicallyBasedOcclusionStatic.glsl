#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 3) in mat4 model;

out vec4 positionOut;
out vec2 texCoordsOut;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    gl_Position = projection * view * positionWorld;
}

#shader fragment
#version 410

void main()
{
    // discard if depth out of range
    float depthCutoff = heightPlusOut.z;
    if (depthCutoff >= 0.0)
    {
        if (gl_FragCoord.z / gl_FragCoord.w > depthCutoff) discard;
    }
    else
    {
        if (gl_FragCoord.z / gl_FragCoord.w <= -depthCutoff) discard;
    }
}
