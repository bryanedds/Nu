#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;

layout (location = 0) in vec3 position;
layout (location = 3) in mat4 model;

out float depthOut;

void main()
{
    vec4 positionWorld = model * vec4(position, 1.0);
    gl_Position = projection * view * positionWorld;
	depthOut = gl_Position.z / gl_Position.w;
}

#shader fragment
#version 410

uniform float lightShadowExponent;

layout (location = 0) out vec2 depths;

in float depthOut;

void main()
{
	float depthExp = exp(lightShadowExponent * depthOut);
	depths.x = gl_FragCoord.z;
	depths.y = depthExp;
}
