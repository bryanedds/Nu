#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;

layout(location = 0) in vec3 position;
layout(location = 3) in mat4 model;

void main()
{
	vec4 positionOut = model * vec4(position, 1.0);
	gl_Position = viewProjection * positionOut;
}

#shader fragment
#version 460 core

uniform float lightShadowExponent;

layout(location = 0) out vec4 moments;

void main()
{
    float z = gl_FragCoord.z;
    float pos = exp(lightShadowExponent * z);
    float neg = -1.0 / pos;
    moments = vec4(pos, pos * pos, neg, neg * neg);
}
//0 // intentional syntax error to disable shader