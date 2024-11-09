#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;

layout(location = 0) in vec3 position;
layout(location = 6) in mat4 model;

out vec4 positionOut;
out float depthDirectionalOut;

void main()
{
	positionOut = model * vec4(position, 1.0);
	gl_Position = projection * view * positionOut;
	depthDirectionalOut = gl_Position.z / gl_Position.w;
}

#shader fragment
#version 410

uniform vec3 eyeCenter;
uniform int lightType;
uniform float lightShadowExponent;

layout(location = 0) out vec2 depths;

in vec4 positionOut;
in float depthDirectionalOut;

void main()
{
	depths.x = lightType == 0 ? length(positionOut.xyz - eyeCenter) : gl_FragCoord.z;
	depths.y = exp(lightShadowExponent * (lightType == 2 ? depthDirectionalOut : gl_FragCoord.z));
}
