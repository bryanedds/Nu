#shader vertex
#version 460 core

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

const float FXAA_SPAN_MAX = 4.0;
const float FXAA_REDUCE_MIN = 1.0 / 64.0;
const float FXAA_REDUCE_MUL = 1.0 / 4.0;

uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

void main()
{
    // compute texel size
    vec2 texelSize = 1.0 / textureSize(inputTexture, 0).xy;

    // compute luminosity values
    vec3 lum = vec3(0.299, 0.587, 0.114);
    float lumTL = dot(lum, texture(inputTexture, vec2(-1.0, -1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumTR = dot(lum, texture(inputTexture, vec2(+1.0, -1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumBL = dot(lum, texture(inputTexture, vec2(-1.0, +1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumBR = dot(lum, texture(inputTexture, vec2(+1.0, +1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumCC = dot(lum, texture(inputTexture, texCoordsOut.xy).xyz);

    // compute blur direction
    vec2 dir;
    dir.x = -((lumTL + lumTR) - (lumBL + lumBR));
    dir.y = +((lumTL + lumBL) - (lumTR + lumBR));
    float dirReduce = max((lumTL + lumTR + lumBL + lumBR) * FXAA_REDUCE_MUL * 0.25, FXAA_REDUCE_MIN);
    float inverseDirAdjustment = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), dir * inverseDirAdjustment)) * texelSize;

    // sample the texture in two locations along the computed direction to create an initial blurred color
    vec3 result1 = 0.5 * (
        texture(inputTexture, dir * vec2(1.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(2.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // sample the texture at additional points and blend them with the initial blur to refine the result
    vec3 result2 = result1 * 0.5 + 0.25 * (
        texture(inputTexture, dir * vec2(0.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(3.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // compute the minimum and maximum luminosity of the surrounding texels to use for edge detection
    float lumMin = min(lumCC, min(min(lumTL, lumTR), min(lumBL, lumBR)));
    float lumMax = max(lumCC, max(max(lumTL, lumTR), max(lumBL, lumBR)));
    float lumResult2 = dot(lum, result2);

    // write    
    frag =
        lumResult2 < lumMin || lumResult2 > lumMax ?
        vec4(result1, 1.0) :
        vec4(result2, 1.0);
}
