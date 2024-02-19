#shader vertex
#version 410

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords; 
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410
#extension GL_ARB_bindless_texture : require

const float FXAA_SPAN_MAX = 8.0;
const float FXAA_REDUCE_MIN = 1.0 / 128.0;
const float FXAA_REDUCE_MUL = 1.0 / 8.0;

layout (bindless_sampler) uniform sampler2D inputTexture;

in vec2 texCoordsOut;

out vec4 frag;

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

    // 
    vec3 result1 = 0.5 * (
        texture(inputTexture, dir * vec2(1.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(2.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // 
    vec3 result2 = result1 * 0.5 + 0.25 * (
        texture(inputTexture, dir * vec2(0.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(3.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // 
    float lumMin = min(lumCC, min(min(lumTL, lumTR), min(lumBL, lumBR)));
    float lumMax = max(lumCC, max(max(lumTL, lumTR), max(lumBL, lumBR)));
    float lumResult2 = dot(lum, result2);

    // write    
    frag =
        lumResult2 < lumMin || lumResult2 > lumMax ?
        vec4(result1, 1.0) :
        vec4(result2, 1.0);
}
