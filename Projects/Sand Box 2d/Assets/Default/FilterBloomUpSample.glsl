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

// This shader performs upsampling on a texture, as taken from Call Of Duty method, presented at ACM Siggraph 2014.

uniform sampler2D sourceTexture;
uniform float filterRadius;

in vec2 texCoordsOut;

layout (location = 0) out vec3 frag;

void main()
{
    // The filter kernel is applied with a radius, specified in texture
    // coordinates, so that the radius will vary across mip resolutions.
    float x = filterRadius;
    float y = filterRadius;

    // Take 9 samples around current texel:
    // a - b - c
    // d - e - f
    // g - h - i
    // === ('e' is the current texel) ===
    vec3 a = texture(sourceTexture, vec2(texCoordsOut.x - x, texCoordsOut.y + y)).rgb;
    vec3 b = texture(sourceTexture, vec2(texCoordsOut.x,     texCoordsOut.y + y)).rgb;
    vec3 c = texture(sourceTexture, vec2(texCoordsOut.x + x, texCoordsOut.y + y)).rgb;

    vec3 d = texture(sourceTexture, vec2(texCoordsOut.x - x, texCoordsOut.y)).rgb;
    vec3 e = texture(sourceTexture, vec2(texCoordsOut.x,     texCoordsOut.y)).rgb;
    vec3 f = texture(sourceTexture, vec2(texCoordsOut.x + x, texCoordsOut.y)).rgb;

    vec3 g = texture(sourceTexture, vec2(texCoordsOut.x - x, texCoordsOut.y - y)).rgb;
    vec3 h = texture(sourceTexture, vec2(texCoordsOut.x,     texCoordsOut.y - y)).rgb;
    vec3 i = texture(sourceTexture, vec2(texCoordsOut.x + x, texCoordsOut.y - y)).rgb;

    // Apply weighted distribution, by using a 3x3 tent filter:
    //  1   | 1 2 1 |
    // -- * | 2 4 2 |
    // 16   | 1 2 1 |
    frag = e*4.0;
    frag += (b+d+f+h)*2.0;
    frag += (a+c+g+i);
    frag *= 1.0 / 16.0;
}
