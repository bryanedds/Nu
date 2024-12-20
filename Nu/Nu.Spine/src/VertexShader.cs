using System;
using System.Numerics;

namespace Spine
{
    static class VertexShader
    {
        public static string vertexShader =
@"
#version 330 core
layout(location = 0) in vec2 aPos;
layout(location = 1) in vec4 aLightColor;
layout(location = 2) in vec2 aTexCoord;
layout(location = 3) in vec4 aDarkColor;

uniform mat4 uMatrix;

out vec4 lightColor;
out vec4 darkColor;
out vec2 texCoord;

void main()
{
    lightColor = aLightColor;
    darkColor = aDarkColor;
    texCoord = aTexCoord;
    gl_Position = uMatrix * vec4(aPos, 0.0, 1.0);
}
";

        public static string fragmentShader =
@"
#version 330 core
in vec4 lightColor;
in vec4 darkColor;
in vec2 texCoord;
out vec4 fragColor;

uniform sampler2D uTexture;

void main()
{
    vec4 texColor = texture(uTexture, texCoord);
    float alpha = texColor.a * lightColor.a;
    fragColor.a = alpha;
    fragColor.rgb = ((texColor.a - 1.0) * darkColor.a + 1.0 - texColor.rgb) * darkColor.rgb + texColor.rgb * lightColor.rgb;
}
";
    }

    /// A vertex of a mesh generated from a Spine skeleton
    public struct Vertex
    {
        public Vector2 Position;
        public uint Color;
        public Vector2 TextureCoordinate;
        public uint Color2;
    }
}