using System;
using System.Collections.Generic;
using System.Numerics;
using System.Drawing;
using System.Text;

namespace Spine
{
    /// A vertex of a mesh generated from a Spine skeleton
    public struct VertexPositionColorTextureColor
    {
        public Vector2 Position;
        public uint Color;
        public float u;
        public float v;
        public uint darkColor;
    }
}
