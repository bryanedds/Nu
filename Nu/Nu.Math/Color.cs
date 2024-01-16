using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// A floating point color.
    /// Copied from - https://github.com/opentk/opentk/blob/opentk5.0/src/OpenTK.Mathematics/Colors/Color4.cs
    /// Modified to provide RGBA format.
    /// </summary>
    public struct Color : IEquatable<Color>
    {
        /// <summary>
        /// The red component.
        /// </summary>
        public float R;

        /// <summary>
        /// The blue component.
        /// </summary>
        public float G;

        /// <summary>
        /// The green component.
        /// </summary>
        public float B;

        /// <summary>
        /// The alpha component.
        /// </summary>
        public float A;

        /// <summary>
        /// The red component as a byte.
        /// </summary>
        public byte R8 => (byte)(R * 255.0f);

        /// <summary>
        /// The green component as a byte.
        /// </summary>
        public byte G8 => (byte)(G * 255.0f);

        /// <summary>
        /// The blue component as a byte.
        /// </summary>
        public byte B8 => (byte)(B * 255.0f);

        /// <summary>
        /// The alpha component as a byte.
        /// </summary>
        public byte A8 => (byte)(A * 255.0f);

        /// <summary>
        /// Instantiates a new <see cref="Vector4"/> from the R, G, B and A components of a <see cref="Color"/>.
        /// </summary>
        /// <param name="col">The color which the instantiated vector will use the X,Y,Z and W from, in that order.</param>
        /// <returns>The vector that was instantiated.</returns>
        public Vector4 Vector4 => new Vector4(R, G, B, A);

        /// <summary>
        /// The packed value in RGBA order.
        /// </summary>
        public uint Rgba =>
            ((uint)(R * 255.0f)) << 24 |
            ((uint)(G * 255.0f)) << 16 |
            ((uint)(B * 255.0f)) << 8 |
            ((uint)(A * 255.0f));

        /// <summary>
        /// The packed value in BGRA order.
        /// </summary>
        public uint Bgra =>
            ((uint)(B * 255.0f)) << 24 |
            ((uint)(G * 255.0f)) << 16 |
            ((uint)(R * 255.0f)) << 8 |
            ((uint)(A * 255.0f));

        /// <summary>
        /// The packed value in ARGB order.
        /// </summary>
        public uint Argb =>
            ((uint)(A * 255.0f)) << 24 |
            ((uint)(R * 255.0f)) << 16 |
            ((uint)(G * 255.0f)) << 8 |
            ((uint)(B * 255.0f));

        /// <summary>
        /// The packed value in ABGR order.
        /// </summary>
        public uint Abgr =>
            ((uint)(A * 255.0f)) << 24 |
            ((uint)(B * 255.0f)) << 16 |
            ((uint)(G * 255.0f)) << 8 |
            ((uint)(R * 255.0f));

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct.
        /// </summary>
        public Color(float r, float g, float b, float a)
        {
            R = r;
            G = g;
            B = b;
            A = a;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct from a Vector4.
        /// </summary>
        public Color(Vector4 v)
		{
            R = v.X;
            G = v.Y;
            B = v.Z;
            A = v.W;
		}

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct from byte values.
        /// </summary>
        public Color(byte r, byte g, byte b, byte a)
        {
            R = r / 255.0f;
            G = g / 255.0f;
            B = b / 255.0f;
            A = a / 255.0f;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct from a packed byte value.
        /// </summary>
        /// <param name="packed"></param>
        public Color(uint packed) :
            this(
                (byte)(packed >> 24),
                (byte)(packed >> 16),
                (byte)(packed >> 8),
                (byte)packed)
		{
            // nothing to do.
		}

        /// <summary>
        /// Compares whether this Color structure is equal to the specified Color.
        /// </summary>
        /// <param name="other">The Color structure to compare to.</param>
        /// <returns>True if both Color structures contain the same components; false otherwise.</returns>
        public bool Equals(Color other)
        {
            return R.Equals(other.R) && G.Equals(other.G) && B.Equals(other.B) && A.Equals(other.A);
        }

        /// <summary>
        /// Multiplies a color by a scalar.
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        public static Color Multiply(Color color, float scale)
        {
            return color * scale;
        }

        /// <summary>
        /// Multiplies a color by the components of a color (scale).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static Color Multiply(Color color, Color scale)
        {
            Color result;
            result.R = color.R * scale.R;
            result.G = color.G * scale.G;
            result.B = color.B * scale.B;
            result.A = color.A * scale.A;
            return result;
        }

        /// <summary>
        /// Divides a color by a scalar.
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        public static Color Divide(Color color, float scale)
        {
            return color / scale;
        }

        /// <summary>
        /// Divides a color by the components of a color (scale).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static Color Divide(Color color, Color scale)
        {
            Color result;
            result.R = color.R / scale.R;
            result.G = color.G / scale.G;
            result.B = color.B / scale.B;
            result.A = color.A / scale.A;
            return result;
        }

        /// <summary>
        /// Adds two instances.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>The result of the calculation.</returns>
        public static Color operator +(Color left, Color right)
        {
            left.R += right.R;
            left.G += right.G;
            left.B += right.B;
            left.A += right.A;
            return left;
        }

        /// <summary>
        /// Subtracts two instances.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>The result of the calculation.</returns>
        public static Color operator -(Color left, Color right)
        {
            left.R -= right.R;
            left.G -= right.G;
            left.B -= right.B;
            left.A -= right.A;
            return left;
        }

        /// <summary>
        /// Multiplies an instance by an integer scalar.
        /// </summary>
        /// <param name="color">The instance.</param>
        /// <param name="scale">The scalar.</param>
        /// <returns>The result of the calculation.</returns>
        public static Color operator *(Color color, float scale)
        {
            color.R *= scale;
            color.G *= scale;
            color.B *= scale;
            color.A *= scale;
            return color;
        }

        /// <summary>
        /// Multiplies an instance by an integer scalar.
        /// </summary>
        /// <param name="scale">The scalar.</param>
        /// <param name="color">The instance.</param>
        /// <returns>The result of the calculation.</returns>
        public static Color operator *(float scale, Color color)
        {
            color.R *= scale;
            color.G *= scale;
            color.B *= scale;
            color.A *= scale;
            return color;
        }

        /// <summary>
        /// Component-wise multiplication between the specified instance by a scale color.
        /// </summary>
        /// <param name="scale">Left operand.</param>
        /// <param name="color">Right operand.</param>
        /// <returns>Result of multiplication.</returns>
        public static Color operator *(Color color, Color scale)
        {
            color.R *= scale.R;
            color.G *= scale.G;
            color.B *= scale.B;
            color.A *= scale.A;
            return color;
        }

        /// <summary>
        /// Divides the instance by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">The instance.</param>
        /// <param name="scale">The scalar.</param>
        /// <returns>The result of the calculation.</returns>
        public static Color operator /(Color color, float scale)
        {
            color.R /= scale;
            color.G /= scale;
            color.B /= scale;
            color.A /= scale;
            return color;
        }

        /// <summary>
        /// Compares two instances for equality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left equals right; false otherwise.</returns>
        public static bool operator ==(Color left, Color right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Compares two instances for inequality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left does not equal right; false otherwise.</returns>
        public static bool operator !=(Color left, Color right)
        {
            return !left.Equals(right);
        }

        /// <summary>
        /// Creates a <see cref="string"/> that describes this Color structure.
        /// </summary>
        /// <returns>A <see cref="string"/> that describes this Color structure.</returns>
        public override string ToString()
        {
            return $"<{R}, {G}, {B}, {A}>";
        }

        /// <summary>
        /// Compares whether this Color structure is equal to the specified object.
        /// </summary>
        /// <param name="obj">An object to compare to.</param>
        /// <returns>True obj is a Color structure with the same components as this Color; false otherwise.</returns>
        public override bool Equals(object obj)
        {
            return obj is Color other && Equals(other);
        }

        /// <summary>
        /// Calculates the hash code for this Color structure.
        /// </summary>
        /// <returns>A System.Int32 containing the hashcode of this Color structure.</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = R.GetHashCode();
                hashCode = (hashCode * 397) ^ G.GetHashCode();
                hashCode = (hashCode * 397) ^ B.GetHashCode();
                hashCode = (hashCode * 397) ^ A.GetHashCode();
                return hashCode;
            }
        }

        /// <summary>
        /// The Zero color.
        /// </summary>
        public static Color Zero => new Color(0.0f, 0.0f, 0.0f, 0.0f);

        /// <summary>
        /// The Unit color.
        /// </summary>
        public static Color One => new Color(1f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color alice blue corresponding to 240 248 255 255 (0.9411765, 0.972549, 1, 1).
        /// </summary>
        public static Color AliceBlue => new Color(0.9411765f, 0.972549f, 1f, 1f);

        /// <summary>
        /// Gets system color antique white corresponding to 250 235 215 255 (0.98039216, 0.92156863, 0.84313726, 1).
        /// </summary>
        public static Color AntiqueWhite => new Color(0.98039216f, 0.92156863f, 0.84313726f, 1f);

        /// <summary>
        /// Gets system color aqua corresponding to 0 255 255 255 (0, 1, 1, 1).
        /// </summary>
        public static Color Aqua => new Color(0f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color aquamarine corresponding to 127 255 212 255 (0.49803922, 1, 0.83137256, 1).
        /// </summary>
        public static Color Aquamarine => new Color(0.49803922f, 1f, 0.83137256f, 1f);

        /// <summary>
        /// Gets system color azure corresponding to 240 255 255 255 (0.9411765, 1, 1, 1).
        /// </summary>
        public static Color Azure => new Color(0.9411765f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color beige corresponding to 245 245 220 255 (0.9607843, 0.9607843, 0.8627451, 1).
        /// </summary>
        public static Color Beige => new Color(0.9607843f, 0.9607843f, 0.8627451f, 1f);

        /// <summary>
        /// Gets system color bisque corresponding to 255 228 196 255 (1, 0.89411765, 0.76862746, 1).
        /// </summary>
        public static Color Bisque => new Color(1f, 0.89411765f, 0.76862746f, 1f);

        /// <summary>
        /// Gets system color black corresponding to 0 0 0 255 (0, 0, 0, 1).
        /// </summary>
        public static Color Black => new Color(0f, 0f, 0f, 1f);

        /// <summary>
        /// Gets system color blanched almond corresponding to 255 235 205 255 (1, 0.92156863, 0.8039216, 1).
        /// </summary>
        public static Color BlanchedAlmond => new Color(1f, 0.92156863f, 0.8039216f, 1f);

        /// <summary>
        /// Gets system color blue corresponding to 0 0 255 255 (0, 0, 1, 1).
        /// </summary>
        public static Color Blue => new Color(0f, 0f, 1f, 1f);

        /// <summary>
        /// Gets system color blue violet corresponding to 138 43 226 255 (0.5411765, 0.16862746, 0.8862745, 1).
        /// </summary>
        public static Color BlueViolet => new Color(0.5411765f, 0.16862746f, 0.8862745f, 1f);

        /// <summary>
        /// Gets system color brown corresponding to 165 42 42 255 (0.64705884, 0.16470589, 0.16470589, 1).
        /// </summary>
        public static Color Brown => new Color(0.64705884f, 0.16470589f, 0.16470589f, 1f);

        /// <summary>
        /// Gets system color burlywood corresponding to 222 184 135 255 (0.87058824, 0.72156864, 0.5294118, 1).
        /// </summary>
        public static Color Burlywood => new Color(0.87058824f, 0.72156864f, 0.5294118f, 1f);

        /// <summary>
        /// Gets system color cadet blue corresponding to 95 158 160 255 (0.37254903, 0.61960787, 0.627451, 1).
        /// </summary>
        public static Color CadetBlue => new Color(0.37254903f, 0.61960787f, 0.627451f, 1f);

        /// <summary>
        /// Gets system color chartreuse corresponding to 127 255 0 255 (0.49803922, 1, 0, 1).
        /// </summary>
        public static Color Chartreuse => new Color(0.49803922f, 1f, 0f, 1f);

        /// <summary>
        /// Gets system color chocolate corresponding to 210 105 30 255 (0.8235294, 0.4117647, 0.11764706, 1).
        /// </summary>
        public static Color Chocolate => new Color(0.8235294f, 0.4117647f, 0.11764706f, 1f);

        /// <summary>
        /// Gets system color coral corresponding to 255 127 80 255 (1, 0.49803922, 0.3137255, 1).
        /// </summary>
        public static Color Coral => new Color(1f, 0.49803922f, 0.3137255f, 1f);

        /// <summary>
        /// Gets system color cornflower blue corresponding to 100 149 237 255 (0.39215687, 0.58431375, 0.92941177, 1).
        /// </summary>
        public static Color CornflowerBlue => new Color(0.39215687f, 0.58431375f, 0.92941177f, 1f);

        /// <summary>
        /// Gets system color cornsilk corresponding to 255 248 220 255 (1, 0.972549, 0.8627451, 1).
        /// </summary>
        public static Color Cornsilk => new Color(1f, 0.972549f, 0.8627451f, 1f);

        /// <summary>
        /// Gets system color crimson corresponding to 220 20 60 255 (0.8627451, 0.078431375, 0.23529412, 1).
        /// </summary>
        public static Color Crimson => new Color(0.8627451f, 0.078431375f, 0.23529412f, 1f);

        /// <summary>
        /// Gets system color cyan corresponding to 0 255 255 255 (0, 1, 1, 1).
        /// </summary>
        public static Color Cyan => new Color(0f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color dark blue corresponding to 0 0 139 255 (0, 0, 0.54509807, 1).
        /// </summary>
        public static Color DarkBlue => new Color(0f, 0f, 0.54509807f, 1f);

        /// <summary>
        /// Gets system color dark cyan corresponding to 0 139 139 255 (0, 0.54509807, 0.54509807, 1).
        /// </summary>
        public static Color DarkCyan => new Color(0f, 0.54509807f, 0.54509807f, 1f);

        /// <summary>
        /// Gets system color dark goldenrod corresponding to 184 134 11 255 (0.72156864, 0.5254902, 0.043137256, 1).
        /// </summary>
        public static Color DarkGoldenrod => new Color(0.72156864f, 0.5254902f, 0.043137256f, 1f);

        /// <summary>
        /// Gets system color dark gray corresponding to 169 169 169 255 (0.6627451, 0.6627451, 0.6627451, 1).
        /// </summary>
        public static Color DarkGray => new Color(0.6627451f, 0.6627451f, 0.6627451f, 1f);

        /// <summary>
        /// Gets system color dark green corresponding to 0 100 0 255 (0, 0.39215687, 0, 1).
        /// </summary>
        public static Color DarkGreen => new Color(0f, 0.39215687f, 0f, 1f);

        /// <summary>
        /// Gets system color dark khaki corresponding to 189 183 107 255 (0.7411765, 0.7176471, 0.41960785, 1).
        /// </summary>
        public static Color DarkKhaki => new Color(0.7411765f, 0.7176471f, 0.41960785f, 1f);

        /// <summary>
        /// Gets system color dark magenta corresponding to 139 0 139 255 (0.54509807, 0, 0.54509807, 1).
        /// </summary>
        public static Color DarkMagenta => new Color(0.54509807f, 0f, 0.54509807f, 1f);

        /// <summary>
        /// Gets system color dark olive green corresponding to 85 107 47 255 (0.33333334, 0.41960785, 0.18431373, 1).
        /// </summary>
        public static Color DarkOliveGreen => new Color(0.33333334f, 0.41960785f, 0.18431373f, 1f);

        /// <summary>
        /// Gets system color dark orange corresponding to 255 140 0 255 (1, 0.54901963, 0, 1).
        /// </summary>
        public static Color DarkOrange => new Color(1f, 0.54901963f, 0f, 1f);

        /// <summary>
        /// Gets system color dark orchid corresponding to 153 50 204 255 (0.6, 0.19607843, 0.8, 1).
        /// </summary>
        public static Color DarkOrchid => new Color(0.6f, 0.19607843f, 0.8f, 1f);

        /// <summary>
        /// Gets system color dark red corresponding to 139 0 0 255 (0.54509807, 0, 0, 1).
        /// </summary>
        public static Color DarkRed => new Color(0.54509807f, 0f, 0f, 1f);

        /// <summary>
        /// Gets system color dark salmon corresponding to 233 150 122 255 (0.9137255, 0.5882353, 0.47843137, 1).
        /// </summary>
        public static Color DarkSalmon => new Color(0.9137255f, 0.5882353f, 0.47843137f, 1f);

        /// <summary>
        /// Gets system color dark sea green corresponding to 143 188 139 255 (0.56078434, 0.7372549, 0.54509807, 1).
        /// </summary>
        public static Color DarkSeaGreen => new Color(0.56078434f, 0.7372549f, 0.54509807f, 1f);

        /// <summary>
        /// Gets system color dark slate blue corresponding to 72 61 139 255 (0.28235295, 0.23921569, 0.54509807, 1).
        /// </summary>
        public static Color DarkSlateBlue => new Color(0.28235295f, 0.23921569f, 0.54509807f, 1f);

        /// <summary>
        /// Gets system color dark slate gray corresponding to 47 79 79 255 (0.18431373, 0.30980393, 0.30980393, 1).
        /// </summary>
        public static Color DarkSlateGray => new Color(0.18431373f, 0.30980393f, 0.30980393f, 1f);

        /// <summary>
        /// Gets system color dark turquoise corresponding to 0 206 209 255 (0, 0.80784315, 0.81960785, 1).
        /// </summary>
        public static Color DarkTurquoise => new Color(0f, 0.80784315f, 0.81960785f, 1f);

        /// <summary>
        /// Gets system color dark violet corresponding to 148 0 211 255 (0.5803922, 0, 0.827451, 1).
        /// </summary>
        public static Color DarkViolet => new Color(0.5803922f, 0f, 0.827451f, 1f);

        /// <summary>
        /// Gets system color deep pink corresponding to 255 20 147 255 (1, 0.078431375, 0.5764706, 1).
        /// </summary>
        public static Color DeepPink => new Color(1f, 0.078431375f, 0.5764706f, 1f);

        /// <summary>
        /// Gets system color deep sky blue corresponding to 0 191 255 255 (0, 0.7490196, 1, 1).
        /// </summary>
        public static Color DeepSkyBlue => new Color(0f, 0.7490196f, 1f, 1f);

        /// <summary>
        /// Gets system color dim gray corresponding to 105 105 105 255 (0.4117647, 0.4117647, 0.4117647, 1).
        /// </summary>
        public static Color DimGray => new Color(0.4117647f, 0.4117647f, 0.4117647f, 1f);

        /// <summary>
        /// Gets system color dodger blue corresponding to 30 144 255 255 (0.11764706, 0.5647059, 1, 1).
        /// </summary>
        public static Color DodgerBlue => new Color(0.11764706f, 0.5647059f, 1f, 1f);

        /// <summary>
        /// Gets system color fire brick corresponding to 178 34 34 255 (0.69803923, 0.13333334, 0.13333334, 1).
        /// </summary>
        public static Color FireBrick => new Color(0.69803923f, 0.13333334f, 0.13333334f, 1f);

        /// <summary>
        /// Gets system color floral white corresponding to 255 250 240 255 (1, 0.98039216, 0.9411765, 1).
        /// </summary>
        public static Color FloralWhite => new Color(1f, 0.98039216f, 0.9411765f, 1f);

        /// <summary>
        /// Gets system color forest green corresponding to 34 139 34 255 (0.13333334, 0.54509807, 0.13333334, 1).
        /// </summary>
        public static Color ForestGreen => new Color(0.13333334f, 0.54509807f, 0.13333334f, 1f);

        /// <summary>
        /// Gets system color fuchsia corresponding to 255 0 255 255 (1, 0, 1, 1).
        /// </summary>
        public static Color Fuchsia => new Color(1f, 0f, 1f, 1f);

        /// <summary>
        /// Gets system color gainsboro corresponding to 220 220 220 255 (0.8627451, 0.8627451, 0.8627451, 1).
        /// </summary>
        public static Color Gainsboro => new Color(0.8627451f, 0.8627451f, 0.8627451f, 1f);

        /// <summary>
        /// Gets system color ghost white corresponding to 248 248 255 255 (0.972549, 0.972549, 1, 1).
        /// </summary>
        public static Color GhostWhite => new Color(0.972549f, 0.972549f, 1f, 1f);

        /// <summary>
        /// Gets system color gold corresponding to 255 215 0 255 (1, 0.84313726, 0, 1).
        /// </summary>
        public static Color Gold => new Color(1f, 0.84313726f, 0f, 1f);

        /// <summary>
        /// Gets system color goldenrod corresponding to 218 165 32 255 (0.85490197, 0.64705884, 0.1254902, 1).
        /// </summary>
        public static Color Goldenrod => new Color(0.85490197f, 0.64705884f, 0.1254902f, 1f);

        /// <summary>
        /// Gets system color gray corresponding to 128 128 128 255 (0.5019608, 0.5019608, 0.5019608, 1).
        /// </summary>
        public static Color Gray => new Color(0.5019608f, 0.5019608f, 0.5019608f, 1f);

        /// <summary>
        /// Gets system color green corresponding to 0 128 0 255 (0, 0.5019608, 0, 1).
        /// </summary>
        public static Color Green => new Color(0f, 0.5019608f, 0f, 1f);

        /// <summary>
        /// Gets system color green yellow corresponding to 173 255 47 255 (0.6784314, 1, 0.18431373, 1).
        /// </summary>
        public static Color GreenYellow => new Color(0.6784314f, 1f, 0.18431373f, 1f);

        /// <summary>
        /// Gets system color honeydew corresponding to 240 255 240 255 (0.9411765, 1, 0.9411765, 1).
        /// </summary>
        public static Color Honeydew => new Color(0.9411765f, 1f, 0.9411765f, 1f);

        /// <summary>
        /// Gets system color hot pink corresponding to 255 105 180 255 (1, 0.4117647, 0.7058824, 1).
        /// </summary>
        public static Color HotPink => new Color(1f, 0.4117647f, 0.7058824f, 1f);

        /// <summary>
        /// Gets system color indian red corresponding to 205 92 92 255 (0.8039216, 0.36078432, 0.36078432, 1).
        /// </summary>
        public static Color IndianRed => new Color(0.8039216f, 0.36078432f, 0.36078432f, 1f);

        /// <summary>
        /// Gets system color indigo corresponding to 75 0 130 255 (0.29411766, 0, 0.50980395, 1).
        /// </summary>
        public static Color Indigo => new Color(0.29411766f, 0f, 0.50980395f, 1f);

        /// <summary>
        /// Gets system color ivory corresponding to 255 255 240 255 (1, 1, 0.9411765, 1).
        /// </summary>
        public static Color Ivory => new Color(1f, 1f, 0.9411765f, 1f);

        /// <summary>
        /// Gets system color khaki corresponding to 240 230 140 255 (0.9411765, 0.9019608, 0.54901963, 1).
        /// </summary>
        public static Color Khaki => new Color(0.9411765f, 0.9019608f, 0.54901963f, 1f);

        /// <summary>
        /// Gets system color lavender corresponding to 230 230 250 255 (0.9019608, 0.9019608, 0.98039216, 1).
        /// </summary>
        public static Color Lavender => new Color(0.9019608f, 0.9019608f, 0.98039216f, 1f);

        /// <summary>
        /// Gets system color lavender blush corresponding to 255 240 245 255 (1, 0.9411765, 0.9607843, 1).
        /// </summary>
        public static Color LavenderBlush => new Color(1f, 0.9411765f, 0.9607843f, 1f);

        /// <summary>
        /// Gets system color lawn green corresponding to 124 252 0 255 (0.4862745, 0.9882353, 0, 1).
        /// </summary>
        public static Color LawnGreen => new Color(0.4862745f, 0.9882353f, 0f, 1f);

        /// <summary>
        /// Gets system color lemon chiffon corresponding to 255 250 205 255 (1, 0.98039216, 0.8039216, 1).
        /// </summary>
        public static Color LemonChiffon => new Color(1f, 0.98039216f, 0.8039216f, 1f);

        /// <summary>
        /// Gets system color light blue corresponding to 173 216 230 255 (0.6784314, 0.84705883, 0.9019608, 1).
        /// </summary>
        public static Color LightBlue => new Color(0.6784314f, 0.84705883f, 0.9019608f, 1f);

        /// <summary>
        /// Gets system color light coral corresponding to 240 128 128 255 (0.9411765, 0.5019608, 0.5019608, 1).
        /// </summary>
        public static Color LightCoral => new Color(0.9411765f, 0.5019608f, 0.5019608f, 1f);

        /// <summary>
        /// Gets system color light cyan corresponding to 224 255 255 255 (0.8784314, 1, 1, 1).
        /// </summary>
        public static Color LightCyan => new Color(0.8784314f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color light goldenrod yellow corresponding to 250 250 210 255 (0.98039216, 0.98039216, 0.8235294, 1).
        /// </summary>
        public static Color LightGoldenrodYellow => new Color(0.98039216f, 0.98039216f, 0.8235294f, 1f);

        /// <summary>
        /// Gets system color light gray corresponding to 211 211 211 255 (0.827451, 0.827451, 0.827451, 1).
        /// </summary>
        public static Color LightGray => new Color(0.827451f, 0.827451f, 0.827451f, 1f);

        /// <summary>
        /// Gets system color light green corresponding to 144 238 144 255 (0.5647059, 0.93333334, 0.5647059, 1).
        /// </summary>
        public static Color LightGreen => new Color(0.5647059f, 0.93333334f, 0.5647059f, 1f);

        /// <summary>
        /// Gets system color light pink corresponding to 255 182 193 255 (1, 0.7137255, 0.75686276, 1).
        /// </summary>
        public static Color LightPink => new Color(1f, 0.7137255f, 0.75686276f, 1f);

        /// <summary>
        /// Gets system color light salmon corresponding to 255 160 122 255 (1, 0.627451, 0.47843137, 1).
        /// </summary>
        public static Color LightSalmon => new Color(1f, 0.627451f, 0.47843137f, 1f);

        /// <summary>
        /// Gets system color light sea green corresponding to 32 178 170 255 (0.1254902, 0.69803923, 0.6666667, 1).
        /// </summary>
        public static Color LightSeaGreen => new Color(0.1254902f, 0.69803923f, 0.6666667f, 1f);

        /// <summary>
        /// Gets system color light sky blue corresponding to 135 206 250 255 (0.5294118, 0.80784315, 0.98039216, 1).
        /// </summary>
        public static Color LightSkyBlue => new Color(0.5294118f, 0.80784315f, 0.98039216f, 1f);

        /// <summary>
        /// Gets system color light slate gray corresponding to 119 136 153 255 (0.46666667, 0.53333336, 0.6, 1).
        /// </summary>
        public static Color LightSlateGray => new Color(0.46666667f, 0.53333336f, 0.6f, 1f);

        /// <summary>
        /// Gets system color light steel blue corresponding to 176 196 222 255 (0.6901961, 0.76862746, 0.87058824, 1).
        /// </summary>
        public static Color LightSteelBlue => new Color(0.6901961f, 0.76862746f, 0.87058824f, 1f);

        /// <summary>
        /// Gets system color light yellow corresponding to 255 255 224 255 (1, 1, 0.8784314, 1).
        /// </summary>
        public static Color LightYellow => new Color(1f, 1f, 0.8784314f, 1f);

        /// <summary>
        /// Gets system color lime corresponding to 0 255 0 255 (0, 1, 0, 1).
        /// </summary>
        public static Color Lime => new Color(0f, 1f, 0f, 1f);

        /// <summary>
        /// Gets system color lime green corresponding to 50 205 50 255 (0.19607843, 0.8039216, 0.19607843, 1).
        /// </summary>
        public static Color LimeGreen => new Color(0.19607843f, 0.8039216f, 0.19607843f, 1f);

        /// <summary>
        /// Gets system color linen corresponding to 250 240 230 255 (0.98039216, 0.9411765, 0.9019608, 1).
        /// </summary>
        public static Color Linen => new Color(0.98039216f, 0.9411765f, 0.9019608f, 1f);

        /// <summary>
        /// Gets system color magenta corresponding to 255 0 255 255 (1, 0, 1, 1).
        /// </summary>
        public static Color Magenta => new Color(1f, 0f, 1f, 1f);

        /// <summary>
        /// Gets system color maroon corresponding to 128 0 0 255 (0.5019608, 0, 0, 1).
        /// </summary>
        public static Color Maroon => new Color(0.5019608f, 0f, 0f, 1f);

        /// <summary>
        /// Gets system color medium aquamarine corresponding to 102 205 170 255 (0.4, 0.8039216, 0.6666667, 1).
        /// </summary>
        public static Color MediumAquamarine => new Color(0.4f, 0.8039216f, 0.6666667f, 1f);

        /// <summary>
        /// Gets system color medium blue corresponding to 0 0 205 255 (0, 0, 0.8039216, 1).
        /// </summary>
        public static Color MediumBlue => new Color(0f, 0f, 0.8039216f, 1f);

        /// <summary>
        /// Gets system color medium orchid corresponding to 186 85 211 255 (0.7294118, 0.33333334, 0.827451, 1).
        /// </summary>
        public static Color MediumOrchid => new Color(0.7294118f, 0.33333334f, 0.827451f, 1f);

        /// <summary>
        /// Gets system color medium purple corresponding to 147 112 219 255 (0.5764706, 0.4392157, 0.85882354, 1).
        /// </summary>
        public static Color MediumPurple => new Color(0.5764706f, 0.4392157f, 0.85882354f, 1f);

        /// <summary>
        /// Gets system color medium sea green corresponding to 60 179 113 255 (0.23529412, 0.7019608, 0.44313726, 1).
        /// </summary>
        public static Color MediumSeaGreen => new Color(0.23529412f, 0.7019608f, 0.44313726f, 1f);

        /// <summary>
        /// Gets system color medium slate blue corresponding to 123 104 238 255 (0.48235294, 0.40784314, 0.93333334, 1).
        /// </summary>
        public static Color MediumSlateBlue => new Color(0.48235294f, 0.40784314f, 0.93333334f, 1f);

        /// <summary>
        /// Gets system color medium spring green corresponding to 0 250 154 255 (0, 0.98039216, 0.6039216, 1).
        /// </summary>
        public static Color MediumSpringGreen => new Color(0f, 0.98039216f, 0.6039216f, 1f);

        /// <summary>
        /// Gets system color medium turquoise corresponding to 72 209 204 255 (0.28235295, 0.81960785, 0.8, 1).
        /// </summary>
        public static Color MediumTurquoise => new Color(0.28235295f, 0.81960785f, 0.8f, 1f);

        /// <summary>
        /// Gets system color medium violet red corresponding to 199 21 133 255 (0.78039217, 0.08235294, 0.52156866, 1).
        /// </summary>
        public static Color MediumVioletRed => new Color(0.78039217f, 0.08235294f, 0.52156866f, 1f);

        /// <summary>
        /// Gets system color midnight blue corresponding to 25 25 112 255 (0.09803922, 0.09803922, 0.4392157, 1).
        /// </summary>
        public static Color MidnightBlue => new Color(0.09803922f, 0.09803922f, 0.4392157f, 1f);

        /// <summary>
        /// Gets system color mint cream corresponding to 245 255 250 255 (0.9607843, 1, 0.98039216, 1).
        /// </summary>
        public static Color MintCream => new Color(0.9607843f, 1f, 0.98039216f, 1f);

        /// <summary>
        /// Gets system color mistyrose corresponding to 255 228 225 255 (1, 0.89411765, 0.88235295, 1).
        /// </summary>
        public static Color MistyRose => new Color(1f, 0.89411765f, 0.88235295f, 1f);

        /// <summary>
        /// Gets system color moccasin corresponding to 255 228 181 255 (1, 0.89411765, 0.70980394, 1).
        /// </summary>
        public static Color Moccasin => new Color(1f, 0.89411765f, 0.70980394f, 1f);

        /// <summary>
        /// Gets system color navajo white corresponding to 255 222 173 255 (1, 0.87058824, 0.6784314, 1).
        /// </summary>
        public static Color NavajoWhite => new Color(1f, 0.87058824f, 0.6784314f, 1f);

        /// <summary>
        /// Gets system color navy corresponding to 0 0 128 255 (0, 0, 0.5019608, 1).
        /// </summary>
        public static Color Navy => new Color(0f, 0f, 0.5019608f, 1f);

        /// <summary>
        /// Gets system color old lace corresponding to 253 245 230 255 (0.99215686, 0.9607843, 0.9019608, 1).
        /// </summary>
        public static Color OldLace => new Color(0.99215686f, 0.9607843f, 0.9019608f, 1f);

        /// <summary>
        /// Gets system color olive corresponding to 128 128 0 255 (0.5019608, 0.5019608, 0, 1).
        /// </summary>
        public static Color Olive => new Color(0.5019608f, 0.5019608f, 0f, 1f);

        /// <summary>
        /// Gets system color olive drab corresponding to 107 142 35 255 (0.41960785, 0.5568628, 0.13725491, 1).
        /// </summary>
        public static Color OliveDrab => new Color(0.41960785f, 0.5568628f, 0.13725491f, 1f);

        /// <summary>
        /// Gets system color orange corresponding to 255 165 0 255 (1, 0.64705884, 0, 1).
        /// </summary>
        public static Color Orange => new Color(1f, 0.64705884f, 0f, 1f);

        /// <summary>
        /// Gets system color orange red corresponding to 255 69 0 255 (1, 0.27058825, 0, 1).
        /// </summary>
        public static Color OrangeRed => new Color(1f, 0.27058825f, 0f, 1f);

        /// <summary>
        /// Gets system color orchid corresponding to 218 112 214 255 (0.85490197, 0.4392157, 0.8392157, 1).
        /// </summary>
        public static Color Orchid => new Color(0.85490197f, 0.4392157f, 0.8392157f, 1f);

        /// <summary>
        /// Gets system color pale goldenrod corresponding to 238 232 170 255 (0.93333334, 0.9098039, 0.6666667, 1).
        /// </summary>
        public static Color PaleGoldenrod => new Color(0.93333334f, 0.9098039f, 0.6666667f, 1f);

        /// <summary>
        /// Gets system color pale green corresponding to 152 251 152 255 (0.59607846, 0.9843137, 0.59607846, 1).
        /// </summary>
        public static Color PaleGreen => new Color(0.59607846f, 0.9843137f, 0.59607846f, 1f);

        /// <summary>
        /// Gets system color pale turquoise corresponding to 175 238 238 255 (0.6862745, 0.93333334, 0.93333334, 1).
        /// </summary>
        public static Color PaleTurquoise => new Color(0.6862745f, 0.93333334f, 0.93333334f, 1f);

        /// <summary>
        /// Gets system color pale violet red corresponding to 219 112 147 255 (0.85882354, 0.4392157, 0.5764706, 1).
        /// </summary>
        public static Color PaleVioletRed => new Color(0.85882354f, 0.4392157f, 0.5764706f, 1f);

        /// <summary>
        /// Gets system color papaya whip corresponding to 255 239 213 255 (1, 0.9372549, 0.8352941, 1).
        /// </summary>
        public static Color PapayaWhip => new Color(1f, 0.9372549f, 0.8352941f, 1f);

        /// <summary>
        /// Gets system color peach puff corresponding to 255 218 185 255 (1, 0.85490197, 0.7254902, 1).
        /// </summary>
        public static Color PeachPuff => new Color(1f, 0.85490197f, 0.7254902f, 1f);

        /// <summary>
        /// Gets system color peru corresponding to 205 133 63 255 (0.8039216, 0.52156866, 0.24705882, 1).
        /// </summary>
        public static Color Peru => new Color(0.8039216f, 0.52156866f, 0.24705882f, 1f);

        /// <summary>
        /// Gets system color pink corresponding to 255 192 203 255 (1, 0.7529412, 0.79607844, 1).
        /// </summary>
        public static Color Pink => new Color(1f, 0.7529412f, 0.79607844f, 1f);

        /// <summary>
        /// Gets system color plum corresponding to 221 160 221 255 (0.8666667, 0.627451, 0.8666667, 1).
        /// </summary>
        public static Color Plum => new Color(0.8666667f, 0.627451f, 0.8666667f, 1f);

        /// <summary>
        /// Gets system color powder blue corresponding to 176 224 230 255 (0.6901961, 0.8784314, 0.9019608, 1).
        /// </summary>
        public static Color PowderBlue => new Color(0.6901961f, 0.8784314f, 0.9019608f, 1f);

        /// <summary>
        /// Gets system color purple corresponding to 128 0 128 255 (0.5019608, 0, 0.5019608, 1).
        /// </summary>
        public static Color Purple => new Color(0.5019608f, 0f, 0.5019608f, 1f);

        /// <summary>
        /// Gets system color red corresponding to 255 0 0 255 (1, 0, 0, 1).
        /// </summary>
        public static Color Red => new Color(1f, 0f, 0f, 1f);

        /// <summary>
        /// Gets system color rosy brown corresponding to 188 143 143 255 (0.7372549, 0.56078434, 0.56078434, 1).
        /// </summary>
        public static Color RosyBrown => new Color(0.7372549f, 0.56078434f, 0.56078434f, 1f);

        /// <summary>
        /// Gets system color royal blue corresponding to 65 105 225 255 (0.25490198, 0.4117647, 0.88235295, 1).
        /// </summary>
        public static Color RoyalBlue => new Color(0.25490198f, 0.4117647f, 0.88235295f, 1f);

        /// <summary>
        /// Gets system color saddle brown corresponding to 139 69 19 255 (0.54509807, 0.27058825, 0.07450981, 1).
        /// </summary>
        public static Color SaddleBrown => new Color(0.54509807f, 0.27058825f, 0.07450981f, 1f);

        /// <summary>
        /// Gets system color salmon corresponding to 250 128 114 255 (0.98039216, 0.5019608, 0.44705883, 1).
        /// </summary>
        public static Color Salmon => new Color(0.98039216f, 0.5019608f, 0.44705883f, 1f);

        /// <summary>
        /// Gets system color sandy brown corresponding to 244 164 96 255 (0.95686275, 0.6431373, 0.3764706, 1).
        /// </summary>
        public static Color SandyBrown => new Color(0.95686275f, 0.6431373f, 0.3764706f, 1f);

        /// <summary>
        /// Gets system color sea green corresponding to 46 139 87 255 (0.18039216, 0.54509807, 0.34117648, 1).
        /// </summary>
        public static Color SeaGreen => new Color(0.18039216f, 0.54509807f, 0.34117648f, 1f);

        /// <summary>
        /// Gets system color seashell corresponding to 255 245 238 255 (1, 0.9607843, 0.93333334, 1).
        /// </summary>
        public static Color Seashell => new Color(1f, 0.9607843f, 0.93333334f, 1f);

        /// <summary>
        /// Gets system color sienna corresponding to 160 82 45 255 (0.627451, 0.32156864, 0.1764706, 1).
        /// </summary>
        public static Color Sienna => new Color(0.627451f, 0.32156864f, 0.1764706f, 1f);

        /// <summary>
        /// Gets system color silver corresponding to 192 192 192 255 (0.7529412, 0.7529412, 0.7529412, 1).
        /// </summary>
        public static Color Silver => new Color(0.7529412f, 0.7529412f, 0.7529412f, 1f);

        /// <summary>
        /// Gets system color sky blue corresponding to 135 206 235 255 (0.5294118, 0.80784315, 0.92156863, 1).
        /// </summary>
        public static Color SkyBlue => new Color(0.5294118f, 0.80784315f, 0.92156863f, 1f);

        /// <summary>
        /// Gets system color slate blue corresponding to 106 90 205 255 (0.41568628, 0.3529412, 0.8039216, 1).
        /// </summary>
        public static Color SlateBlue => new Color(0.41568628f, 0.3529412f, 0.8039216f, 1f);

        /// <summary>
        /// Gets system color slate gray corresponding to 112 128 144 255 (0.4392157, 0.5019608, 0.5647059, 1).
        /// </summary>
        public static Color SlateGray => new Color(0.4392157f, 0.5019608f, 0.5647059f, 1f);

        /// <summary>
        /// Gets system color snow corresponding to 255 250 250 255 (1, 0.98039216, 0.98039216, 1).
        /// </summary>
        public static Color Snow => new Color(1f, 0.98039216f, 0.98039216f, 1f);

        /// <summary>
        /// Gets system color spring green corresponding to 0 255 127 255 (0, 1, 0.49803922, 1).
        /// </summary>
        public static Color SpringGreen => new Color(0f, 1f, 0.49803922f, 1f);

        /// <summary>
        /// Gets system color steel blue corresponding to 70 130 180 255 (0.27450982, 0.50980395, 0.7058824, 1).
        /// </summary>
        public static Color SteelBlue => new Color(0.27450982f, 0.50980395f, 0.7058824f, 1f);

        /// <summary>
        /// Gets system color tan corresponding to 210 180 140 255 (0.8235294, 0.7058824, 0.54901963, 1).
        /// </summary>
        public static Color Tan => new Color(0.8235294f, 0.7058824f, 0.54901963f, 1f);

        /// <summary>
        /// Gets system color teal corresponding to 0 128 128 255 (0, 0.5019608, 0.5019608, 1).
        /// </summary>
        public static Color Teal => new Color(0f, 0.5019608f, 0.5019608f, 1f);

        /// <summary>
        /// Gets system color thistle corresponding to 216 191 216 255 (0.84705883, 0.7490196, 0.84705883, 1).
        /// </summary>
        public static Color Thistle => new Color(0.84705883f, 0.7490196f, 0.84705883f, 1f);

        /// <summary>
        /// Gets system color tomato corresponding to 255 99 71 255 (1, 0.3882353, 0.2784314, 1).
        /// </summary>
        public static Color Tomato => new Color(1f, 0.3882353f, 0.2784314f, 1f);

        /// <summary>
        /// Gets system color turquoise corresponding to 64 224 208 255 (0.2509804, 0.8784314, 0.8156863, 1).
        /// </summary>
        public static Color Turquoise => new Color(0.2509804f, 0.8784314f, 0.8156863f, 1f);

        /// <summary>
        /// Gets system color violet corresponding to 238 130 238 255 (0.93333334, 0.50980395, 0.93333334, 1).
        /// </summary>
        public static Color Violet => new Color(0.93333334f, 0.50980395f, 0.93333334f, 1f);

        /// <summary>
        /// Gets system color wheat corresponding to 245 222 179 255 (0.9607843, 0.87058824, 0.7019608, 1).
        /// </summary>
        public static Color Wheat => new Color(0.9607843f, 0.87058824f, 0.7019608f, 1f);

        /// <summary>
        /// Gets system color white corresponding to 255 255 255 255 (1, 1, 1, 1).
        /// </summary>
        public static Color White => new Color(1f, 1f, 1f, 1f);

        /// <summary>
        /// Gets system color white smoke corresponding to 245 245 245 255 (0.9607843, 0.9607843, 0.9607843, 1).
        /// </summary>
        public static Color WhiteSmoke => new Color(0.9607843f, 0.9607843f, 0.9607843f, 1f);

        /// <summary>
        /// Gets system color yellow corresponding to 255 255 0 255 (1, 1, 0, 1).
        /// </summary>
        public static Color Yellow => new Color(1f, 1f, 0f, 1f);

        /// <summary>
        /// Gets system color yellow green corresponding to 154 205 50 255 (0.6039216, 0.8039216, 0.19607843, 1).
        /// </summary>
        public static Color YellowGreen => new Color(0.6039216f, 0.8039216f, 0.19607843f, 1f);
    }
}