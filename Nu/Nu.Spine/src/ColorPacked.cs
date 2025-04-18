using System.Diagnostics;
using System.Numerics;
using System.Runtime.Serialization;
using System.Text;
using System;

namespace Spine
{
    /// <summary>
    /// Describes a 32-bit packed color.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/develop/MonoGame.Framework/Color.cs
    /// </summary>
    [DataContract]
    [DebuggerDisplay("{DebugDisplayString,nq}")]
    public struct ColorPacked : IEquatable<ColorPacked>
    {
        static ColorPacked()
        {
            Black = new ColorPacked(0xff000000);
            White = new ColorPacked(uint.MaxValue);
        }

        // Stored as RGBA with R in the least significant octet:
        // |-------|-------|-------|-------
        // A       B       G       R
        private uint _packedValue;

        /// <summary>
        /// Constructs an RGBA color from a packed value.
        /// The value is a 32-bit unsigned integer, with R in the least significant octet.
        /// </summary>
        /// <param name="packedValue">The packed value.</param>
        public ColorPacked(uint packedValue)
        {
            _packedValue = packedValue;
        }


        /// <summary>
        /// Constructs an RGBA color from the XYZW unit length components of a vector.
        /// </summary>
        /// <param name="color">A <see cref="Vector4"/> representing color.</param>
        public ColorPacked(Vector4 color)
            : this((int)(color.X * 255), (int)(color.Y * 255), (int)(color.Z * 255), (int)(color.W * 255))
        {
        }

        /// <summary>
        /// Constructs an RGBA color from the XYZ unit length components of a vector. Alpha value will be opaque.
        /// </summary>
        /// <param name="color">A <see cref="Vector3"/> representing color.</param>
        public ColorPacked(Vector3 color)
            : this((int)(color.X * 255), (int)(color.Y * 255), (int)(color.Z * 255))
        {
        }

        /// <summary>
        /// Constructs an RGBA color from a <see cref="ColorPacked"/> and an alpha value.
        /// </summary>
        /// <param name="color">A <see cref="ColorPacked"/> for RGB values of new <see cref="ColorPacked"/> instance.</param>
        /// <param name="alpha">The alpha component value from 0 to 255.</param>
        public ColorPacked(ColorPacked color, int alpha)
        {
            if ((alpha & 0xFFFFFF00) != 0)
            {
                var clampedA = (uint)MathHelper.Clamp(alpha, Byte.MinValue, Byte.MaxValue);

                _packedValue = (color._packedValue & 0x00FFFFFF) | (clampedA << 24);
            }
            else
            {
                _packedValue = (color._packedValue & 0x00FFFFFF) | ((uint)alpha << 24);
            }
        }

        /// <summary>
        /// Constructs an RGBA color from color and alpha value.
        /// </summary>
        /// <param name="color">A <see cref="ColorPacked"/> for RGB values of new <see cref="ColorPacked"/> instance.</param>
        /// <param name="alpha">Alpha component value from 0.0f to 1.0f.</param>
        public ColorPacked(ColorPacked color, float alpha) :
            this(color, (int)(alpha * 255))
        {
        }

        /// <summary>
        /// Constructs an RGBA color from scalars representing red, green and blue values. Alpha value will be opaque.
        /// </summary>
        /// <param name="r">Red component value from 0.0f to 1.0f.</param>
        /// <param name="g">Green component value from 0.0f to 1.0f.</param>
        /// <param name="b">Blue component value from 0.0f to 1.0f.</param>
        public ColorPacked(float r, float g, float b)
            : this((int)(r * 255), (int)(g * 255), (int)(b * 255))
        {
        }

        /// <summary>
        /// Constructs an RGBA color from scalars representing red, green, blue and alpha values.
        /// </summary>
        /// <param name="r">Red component value from 0.0f to 1.0f.</param>
        /// <param name="g">Green component value from 0.0f to 1.0f.</param>
        /// <param name="b">Blue component value from 0.0f to 1.0f.</param>
        /// <param name="alpha">Alpha component value from 0.0f to 1.0f.</param>
        public ColorPacked(float r, float g, float b, float alpha)
            : this((int)(r * 255), (int)(g * 255), (int)(b * 255), (int)(alpha * 255))
        {
        }

        /// <summary>
        /// Constructs an RGBA color from scalars representing red, green and blue values. Alpha value will be opaque.
        /// </summary>
        /// <param name="r">Red component value from 0 to 255.</param>
        /// <param name="g">Green component value from 0 to 255.</param>
        /// <param name="b">Blue component value from 0 to 255.</param>
        public ColorPacked(int r, int g, int b)
        {
            _packedValue = 0xFF000000; // A = 255

            if (((r | g | b) & 0xFFFFFF00) != 0)
            {
                var clampedR = (uint)MathHelper.Clamp(r, Byte.MinValue, Byte.MaxValue);
                var clampedG = (uint)MathHelper.Clamp(g, Byte.MinValue, Byte.MaxValue);
                var clampedB = (uint)MathHelper.Clamp(b, Byte.MinValue, Byte.MaxValue);

                _packedValue |= (clampedB << 16) | (clampedG << 8) | (clampedR);
            }
            else
            {
                _packedValue |= ((uint)b << 16) | ((uint)g << 8) | ((uint)r);
            }
        }

        /// <summary>
        /// Constructs an RGBA color from scalars representing red, green, blue and alpha values.
        /// </summary>
        /// <param name="r">Red component value from 0 to 255.</param>
        /// <param name="g">Green component value from 0 to 255.</param>
        /// <param name="b">Blue component value from 0 to 255.</param>
        /// <param name="alpha">Alpha component value from 0 to 255.</param>
        public ColorPacked(int r, int g, int b, int alpha)
        {
            if (((r | g | b | alpha) & 0xFFFFFF00) != 0)
            {
                var clampedR = (uint)MathHelper.Clamp(r, Byte.MinValue, Byte.MaxValue);
                var clampedG = (uint)MathHelper.Clamp(g, Byte.MinValue, Byte.MaxValue);
                var clampedB = (uint)MathHelper.Clamp(b, Byte.MinValue, Byte.MaxValue);
                var clampedA = (uint)MathHelper.Clamp(alpha, Byte.MinValue, Byte.MaxValue);

                _packedValue = (clampedA << 24) | (clampedB << 16) | (clampedG << 8) | (clampedR);
            }
            else
            {
                _packedValue = ((uint)alpha << 24) | ((uint)b << 16) | ((uint)g << 8) | ((uint)r);
            }
        }

        /// <summary>
        /// Constructs an RGBA color from scalars representing red, green, blue and alpha values.
        /// </summary>
        /// <remarks>
        /// This overload sets the values directly without clamping, and may therefore be faster than the other overloads.
        /// </remarks>
        /// <param name="r"></param>
        /// <param name="g"></param>
        /// <param name="b"></param>
        /// <param name="alpha"></param>
        public ColorPacked(byte r, byte g, byte b, byte alpha)
        {
            _packedValue = ((uint)alpha << 24) | ((uint)b << 16) | ((uint)g << 8) | (r);
        }

        /// <summary>
        /// Gets or sets the blue component.
        /// </summary>
        [DataMember]
        public byte B
        {
            get
            {
                unchecked
                {
                    return (byte)(this._packedValue >> 16);
                }
            }
            set
            {
                this._packedValue = (this._packedValue & 0xff00ffff) | ((uint)value << 16);
            }
        }

        /// <summary>
        /// Gets or sets the green component.
        /// </summary>
        [DataMember]
        public byte G
        {
            get
            {
                unchecked
                {
                    return (byte)(this._packedValue >> 8);
                }
            }
            set
            {
                this._packedValue = (this._packedValue & 0xffff00ff) | ((uint)value << 8);
            }
        }

        /// <summary>
        /// Gets or sets the red component.
        /// </summary>
        [DataMember]
        public byte R
        {
            get
            {
                unchecked
                {
                    return (byte)this._packedValue;
                }
            }
            set
            {
                this._packedValue = (this._packedValue & 0xffffff00) | value;
            }
        }

        /// <summary>
        /// Gets or sets the alpha component.
        /// </summary>
        [DataMember]
        public byte A
        {
            get
            {
                unchecked
                {
                    return (byte)(this._packedValue >> 24);
                }
            }
            set
            {
                this._packedValue = (this._packedValue & 0x00ffffff) | ((uint)value << 24);
            }
        }

        /// <summary>
        /// Compares whether two <see cref="ColorPacked"/> instances are equal.
        /// </summary>
        /// <param name="a"><see cref="ColorPacked"/> instance on the left of the equal sign.</param>
        /// <param name="b"><see cref="ColorPacked"/> instance on the right of the equal sign.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public static bool operator ==(ColorPacked a, ColorPacked b)
        {
            return (a._packedValue == b._packedValue);
        }

        /// <summary>
        /// Compares whether two <see cref="ColorPacked"/> instances are not equal.
        /// </summary>
        /// <param name="a"><see cref="ColorPacked"/> instance on the left of the not equal sign.</param>
        /// <param name="b"><see cref="ColorPacked"/> instance on the right of the not equal sign.</param>
        /// <returns><c>true</c> if the instances are not equal; <c>false</c> otherwise.</returns>	
        public static bool operator !=(ColorPacked a, ColorPacked b)
        {
            return (a._packedValue != b._packedValue);
        }

        /// <summary>
        /// Gets the hash code of this <see cref="ColorPacked"/>.
        /// </summary>
        /// <returns>Hash code of this <see cref="ColorPacked"/>.</returns>
        public override int GetHashCode()
        {
            return this._packedValue.GetHashCode();
        }

        /// <summary>
        /// Compares whether current instance is equal to specified object.
        /// </summary>
        /// <param name="obj">The <see cref="ColorPacked"/> to compare.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public override bool Equals(object obj)
        {
            return ((obj is ColorPacked) && this.Equals((ColorPacked)obj));
        }

        #region Color Bank

        /// <summary>
        /// Transparent color (R:0,G:0,B:0,A:0).
        /// </summary>
        public static ColorPacked Transparent
        {
            get;
            private set;
        }

        /// <summary>
        /// AliceBlue color (R:240,G:248,B:255,A:255).
        /// </summary>
        public static ColorPacked AliceBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// AntiqueWhite color (R:250,G:235,B:215,A:255).
        /// </summary>
        public static ColorPacked AntiqueWhite
        {
            get;
            private set;
        }

        /// <summary>
        /// Aqua color (R:0,G:255,B:255,A:255).
        /// </summary>
        public static ColorPacked Aqua
        {
            get;
            private set;
        }

        /// <summary>
        /// Aquamarine color (R:127,G:255,B:212,A:255).
        /// </summary>
        public static ColorPacked Aquamarine
        {
            get;
            private set;
        }

        /// <summary>
        /// Azure color (R:240,G:255,B:255,A:255).
        /// </summary>
        public static ColorPacked Azure
        {
            get;
            private set;
        }

        /// <summary>
        /// Beige color (R:245,G:245,B:220,A:255).
        /// </summary>
        public static ColorPacked Beige
        {
            get;
            private set;
        }

        /// <summary>
        /// Bisque color (R:255,G:228,B:196,A:255).
        /// </summary>
        public static ColorPacked Bisque
        {
            get;
            private set;
        }

        /// <summary>
        /// Black color (R:0,G:0,B:0,A:255).
        /// </summary>
        public static ColorPacked Black
        {
            get;
            private set;
        }

        /// <summary>
        /// BlanchedAlmond color (R:255,G:235,B:205,A:255).
        /// </summary>
        public static ColorPacked BlanchedAlmond
        {
            get;
            private set;
        }

        /// <summary>
        /// Blue color (R:0,G:0,B:255,A:255).
        /// </summary>
        public static ColorPacked Blue
        {
            get;
            private set;
        }

        /// <summary>
        /// BlueViolet color (R:138,G:43,B:226,A:255).
        /// </summary>
        public static ColorPacked BlueViolet
        {
            get;
            private set;
        }

        /// <summary>
        /// Brown color (R:165,G:42,B:42,A:255).
        /// </summary>
        public static ColorPacked Brown
        {
            get;
            private set;
        }

        /// <summary>
        /// BurlyWood color (R:222,G:184,B:135,A:255).
        /// </summary>
        public static ColorPacked BurlyWood
        {
            get;
            private set;
        }

        /// <summary>
        /// CadetBlue color (R:95,G:158,B:160,A:255).
        /// </summary>
        public static ColorPacked CadetBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// Chartreuse color (R:127,G:255,B:0,A:255).
        /// </summary>
        public static ColorPacked Chartreuse
        {
            get;
            private set;
        }

        /// <summary>
        /// Chocolate color (R:210,G:105,B:30,A:255).
        /// </summary>
        public static ColorPacked Chocolate
        {
            get;
            private set;
        }

        /// <summary>
        /// Coral color (R:255,G:127,B:80,A:255).
        /// </summary>
        public static ColorPacked Coral
        {
            get;
            private set;
        }

        /// <summary>
        /// CornflowerBlue color (R:100,G:149,B:237,A:255).
        /// </summary>
        public static ColorPacked CornflowerBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// Cornsilk color (R:255,G:248,B:220,A:255).
        /// </summary>
        public static ColorPacked Cornsilk
        {
            get;
            private set;
        }

        /// <summary>
        /// Crimson color (R:220,G:20,B:60,A:255).
        /// </summary>
        public static ColorPacked Crimson
        {
            get;
            private set;
        }

        /// <summary>
        /// Cyan color (R:0,G:255,B:255,A:255).
        /// </summary>
        public static ColorPacked Cyan
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkBlue color (R:0,G:0,B:139,A:255).
        /// </summary>
        public static ColorPacked DarkBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkCyan color (R:0,G:139,B:139,A:255).
        /// </summary>
        public static ColorPacked DarkCyan
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkGoldenrod color (R:184,G:134,B:11,A:255).
        /// </summary>
        public static ColorPacked DarkGoldenrod
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkGray color (R:169,G:169,B:169,A:255).
        /// </summary>
        public static ColorPacked DarkGray
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkGreen color (R:0,G:100,B:0,A:255).
        /// </summary>
        public static ColorPacked DarkGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkKhaki color (R:189,G:183,B:107,A:255).
        /// </summary>
        public static ColorPacked DarkKhaki
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkMagenta color (R:139,G:0,B:139,A:255).
        /// </summary>
        public static ColorPacked DarkMagenta
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkOliveGreen color (R:85,G:107,B:47,A:255).
        /// </summary>
        public static ColorPacked DarkOliveGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkOrange color (R:255,G:140,B:0,A:255).
        /// </summary>
        public static ColorPacked DarkOrange
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkOrchid color (R:153,G:50,B:204,A:255).
        /// </summary>
        public static ColorPacked DarkOrchid
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkRed color (R:139,G:0,B:0,A:255).
        /// </summary>
        public static ColorPacked DarkRed
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkSalmon color (R:233,G:150,B:122,A:255).
        /// </summary>
        public static ColorPacked DarkSalmon
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkSeaGreen color (R:143,G:188,B:139,A:255).
        /// </summary>
        public static ColorPacked DarkSeaGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkSlateBlue color (R:72,G:61,B:139,A:255).
        /// </summary>
        public static ColorPacked DarkSlateBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkSlateGray color (R:47,G:79,B:79,A:255).
        /// </summary>
        public static ColorPacked DarkSlateGray
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkTurquoise color (R:0,G:206,B:209,A:255).
        /// </summary>
        public static ColorPacked DarkTurquoise
        {
            get;
            private set;
        }

        /// <summary>
        /// DarkViolet color (R:148,G:0,B:211,A:255).
        /// </summary>
        public static ColorPacked DarkViolet
        {
            get;
            private set;
        }

        /// <summary>
        /// DeepPink color (R:255,G:20,B:147,A:255).
        /// </summary>
        public static ColorPacked DeepPink
        {
            get;
            private set;
        }

        /// <summary>
        /// DeepSkyBlue color (R:0,G:191,B:255,A:255).
        /// </summary>
        public static ColorPacked DeepSkyBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// DimGray color (R:105,G:105,B:105,A:255).
        /// </summary>
        public static ColorPacked DimGray
        {
            get;
            private set;
        }

        /// <summary>
        /// DodgerBlue color (R:30,G:144,B:255,A:255).
        /// </summary>
        public static ColorPacked DodgerBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// Firebrick color (R:178,G:34,B:34,A:255).
        /// </summary>
        public static ColorPacked Firebrick
        {
            get;
            private set;
        }

        /// <summary>
        /// FloralWhite color (R:255,G:250,B:240,A:255).
        /// </summary>
        public static ColorPacked FloralWhite
        {
            get;
            private set;
        }

        /// <summary>
        /// ForestGreen color (R:34,G:139,B:34,A:255).
        /// </summary>
        public static ColorPacked ForestGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// Fuchsia color (R:255,G:0,B:255,A:255).
        /// </summary>
        public static ColorPacked Fuchsia
        {
            get;
            private set;
        }

        /// <summary>
        /// Gainsboro color (R:220,G:220,B:220,A:255).
        /// </summary>
        public static ColorPacked Gainsboro
        {
            get;
            private set;
        }

        /// <summary>
        /// GhostWhite color (R:248,G:248,B:255,A:255).
        /// </summary>
        public static ColorPacked GhostWhite
        {
            get;
            private set;
        }
        /// <summary>
        /// Gold color (R:255,G:215,B:0,A:255).
        /// </summary>
        public static ColorPacked Gold
        {
            get;
            private set;
        }

        /// <summary>
        /// Goldenrod color (R:218,G:165,B:32,A:255).
        /// </summary>
        public static ColorPacked Goldenrod
        {
            get;
            private set;
        }

        /// <summary>
        /// Gray color (R:128,G:128,B:128,A:255).
        /// </summary>
        public static ColorPacked Gray
        {
            get;
            private set;
        }

        /// <summary>
        /// Green color (R:0,G:128,B:0,A:255).
        /// </summary>
        public static ColorPacked Green
        {
            get;
            private set;
        }

        /// <summary>
        /// GreenYellow color (R:173,G:255,B:47,A:255).
        /// </summary>
        public static ColorPacked GreenYellow
        {
            get;
            private set;
        }

        /// <summary>
        /// Honeydew color (R:240,G:255,B:240,A:255).
        /// </summary>
        public static ColorPacked Honeydew
        {
            get;
            private set;
        }

        /// <summary>
        /// HotPink color (R:255,G:105,B:180,A:255).
        /// </summary>
        public static ColorPacked HotPink
        {
            get;
            private set;
        }

        /// <summary>
        /// IndianRed color (R:205,G:92,B:92,A:255).
        /// </summary>
        public static ColorPacked IndianRed
        {
            get;
            private set;
        }

        /// <summary>
        /// Indigo color (R:75,G:0,B:130,A:255).
        /// </summary>
        public static ColorPacked Indigo
        {
            get;
            private set;
        }

        /// <summary>
        /// Ivory color (R:255,G:255,B:240,A:255).
        /// </summary>
        public static ColorPacked Ivory
        {
            get;
            private set;
        }

        /// <summary>
        /// Khaki color (R:240,G:230,B:140,A:255).
        /// </summary>
        public static ColorPacked Khaki
        {
            get;
            private set;
        }

        /// <summary>
        /// Lavender color (R:230,G:230,B:250,A:255).
        /// </summary>
        public static ColorPacked Lavender
        {
            get;
            private set;
        }

        /// <summary>
        /// LavenderBlush color (R:255,G:240,B:245,A:255).
        /// </summary>
        public static ColorPacked LavenderBlush
        {
            get;
            private set;
        }

        /// <summary>
        /// LawnGreen color (R:124,G:252,B:0,A:255).
        /// </summary>
        public static ColorPacked LawnGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// LemonChiffon color (R:255,G:250,B:205,A:255).
        /// </summary>
        public static ColorPacked LemonChiffon
        {
            get;
            private set;
        }

        /// <summary>
        /// LightBlue color (R:173,G:216,B:230,A:255).
        /// </summary>
        public static ColorPacked LightBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// LightCoral color (R:240,G:128,B:128,A:255).
        /// </summary>
        public static ColorPacked LightCoral
        {
            get;
            private set;
        }

        /// <summary>
        /// LightCyan color (R:224,G:255,B:255,A:255).
        /// </summary>
        public static ColorPacked LightCyan
        {
            get;
            private set;
        }

        /// <summary>
        /// LightGoldenrodYellow color (R:250,G:250,B:210,A:255).
        /// </summary>
        public static ColorPacked LightGoldenrodYellow
        {
            get;
            private set;
        }

        /// <summary>
        /// LightGray color (R:211,G:211,B:211,A:255).
        /// </summary>
        public static ColorPacked LightGray
        {
            get;
            private set;
        }

        /// <summary>
        /// LightGreen color (R:144,G:238,B:144,A:255).
        /// </summary>
        public static ColorPacked LightGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// LightPink color (R:255,G:182,B:193,A:255).
        /// </summary>
        public static ColorPacked LightPink
        {
            get;
            private set;
        }

        /// <summary>
        /// LightSalmon color (R:255,G:160,B:122,A:255).
        /// </summary>
        public static ColorPacked LightSalmon
        {
            get;
            private set;
        }

        /// <summary>
        /// LightSeaGreen color (R:32,G:178,B:170,A:255).
        /// </summary>
        public static ColorPacked LightSeaGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// LightSkyBlue color (R:135,G:206,B:250,A:255).
        /// </summary>
        public static ColorPacked LightSkyBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// LightSlateGray color (R:119,G:136,B:153,A:255).
        /// </summary>
        public static ColorPacked LightSlateGray
        {
            get;
            private set;
        }

        /// <summary>
        /// LightSteelBlue color (R:176,G:196,B:222,A:255).
        /// </summary>
        public static ColorPacked LightSteelBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// LightYellow color (R:255,G:255,B:224,A:255).
        /// </summary>
        public static ColorPacked LightYellow
        {
            get;
            private set;
        }

        /// <summary>
        /// Lime color (R:0,G:255,B:0,A:255).
        /// </summary>
        public static ColorPacked Lime
        {
            get;
            private set;
        }

        /// <summary>
        /// LimeGreen color (R:50,G:205,B:50,A:255).
        /// </summary>
        public static ColorPacked LimeGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// Linen color (R:250,G:240,B:230,A:255).
        /// </summary>
        public static ColorPacked Linen
        {
            get;
            private set;
        }

        /// <summary>
        /// Magenta color (R:255,G:0,B:255,A:255).
        /// </summary>
        public static ColorPacked Magenta
        {
            get;
            private set;
        }

        /// <summary>
        /// Maroon color (R:128,G:0,B:0,A:255).
        /// </summary>
        public static ColorPacked Maroon
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumAquamarine color (R:102,G:205,B:170,A:255).
        /// </summary>
        public static ColorPacked MediumAquamarine
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumBlue color (R:0,G:0,B:205,A:255).
        /// </summary>
        public static ColorPacked MediumBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumOrchid color (R:186,G:85,B:211,A:255).
        /// </summary>
        public static ColorPacked MediumOrchid
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumPurple color (R:147,G:112,B:219,A:255).
        /// </summary>
        public static ColorPacked MediumPurple
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumSeaGreen color (R:60,G:179,B:113,A:255).
        /// </summary>
        public static ColorPacked MediumSeaGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumSlateBlue color (R:123,G:104,B:238,A:255).
        /// </summary>
        public static ColorPacked MediumSlateBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumSpringGreen color (R:0,G:250,B:154,A:255).
        /// </summary>
        public static ColorPacked MediumSpringGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumTurquoise color (R:72,G:209,B:204,A:255).
        /// </summary>
        public static ColorPacked MediumTurquoise
        {
            get;
            private set;
        }

        /// <summary>
        /// MediumVioletRed color (R:199,G:21,B:133,A:255).
        /// </summary>
        public static ColorPacked MediumVioletRed
        {
            get;
            private set;
        }

        /// <summary>
        /// MidnightBlue color (R:25,G:25,B:112,A:255).
        /// </summary>
        public static ColorPacked MidnightBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// MintCream color (R:245,G:255,B:250,A:255).
        /// </summary>
        public static ColorPacked MintCream
        {
            get;
            private set;
        }

        /// <summary>
        /// MistyRose color (R:255,G:228,B:225,A:255).
        /// </summary>
        public static ColorPacked MistyRose
        {
            get;
            private set;
        }

        /// <summary>
        /// Moccasin color (R:255,G:228,B:181,A:255).
        /// </summary>
        public static ColorPacked Moccasin
        {
            get;
            private set;
        }

        /// <summary>
        /// MonoGame orange theme color (R:231,G:60,B:0,A:255).
        /// </summary>
        public static ColorPacked MonoGameOrange
        {
            get;
            private set;
        }

        /// <summary>
        /// NavajoWhite color (R:255,G:222,B:173,A:255).
        /// </summary>
        public static ColorPacked NavajoWhite
        {
            get;
            private set;
        }

        /// <summary>
        /// Navy color (R:0,G:0,B:128,A:255).
        /// </summary>
        public static ColorPacked Navy
        {
            get;
            private set;
        }

        /// <summary>
        /// OldLace color (R:253,G:245,B:230,A:255).
        /// </summary>
        public static ColorPacked OldLace
        {
            get;
            private set;
        }

        /// <summary>
        /// Olive color (R:128,G:128,B:0,A:255).
        /// </summary>
        public static ColorPacked Olive
        {
            get;
            private set;
        }

        /// <summary>
        /// OliveDrab color (R:107,G:142,B:35,A:255).
        /// </summary>
        public static ColorPacked OliveDrab
        {
            get;
            private set;
        }

        /// <summary>
        /// Orange color (R:255,G:165,B:0,A:255).
        /// </summary>
        public static ColorPacked Orange
        {
            get;
            private set;
        }

        /// <summary>
        /// OrangeRed color (R:255,G:69,B:0,A:255).
        /// </summary>
        public static ColorPacked OrangeRed
        {
            get;
            private set;
        }

        /// <summary>
        /// Orchid color (R:218,G:112,B:214,A:255).
        /// </summary>
        public static ColorPacked Orchid
        {
            get;
            private set;
        }

        /// <summary>
        /// PaleGoldenrod color (R:238,G:232,B:170,A:255).
        /// </summary>
        public static ColorPacked PaleGoldenrod
        {
            get;
            private set;
        }

        /// <summary>
        /// PaleGreen color (R:152,G:251,B:152,A:255).
        /// </summary>
        public static ColorPacked PaleGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// PaleTurquoise color (R:175,G:238,B:238,A:255).
        /// </summary>
        public static ColorPacked PaleTurquoise
        {
            get;
            private set;
        }
        /// <summary>
        /// PaleVioletRed color (R:219,G:112,B:147,A:255).
        /// </summary>
        public static ColorPacked PaleVioletRed
        {
            get;
            private set;
        }

        /// <summary>
        /// PapayaWhip color (R:255,G:239,B:213,A:255).
        /// </summary>
        public static ColorPacked PapayaWhip
        {
            get;
            private set;
        }

        /// <summary>
        /// PeachPuff color (R:255,G:218,B:185,A:255).
        /// </summary>
        public static ColorPacked PeachPuff
        {
            get;
            private set;
        }

        /// <summary>
        /// Peru color (R:205,G:133,B:63,A:255).
        /// </summary>
        public static ColorPacked Peru
        {
            get;
            private set;
        }

        /// <summary>
        /// Pink color (R:255,G:192,B:203,A:255).
        /// </summary>
        public static ColorPacked Pink
        {
            get;
            private set;
        }

        /// <summary>
        /// Plum color (R:221,G:160,B:221,A:255).
        /// </summary>
        public static ColorPacked Plum
        {
            get;
            private set;
        }

        /// <summary>
        /// PowderBlue color (R:176,G:224,B:230,A:255).
        /// </summary>
        public static ColorPacked PowderBlue
        {
            get;
            private set;
        }

        /// <summary>
        ///  Purple color (R:128,G:0,B:128,A:255).
        /// </summary>
        public static ColorPacked Purple
        {
            get;
            private set;
        }

        /// <summary>
        /// Red color (R:255,G:0,B:0,A:255).
        /// </summary>
        public static ColorPacked Red
        {
            get;
            private set;
        }

        /// <summary>
        /// RosyBrown color (R:188,G:143,B:143,A:255).
        /// </summary>
        public static ColorPacked RosyBrown
        {
            get;
            private set;
        }

        /// <summary>
        /// RoyalBlue color (R:65,G:105,B:225,A:255).
        /// </summary>
        public static ColorPacked RoyalBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// SaddleBrown color (R:139,G:69,B:19,A:255).
        /// </summary>
        public static ColorPacked SaddleBrown
        {
            get;
            private set;
        }

        /// <summary>
        /// Salmon color (R:250,G:128,B:114,A:255).
        /// </summary>
        public static ColorPacked Salmon
        {
            get;
            private set;
        }

        /// <summary>
        /// SandyBrown color (R:244,G:164,B:96,A:255).
        /// </summary>
        public static ColorPacked SandyBrown
        {
            get;
            private set;
        }

        /// <summary>
        /// SeaGreen color (R:46,G:139,B:87,A:255).
        /// </summary>
        public static ColorPacked SeaGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// SeaShell color (R:255,G:245,B:238,A:255).
        /// </summary>
        public static ColorPacked SeaShell
        {
            get;
            private set;
        }

        /// <summary>
        /// Sienna color (R:160,G:82,B:45,A:255).
        /// </summary>
        public static ColorPacked Sienna
        {
            get;
            private set;
        }

        /// <summary>
        /// Silver color (R:192,G:192,B:192,A:255).
        /// </summary>
        public static ColorPacked Silver
        {
            get;
            private set;
        }

        /// <summary>
        /// SkyBlue color (R:135,G:206,B:235,A:255).
        /// </summary>
        public static ColorPacked SkyBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// SlateBlue color (R:106,G:90,B:205,A:255).
        /// </summary>
        public static ColorPacked SlateBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// SlateGray color (R:112,G:128,B:144,A:255).
        /// </summary>
        public static ColorPacked SlateGray
        {
            get;
            private set;
        }

        /// <summary>
        /// Snow color (R:255,G:250,B:250,A:255).
        /// </summary>
        public static ColorPacked Snow
        {
            get;
            private set;
        }

        /// <summary>
        /// SpringGreen color (R:0,G:255,B:127,A:255).
        /// </summary>
        public static ColorPacked SpringGreen
        {
            get;
            private set;
        }

        /// <summary>
        /// SteelBlue color (R:70,G:130,B:180,A:255).
        /// </summary>
        public static ColorPacked SteelBlue
        {
            get;
            private set;
        }

        /// <summary>
        /// Tan color (R:210,G:180,B:140,A:255).
        /// </summary>
        public static ColorPacked Tan
        {
            get;
            private set;
        }

        /// <summary>
        /// Teal color (R:0,G:128,B:128,A:255).
        /// </summary>
        public static ColorPacked Teal
        {
            get;
            private set;
        }

        /// <summary>
        /// Thistle color (R:216,G:191,B:216,A:255).
        /// </summary>
        public static ColorPacked Thistle
        {
            get;
            private set;
        }

        /// <summary>
        /// Tomato color (R:255,G:99,B:71,A:255).
        /// </summary>
        public static ColorPacked Tomato
        {
            get;
            private set;
        }

        /// <summary>
        /// Turquoise color (R:64,G:224,B:208,A:255).
        /// </summary>
        public static ColorPacked Turquoise
        {
            get;
            private set;
        }

        /// <summary>
        /// Violet color (R:238,G:130,B:238,A:255).
        /// </summary>
        public static ColorPacked Violet
        {
            get;
            private set;
        }

        /// <summary>
        /// Wheat color (R:245,G:222,B:179,A:255).
        /// </summary>
        public static ColorPacked Wheat
        {
            get;
            private set;
        }

        /// <summary>
        /// White color (R:255,G:255,B:255,A:255).
        /// </summary>
        public static ColorPacked White
        {
            get;
            private set;
        }

        /// <summary>
        /// WhiteSmoke color (R:245,G:245,B:245,A:255).
        /// </summary>
        public static ColorPacked WhiteSmoke
        {
            get;
            private set;
        }

        /// <summary>
        /// Yellow color (R:255,G:255,B:0,A:255).
        /// </summary>
        public static ColorPacked Yellow
        {
            get;
            private set;
        }

        /// <summary>
        /// YellowGreen color (R:154,G:205,B:50,A:255).
        /// </summary>
        public static ColorPacked YellowGreen
        {
            get;
            private set;
        }
        #endregion

        /// <summary>
        /// Performs linear interpolation of <see cref="ColorPacked"/>.
        /// </summary>
        /// <param name="value1">Source <see cref="ColorPacked"/>.</param>
        /// <param name="value2">Destination <see cref="ColorPacked"/>.</param>
        /// <param name="amount">Interpolation factor.</param>
        /// <returns>Interpolated <see cref="ColorPacked"/>.</returns>
        public static ColorPacked Lerp(ColorPacked value1, ColorPacked value2, Single amount)
        {
            amount = MathHelper.Clamp(amount, 0, 1);
            return new ColorPacked(
                (int)MathHelper.Lerp(value1.R, value2.R, amount),
                (int)MathHelper.Lerp(value1.G, value2.G, amount),
                (int)MathHelper.Lerp(value1.B, value2.B, amount),
                (int)MathHelper.Lerp(value1.A, value2.A, amount));
        }

        /// <summary>
        /// <see cref="ColorPacked.Lerp"/> should be used instead of this function.
        /// </summary>
        /// <returns>Interpolated <see cref="ColorPacked"/>.</returns>
        [Obsolete("Color.Lerp should be used instead of this function.")]
        public static ColorPacked LerpPrecise(ColorPacked value1, ColorPacked value2, Single amount)
        {
            amount = MathHelper.Clamp(amount, 0, 1);
            return new ColorPacked(
                (int)MathHelper.LerpPrecise(value1.R, value2.R, amount),
                (int)MathHelper.LerpPrecise(value1.G, value2.G, amount),
                (int)MathHelper.LerpPrecise(value1.B, value2.B, amount),
                (int)MathHelper.LerpPrecise(value1.A, value2.A, amount));
        }

        /// <summary>
        /// Multiplies the RGBA component values of a color by the specified value.
        /// </summary>
        /// <param name="value">The source color value to multiply.</param>
        /// <param name="scale">The value to multiply the RGBA component values by.</param>
        /// <returns>The new color value created as a result of the multiplication.</returns>
        public static ColorPacked Multiply(ColorPacked value, float scale)
        {
            return new ColorPacked((int)(value.R * scale), (int)(value.G * scale), (int)(value.B * scale), (int)(value.A * scale));
        }

        /// <summary>
        /// Multiplies the Alpha component value of a color by the specified value.
        /// </summary>
        /// <param name="value">The source color value to multiply.</param>
        /// <param name="scale">The value to multiply the Alpha component value by.</param>
        /// <returns>The new color value created as a result of the multiplication.</returns>
        public static ColorPacked MultiplyAlpha(ColorPacked value, float scale)
        {
            return new ColorPacked(value.R, value.G, value.B, (int)(value.A * scale));
        }

        /// <summary>
        /// Multiplies the RGBA component values of a color by the specified value.
        /// </summary>
        /// <param name="value">The source color value to multiply.</param>
        /// <param name="scale">The value to multiply the RGBA component values by.</param>
        /// <returns>The new color value created as a result of the multiplication.</returns>
        public static ColorPacked operator *(ColorPacked value, float scale)
        {
            return new ColorPacked((int)(value.R * scale), (int)(value.G * scale), (int)(value.B * scale), (int)(value.A * scale));
        }

        /// <summary>
        /// Multiplies the RGBA component values of a color by the specified value.
        /// </summary>
        /// <param name="scale">The value to multiply the RGBA component values by.</param>
        /// <param name="value">The source color value to multiply.</param>
        /// <returns>The new color value created as a result of the multiplication.</returns>
        public static ColorPacked operator *(float scale, ColorPacked value)
        {
            return new ColorPacked((int)(value.R * scale), (int)(value.G * scale), (int)(value.B * scale), (int)(value.A * scale));
        }

        /// <summary>
        /// Multiplies two colors compenent-wise.
        /// </summary>
        /// <param name="color1">The first color to be multiplied.</param>
        /// <param name="color2">The second color to be multiplied.</param>
        /// <returns>The new color value created as a result of the multiplication.</returns>
        public static ColorPacked operator *(ColorPacked color1, ColorPacked color2)
        {
            return new ColorPacked(
                (color1.R * color2.R) / 255,
                (color1.G * color2.G) / 255,
                (color1.B * color2.B) / 255,
                (color1.A * color2.A) / 255
            );
        }

        /// <summary>
        /// Gets a <see cref="Vector3"/> representation for this object.
        /// </summary>
        /// <returns>A <see cref="Vector3"/> representation for this object.</returns>
        public Vector3 ToVector3()
        {
            return new Vector3(R / 255.0f, G / 255.0f, B / 255.0f);
        }

        /// <summary>
        /// Gets a <see cref="Vector4"/> representation for this object.
        /// </summary>
        /// <returns>A <see cref="Vector4"/> representation for this object.</returns>
        public Vector4 ToVector4()
        {
            return new Vector4(R / 255.0f, G / 255.0f, B / 255.0f, A / 255.0f);
        }

        /// <summary>
        /// Gets or sets packed value of this <see cref="ColorPacked"/>.
        /// </summary>
        public UInt32 PackedValue
        {
            get { return _packedValue; }
            set { _packedValue = value; }
        }


        internal string DebugDisplayString
        {
            get
            {
                return string.Concat(
                    this.R.ToString(), "  ",
                    this.G.ToString(), "  ",
                    this.B.ToString(), "  ",
                    this.A.ToString()
                );
            }
        }


        /// <summary>
        /// Returns a <see cref="String"/> representation of this <see cref="ColorPacked"/> in the format:
        /// {R:[red] G:[green] B:[blue] A:[alpha]}
        /// </summary>
        /// <returns><see cref="String"/> representation of this <see cref="ColorPacked"/>.</returns>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(25);
            sb.Append("{R:");
            sb.Append(R);
            sb.Append(" G:");
            sb.Append(G);
            sb.Append(" B:");
            sb.Append(B);
            sb.Append(" A:");
            sb.Append(A);
            sb.Append("}");
            return sb.ToString();
        }

        /// <summary>
        /// Translate a non-premultipled alpha <see cref="ColorPacked"/> to a <see cref="ColorPacked"/> that contains premultiplied alpha.
        /// </summary>
        /// <param name="vector">A <see cref="Vector4"/> representing color.</param>
        /// <returns>A <see cref="ColorPacked"/> which contains premultiplied alpha data.</returns>
        public static ColorPacked FromNonPremultiplied(Vector4 vector)
        {
            return new ColorPacked(vector.X * vector.W, vector.Y * vector.W, vector.Z * vector.W, vector.W);
        }

        /// <summary>
        /// Translate a non-premultipled alpha <see cref="ColorPacked"/> to a <see cref="ColorPacked"/> that contains premultiplied alpha.
        /// </summary>
        /// <param name="r">Red component value.</param>
        /// <param name="g">Green component value.</param>
        /// <param name="b">Blue component value.</param>
        /// <param name="a">Alpha component value.</param>
        /// <returns>A <see cref="ColorPacked"/> which contains premultiplied alpha data.</returns>
        public static ColorPacked FromNonPremultiplied(int r, int g, int b, int a)
        {
            return new ColorPacked(r * a / 255, g * a / 255, b * a / 255, a);
        }

        #region IEquatable<Color> Members

        /// <summary>
        /// Compares whether current instance is equal to specified <see cref="ColorPacked"/>.
        /// </summary>
        /// <param name="other">The <see cref="ColorPacked"/> to compare.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public bool Equals(ColorPacked other)
        {
            return this.PackedValue == other.PackedValue;
        }

        #endregion

        /// <summary>
        /// Deconstruction method for <see cref="ColorPacked"/>.
        /// </summary>
        /// <param name="r">Red component value from 0 to 255.</param>
        /// <param name="g">Green component value from 0 to 255.</param>
        /// <param name="b">Blue component value from 0 to 255.</param>
        public void Deconstruct(out byte r, out byte g, out byte b)
        {
            r = R;
            g = G;
            b = B;
        }

        /// <summary>
        /// Deconstruction method for <see cref="ColorPacked"/>.
        /// </summary>
        /// <param name="r">Red component value from 0.0f to 1.0f.</param>
        /// <param name="g">Green component value from 0.0f to 1.0f.</param>
        /// <param name="b">Blue component value from 0.0f to 1.0f.</param>
        public void Deconstruct(out float r, out float g, out float b)
        {
            r = R / 255f;
            g = G / 255f;
            b = B / 255f;
        }

        /// <summary>
        /// Deconstruction method for <see cref="ColorPacked"/> with Alpha.
        /// </summary>
        /// <param name="r">Red component value from 0 to 255.</param>
        /// <param name="g">Green component value from 0 to 255.</param>
        /// <param name="b">Blue component value from 0 to 255.</param>
        /// <param name="a">Alpha component value from 0 to 255.</param>
        public void Deconstruct(out byte r, out byte g, out byte b, out byte a)
        {
            r = R;
            g = G;
            b = B;
            a = A;
        }

        /// <summary>
        /// Deconstruction method for <see cref="ColorPacked"/> with Alpha.
        /// </summary>
        /// <param name="r">Red component value from 0.0f to 1.0f.</param>
        /// <param name="g">Green component value from 0.0f to 1.0f.</param>
        /// <param name="b">Blue component value from 0.0f to 1.0f.</param>
        /// <param name="a">Alpha component value from 0.0f to 1.0f.</param>
        public void Deconstruct(out float r, out float g, out float b, out float a)
        {
            r = R / 255f;
            g = G / 255f;
            b = B / 255f;
            a = A / 255f;
        }
    }
}