//
//  Color.cs
//
//  Copyright (C) OpenTK
//
//  This software may be modified and distributed under the terms
//  of the MIT license. See the LICENSE file for details.
//

using System;
using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;

namespace Nu
{
    /// <summary>
    /// Represents a color using four bytes.
    /// </summary>
    /// <remarks>
    /// The Color structure is suitable for interoperation with unmanaged code requiring four consecutive bytes.
    /// </remarks>
    [Serializable]
    [StructLayout(LayoutKind.Sequential)]
    public struct Color : IEquatable<Color>, IComparable<Color>, IComparable
    {
        /// <summary>
        /// The X component of the Color.
        /// </summary>
        public byte R;

        /// <summary>
        /// The Y component of the Color.
        /// </summary>
        public byte G;

        /// <summary>
        /// The Z component of the Color.
        /// </summary>
        public byte B;

        /// <summary>
        /// The W component of the Color.
        /// </summary>
        public byte A;

        /// <summary>
        /// Defines a zero-length <see cref="Color"/>.
        /// </summary>
        public static readonly Color Zero = new Color(0, 0, 0, 0);

        /// <summary>
        /// Defines an instance with all components set to 1.
        /// </summary>
        public static readonly Color One = new Color(1, 1, 1, 1);

        /// <summary>
        /// The white color.
        /// </summary>
        public static readonly Color White = new Color(255, 255, 255, 255);

        /// <summary>
        /// The black color.
        /// </summary>
        public static readonly Color Black = new Color(0, 0, 0, 255);

        /// <summary>
        /// Defines the size of the <see cref="Color"/> struct in bytes.
        /// </summary>
        public static readonly int SizeInBytes = Marshal.SizeOf<Color>();

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct.
        /// </summary>
        /// <param name="value">The value that will initialize this instance.</param>
        public Color(byte value)
        {
            R = value;
            G = value;
            B = value;
            A = value;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct.
        /// </summary>
        public Color(byte r, byte g, byte b, byte a)
        {
            R = r;
            G = g;
            B = b;
            A = a;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct.
        /// </summary>
        public Color(Vector4 vector)
        {
            R = (byte)vector.X;
            G = (byte)vector.Y;
            B = (byte)vector.Z;
            A = (byte)vector.W;
        }

        /// <summary>
        /// Gets or sets the value at the index of the color.
        /// </summary>
        /// <param name="index">The index of the component from the color.</param>
        /// <exception cref="IndexOutOfRangeException">Thrown if the index is less than 0 or greater than 3.</exception>
        public byte this[int index]
        {
            get
            {
                if (index == 0)
                {
                    return R;
                }

                if (index == 1)
                {
                    return G;
                }

                if (index == 2)
                {
                    return B;
                }

                if (index == 3)
                {
                    return A;
                }

                throw new IndexOutOfRangeException("You tried to access this color at index: " + index);
            }

            set
            {
                if (index == 0)
                {
                    R = value;
                }
                else if (index == 1)
                {
                    G = value;
                }
                else if (index == 2)
                {
                    B = value;
                }
                else if (index == 3)
                {
                    A = value;
                }
                else
                {
                    throw new IndexOutOfRangeException("You tried to set this color at index: " + index);
                }
            }
        }

        /// <summary>
        /// Adds two colors.
        /// </summary>
        /// <param name="a">Left operand.</param>
        /// <param name="b">Right operand.</param>
        /// <returns>Result of operation.</returns>
        [Pure]
        public static Color Add(Color a, Color b)
        {
            Add(ref a, ref b, out a);
            return a;
        }

        /// <summary>
        /// Adds two colors.
        /// </summary>
        /// <param name="a">Left operand.</param>
        /// <param name="b">Right operand.</param>
        /// <param name="result">Result of operation.</param>
        public static void Add(ref Color a, ref Color b, out Color result)
        {
            result.R = (byte)(a.R + b.R);
            result.G = (byte)(a.G + b.G);
            result.B = (byte)(a.B + b.B);
            result.A = (byte)(a.A + b.A);
        }

        /// <summary>
        /// Subtract one color from another.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>Result of subtraction.</returns>
        [Pure]
        public static Color Subtract(Color a, Color b)
        {
            Subtract(ref a, ref b, out a);
            return a;
        }

        /// <summary>
        /// Subtract one color from another.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">Result of subtraction.</param>
        public static void Subtract(ref Color a, ref Color b, out Color result)
        {
            result.R = (byte)(a.R - b.R);
            result.G = (byte)(a.G - b.G);
            result.B = (byte)(a.B - b.B);
            result.A = (byte)(a.A - b.A);
        }

        /// <summary>
        /// Multiplies a color by an integer scalar.
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Color Multiply(Color color, float scale)
        {
            Multiply(ref color, scale, out color);
            return color;
        }

        /// <summary>
        /// Multiplies a color by an integer scalar.
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Multiply(ref Color color, float scale, out Color result)
        {
            result.R = (byte)(color.R * scale);
            result.G = (byte)(color.G * scale);
            result.B = (byte)(color.B * scale);
            result.A = (byte)(color.A * scale);
        }

        /// <summary>
        /// Multiplies a color by the components a color (scale).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Color Multiply(Color color, Color scale)
        {
            Multiply(ref color, ref scale, out color);
            return color;
        }

        /// <summary>
        /// Multiplies a color by the components of a color (scale).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Multiply(ref Color color, ref Color scale, out Color result)
        {
            result.R = (byte)(color.R * (scale.R / 255.0f));
            result.G = (byte)(color.G * (scale.G / 255.0f));
            result.B = (byte)(color.B * (scale.B / 255.0f));
            result.A = (byte)(color.A * (scale.A / 255.0f));
        }

        /// <summary>
        /// Divides a color by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Color Divide(Color color, float scale)
        {
            Divide(ref color, scale, out color);
            return color;
        }

        /// <summary>
        /// Divides a color by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Divide(ref Color color, float scale, out Color result)
        {
            result.R = (byte)(color.R / scale);
            result.G = (byte)(color.G / scale);
            result.B = (byte)(color.B / scale);
            result.A = (byte)(color.A / scale);
        }

        /// <summary>
        /// Divides a color by the components of a color using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Color Divide(Color color, Color scale)
        {
            Divide(ref color, ref scale, out color);
            return color;
        }

        /// <summary>
        /// Divides a color by the components of a color using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Divide(ref Color color, ref Color scale, out Color result)
        {
            result.R = (byte)(color.R / (scale.R / 255.0f));
            result.G = (byte)(color.G / (scale.G / 255.0f));
            result.B = (byte)(color.B / (scale.B / 255.0f));
            result.A = (byte)(color.A / (scale.A / 255.0f));
        }

        /// <summary>
        /// Returns a color created from the smallest of the corresponding components of the given colors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The component-wise minimum.</returns>
        [Pure]
        public static Color ComponentMin(Color a, Color b)
        {
            a.R = a.R < b.R ? a.R : b.R;
            a.G = a.G < b.G ? a.G : b.G;
            a.B = a.B < b.B ? a.B : b.B;
            a.A = a.A < b.A ? a.A : b.A;
            return a;
        }

        /// <summary>
        /// Returns a color created from the smallest of the corresponding components of the given colors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">The component-wise minimum.</param>
        public static void ComponentMin(ref Color a, ref Color b, out Color result)
        {
            result.R = a.R < b.R ? a.R : b.R;
            result.G = a.G < b.G ? a.G : b.G;
            result.B = a.B < b.B ? a.B : b.B;
            result.A = a.A < b.A ? a.A : b.A;
        }

        /// <summary>
        /// Returns a color created from the largest of the corresponding components of the given colors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The component-wise maximum.</returns>
        [Pure]
        public static Color ComponentMax(Color a, Color b)
        {
            a.R = a.R > b.R ? a.R : b.R;
            a.G = a.G > b.G ? a.G : b.G;
            a.B = a.B > b.B ? a.B : b.B;
            a.A = a.A > b.A ? a.A : b.A;
            return a;
        }

        /// <summary>
        /// Returns a color created from the largest of the corresponding components of the given colors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">The component-wise maximum.</param>
        public static void ComponentMax(ref Color a, ref Color b, out Color result)
        {
            result.R = a.R > b.R ? a.R : b.R;
            result.G = a.G > b.G ? a.G : b.G;
            result.B = a.B > b.B ? a.B : b.B;
            result.A = a.A > b.A ? a.A : b.A;
        }

        /// <summary>
        /// Clamp a color to the given minimum and maximum colors.
        /// </summary>
        /// <param name="color">Input color.</param>
        /// <param name="min">Minimum color.</param>
        /// <param name="max">Maximum color.</param>
        /// <returns>The clamped color.</returns>
        [Pure]
        public static Color Clamp(Color color, Color min, Color max)
        {
            color.R = color.R < min.R ? min.R : color.R > max.R ? max.R : color.R;
            color.G = color.G < min.G ? min.G : color.G > max.G ? max.G : color.G;
            color.B = color.B < min.B ? min.B : color.B > max.B ? max.B : color.B;
            color.A = color.A < min.A ? min.A : color.A > max.A ? max.A : color.A;
            return color;
        }

        /// <summary>
        /// Clamp a color to the given minimum and maximum colors.
        /// </summary>
        /// <param name="color">Input color.</param>
        /// <param name="min">Minimum color.</param>
        /// <param name="max">Maximum color.</param>
        /// <param name="result">The clamped color.</param>
        public static void Clamp(ref Color color, ref Color min, ref Color max, out Color result)
        {
            result.R = color.R < min.R ? min.R : color.R > max.R ? max.R : color.R;
            result.G = color.G < min.G ? min.G : color.G > max.G ? max.G : color.G;
            result.B = color.B < min.B ? min.B : color.B > max.B ? max.B : color.B;
            result.A = color.A < min.A ? min.A : color.A > max.A ? max.A : color.A;
        }

        /// <summary>
        /// Gets a <see cref="Vector4"/> object with the same component values as the <see cref="Color"/> instance.
        /// </summary>
        /// <returns>The resulting <see cref="Vector4"/> instance.</returns>
        public Vector4 ToVector4()
        {
            return new Vector4(R, G, B, A);
        }

        /// <summary>
        /// Gets a <see cref="Vector4"/> object with the same component values as the <see cref="Color"/> instance.
        /// </summary>
        /// <param name="input">The given <see cref="Color"/> to convert.</param>
        /// <param name="result">The resulting <see cref="Vector4"/>.</param>
        public static void ToVector4(ref Color input, out Vector4 result)
        {
            result.X = input.R;
            result.Y = input.G;
            result.Z = input.B;
            result.W = input.A;
        }

        /// <summary>
        /// Adds two instances.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
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
        [Pure]
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
        [Pure]
        public static Color operator *(Color color, float scale)
        {
            color.R = (byte)(color.R * scale);
            color.G = (byte)(color.G * scale);
            color.B = (byte)(color.B * scale);
            color.A = (byte)(color.A * scale);
            return color;
        }

        /// <summary>
        /// Multiplies an instance by an integer scalar.
        /// </summary>
        /// <param name="scale">The scalar.</param>
        /// <param name="color">The instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Color operator *(float scale, Color color)
        {
            color.R = (byte)(color.R * scale);
            color.G = (byte)(color.G * scale);
            color.B = (byte)(color.B * scale);
            color.A = (byte)(color.A * scale);
            return color;
        }

        /// <summary>
        /// Component-wise multiplication between the specified instance by a scale color.
        /// </summary>
        /// <param name="scale">Left operand.</param>
        /// <param name="color">Right operand.</param>
        /// <returns>Result of multiplication.</returns>
        [Pure]
        public static Color operator *(Color color, Color scale)
        {
            color.R = (byte)(color.R * (scale.R / 255.0f));
            color.G = (byte)(color.G * (scale.G / 255.0f));
            color.B = (byte)(color.B * (scale.B / 255.0f));
            color.A = (byte)(color.A * (scale.A / 255.0f));
            return color;
        }

        /// <summary>
        /// Divides the instance by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="color">The instance.</param>
        /// <param name="scale">The scalar.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Color operator /(Color color, float scale)
        {
            color.R = (byte)(color.R / scale);
            color.G = (byte)(color.G / scale);
            color.B = (byte)(color.B / scale);
            color.A = (byte)(color.A / scale);
            return color;
        }

        /// <summary>
        /// Compares two instances for equality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left equals right; false otherwise.</returns>
        [Pure]
        public static bool operator ==(Color left, Color right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Compares two instances for inequality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left does not equa lright; false otherwise.</returns>
        [Pure]
        public static bool operator !=(Color left, Color right)
        {
            return !left.Equals(right);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Color"/> struct using a tuple containing the component
        /// values.
        /// </summary>
        /// <param name="values">A tuple containing the component values.</param>
        /// <returns>A new instance of the <see cref="Color"/> struct with the given component values.</returns>
        [Pure]
        public static implicit operator Color((byte R, byte G, byte B, byte A) values)
        {
            return new Color(values.R, values.G, values.B, values.A);
        }

        private static readonly string ListSeparator = CultureInfo.CurrentCulture.TextInfo.ListSeparator;

        /// <inheritdoc />
        public override string ToString()
        {
            return string.Format("({0}{4} {1}{4} {2}{4} {3})", R, G, B, A, ListSeparator);
        }

        /// <summary>
        /// Deconstructs the color into it's individual components.
        /// </summary>
        [Pure]
        public void Deconstruct(out byte r, out byte g, out byte b, out byte a)
        {
            r = R;
            g = G;
            b = B;
            a = A;
        }

        #region Equatable and Comparable Members

        /// <summary>
        /// Returns the hashcode for this instance.
        /// </summary>
        /// <returns>A System.Int32 containing the unique hashcode for this instance.</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = R;
                hashCode = (hashCode * 397) ^ G;
                hashCode = (hashCode * 397) ^ B;
                hashCode = (hashCode * 397) ^ A;
                return hashCode;
            }
        }

        /// <summary>
        /// Indicates whether this instance and a specified object are equal.
        /// </summary>
        /// <param name="obj">The object to compare to.</param>
        /// <returns>True if the instances are equal; false otherwise.</returns>
        [Pure]
        public override bool Equals(object obj)
        {
            if (!(obj is Color))
            {
                return false;
            }

            return Equals((Color)obj);
        }

        /// <summary>Indicates whether the current color is equal to another color.</summary>
        /// <param name="other">A color to compare with this color.</param>
        /// <returns>true if the current color is equal to the color parameter; otherwise, false.</returns>
        public bool Equals(Color other)
        {
            return
                R == other.R &&
                G == other.G &&
                B == other.B &&
                A == other.A;
        }

        public int CompareTo(Color other)
        {
            var result = R.CompareTo(other.R);
            if (result != 0) return result;
            result = G.CompareTo(other.G);
            if (result != 0) return result;
            result = B.CompareTo(other.B);
            if (result != 0) return result;
            result = A.CompareTo(other.A);
            return result;
        }

        public int CompareTo(object obj)
        {
            if (obj is Color) return CompareTo((Color)obj);
            return -1;
        }

        #endregion
    }
}