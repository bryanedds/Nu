//
//  Vector4i.cs
//
//  Copyright (C) OpenTK
//
//  This software may be modified and distributed under the terms
//  of the MIT license. See the LICENSE file for details.
//

using System;
using System.Diagnostics.Contracts;
using System.Runtime.InteropServices;

namespace Nu
{
    /// <summary>
    /// Represents a 4D vector using four 32-bit integer numbers.
    /// </summary>
    /// <remarks>
    /// The Vector4i structure is suitable for interoperation with unmanaged code requiring four consecutive integers.
    /// </remarks>
    [Serializable]
    [StructLayout(LayoutKind.Sequential)]
    public struct Vector4i : IEquatable<Vector4i>, IComparable<Vector4i>, IComparable
    {
        /// <summary>
        /// The X component of the Vector4i.
        /// </summary>
        public int X;

        /// <summary>
        /// The Y component of the Vector4i.
        /// </summary>
        public int Y;

        /// <summary>
        /// The Z component of the Vector4i.
        /// </summary>
        public int Z;

        /// <summary>
        /// The W component of the Vector4i.
        /// </summary>
        public int W;

        /// <summary>
        /// Defines a unit-length <see cref="Vector4i"/> that points towards the X-axis.
        /// </summary>
        public static readonly Vector4i UnitX = new Vector4i(1, 0, 0, 0);

        /// <summary>
        /// Defines a unit-length <see cref="Vector4i"/> that points towards the Y-axis.
        /// </summary>
        public static readonly Vector4i UnitY = new Vector4i(0, 1, 0, 0);

        /// <summary>
        /// Defines a unit-length <see cref="Vector4i"/> that points towards the Z-axis.
        /// </summary>
        public static readonly Vector4i UnitZ = new Vector4i(0, 0, 1, 0);

        /// <summary>
        /// Defines a unit-length <see cref="Vector4i"/> that points towards the W-axis.
        /// </summary>
        public static readonly Vector4i UnitW = new Vector4i(0, 0, 0, 1);

        /// <summary>
        /// Defines a zero-length <see cref="Vector4i"/>.
        /// </summary>
        public static readonly Vector4i Zero = new Vector4i(0, 0, 0, 0);

        /// <summary>
        /// Defines an instance with all components set to 1.
        /// </summary>
        public static readonly Vector4i One = new Vector4i(1, 1, 1, 1);

        /// <summary>
        /// Defines the size of the <see cref="Vector4i"/> struct in bytes.
        /// </summary>
        public static readonly int SizeInBytes = Marshal.SizeOf<Vector4i>();

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector4i"/> struct.
        /// </summary>
        /// <param name="value">The value that will initialize this instance.</param>
        public Vector4i(int value)
        {
            X = value;
            Y = value;
            Z = value;
            W = value;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector4i"/> struct.
        /// </summary>
        /// <param name="x">The X component of the Vector4i.</param>
        /// <param name="y">The Y component of the Vector4i.</param>
        /// <param name="z">The Z component of the Vector4i.</param>
        /// <param name="w">The W component of the Vector4i.</param>
        public Vector4i(int x, int y, int z, int w)
        {
            X = x;
            Y = y;
            Z = z;
            W = w;
        }

        /// <summary>
        /// Gets or sets the value at the index of the vector.
        /// </summary>
        /// <param name="index">The index of the component from the vector.</param>
        /// <exception cref="IndexOutOfRangeException">Thrown if the index is less than 0 or greater than 3.</exception>
        public int this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return X;
                    case 1: return Y;
                    case 2: return Z;
                    case 3: return W;
                    default: throw new IndexOutOfRangeException("You tried to access this vector at index: " + index);
                }
            }

            set
            {
                switch (index)
                {
                    case 0: X = value; break;
                    case 1: Y = value; break;
                    case 2: Z = value; break;
                    case 3: W = value; break;
                    default: throw new IndexOutOfRangeException("You tried to set this vector at index: " + index);
                }
            }
        }

        /// <summary>
        /// Adds two vectors.
        /// </summary>
        /// <param name="a">Left operand.</param>
        /// <param name="b">Right operand.</param>
        /// <returns>Result of operation.</returns>
        [Pure]
        public static Vector4i Add(Vector4i a, Vector4i b)
        {
            Add(in a, in b, out a);
            return a;
        }

        /// <summary>
        /// Adds two vectors.
        /// </summary>
        /// <param name="a">Left operand.</param>
        /// <param name="b">Right operand.</param>
        /// <param name="result">Result of operation.</param>
        public static void Add(in Vector4i a, in Vector4i b, out Vector4i result)
        {
            result.X = a.X + b.X;
            result.Y = a.Y + b.Y;
            result.Z = a.Z + b.Z;
            result.W = a.W + b.W;
        }

        /// <summary>
        /// Subtract one Vector from another.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>Result of subtraction.</returns>
        [Pure]
        public static Vector4i Subtract(Vector4i a, Vector4i b)
        {
            Subtract(in a, in b, out a);
            return a;
        }

        /// <summary>
        /// Subtract one Vector from another.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">Result of subtraction.</param>
        public static void Subtract(in Vector4i a, in Vector4i b, out Vector4i result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
            result.Z = a.Z - b.Z;
            result.W = a.W - b.W;
        }

        /// <summary>
        /// Multiplies a vector by an integer scalar.
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Vector4i Multiply(Vector4i vector, int scale)
        {
            Multiply(in vector, scale, out vector);
            return vector;
        }

        /// <summary>
        /// Multiplies a vector by an integer scalar.
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Multiply(in Vector4i vector, int scale, out Vector4i result)
        {
            result.X = vector.X * scale;
            result.Y = vector.Y * scale;
            result.Z = vector.Z * scale;
            result.W = vector.W * scale;
        }

        /// <summary>
        /// Multiplies a vector by the components a vector (scale).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Vector4i Multiply(Vector4i vector, Vector4i scale)
        {
            Multiply(in vector, in scale, out vector);
            return vector;
        }

        /// <summary>
        /// Multiplies a vector by the components of a vector (scale).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Multiply(in Vector4i vector, in Vector4i scale, out Vector4i result)
        {
            result.X = vector.X * scale.X;
            result.Y = vector.Y * scale.Y;
            result.Z = vector.Z * scale.Z;
            result.W = vector.W * scale.W;
        }

        /// <summary>
        /// Divides a vector by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Vector4i Divide(Vector4i vector, int scale)
        {
            Divide(in vector, scale, out vector);
            return vector;
        }

        /// <summary>
        /// Divides a vector by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Divide(in Vector4i vector, int scale, out Vector4i result)
        {
            result.X = vector.X / scale;
            result.Y = vector.Y / scale;
            result.Z = vector.Z / scale;
            result.W = vector.W / scale;
        }

        /// <summary>
        /// Divides a vector by the components of a vector using integer division, floor(a/b).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <returns>Result of the operation.</returns>
        [Pure]
        public static Vector4i Divide(Vector4i vector, Vector4i scale)
        {
            Divide(in vector, in scale, out vector);
            return vector;
        }

        /// <summary>
        /// Divides a vector by the components of a vector using integer division, floor(a/b).
        /// </summary>
        /// <param name="vector">Left operand.</param>
        /// <param name="scale">Right operand.</param>
        /// <param name="result">Result of the operation.</param>
        public static void Divide(in Vector4i vector, in Vector4i scale, out Vector4i result)
        {
            result.X = vector.X / scale.X;
            result.Y = vector.Y / scale.Y;
            result.Z = vector.Z / scale.Z;
            result.W = vector.W / scale.W;
        }

        /// <summary>
        /// Returns a vector created from the smallest of the corresponding components of the given vectors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The component-wise minimum.</returns>
        [Pure]
        public static Vector4i ComponentMin(Vector4i a, Vector4i b)
        {
            a.X = a.X < b.X ? a.X : b.X;
            a.Y = a.Y < b.Y ? a.Y : b.Y;
            a.Z = a.Z < b.Z ? a.Z : b.Z;
            a.W = a.W < b.W ? a.W : b.W;
            return a;
        }

        /// <summary>
        /// Returns a vector created from the smallest of the corresponding components of the given vectors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">The component-wise minimum.</param>
        public static void ComponentMin(in Vector4i a, in Vector4i b, out Vector4i result)
        {
            result.X = a.X < b.X ? a.X : b.X;
            result.Y = a.Y < b.Y ? a.Y : b.Y;
            result.Z = a.Z < b.Z ? a.Z : b.Z;
            result.W = a.W < b.W ? a.W : b.W;
        }

        /// <summary>
        /// Returns a vector created from the largest of the corresponding components of the given vectors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The component-wise maximum.</returns>
        [Pure]
        public static Vector4i ComponentMax(Vector4i a, Vector4i b)
        {
            a.X = a.X > b.X ? a.X : b.X;
            a.Y = a.Y > b.Y ? a.Y : b.Y;
            a.Z = a.Z > b.Z ? a.Z : b.Z;
            a.W = a.W > b.W ? a.W : b.W;
            return a;
        }

        /// <summary>
        /// Returns a vector created from the largest of the corresponding components of the given vectors.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <param name="result">The component-wise maximum.</param>
        public static void ComponentMax(in Vector4i a, in Vector4i b, out Vector4i result)
        {
            result.X = a.X > b.X ? a.X : b.X;
            result.Y = a.Y > b.Y ? a.Y : b.Y;
            result.Z = a.Z > b.Z ? a.Z : b.Z;
            result.W = a.W > b.W ? a.W : b.W;
        }

        /// <summary>
        /// Clamp a vector to the given minimum and maximum vectors.
        /// </summary>
        /// <param name="vec">Input vector.</param>
        /// <param name="min">Minimum vector.</param>
        /// <param name="max">Maximum vector.</param>
        /// <returns>The clamped vector.</returns>
        [Pure]
        public static Vector4i Clamp(Vector4i vec, Vector4i min, Vector4i max)
        {
            vec.X = vec.X < min.X ? min.X : vec.X > max.X ? max.X : vec.X;
            vec.Y = vec.Y < min.Y ? min.Y : vec.Y > max.Y ? max.Y : vec.Y;
            vec.Z = vec.Z < min.Z ? min.Z : vec.Z > max.Z ? max.Z : vec.Z;
            vec.W = vec.W < min.W ? min.W : vec.W > max.W ? max.W : vec.W;
            return vec;
        }

        /// <summary>
        /// Clamp a vector to the given minimum and maximum vectors.
        /// </summary>
        /// <param name="vec">Input vector.</param>
        /// <param name="min">Minimum vector.</param>
        /// <param name="max">Maximum vector.</param>
        /// <param name="result">The clamped vector.</param>
        public static void Clamp(in Vector4i vec, in Vector4i min, in Vector4i max, out Vector4i result)
        {
            result.X = vec.X < min.X ? min.X : vec.X > max.X ? max.X : vec.X;
            result.Y = vec.Y < min.Y ? min.Y : vec.Y > max.Y ? max.Y : vec.Y;
            result.Z = vec.Z < min.Z ? min.Z : vec.Z > max.Z ? max.Z : vec.Z;
            result.W = vec.W < min.W ? min.W : vec.W > max.W ? max.W : vec.W;
        }

        /// <summary>
        /// Adds two instances.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator +(Vector4i left, Vector4i right)
        {
            left.X += right.X;
            left.Y += right.Y;
            left.Z += right.Z;
            left.W += right.W;
            return left;
        }

        /// <summary>
        /// Subtracts two instances.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator -(Vector4i left, Vector4i right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            left.Z -= right.Z;
            left.W -= right.W;
            return left;
        }

        /// <summary>
        /// Negates an instance.
        /// </summary>
        /// <param name="vec">The instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator -(Vector4i vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            vec.Z = -vec.Z;
            vec.W = -vec.W;
            return vec;
        }

        /// <summary>
        /// Multiplies an instance by an integer scalar.
        /// </summary>
        /// <param name="vec">The instance.</param>
        /// <param name="scale">The scalar.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator *(Vector4i vec, int scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }

        /// <summary>
        /// Multiplies an instance by an integer scalar.
        /// </summary>
        /// <param name="scale">The scalar.</param>
        /// <param name="vec">The instance.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator *(int scale, Vector4i vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }

        /// <summary>
        /// Component-wise multiplication between the specified instance by a scale vector.
        /// </summary>
        /// <param name="scale">Left operand.</param>
        /// <param name="vec">Right operand.</param>
        /// <returns>Result of multiplication.</returns>
        [Pure]
        public static Vector4i operator *(Vector4i vec, Vector4i scale)
        {
            vec.X *= scale.X;
            vec.Y *= scale.Y;
            vec.Z *= scale.Z;
            vec.W *= scale.W;
            return vec;
        }

        /// <summary>
        /// Divides the instance by a scalar using integer division, floor(a/b).
        /// </summary>
        /// <param name="vec">The instance.</param>
        /// <param name="scale">The scalar.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator /(Vector4i vec, int scale)
        {
            vec.X /= scale;
            vec.Y /= scale;
            vec.Z /= scale;
            vec.W /= scale;
            return vec;
        }

        /// <summary>
        /// Divides the instance by another Vector4i using integer division, floor(a/b).
        /// </summary>
        /// <param name="vec">The instance.</param>
        /// <param name="vec2">The divisor.</param>
        /// <returns>The result of the calculation.</returns>
        [Pure]
        public static Vector4i operator /(Vector4i vec, Vector4i vec2)
        {
            vec.X /= vec2.X;
            vec.Y /= vec2.Y;
            vec.Z /= vec2.Z;
            vec.W /= vec2.W;
            return vec;
        }

        /// <summary>
        /// Compares two instances for equality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left equals right; false otherwise.</returns>
        [Pure]
        public static bool operator ==(Vector4i left, Vector4i right)
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
        public static bool operator !=(Vector4i left, Vector4i right)
        {
            return !left.Equals(right);
        }

        /// <inheritdoc />
        public override string ToString()
        {
            return $"<{X}, {Y}, {Z}, {W}>";
        }

        /// <summary>
        /// Returns the hashcode for this instance.
        /// </summary>
        /// <returns>A System.Int32 containing the unique hashcode for this instance.</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = X;
                hashCode = (hashCode * 397) ^ Y;
                hashCode = (hashCode * 397) ^ Z;
                hashCode = (hashCode * 397) ^ W;
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
            if (!(obj is Vector4i))
            {
                return false;
            }

            return Equals((Vector4i)obj);
        }

        /// <summary>Indicates whether the current vector is equal to another vector.</summary>
        /// <param name="other">A vector to compare with this vector.</param>
        /// <returns>true if the current vector is equal to the vector parameter; otherwise, false.</returns>
        public bool Equals(Vector4i other)
        {
            return
                X == other.X &&
                Y == other.Y &&
                Z == other.Z &&
                W == other.W;
        }

        public int CompareTo(Vector4i other)
        {
            var result = X.CompareTo(other.X);
            if (result != 0) return result;
            result = Y.CompareTo(other.Y);
            if (result != 0) return result;
            result = Z.CompareTo(other.Z);
            if (result != 0) return result;
            result = W.CompareTo(other.W);
            return result;
        }

        public int CompareTo(object obj)
        {
            if (obj is Vector4i) return CompareTo((Vector4i)obj);
            return -1;
        }
    }
}