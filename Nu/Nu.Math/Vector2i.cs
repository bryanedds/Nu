using System;
using System.Globalization;
using System.Numerics;
using System.Runtime.InteropServices;

namespace Nu
{
    /// <summary>
    /// A 2d int vector.
    /// </summary>
    public struct Vector2i : IEquatable<Vector2i>, IComparable<Vector2i>, IComparable
    {
        public Vector2i(int x, int y)
        {
            X = x;
            Y = y;
        }

        public Vector2i(int n)
        {
            X = n;
            Y = n;
        }

        public float LengthSquared
        {
            get { return X * X + Y * Y; }
        }

        public float Length
        {
            get { return (float)Math.Sqrt(LengthSquared); }
        }

        public static Vector2i operator +(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X + v2.X,
                v.Y + v2.Y);
        }

        public static Vector2i operator -(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X - v2.X,
                v.Y - v2.Y);
        }

        public static Vector2i operator *(Vector2i v, int n)
        {
            return new Vector2i(
                v.X * n,
                v.Y * n);
        }

        public static Vector2i operator *(int n, Vector2i v)
        {
            return new Vector2i(
                n * v.X,
                n * v.Y);
        }

        public static Vector2i operator *(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X * v2.X,
                v.Y * v2.Y);
        }

        public static Vector2i operator /(Vector2i v, int n)
        {
            return new Vector2i(
                v.X / n,
                v.Y / n);
        }

        public static Vector2i operator /(int n, Vector2i v)
        {
            return new Vector2i(
                n / v.X,
                n / v.Y);
        }

        public static Vector2i operator /(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X / v2.X,
                v.Y / v2.Y);
        }

        public static Vector2i Multiply(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X * v2.X,
                v.Y * v2.Y);
        }

        public static Vector2i Divide(Vector2i v, Vector2i v2)
        {
            return new Vector2i(
                v.X / v2.X,
                v.Y / v2.Y);
        }

        public int this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return X;
                    case 1: return Y;
                    default: throw new IndexOutOfRangeException("You tried to access this vector at index: " + index);
                }
            }

            set
            {
                switch (index)
                {
                    case 0: X = value; break;
                    case 1: Y = value; break;
                    default: throw new IndexOutOfRangeException("You tried to set this vector at index: " + index);
                }
            }
        }

        public static Vector2i UnitX { get { return new Vector2i(1, 0); } }
        public static Vector2i UnitY { get { return new Vector2i(0, 1); } }
        public static Vector2i Zero { get { return new Vector2i(0); } }
        public static Vector2i One { get { return new Vector2i(1); } }
        public static Vector2i Up { get { return new Vector2i(0, 1); } }
        public static Vector2i Right { get { return new Vector2i(1, 0); } }
        public static Vector2i Down { get { return new Vector2i(0, -1); } }
        public static Vector2i Left { get { return new Vector2i(-1, 0); } }
        public static readonly int SizeInBytes = Marshal.SizeOf<Vector2i>();

        /// <inheritdoc />
        public override string ToString()
        {
            return string.Format("({0}, {1})", X, Y);
        }

        public override int GetHashCode()
        {
            return X ^ Y;
        }

        public bool Equals(Vector2i other)
        {
            return
                X == other.X &&
                Y == other.Y;
        }

        public int CompareTo(Vector2i other)
        {
            var result = X.CompareTo(other.X);
            if (result != 0) return result;
            result = Y.CompareTo(other.Y);
            return result;
        }

        public int CompareTo(object obj)
        {
            if (obj is Vector2i) return CompareTo((Vector2i)obj);
            return -1;
        }

        public int X;
        public int Y;
    }
}
