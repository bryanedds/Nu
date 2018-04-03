using System;

namespace OpenTK
{
    /// <summary>
    /// A 2d int vector.
    /// NOTE: implements a very arbitrary comparison method.
    /// </summary>
    public struct Vector2i : IComparable<Vector2i>, IComparable
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

        public Vector2i(Vector2 v)
        {
            X = (int) v.X;
            Y = (int) v.Y;
        }

        public int CompareTo(Vector2i that)
        {
            if (X < that.X) return -1;
            if (X > that.X) return 1;
            if (Y < that.Y) return -1;
            if (Y > that.Y) return 1;
            return 0;
        }

        public int CompareTo(object that)
        {
            return CompareTo((Vector2i)that);
        }

        public Vector2 Vector2
        {
            get { return new Vector2(X, Y); }
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

        public static Vector2i Zero { get { return new Vector2i(0); } }
        public static Vector2i One { get { return new Vector2i(1); } }
        public static Vector2i Up { get { return new Vector2i(0, 1); } }
        public static Vector2i Right { get { return new Vector2i(1, 0); } }
        public static Vector2i Down { get { return new Vector2i(0, -1); } }
        public static Vector2i Left { get { return new Vector2i(-1, 0); } }

        public override string ToString()
        {
            return "(" + X + ", " + Y + ")";
        }

        public override int GetHashCode()
        {
            return X ^ Y;
        }

        public int X;
        public int Y;
    }
}
