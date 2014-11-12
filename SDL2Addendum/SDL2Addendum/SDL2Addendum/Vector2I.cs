using System;

namespace OpenTK
{
    /// <summary>
    /// A 2d int vector.
    /// NOTE: implements a very arbitrary comparison method.
    /// </summary>
    public struct Vector2I : IComparable<Vector2I>, IComparable
    {
        public Vector2I(int x, int y)
        {
            X = x;
            Y = y;
        }

        public int CompareTo(Vector2I that)
        {
            if (X < that.X) return -1;
            if (X > that.X) return 1;
            if (Y < that.Y) return -1;
            if (Y > that.Y) return 1;
            return 0;
        }

        public int CompareTo(object that)
        {
            return CompareTo((Vector2I)that);
        }

        public static Vector2I operator +(Vector2I v, Vector2I v2)
        {
            return new Vector2I(
                v.X + v2.X,
                v.Y + v2.Y);
        }

        public static Vector2I operator -(Vector2I v, Vector2I v2)
        {
            return new Vector2I(
                v.X - v2.X,
                v.Y - v2.Y);
        }

        public static Vector2I operator *(Vector2I v, int n)
        {
            return new Vector2I(
                v.X * n,
                v.Y * n);
        }

        public static Vector2I operator *(int n, Vector2I v)
        {
            return new Vector2I(
                n * v.X,
                n * v.Y);
        }

        public static Vector2I operator /(int n, Vector2I v)
        {
            return new Vector2I(
                n / v.X,
                n / v.Y);
        }

        public int X;
        public int Y;
    }
}
