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

        public int CompareTo(Vector2I other)
        {
            if (X < other.X) return -1;
            if (X > other.X) return 1;
            if (Y < other.Y) return -1;
            if (Y > other.Y) return 1;
            return 0;
        }

        public int CompareTo(object other)
        {
            return CompareTo((Vector2I)other);
        }

        public int X;
        public int Y;
    }
}
