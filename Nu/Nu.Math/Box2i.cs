//
// Box2i.cs
//
// Copyright (C) 2019 OpenTK
//
// This software may be modified and distributed under the terms
// of the MIT license. See the LICENSE file for details.
//

using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace Nu
{
    /// <summary>
    /// Defines an axis-aligned 2D box (rectangle) in integers.
    /// Copied from - https://github.com/opentk/opentk/blob/opentk5.0/src/OpenTK.Mathematics/Geometry/Box2.cs
    /// Heavily modified by BGE to more closely conform to System.Numerics and use a size-preserving representation
    /// ([min, size] instead of [min, max]), as well as using integers.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Box2i : IEquatable<Box2i>
    {
        /// <summary>
        /// The min of the box.
        /// </summary>
        public Vector2i Min;

        /// <summary>
        /// The size of the box.
        /// </summary>
        public Vector2i Size;

        /// <summary>
        /// Initializes a new instance of the <see cref="Box2i"/> struct.
        /// </summary>
        public Box2i(Vector2i min, Vector2i size)
        {
            Min = min;
            Size = size;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Box2i"/> struct.
        /// </summary>
        public Box2i(int minX, int minY, int sizeX, int sizeY)
        {
            Min = new Vector2i(minX, minY);
            Size = new Vector2i(sizeX, sizeY);
        }

        /// <summary>
        ///   Check if this <see cref="Box2i"/> contains another <see cref="Box2i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2i"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box2i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType Contains(Box2i box)
        {
            //test if all corner is in the same side of a face by just checking min and max
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            if (max2.X < Min.X
                || min2.X > max.X
                || max2.Y < min.Y
                || min2.Y > max.Y)
                return ContainmentType.Disjoint;


            if (min2.X >= Min.X
                && max2.X <= max.X
                && min2.Y >= min.Y
                && max2.Y <= max.Y)
                return ContainmentType.Contains;

            return ContainmentType.Intersects;
        }

        /// <summary>
        ///   Check if this <see cref="Box2i"/> contains another <see cref="Box2i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2i"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box2i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void Contains(ref Box2i box, out ContainmentType result)
        {
            result = Contains(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box2i"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test.</param>
        /// <returns>
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box2i"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </returns>
        public ContainmentType Contains(Vector2 point)
        {
            ContainmentType result;
            this.Contains(ref point, out result);
            return result;
        }

        /// <summary>
        ///   Check if this <see cref="Box2i"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector2"/> to test.</param>
        /// <param name="result">
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box2i"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </param>
        public void Contains(ref Vector2 point, out ContainmentType result)
        {
            //first we get if point is out of box
            var min = Min;
            var max = min + Size;
            if (point.X < min.X
                || point.X > max.X
                || point.Y < min.Y
                || point.Y > max.Y)
            {
                result = ContainmentType.Disjoint;
            }
            else
            {
                result = ContainmentType.Contains;
            }
        }

        /// <summary>
        /// Gets a box with a min 0,0 with the a size of 0,0.
        /// </summary>
        public static readonly Box2i Zero = default(Box2i);

        /// <summary>
        /// Gets a box with a min 0,0 with the a size of 1,1.
        /// </summary>
        public static readonly Box2i Unit = new Box2i(new Vector2i(0, 0), new Vector2i(1, 1));

        /// <summary>
        /// Create a bounding box by enclosing two points.
        /// </summary>
        public static Box2i Enclose(Vector2i point, Vector2i point2)
        {
            var min = new Vector2i(
                System.Math.Min(point.X, point2.X),
                System.Math.Min(point.Y, point2.Y));
            var min2 = new Vector2i(
                System.Math.Max(point.X, point2.X),
                System.Math.Max(point.Y, point2.Y));
            return new Box2i(min, min2 - min);
        }

        /// <summary>
        /// Equality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator ==(Box2i left, Box2i right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Inequality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator !=(Box2i left, Box2i right)
        {
            return !(left == right);
        }

        /// <inheritdoc/>
        public override bool Equals(object obj)
        {
            return obj is Box2i box && Equals(box);
        }

        /// <inheritdoc/>
        public bool Equals(Box2i other)
        {
            return
                Min.Equals(other.Min) &&
                Size.Equals(other.Size);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = Min.GetHashCode();
            hashCode = (hashCode * 397) ^ Size.GetHashCode();
            return hashCode;
        }

        /// <inheritdoc/>
        public override string ToString()
        {
            return $"{{Min:{Min} Size:{Size}}}";
        }
    }
}
