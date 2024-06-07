//
// Box2.cs
//
// Copyright (C) 2019 OpenTK
//
// This software may be modified and distributed under the terms
// of the MIT license. See the LICENSE file for details.
//

using System;
using System.Runtime.InteropServices;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Defines an axis-aligned 2D box (rectangle).
    /// Copied from - https://github.com/opentk/opentk/blob/opentk5.0/src/OpenTK.Mathematics/Geometry/Box2.cs
    /// Heavily modified by BGE to more closely conform to System.Numerics and use a size-preserving representation
    /// ([min, size] instead of [min, max]).
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Box2 : IEquatable<Box2>
    {
        /// <summary>
        /// The minimum of the box.
        /// </summary>
        public Vector2 Min;

        /// <summary>
        /// The size of the box.
        /// </summary>
        public Vector2 Size;

        /// <summary>
        /// Initializes a new instance of the <see cref="Box2"/> struct.
        /// </summary>
        public Box2(Vector2 min, Vector2 size)
        {
            Min = min;
            Size = size;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Box2"/> struct.
        /// </summary>
        public Box2(float minX, float minY, float sizeX, float sizeY)
        {
            Min = new Vector2(minX, minY);
            Size = new Vector2(sizeX, sizeY);
        }

        /// <summary>
        ///   Check if this <see cref="Box2"/> contains another <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box2"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType Contains(Box2 box)
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
        ///   Check if this <see cref="Box2"/> contains another <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box2"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void Contains(ref Box2 box, out ContainmentType result)
        {
            result = Contains(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box2"/> contains another <see cref="Box2"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box2"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType ContainsExclusive(Box2 box)
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


            if (min2.X > Min.X
                && max2.X < max.X
                && min2.Y > min.Y
                && max2.Y < max.Y)
                return ContainmentType.Contains;

            return ContainmentType.Intersects;
        }

        /// <summary>
        ///   Check if this <see cref="Box2"/> contains another <see cref="Box2"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box2"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void ContainsExclusive(ref Box2 box, out ContainmentType result)
        {
            result = ContainsExclusive(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box2"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test.</param>
        /// <returns>
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box2"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </returns>
        public ContainmentType Contains(Vector2 point)
        {
            ContainmentType result;
            this.Contains(ref point, out result);
            return result;
        }

        /// <summary>
        ///   Check if this <see cref="Box2"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector2"/> to test.</param>
        /// <param name="result">
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box2"/> contains
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
        public static readonly Box2 Zero = default(Box2);

        /// <summary>
        /// Gets a box with a min 0,0 with the a size of 1,1.
        /// </summary>
        public static readonly Box2 Unit = new Box2(new Vector2(0, 0), new Vector2(1, 1));

        /// <summary>
        /// Create a bounding box by enclosing two points.
        /// </summary>
        public static Box2 Enclose(Vector2 point, Vector2 point2)
        {
            var min = new Vector2(
                System.Math.Min(point.X, point2.X),
                System.Math.Min(point.Y, point2.Y));
            var min2 = new Vector2(
                System.Math.Max(point.X, point2.X),
                System.Math.Max(point.Y, point2.Y));
            return new Box2(min, min2 - min);
        }

        /// <summary>
        /// Create a bounding box by enclosing multiple points.
        /// </summary>
        public static Box2 Enclose(Vector2[] points)
        {
            if (points.Length == 0) return default(Box2);
            var bounds = new Box2(points[0], Vector2.Zero);
            foreach (var point in points) bounds = bounds.Combine(point);
            return bounds;
        }

        /// <summary>
        /// Equality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator ==(Box2 left, Box2 right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Inequality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator !=(Box2 left, Box2 right)
        {
            return !(left == right);
        }

        /// <inheritdoc/>
        public override bool Equals(object obj)
        {
            return obj is Box2 box && Equals(box);
        }

        /// <inheritdoc/>
        public bool Equals(Box2 other)
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

        /// <summary>
        /// Resize the box by a scalar factor while keeping its center fixed.
        /// </summary>
        public Box2 ScaleUniform(float scalar)
        {
            Vector2 newSize = Size * scalar;
            Vector2 displacement = (newSize - Size) * 0.5f;
            return new Box2(Min - displacement, newSize);
        }

        /// <summary>
        /// Combine area of box with a point.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box2 Combine(Vector2 point)
        {
            var min = Min;
            var max = min + Size;
            var min2 = new Vector2();
            var max2 = new Vector2();
            min2.X = System.Math.Min(min.X, point.X);
            min2.Y = System.Math.Min(min.Y, point.Y);
            max2.X = System.Math.Max(max.X, point.X);
            max2.Y = System.Math.Max(max.Y, point.Y);
            return new Box2(min2, max2 - min2);
        }

        /// <summary>
        /// Combine area of two boxes.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box2 Combine(Box2 box)
        {
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            var min3 = new Vector2();
            var max3 = new Vector2();
            min3.X = System.Math.Min(min.X, min2.X);
            min3.Y = System.Math.Min(min.Y, min2.Y);
            max3.X = System.Math.Max(max.X, max2.X);
            max3.Y = System.Math.Max(max.Y, max2.Y);
            return new Box2(min3, max3 - min3);
        }

        /// <summary>
        /// Check if this <see cref="Box2"/> is intersected by a <see cref="Vector2"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector2"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box2"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Vector2 point)
        {
            bool result;
            Intersects(in point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box2"/> is intersected by a <see cref="Vector2"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector2"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box2"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Vector2 point, out bool result)
        {
            Vector2 min = this.Min, max = this.Min + this.Size;
            result =
                max.X >= point.X &&
                max.Y >= point.Y &&
                min.X <= point.X &&
                min.Y <= point.Y;
        }

        /// <summary>
        /// Check if this <see cref="Box2"/> intersects another <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box2"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Box2 box)
        {
            bool result;
            Intersects(in box, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box2"/> intersects another <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box2"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Box2 box, out bool result)
        {
            Vector2 min = this.Min, max = this.Min + this.Size;
            Vector2 min2 = box.Min, max2 = box.Min + box.Size;
            result =
                !(max.X < min2.X ||
                  max.Y < min2.Y ||
                  min.X > max2.X ||
                  min.Y > max2.Y);
        }
    }
}
