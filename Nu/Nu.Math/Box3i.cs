//
// Box3i.cs
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
    /// Defines an axis-aligned 3D box (cube).
    /// Copied from - https://github.com/opentk/opentk/blob/opentk5.0/src/OpenTK.Mathematics/Geometry/Box3.cs
    /// Heavily modified by BGE to more closely conform to System.Numerics and use a size-preserving representation
    /// ([min, size] instead of [min, max]).
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Box3i : IEquatable<Box3i>
    {
        /// <summary>
        /// The min of the box.
        /// </summary>
        public Vector3i Min;

        /// <summary>
        /// The size of the box.
        /// </summary>
        public Vector3i Size;

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3i"/> struct.
        /// </summary>
        public Box3i(Vector3i min, Vector3i size)
        {
            Min = min;
            Size = size;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3i"/> struct.
        /// </summary>
        public Box3i(int minX, int minY, int minZ, int sizeX, int sizeY, int sizeZ)
        {
            Min = new Vector3i(minX, minY, minZ);
            Size = new Vector3i(sizeX, sizeY, sizeZ);
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains another <see cref="Box3i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box3i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType Contains(Box3i box)
        {
            //test if all corner is in the same side of a face by just checking min and max
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            if (max2.X < Min.X
                || min2.X > max.X
                || max2.Y < min.Y
                || min2.Y > max.Y
                || max2.Z < min.Z
                || min2.Z > max.Z)
                return ContainmentType.Disjoint;


            if (min2.X >= Min.X
                && max2.X <= max.X
                && min2.Y >= min.Y
                && max2.Y <= max.Y
                && min2.Z >= min.Z
                && max2.Z <= max.Z)
                return ContainmentType.Contains;

            return ContainmentType.Intersects;
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains another <see cref="Box3i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box3i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void Contains(ref Box3i box, out ContainmentType result)
        {
            result = Contains(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains another <see cref="Box3i"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box3i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType ContainsExclusive(Box3i box)
        {
            //test if all corner is in the same side of a face by just checking min and max
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            if (max2.X < Min.X
                || min2.X > max.X
                || max2.Y < min.Y
                || min2.Y > max.Y
                || max2.Z < min.Z
                || min2.Z > max.Z)
                return ContainmentType.Disjoint;


            if (min2.X > Min.X
                && max2.X < max.X
                && min2.Y > min.Y
                && max2.Y < max.Y
                && min2.Z > min.Z
                && max2.Z < max.Z)
                return ContainmentType.Contains;

            return ContainmentType.Intersects;
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains another <see cref="Box3i"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box3i"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void ContainsExclusive(ref Box3i box, out ContainmentType result)
        {
            result = ContainsExclusive(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3i"/> to test.</param>
        /// <returns>
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box3i"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </returns>
        public ContainmentType Contains(Vector3i point)
        {
            ContainmentType result;
            this.Contains(ref point, out result);
            return result;
        }

        /// <summary>
        ///   Check if this <see cref="Box3i"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3i"/> to test.</param>
        /// <param name="result">
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box3i"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </param>
        public void Contains(ref Vector3i point, out ContainmentType result)
        {
            //first we get if point is out of box
            var min = Min;
            var max = min + Size;
            if (point.X < min.X
                || point.X > max.X
                || point.Y < min.Y
                || point.Y > max.Y
                || point.Z < min.Z
                || point.Z > max.Z)
            {
                result = ContainmentType.Disjoint;
            }
            else
            {
                result = ContainmentType.Contains;
            }
        }

        /// Gets a box with a min 0,0,0 with the a size of 0,0,0.
        /// </summary>
        public static readonly Box3i Zero = default(Box3i);

        /// <summary>
        /// Gets a box with a min 0,0,0 with the a size of 1,1,1.
        /// </summary>
        public static readonly Box3i Unit = new Box3i(new Vector3i(0, 0, 0), new Vector3i(1, 1, 1));

        /// <summary>
        /// Create a bounding box by enclosing two points.
        /// </summary>
        public static Box3i Enclose(Vector3i point, Vector3i point2)
        {
            var min = new Vector3i(
                System.Math.Min(point.X, point2.X),
                System.Math.Min(point.Y, point2.Y),
                System.Math.Min(point.Z, point2.Z));
            var min2 = new Vector3i(
                System.Math.Max(point.X, point2.X),
                System.Math.Max(point.Y, point2.Y),
                System.Math.Max(point.Z, point2.Z));
            return new Box3i(min, min2 - min);
        }

        /// <summary>
        /// Create a bounding box by enclosing multiple points.
        /// </summary>
        public static Box3i Enclose(Vector3i[] points)
        {
            if (points.Length == 0) return default(Box3i);
            var bounds = new Box3i(points[0], Vector3i.Zero);
            foreach (var point in points) bounds = bounds.Combine(point);
            return bounds;
        }

        /// <summary>
        /// Equality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator ==(Box3i left, Box3i right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Inequality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator !=(Box3i left, Box3i right)
        {
            return !(left == right);
        }

        /// <inheritdoc/>
        public override bool Equals(object obj)
        {
            return obj is Box3i box && Equals(box);
        }

        /// <inheritdoc/>
        public bool Equals(Box3i other)
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
        /// The mirror image of a box.
        /// </summary>
        public Box3i Mirror
        {
            get
            {
                var min = Min;
                var max = min + Size;
                var min2 = -max;
                var max2 = -min;
                var min3i = new Vector3i();
                var max3i = new Vector3i();
                min3i.X = System.Math.Min(min.X, min2.X);
                min3i.Y = System.Math.Min(min.Y, min2.Y);
                min3i.Z = System.Math.Min(min.Z, min2.Z);
                max3i.X = System.Math.Max(max.X, max2.X);
                max3i.Y = System.Math.Max(max.Y, max2.Y);
                max3i.Z = System.Math.Max(max.Z, max2.Z);
                return new Box3i(min3i, max3i - min3i);
            }
        }

        /// <summary>
        /// Get an array of <see cref="Vector3i"/> containing the corners of this <see cref="Box3i"/>.
        /// </summary>
        /// <returns>An array of <see cref="Vector3i"/> containing the corners of this <see cref="Box3i"/>.</returns>
        public Vector3i[] Corners
        {
            get
            {
                Vector3i min = this.Min, max = this.Min + this.Size;
                return new Vector3i[] {
                    new Vector3i(min.X, min.Y, min.Z),
                    new Vector3i(min.X, min.Y, max.Z),
                    new Vector3i(max.X, min.Y, max.Z),
                    new Vector3i(max.X, min.Y, min.Z),
                    new Vector3i(max.X, max.Y, max.Z),
                    new Vector3i(min.X, max.Y, max.Z),
                    new Vector3i(min.X, max.Y, min.Z),
                    new Vector3i(max.X, max.Y, min.Z),
                };
            }
        }

        /// <summary>
        /// Combine area of box with a point.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box3i Combine(Vector3i point)
        {
            var min = Min;
            var max = min + Size;
            var min2 = new Vector3i();
            var max2 = new Vector3i();
            min2.X = System.Math.Min(min.X, point.X);
            min2.Y = System.Math.Min(min.Y, point.Y);
            min2.Z = System.Math.Min(min.Z, point.Z);
            max2.X = System.Math.Max(max.X, point.X);
            max2.Y = System.Math.Max(max.Y, point.Y);
            max2.Z = System.Math.Max(max.Z, point.Z);
            return new Box3i(min2, max2 - min2);
        }

        /// <summary>
        /// Combine area of two boxes.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box3i Combine(Box3i box)
        {
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            var min3i = new Vector3i();
            var max3i = new Vector3i();
            min3i.X = System.Math.Min(min.X, min2.X);
            min3i.Y = System.Math.Min(min.Y, min2.Y);
            min3i.Z = System.Math.Min(min.Z, min2.Z);
            max3i.X = System.Math.Max(max.X, max2.X);
            max3i.Y = System.Math.Max(max.Y, max2.Y);
            max3i.Z = System.Math.Max(max.Z, max2.Z);
            return new Box3i(min3i, max3i - min3i);
        }

        /// <summary>
        /// Check if this <see cref="Box3i"/> is intersected by a <see cref="Vector3i"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3i"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3i"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Vector3i point)
        {
            bool result;
            Intersects(in point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3i"/> is intersected by a <see cref="Vector3i"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3i"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3i"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Vector3i point, out bool result)
        {
            Vector3i min = this.Min, max = this.Min + this.Size;
            result =
                max.X >= point.X &&
                max.Y >= point.Y &&
                max.Z >= point.Z &&
                min.X <= point.X &&
                min.Y <= point.Y &&
                min.Z <= point.Z;
        }

        /// <summary>
        /// Check if this <see cref="Box3i"/> intersects another <see cref="Box3i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3i"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Box3i box)
        {
            bool result;
            Intersects(in box, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3i"/> intersects another <see cref="Box3i"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3i"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3i"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Box3i box, out bool result)
        {
            Vector3i min = this.Min, max = this.Min + this.Size;
            Vector3i min2 = box.Min, max2 = box.Min + box.Size;
            result =
                !(max.X < min2.X ||
                  max.Y < min2.Y ||
                  max.Z < min2.Z ||
                  min.X > max2.X ||
                  min.Y > max2.Y ||
                  min.Z > max2.Z);
        }
    }
}
