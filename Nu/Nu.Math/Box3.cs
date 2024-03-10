//
// Box3.cs
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
    /// Defines an axis-aligned 3D box (cube).
    /// Copied from - https://github.com/opentk/opentk/blob/opentk5.0/src/OpenTK.Mathematics/Geometry/Box3.cs
    /// Heavily modified by BGE to more closely conform to System.Numerics and use a size-preserving representation
    /// ([min, size] instead of [min, max]).
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Box3 : IEquatable<Box3>
    {
        /// <summary>
        /// The min of the box.
        /// </summary>
        public Vector3 Min;

        /// <summary>
        /// The size of the box.
        /// </summary>
        public Vector3 Size;

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3"/> struct.
        /// </summary>
        public Box3(Vector3 min, Vector3 size)
        {
            Min = min;
            Size = size;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3"/> struct.
        /// </summary>
        public Box3(float minX, float minY, float minZ, float sizeX, float sizeY, float sizeZ)
        {
            Min = new Vector3(minX, minY, minZ);
            Size = new Vector3(sizeX, sizeY, sizeZ);
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains another <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType Contains(Box3 box)
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
        ///   Check if this <see cref="Box3"/> contains another <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void Contains(ref Box3 box, out ContainmentType result)
        {
            result = Contains(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains another <see cref="Box3"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </returns>
        public ContainmentType ContainsExclusive(Box3 box)
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
        ///   Check if this <see cref="Box3"/> contains another <see cref="Box3"/> AND no box outside of this box intersects with it.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="box"/>.
        /// </param>
        public void ContainsExclusive(ref Box3 box, out ContainmentType result)
        {
            result = ContainsExclusive(box);
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for overlap.</param>
        /// <returns>
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="sphere"/>.
        /// </returns>
        public ContainmentType Contains(Sphere sphere)
        {
            var min = Min;
            var max = min + Size;
            if (sphere.Center.X - Min.X >= sphere.Radius
                && sphere.Center.Y - Min.Y >= sphere.Radius
                && sphere.Center.Z - Min.Z >= sphere.Radius
                && max.X - sphere.Center.X >= sphere.Radius
                && max.Y - sphere.Center.Y >= sphere.Radius
                && max.Z - sphere.Center.Z >= sphere.Radius)
                return ContainmentType.Contains;

            double dmin = 0;

            double e = sphere.Center.X - Min.X;
            if (e < 0)
            {
                if (e < -sphere.Radius)
                {
                    return ContainmentType.Disjoint;
                }
                dmin += e * e;
            }
            else
            {
                e = sphere.Center.X - max.X;
                if (e > 0)
                {
                    if (e > sphere.Radius)
                    {
                        return ContainmentType.Disjoint;
                    }
                    dmin += e * e;
                }
            }

            e = sphere.Center.Y - Min.Y;
            if (e < 0)
            {
                if (e < -sphere.Radius)
                {
                    return ContainmentType.Disjoint;
                }
                dmin += e * e;
            }
            else
            {
                e = sphere.Center.Y - max.Y;
                if (e > 0)
                {
                    if (e > sphere.Radius)
                    {
                        return ContainmentType.Disjoint;
                    }
                    dmin += e * e;
                }
            }

            e = sphere.Center.Z - min.Z;
            if (e < 0)
            {
                if (e < -sphere.Radius)
                {
                    return ContainmentType.Disjoint;
                }
                dmin += e * e;
            }
            else
            {
                e = sphere.Center.Z - max.Z;
                if (e > 0)
                {
                    if (e > sphere.Radius)
                    {
                        return ContainmentType.Disjoint;
                    }
                    dmin += e * e;
                }
            }

            if (dmin <= sphere.Radius * sphere.Radius)
                return ContainmentType.Intersects;

            return ContainmentType.Disjoint;
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for overlap.</param>
        /// <param name="result">
        ///   A value indicating if this <see cref="Box3"/> contains,
        ///   intersects with or is disjoint with <paramref name="sphere"/>.
        /// </param>
        public void Contains(ref Sphere sphere, out ContainmentType result)
        {
            result = this.Contains(sphere);
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test.</param>
        /// <returns>
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box3"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </returns>
        public ContainmentType Contains(Vector3 point)
        {
            ContainmentType result;
            this.Contains(ref point, out result);
            return result;
        }

        /// <summary>
        ///   Check if this <see cref="Box3"/> contains a point.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test.</param>
        /// <param name="result">
        ///   <see cref="ContainmentType.Contains"/> if this <see cref="Box3"/> contains
        ///   <paramref name="point"/> or <see cref="ContainmentType.Disjoint"/> if it does not.
        /// </param>
        public void Contains(ref Vector3 point, out ContainmentType result)
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
        public static readonly Box3 Zero = default(Box3);

        /// <summary>
        /// Gets a box with a min 0,0,0 with the a size of 1,1,1.
        /// </summary>
        public static readonly Box3 Unit = new Box3(new Vector3(0, 0, 0), new Vector3(1, 1, 1));

        /// <summary>
        /// Create a bounding box by enclosing two points.
        /// </summary>
        public static Box3 Enclose(Vector3 point, Vector3 point2)
		{
            var min = new Vector3(
                System.Math.Min(point.X, point2.X),
                System.Math.Min(point.Y, point2.Y),
                System.Math.Min(point.Z, point2.Z));
            var min2 = new Vector3(
                System.Math.Max(point.X, point2.X),
                System.Math.Max(point.Y, point2.Y),
                System.Math.Max(point.Z, point2.Z));
            return new Box3(min, min2 - min);
        }

        /// <summary>
        /// Create a bounding box by enclosing multiple points.
        /// </summary>
        public static Box3 Enclose(Vector3[] points)
		{
            if (points.Length == 0) return default(Box3);
            var bounds = new Box3(points[0], Vector3.Zero);
            foreach (var point in points) bounds = bounds.Combine(point);
            return bounds;
        }

        /// <summary>
        /// Equality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator ==(Box3 left, Box3 right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Inequality comparator.
        /// </summary>
        /// <param name="left">The left operand.</param>
        /// <param name="right">The right operand.</param>
        public static bool operator !=(Box3 left, Box3 right)
        {
            return !(left == right);
        }

        /// <inheritdoc/>
        public override bool Equals(object obj)
        {
            return obj is Box3 box && Equals(box);
        }

        /// <inheritdoc/>
        public bool Equals(Box3 other)
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
        /// Get an array of <see cref="Vector3"/> containing the corners of this <see cref="Box3"/>.
        /// </summary>
        /// <returns>An array of <see cref="Vector3"/> containing the corners of this <see cref="Box3"/>.</returns>
        public Vector3[] Corners
		{
            get
            {
                Vector3 min = this.Min, max = this.Min + this.Size;
                return new Vector3[] {
                    new Vector3(min.X, min.Y, min.Z),
                    new Vector3(min.X, min.Y, max.Z),
                    new Vector3(max.X, min.Y, max.Z),
                    new Vector3(max.X, min.Y, min.Z),
                    new Vector3(max.X, max.Y, max.Z),
                    new Vector3(min.X, max.Y, max.Z),
                    new Vector3(min.X, max.Y, min.Z),
                    new Vector3(max.X, max.Y, min.Z),
                };
            }
        }

        /// <summary>
        /// Get an array of <see cref="Vector3"/> containing the face centers of this <see cref="Box3"/>.
        /// </summary>
        /// <returns>An array of <see cref="Vector3"/> containing the face centers of this <see cref="Box3"/>.</returns>
        public Vector3[] FaceCenters
		{
            get
            {
                Vector3 min = this.Min;
                Vector3 size = this.Size;
                Vector3 max = min + size;
                Vector3 center = min + size * 0.5f;
                return new Vector3[] {
                    new Vector3(max.X, center.Y, center.Z), // right
                    new Vector3(min.X, center.Y, center.Z), // left
                    new Vector3(center.X, max.Y, center.Z), // top
                    new Vector3(center.X, min.Y, center.Z), // bottom
                    new Vector3(center.X, center.Y, max.Z), // back
                    new Vector3(center.X, center.Y, min.Z), // front
                };
            }
        }

        /// <summary>
        /// Resize the box by a scalar factor while keeping its center fixed.
        /// </summary>
        public Box3 ScaleUniform(float scalar)
        {
            Vector3 newSize = Size * scalar;
            Vector3 displacement = (newSize - Size) * 0.5f;
            return new Box3 (Min - displacement, newSize);
        }

        /// <summary>
        /// Combine area of box with a point.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box3 Combine(Vector3 point)
        {
            var min = Min;
            var max = min + Size;
            var min2 = new Vector3();
            var max2 = new Vector3();
            min2.X = System.Math.Min(min.X, point.X);
            min2.Y = System.Math.Min(min.Y, point.Y);
            min2.Z = System.Math.Min(min.Z, point.Z);
            max2.X = System.Math.Max(max.X, point.X);
            max2.Y = System.Math.Max(max.Y, point.Y);
            max2.Z = System.Math.Max(max.Z, point.Z);
            return new Box3(min2, max2 - min2);
        }

        /// <summary>
        /// Combine area of two boxes.
        /// </summary>
        /// <param name="box"></param>
        /// <returns>Box containing area of both.</returns>
        public Box3 Combine(Box3 box)
        {
            var min = Min;
            var max = min + Size;
            var min2 = box.Min;
            var max2 = min2 + box.Size;
            var min3 = new Vector3();
            var max3 = new Vector3();
            min3.X = System.Math.Min(min.X, min2.X);
            min3.Y = System.Math.Min(min.Y, min2.Y);
            min3.Z = System.Math.Min(min.Z, min2.Z);
            max3.X = System.Math.Max(max.X, max2.X);
            max3.Y = System.Math.Max(max.Y, max2.Y);
            max3.Z = System.Math.Max(max.Z, max2.Z);
            return new Box3(min3, max3 - min3);
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> is intersected by a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Vector3 point)
        {
            bool result;
            Intersects(in point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> is intersected by a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="point"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Vector3 point, out bool result)
        {
            Vector3 min = this.Min, max = this.Min + this.Size;
            result =
                max.X >= point.X &&
                max.Y >= point.Y &&
                max.Z >= point.Z &&
                min.X <= point.X &&
                min.Y <= point.Y &&
                min.Z <= point.Z;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects another <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Box3 box)
        {
            bool result;
            Intersects(in box, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects another <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="box"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Box3 box, out bool result)
        {
            Vector3 min = this.Min, max = this.Min + this.Size;
            Vector3 min2 = box.Min, max2 = box.Min + box.Size;
            result =
                !(max.X < min2.X ||
                  max.Y < min2.Y ||
                  max.Z < min2.Z ||
                  min.X > max2.X ||
                  min.Y > max2.Y ||
                  min.Z > max2.Z);
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Frustum"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="sphere"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public bool Intersects(Sphere sphere)
        {
            bool result;
            Intersects(in sphere, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Frustum"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="sphere"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Sphere sphere, out bool result)
        {
            Vector3 min = this.Min, max = this.Min + this.Size;
            var squareDistance = 0.0f;
            var point = sphere.Center;
            if (point.X < min.X) squareDistance += (min.X - point.X) * (min.X - point.X);
            if (point.X > max.X) squareDistance += (point.X - max.X) * (point.X - max.X);
            if (point.Y < min.Y) squareDistance += (min.Y - point.Y) * (min.Y - point.Y);
            if (point.Y > max.Y) squareDistance += (point.Y - max.Y) * (point.Y - max.Y);
            if (point.Z < min.Z) squareDistance += (min.Z - point.Z) * (min.Z - point.Z);
            if (point.Z > max.Z) squareDistance += (point.Z - max.Z) * (point.Z - max.Z);
            result = squareDistance <= sphere.Radius * sphere.Radius;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects a <see cref="Plane3"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane3"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="plane"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public PlaneIntersectionType Intersects(Plane3 plane)
        {
            PlaneIntersectionType result;
            Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects a <see cref="Plane3"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane3"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="plane"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Plane3 plane, out PlaneIntersectionType result)
        {
            // See http://zach.in.tu-clausthal.de/teaching/cg_literatur/lighthouse3d_view_frustum_culling/index.html

            Vector3 positiveVertex;
            Vector3 negativeVertex;

            var min = Min;
            var max = Min + Size;

            if (plane.Normal.X >= 0)
            {
                positiveVertex.X = max.X;
                negativeVertex.X = min.X;
            }
            else
            {
                positiveVertex.X = min.X;
                negativeVertex.X = max.X;
            }

            if (plane.Normal.Y >= 0)
            {
                positiveVertex.Y = max.Y;
                negativeVertex.Y = min.Y;
            }
            else
            {
                positiveVertex.Y = min.Y;
                negativeVertex.Y = max.Y;
            }

            if (plane.Normal.Z >= 0)
            {
                positiveVertex.Z = max.Z;
                negativeVertex.Z = min.Z;
            }
            else
            {
                positiveVertex.Z = min.Z;
                negativeVertex.Z = max.Z;
            }

            // Inline Vector3.Dot(plane.Normal, negativeVertex) + plane.D;
            var distance = plane.Normal.X * negativeVertex.X + plane.Normal.Y * negativeVertex.Y + plane.Normal.Z * negativeVertex.Z + plane.D;
            if (distance > 0)
            {
                result = PlaneIntersectionType.Front;
                return;
            }

            // Inline Vector3.Dot(plane.Normal, positiveVertex) + plane.D;
            distance = plane.Normal.X * positiveVertex.X + plane.Normal.Y * positiveVertex.Y + plane.Normal.Z * positiveVertex.Z + plane.D;
            if (distance < 0)
            {
                result = PlaneIntersectionType.Back;
                return;
            }

            result = PlaneIntersectionType.Intersecting;
        }
    }
}
