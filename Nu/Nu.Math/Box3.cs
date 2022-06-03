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
    /// ([pos, siz] instead of [min, max]).
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Box3 : IEquatable<Box3>
    {
        /// <summary>
        /// The position of the box.
        /// </summary>
        public Vector3 Position;

        /// <summary>
        /// The size of the box.
        /// </summary>
        public Vector3 Size;

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3"/> struct.
        /// </summary>
        public Box3(Vector3 position, Vector3 size)
        {
            Position = position;
            Size = size;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Box3"/> struct.
        /// </summary>
        public Box3(float positionX, float positionY, float positionZ, float sizeX, float sizeY, float sizeZ)
        {
            Position = new Vector3(positionX, positionY, positionZ);
            Size = new Vector3(sizeX, sizeY, sizeZ);
        }

        /// Gets a box with a position 0,0,0 with the a size of 0,0,0.
        /// </summary>
        public static readonly Box3 Zero = default(Box3);

        /// <summary>
        /// Gets a box with a position 0,0 with the a size of 1,1.
        /// </summary>
        public static readonly Box2 Unit = new Box2(new Vector2(0, 0), new Vector2(1, 1));

        /// <summary>
        /// Create a bounding box by enclosing two points.
        /// </summary>
        public static Box3 Enclose(Vector3 point, Vector3 point2)
		{
            var position = new Vector3(
                Math.Min(point.X, point2.X),
                Math.Min(point.Y, point2.Y),
                Math.Min(point.Z, point2.Z));
            var position2 = new Vector3(
                Math.Max(point.X, point2.X),
                Math.Max(point.Y, point2.Y),
                Math.Max(point.Z, point2.Z));
            return new Box3(position, position2 - position);
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
                Position.Equals(other.Position) &&
                Size.Equals(other.Size);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = Position.GetHashCode();
            hashCode = (hashCode * 397) ^ Size.GetHashCode();
            return hashCode;
        }

        /// <inheritdoc/>
        public override string ToString()
        {
            return string.Format("{0}\n{1}", Position, Size);
        }

		/// <summary>
		/// Get an array of <see cref="Vector3"/> containing the corners of this <see cref="Box3"/>.
		/// </summary>
		/// <returns>An array of <see cref="Vector3"/> containing the corners of this <see cref="Box3"/>.</returns>
		public Vector3[] Corners
		{
            get
            {
                Vector3 min = this.Position, max = this.Position + this.Size;
                return new Vector3[] {
                    new Vector3(min.X, max.Y, max.Z),
                    new Vector3(max.X, max.Y, max.Z),
                    new Vector3(max.X, min.Y, max.Z),
                    new Vector3(min.X, min.Y, max.Z),
                    new Vector3(min.X, max.Y, min.Z),
                    new Vector3(max.X, max.Y, min.Z),
                    new Vector3(max.X, min.Y, min.Z),
                    new Vector3(min.X, min.Y, min.Z)
                };
            }
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
            Vector3 min = this.Position, max = this.Position + this.Size;
            result =
                max.X <= point.X &&
                max.Y <= point.Y &&
                max.Z <= point.Z &&
                min.X >= point.X &&
                min.Y >= point.Y &&
                min.Z >= point.Z;
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
            Vector3 min = this.Position, max = this.Position + this.Size;
            Vector3 min2 = box.Position, max2 = box.Position + box.Size;
            result =
                max.X <= min2.X &&
                max.Y <= min2.Y &&
                max.Z <= min2.Z &&
                min.X >= max2.X &&
                min.Y >= max2.Y &&
                min.Z >= max2.Z;
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
            Vector3 min = this.Position, max = this.Position + this.Size;
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
        /// Check if this <see cref="Box3"/> intersects a <see cref="Plane"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane"/> to test for intersection.</param>
        /// <returns>
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="plane"/>,
        ///   <code>false</code> if it does not.
        /// </returns>
        public PlaneIntersectionType Intersects(Plane plane)
        {
            PlaneIntersectionType result;
            Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Box3"/> intersects a <see cref="Plane"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane"/> to test for intersection.</param>
        /// <param name="result">
        ///   <code>true</code> if this <see cref="Box3"/> intersects <paramref name="plane"/>,
        ///   <code>false</code> if it does not.
        /// </param>
        public void Intersects(in Plane plane, out PlaneIntersectionType result)
        {
            // See http://zach.in.tu-clausthal.de/teaching/cg_literatur/lighthouse3d_view_frustum_culling/index.html

            Vector3 positiveVertex;
            Vector3 negativeVertex;

            var min = Position;
            var max = Position + Size;

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
