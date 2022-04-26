// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Extension for Plane type.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v3.8/MonoGame.Framework/Plane.cs
    /// </summary>
	public static class PlaneExtensions
	{
        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Box3"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(this Plane plane, Box3 box)
        {
            return box.Intersects(plane);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Box3"/>.
        /// </param>
        public static void Intersects(ref this Plane plane, ref Box3 box, out PlaneIntersectionType result)
        {
            box.Intersects(ref plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">The <see cref="Frustum"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Frustum"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(this Plane plane, Frustum frustum)
        {
            return frustum.Intersects(plane);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">The <see cref="Frustum"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Frustum"/>.
        /// </param>
        /// </returns>
        public static void Intersects(ref this Plane plane, ref Frustum frustum, out PlaneIntersectionType result)
        {
            frustum.Intersects(ref plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Sphere"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(this Plane plane, Sphere sphere)
        {
            return sphere.Intersects(plane);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Sphere"/>.
        /// </param>
        public static void Intersects(ref this Plane plane, ref Sphere sphere, out PlaneIntersectionType result)
        {
            sphere.Intersects(ref plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Vector3"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Vector3"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(this Plane plane, Vector3 point)
        {
            PlaneIntersectionType result;
            plane.Intersects(ref point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Vector3"/>.
        /// </param>
        public static void Intersects(ref this Plane plane, ref Vector3 point, out PlaneIntersectionType result)
        {
            float distance;
            plane.DotCoordinate(ref point, out distance);

            if (distance > 0)
            {
                result = PlaneIntersectionType.Front;
                return;
            }

            if (distance < 0)
            {
                result = PlaneIntersectionType.Back;
                return;
            }

            result = PlaneIntersectionType.Intersecting;
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <returns>
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane"/>.
        /// </returns>
        public static float DotCoordinate(this Plane plane, Vector3 value)
        {
            return (plane.Normal.X * value.X) + (plane.Normal.Y * value.Y) + (plane.Normal.Z * value.Z) + plane.D;
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <param name="result">
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane"/>.
        /// </param>
        public static void DotCoordinate(ref this Plane plane, ref Vector3 value, out float result)
        {
            result = (plane.Normal.X * value.X) + (plane.Normal.Y * value.Y) + (plane.Normal.Z * value.Z) + plane.D;
        }

        /// <summary>
        /// Returns a value indicating what side (positive/negative) of a plane a point is
        /// </summary>
        /// <param name="plane">The plane to check against</param>
        /// <param name="point">The point to check with</param>
        /// <returns>Greater than zero if on the positive side, less than zero if on the negative size, 0 otherwise</returns>
        public static float ClassifyPoint(ref this Plane plane, ref Vector3 point)
        {
            return point.X * plane.Normal.X + point.Y * plane.Normal.Y + point.Z * plane.Normal.Z + plane.D;
        }
    }
}
