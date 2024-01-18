// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Collections.Generic;
using System.Numerics;

namespace Nu
{
	internal class Plane3Helper
    {
        /// <summary>
        /// Returns a value indicating what side (positive/negative) of a plane a point is
        /// </summary>
        /// <param name="point">The point to check with</param>
        /// <param name="plane">The plane to check against</param>
        /// <returns>Greater than zero if on the positive side, less than zero if on the negative size, 0 otherwise</returns>
        public static float ClassifyPoint(ref Vector3 point, ref Plane3 plane)
        {
            return point.X * plane.Normal.X + point.Y * plane.Normal.Y + point.Z * plane.Normal.Z + plane.D;
        }

        /// <summary>
        /// Returns the perpendicular distance from a point to a plane
        /// </summary>
        /// <param name="point">The point to check</param>
        /// <param name="plane">The place to check</param>
        /// <returns>The perpendicular distance from the point to the plane</returns>
        public static float PerpendicularDistance(ref Vector3 point, ref Plane3 plane)
        {
            // dist = (ax + by + cz + d) / sqrt(a*a + b*b + c*c)
            return (float)System.Math.Abs((plane.Normal.X * point.X + plane.Normal.Y * point.Y + plane.Normal.Z * point.Z)
                                    / System.Math.Sqrt(plane.Normal.X * plane.Normal.X + plane.Normal.Y * plane.Normal.Y + plane.Normal.Z * plane.Normal.Z));
        }
    }

    /// <summary>
    /// A plane in 3d space, represented by its normal away from the origin and its distance from the origin, D.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v3.8/MonoGame.Framework/Ray.cs
    /// </summary>
    public struct Plane3 : IEquatable<Plane3>
    {
        #region Public Fields

        /// <summary>
        /// The distance of the <see cref="Plane3"/> to the origin.
        /// </summary>
        public float D;

        /// <summary>
        /// The normal of the <see cref="Plane3"/>.
        /// </summary>
        public Vector3 Normal;

        #endregion Public Fields


        #region Constructors

        /// <summary>
        /// Create a <see cref="Plane3"/> with the first three components of the specified <see cref="Vector4"/>
        /// as the normal and the last component as the distance to the origin.
        /// </summary>
        /// <param name="value">A vector holding the normal and distance to origin.</param>
        public Plane3(Vector4 value)
            : this(new Vector3(value.X, value.Y, value.Z), value.W)
        {

        }

        /// <summary>
        /// Create a <see cref="Plane3"/> with the specified normal and distance to the origin.
        /// </summary>
        /// <param name="normal">The normal of the plane.</param>
        /// <param name="d">The distance to the origin.</param>
        public Plane3(Vector3 normal, float d)
        {
            Normal = normal;
            D = d;
        }

        /// <summary>
        /// Create the <see cref="Plane3"/> that contains the three specified points.
        /// </summary>
        /// <param name="a">A point the created <see cref="Plane3"/> should contain.</param>
        /// <param name="b">A point the created <see cref="Plane3"/> should contain.</param>
        /// <param name="c">A point the created <see cref="Plane3"/> should contain.</param>
        public Plane3(Vector3 a, Vector3 b, Vector3 c)
        {
            Vector3 ab = b - a;
            Vector3 ac = c - a;

            Vector3 cross = Vector3.Cross(ab, ac);
            Normal = Vector3.Normalize(cross);
            D = -(Vector3.Dot(Normal, a));
        }

        /// <summary>
        /// Create a <see cref="Plane3"/> with the first three values as the X, Y and Z
        /// components of the normal and the last value as the distance to the origin.
        /// </summary>
        /// <param name="a">The X component of the normal.</param>
        /// <param name="b">The Y component of the normal.</param>
        /// <param name="c">The Z component of the normal.</param>
        /// <param name="d">The distance to the origin.</param>
        public Plane3(float a, float b, float c, float d)
            : this(new Vector3(a, b, c), d)
        {

        }

        /// <summary>
        /// Create a <see cref="Plane3"/> that contains the specified point and has the specified <see cref="Normal"/> vector.
        /// </summary>
        /// <param name="pointOnPlane">A point the created <see cref="Plane3"/> should contain.</param>
        /// <param name="normal">The normal of the plane.</param>
        public Plane3(Vector3 pointOnPlane, Vector3 normal)
        {
            Normal = normal;
            D = -(
                pointOnPlane.X * normal.X +
                pointOnPlane.Y * normal.Y +
                pointOnPlane.Z * normal.Z
            );
        }

        #endregion Constructors


        #region Public Methods

        /// <summary>
        /// Get the dot product of a <see cref="Vector4"/> with this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector4"/> to calculate the dot product with.</param>
        /// <returns>The dot product of the specified <see cref="Vector4"/> and this <see cref="Plane3"/>.</returns>
        public float Dot(Vector4 value)
        {
            return ((((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z)) + (this.D * value.W));
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector4"/> with this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector4"/> to calculate the dot product with.</param>
        /// <param name="result">
        /// The dot product of the specified <see cref="Vector4"/> and this <see cref="Plane3"/>.
        /// </param>
        public void Dot(ref Vector4 value, out float result)
        {
            result = (((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z)) + (this.D * value.W);
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <returns>
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </returns>
        public float DotCoordinate(Vector3 value)
        {
            return ((((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z)) + this.D);
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <param name="result">
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </param>
        public void DotCoordinate(ref Vector3 value, out float result)
        {
            result = (((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z)) + this.D;
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <returns>
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane3"/>.
        /// </returns>
        public float DotNormal(Vector3 value)
        {
            return (((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z));
        }

        /// <summary>
        /// Get the dot product of a <see cref="Vector3"/> with
        /// the <see cref="Normal"/> vector of this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <param name="result">
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane3"/>.
        /// </param>
        public void DotNormal(ref Vector3 value, out float result)
        {
            result = ((this.Normal.X * value.X) + (this.Normal.Y * value.Y)) + (this.Normal.Z * value.Z);
        }

        /// <summary>
        /// Transforms a normalized plane by a matrix.
        /// </summary>
        /// <param name="plane">The normalized plane to transform.</param>
        /// <param name="matrix">The transformation matrix.</param>
        /// <returns>The transformed plane.</returns>
        public static Plane3 Transform(Plane3 plane, Matrix4x4 matrix)
        {
            Plane3 result;
            Transform(ref plane, ref matrix, out result);
            return result;
        }

        /// <summary>
        /// Transforms a normalized plane by a matrix.
        /// </summary>
        /// <param name="plane">The normalized plane to transform.</param>
        /// <param name="matrix">The transformation matrix.</param>
        /// <param name="result">The transformed plane.</param>
        public static void Transform(ref Plane3 plane, ref Matrix4x4 matrix, out Plane3 result)
        {
            // See "Transforming Normals" in http://www.glprogramming.com/red/appendixf.html
            // for an explanation of how this works.

            Matrix4x4 transformedMatrix;
            Matrix4x4.Invert(matrix, out transformedMatrix);
            transformedMatrix = Matrix4x4.Transpose(transformedMatrix);

            var vector = new Vector4(plane.Normal, plane.D);

            Vector4 transformedVector = Vector4.Transform(vector, transformedMatrix);

            result = new Plane3(transformedVector);
        }

        /// <summary>
        /// Transforms a normalized plane by a quaternion rotation.
        /// </summary>
        /// <param name="plane">The normalized plane to transform.</param>
        /// <param name="rotation">The quaternion rotation.</param>
        /// <returns>The transformed plane.</returns>
        public static Plane3 Transform(Plane3 plane, Quaternion rotation)
        {
            Plane3 result;
            Transform(ref plane, ref rotation, out result);
            return result;
        }

        /// <summary>
        /// Transforms a normalized plane by a quaternion rotation.
        /// </summary>
        /// <param name="plane">The normalized plane to transform.</param>
        /// <param name="rotation">The quaternion rotation.</param>
        /// <param name="result">The transformed plane.</param>
        public static void Transform(ref Plane3 plane, ref Quaternion rotation, out Plane3 result)
        {
            result.Normal = Vector3.Transform(plane.Normal, rotation);
            result.D = plane.D;
        }

        /// <summary>
        /// Normalize the normal vector of this plane.
        /// </summary>
        public void Normalize()
        {
            float length = Normal.Length();
            float factor =  1f / length;            
            Normal = Vector3.Multiply(Normal, factor);
            D = D * factor;
        }

        /// <summary>
        /// Get a normalized version of the specified plane.
        /// </summary>
        /// <param name="value">The <see cref="Plane3"/> to normalize.</param>
        /// <returns>A normalized version of the specified <see cref="Plane3"/>.</returns>
        public static Plane3 Normalize(Plane3 value)
        {
			Plane3 ret;
			Normalize(ref value, out ret);
			return ret;
        }

        /// <summary>
        /// Get a normalized version of the specified plane.
        /// </summary>
        /// <param name="value">The <see cref="Plane3"/> to normalize.</param>
        /// <param name="result">A normalized version of the specified <see cref="Plane3"/>.</param>
        public static void Normalize(ref Plane3 value, out Plane3 result)
        {
            float length = value.Normal.Length();
            float factor =  1f / length;
            result.Normal = Vector3.Multiply(value.Normal, factor);
            result.D = value.D * factor;
        }

        /// <summary>
        /// Check if two planes are not equal.
        /// </summary>
        /// <param name="plane1">A <see cref="Plane3"/> to check for inequality.</param>
        /// <param name="plane2">A <see cref="Plane3"/> to check for inequality.</param>
        /// <returns><code>true</code> if the two planes are not equal, <code>false</code> if they are.</returns>
        public static bool operator !=(Plane3 plane1, Plane3 plane2)
        {
            return !plane1.Equals(plane2);
        }

        /// <summary>
        /// Check if two planes are equal.
        /// </summary>
        /// <param name="plane1">A <see cref="Plane3"/> to check for equality.</param>
        /// <param name="plane2">A <see cref="Plane3"/> to check for equality.</param>
        /// <returns><code>true</code> if the two planes are equal, <code>false</code> if they are not.</returns>
        public static bool operator ==(Plane3 plane1, Plane3 plane2)
        {
            return plane1.Equals(plane2);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> is equal to another <see cref="Plane3"/>.
        /// </summary>
        /// <param name="other">An <see cref="Object"/> to check for equality with this <see cref="Plane3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="object"/> is equal to this <see cref="Plane3"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public override bool Equals(object other)
        {
            return (other is Plane3) ? this.Equals((Plane3)other) : false;
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> is equal to another <see cref="Plane3"/>.
        /// </summary>
        /// <param name="other">A <see cref="Plane3"/> to check for equality with this <see cref="Plane3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Plane3"/> is equal to this one,
        /// <code>false</code> if it is not.
        /// </returns>
        public bool Equals(Plane3 other)
        {
            return ((Normal == other.Normal) && (D == other.D));
        }

        /// <summary>
        /// Get a hash code for this <see cref="Plane3"/>.
        /// </summary>
        /// <returns>A hash code for this <see cref="Plane3"/>.</returns>
        public override int GetHashCode()
        {
            return Normal.GetHashCode() ^ D.GetHashCode();
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Box3"/>.
        /// </returns>
        public PlaneIntersectionType Intersects(Box3 box)
        {
            return box.Intersects(this);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Box3"/>.
        /// </param>
        public void Intersects(in Box3 box, out PlaneIntersectionType result)
        {
            box.Intersects(in this, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="BoundingFrustum"/>.
        /// </summary>
        /// <param name="frustum">The <see cref="BoundingFrustum"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="BoundingFrustum"/>.
        /// </returns>
        public PlaneIntersectionType Intersects(Frustum frustum)
        {
            return frustum.Intersects(this);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="BoundingSphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="BoundingSphere"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="BoundingSphere"/>.
        /// </returns>
        public PlaneIntersectionType Intersects(Sphere sphere)
        {
            return sphere.Intersects(this);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="BoundingSphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="BoundingSphere"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="BoundingSphere"/>.
        /// </param>
        public void Intersects(in Sphere sphere, out PlaneIntersectionType result)
        {
            sphere.Intersects(in this, out result);
        }

        internal PlaneIntersectionType Intersects(ref Vector3 point)
        {
            float distance;
            DotCoordinate(ref point, out distance);

            if (distance > 0)
                return PlaneIntersectionType.Front;

            if (distance < 0)
                return PlaneIntersectionType.Back;

            return PlaneIntersectionType.Intersecting;
        }

        /// <summary>
        /// Get a <see cref="String"/> representation of this <see cref="Plane3"/>.
        /// </summary>
        /// <returns>A <see cref="String"/> representation of this <see cref="Plane3"/>.</returns>
        public override string ToString()
        {
            return $"{{Normal:{Normal} D:{D}}}";
        }

        #endregion
    }
}
