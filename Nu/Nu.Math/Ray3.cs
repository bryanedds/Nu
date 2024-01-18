// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Collections.Generic;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Represents a ray with an origin and a direction in 3D space.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v3.8/MonoGame.Framework/Ray.cs
    /// Renamed Position to Origin by BGE for nomenclature more consistent with Nu.
    /// </summary>
    public struct Ray3 : IEquatable<Ray3>
    {
        /// <summary>
        /// The origin of this <see cref="Ray3"/>.
        /// </summary>
        public Vector3 Origin;

        /// <summary>
        /// The direction of this <see cref="Ray3"/>.
        /// </summary>
        public Vector3 Direction;

        /// <summary>
        /// Create a <see cref="Ray3"/>.
        /// </summary>
        /// <param name="origin">The origin of the <see cref="Ray3"/>.</param>
        /// <param name="direction">The direction of the <see cref="Ray3"/>.</param>
        public Ray3(Vector3 origin, Vector3 direction)
        {
            this.Origin = origin;
            this.Direction = direction;
        }

        /// <summary>
        /// Check if the specified <see cref="Object"/> is equal to this <see cref="Ray3"/>.
        /// </summary>
        /// <param name="obj">The <see cref="Object"/> to test for equality with this <see cref="Ray3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Object"/> is equal to this <see cref="Ray3"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public override bool Equals(object obj)
        {
            return (obj is Ray3) && this.Equals((Ray3)obj);
        }

        /// <summary>
        /// Check if the specified <see cref="Ray3"/> is equal to this <see cref="Ray3"/>.
        /// </summary>
        /// <param name="other">The <see cref="Ray3"/> to test for equality with this <see cref="Ray3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Ray3"/> is equal to this <see cref="Ray3"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public bool Equals(Ray3 other)
        {
            return this.Origin.Equals(other.Origin) && this.Direction.Equals(other.Direction);
        }

        /// <summary>
        /// Get a hash code for this <see cref="Ray3"/>.
        /// </summary>
        /// <returns>A hash code for this <see cref="Ray3"/>.</returns>
        public override int GetHashCode()
        {
            return Origin.GetHashCode() ^ Direction.GetHashCode();
        }

        /// <summary>
        /// Transform this <see cref="Ray3"/> by a matrix.
        /// </summary>
        public Ray3 Transform(Matrix4x4 m)
        {
            Vector3 origin = Vector3.Transform(Origin, m);
            Vector3 direction = Vector3.TransformNormal(Direction, m);
            return new Ray3(origin, direction);
		}

        /// <summary>
        /// Transform this <see cref="Ray3"/> by a quaternion.
        /// </summary>
        public Ray3 Transform(Quaternion q)
		{
            var a = Vector3.Transform(Origin, q);
            var b = Vector3.Transform(Origin + Direction, q);
            return new Ray3(a, Vector3.Normalize(b - a));
		}

        /// <summary>
        /// Project a point onto the ray.
        /// </summary>
        public Vector3 Project(Vector3 p)
		{
            var a = Origin;
            var b = Origin + Direction;
            var c = p - a;
            var d = b - a;
            return a + Vector3.Dot(c, d) / Vector3.Dot(d, d) * d;
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <returns>
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Box3"/>.
        /// </returns>
        public float? Intersects(Box3 box)
        {
            const float Epsilon = 1e-6f;

            Vector3 min = box.Min, max = box.Min + box.Size;
            float? tMin = null, tMax = null;

            if (System.Math.Abs(Direction.X) < Epsilon)
            {
                if (Origin.X < min.X || Origin.X > max.X)
                    return null;
            }
            else
            {
                tMin = (min.X - Origin.X) / Direction.X;
                tMax = (max.X - Origin.X) / Direction.X;

                if (tMin > tMax)
                {
                    var temp = tMin;
                    tMin = tMax;
                    tMax = temp;
                }
            }

            if (System.Math.Abs(Direction.Y) < Epsilon)
            {
                if (Origin.Y < min.Y || Origin.Y > max.Y)
                    return null;
            }
            else
            {
                var tMinY = (min.Y - Origin.Y) / Direction.Y;
                var tMaxY = (max.Y - Origin.Y) / Direction.Y;

                if (tMinY > tMaxY)
                {
                    var temp = tMinY;
                    tMinY = tMaxY;
                    tMaxY = temp;
                }

                if ((tMin.HasValue && tMin > tMaxY) || (tMax.HasValue && tMinY > tMax))
                    return null;

                if (!tMin.HasValue || tMinY > tMin) tMin = tMinY;
                if (!tMax.HasValue || tMaxY < tMax) tMax = tMaxY;
            }

            if (System.Math.Abs(Direction.Z) < Epsilon)
            {
                if (Origin.Z < min.Z || Origin.Z > max.Z)
                    return null;
            }
            else
            {
                var tMinZ = (min.Z - Origin.Z) / Direction.Z;
                var tMaxZ = (max.Z - Origin.Z) / Direction.Z;

                if (tMinZ > tMaxZ)
                {
                    var temp = tMinZ;
                    tMinZ = tMaxZ;
                    tMaxZ = temp;
                }

                if ((tMin.HasValue && tMin > tMaxZ) || (tMax.HasValue && tMinZ > tMax))
                    return null;

                if (!tMin.HasValue || tMinZ > tMin) tMin = tMinZ;
                if (!tMax.HasValue || tMaxZ < tMax) tMax = tMaxZ;
            }

            // having a positive tMax and a negative tMin means the ray is inside the box
            // we expect the intesection distance to be 0 in that case
            if ((tMin.HasValue && tMin < 0) && tMax > 0) return 0;

            // a negative tMin means that the intersection point is behind the ray's origin
            // we discard these as not hitting the AABB
            if (tMin < 0) return null;

            return tMin;
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Box3"/>.
        /// </param>
        public void Intersects(in Box3 box, out float? result)
        {
            result = Intersects(box);
        }

        public float? Intersects(Frustum frustum)
        {
            if (frustum == null)
			{
				throw new ArgumentNullException("frustum");
			}
			
			return frustum.Intersects(this);			
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Box"/> to test for intersection.</param>
        /// <returns>
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Sphere"/>.
        /// </returns>
        public float? Intersects(Sphere sphere)
        {
            float? result;
            Intersects(in sphere, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Plane3"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane3"/> to test for intersection.</param>
        /// <returns>
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Plane3"/>.
        /// </returns>
        public float? Intersects(Plane3 plane)
        {
            float? result;
            Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Plane3"/>.
        /// </summary>
        /// <param name="plane">The <see cref="Plane3"/> to test for intersection.</param>
        /// <param name="result">
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Plane3"/>.
        /// </param>
        public void Intersects(in Plane3 plane, out float? result)
        {
            var den = Vector3.Dot(Direction, plane.Normal);
            if (System.Math.Abs(den) < 0.00001f)
            {
                result = null;
                return;
            }

            result = (-plane.D - Vector3.Dot(plane.Normal, Origin)) / den;

            if (result < 0.0f)
            {
                if (result < -0.00001f)
                {
                    result = null;
                    return;
                }

                result = 0.0f;
            }
        }

        /// <summary>
        /// Check if this <see cref="Ray3"/> intersects the specified <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray3"/> does not intersect the <see cref="Sphere"/>.
        /// </param>
        public void Intersects(in Sphere sphere, out float? result)
        {
            // Find the vector between where the ray starts the the sphere's centre
            Vector3 difference = sphere.Center - this.Origin;

            float differenceLengthSquared = difference.LengthSquared();
            float sphereRadiusSquared = sphere.Radius * sphere.Radius;

            // If the distance between the ray start and the sphere's centre is less than
            // the radius of the sphere, it means we've intersected. N.B. checking the LengthSquared is faster.
            if (differenceLengthSquared < sphereRadiusSquared)
            {
                result = 0.0f;
                return;
            }

            float distanceAlongRay = Vector3.Dot(this.Direction, difference);
            // If the ray is pointing away from the sphere then we don't ever intersect
            if (distanceAlongRay < 0)
            {
                result = null;
                return;
            }

            // Next we kinda use Pythagoras to check if we are within the bounds of the sphere
            // if x = radius of sphere
            // if y = distance between ray origin and sphere centre
            // if z = the distance we've travelled along the ray
            // if x^2 + z^2 - y^2 < 0, we do not intersect
            float dist = sphereRadiusSquared + distanceAlongRay * distanceAlongRay - differenceLengthSquared;

            result = (dist < 0) ? null : distanceAlongRay - (float?)System.Math.Sqrt(dist);
        }

        /// <summary>
        /// Get all of the ray intersections of the given mesh.
        /// </summary>
        public IEnumerable<(int, float)> Intersects(int[] indices, Vector3[] vertices)
        {
            var faceCount = indices.Length / 3;
            for (var i = 0; i < faceCount; ++i)
            {
                // Retrieve vertex.
                Vector3 a = vertices[indices[i * 3]];
                Vector3 b = vertices[indices[i * 3 + 1]];
                Vector3 c = vertices[indices[i * 3 + 2]];
                
                // Compute vectors along two edges of the triangle.
                Vector3 edge1 = b - a;
                Vector3 edge2 = c - a;

                // Compute the determinant.
                Vector3 directionCrossEdge2 = Vector3.Cross(Direction, edge2);
                float determinant = Vector3.Dot(edge1, directionCrossEdge2);

                // If the ray is parallel to the triangle plane, there is no collision.
                if (determinant > -float.Epsilon && determinant < float.Epsilon)
                    continue;

                // Calculate the U parameter of the intersection point.
                float inverseDeterminant = 1.0f / determinant;
                Vector3 distanceVector = Origin - a;
                float triangleU = Vector3.Dot(distanceVector, directionCrossEdge2);
                triangleU *= inverseDeterminant;

                // Make sure it is inside the triangle.
                if (triangleU < 0 || triangleU > 1)
                    continue;

                // Calculate the V parameter of the intersection point.
                Vector3 distanceCrossEdge1 = Vector3.Cross(distanceVector, edge1);
                float triangleV = Vector3.Dot(Direction, distanceCrossEdge1);
                triangleV *= inverseDeterminant;

                // Make sure it is inside the triangle.
                if (triangleV < 0 || triangleU + triangleV > 1)
                    continue;

                // Compute the distance along the ray to the triangle.
                float rayDistance = Vector3.Dot(edge2, distanceCrossEdge1);
                rayDistance *= inverseDeterminant;

                // Is the triangle behind the ray origin?
                if (rayDistance >= 0)
                    yield return (i, rayDistance);
            }
        }

        /// <summary>
        /// Attempt to get the first found intersection from an array of triangle vertices.
        /// </summary>
        public bool Intersects(int[] indices, Vector3[] vertices, out float? result)
		{
            var enr = Intersects(indices, vertices).GetEnumerator();
            if (enr.MoveNext())
            {
                var (_, t) = enr.Current;
                result = t;
                return true;
            }
            result = null;
            return false;
        }

        /// <summary>
        /// Attempt to find the intersection of the <see cref="Ray3"/> with a <see cref="Plane3"/>.
        /// TODO: implement this in terms of Intersects?
        /// </summary>
        public Vector3? Intersection(Plane3 plane)
        {
            var d = Vector3.Dot(plane.Normal * -plane.D, -plane.Normal);
            var t =
                -(d + Origin.Z * plane.Normal.Z + Origin.Y * plane.Normal.Y + Origin.X * plane.Normal.X) /
                +(Direction.Z * plane.Normal.Z + Direction.Y * plane.Normal.Y + Direction.X * plane.Normal.X);
            return Origin + t * Direction;
        }

        /// <summary>
        /// Check if two rays are not equal.
        /// </summary>
        /// <param name="a">A ray to check for inequality.</param>
        /// <param name="b">A ray to check for inequality.</param>
        /// <returns><code>true</code> if the two rays are not equal, <code>false</code> if they are.</returns>
        public static bool operator !=(Ray3 a, Ray3 b)
        {
            return !a.Equals(b);
        }

        /// <summary>
        /// Check if two rays are equal.
        /// </summary>
        /// <param name="a">A ray to check for equality.</param>
        /// <param name="b">A ray to check for equality.</param>
        /// <returns><code>true</code> if the two rays are equals, <code>false</code> if they are not.</returns>
        public static bool operator ==(Ray3 a, Ray3 b)
        {
            return a.Equals(b);
        }

        /// <summary>
        /// Get a <see cref="String"/> representation of this <see cref="Ray3"/>.
        /// </summary>
        /// <returns>A <see cref="String"/> representation of this <see cref="Ray3"/>.</returns>
        public override string ToString()
        {
            return $"{{Origin:{Origin} Direction:{Direction}}}";
        }
    }
}