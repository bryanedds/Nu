// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Represents a ray with an origin and a direction in 2D space.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v2.8/MonoGame.Framework/Ray.cs
    /// Renamed Position to Origin by BGE for nomenclature more consistent with Nu.
    /// TODO: implement Circle type and intersection with this and it.
    /// </summary>
    public struct Ray2 : IEquatable<Ray2>
    {
        /// <summary>
        /// The origin of this <see cref="Ray2"/>.
        /// </summary>
        public Vector2 Origin;

        /// <summary>
        /// The direction of this <see cref="Ray2"/>.
        /// </summary>
        public Vector2 Direction;

        /// <summary>
        /// Create a <see cref="Ray2"/>.
        /// </summary>
        /// <param name="origin">The origin of the <see cref="Ray2"/>.</param>
        /// <param name="direction">The direction of the <see cref="Ray2"/>.</param>
        public Ray2(Vector2 origin, Vector2 direction)
        {
            this.Origin = origin;
            this.Direction = direction;
        }

        /// <summary>
        /// Check if the specified <see cref="Object"/> is equal to this <see cref="Ray2"/>.
        /// </summary>
        /// <param name="obj">The <see cref="Object"/> to test for equality with this <see cref="Ray2"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Object"/> is equal to this <see cref="Ray2"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public override bool Equals(object obj)
        {
            return (obj is Ray2) && this.Equals((Ray2)obj);
        }

        /// <summary>
        /// Check if the specified <see cref="Ray2"/> is equal to this <see cref="Ray2"/>.
        /// </summary>
        /// <param name="other">The <see cref="Ray2"/> to test for equality with this <see cref="Ray2"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Ray2"/> is equal to this <see cref="Ray2"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public bool Equals(Ray2 other)
        {
            return this.Origin.Equals(other.Origin) && this.Direction.Equals(other.Direction);
        }

        /// <summary>
        /// Get a hash code for this <see cref="Ray2"/>.
        /// </summary>
        /// <returns>A hash code for this <see cref="Ray2"/>.</returns>
        public override int GetHashCode()
        {
            return Origin.GetHashCode() ^ Direction.GetHashCode();
        }

        /// <summary>
        /// Transform this <see cref="Ray2"/> by a matrix.
        /// </summary>
        public Ray2 Transform(Matrix4x4 m)
        {
            Vector2 origin = Vector2.Transform(Origin, m);
            Vector2 direction = Vector2.TransformNormal(Direction, m);
            return new Ray2(origin, direction);
        }

        /// <summary>
        /// Transform this <see cref="Ray2"/> by a quaternion.
        /// </summary>
        public Ray2 Transform(Quaternion q)
        {
            var a = Vector2.Transform(Origin, q);
            var b = Vector2.Transform(Origin + Direction, q);
            return new Ray2(a, Vector2.Normalize(b - a));
        }

        /// <summary>
        /// Project a point onto the ray.
        /// </summary>
        public Vector2 Project(Vector2 p)
        {
            var a = Origin;
            var b = Origin + Direction;
            var c = p - a;
            var d = b - a;
            return a + Vector2.Dot(c, d) / Vector2.Dot(d, d) * d;
        }

        /// <summary>
        /// Check if this <see cref="Ray2"/> intersects the specified <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for intersection.</param>
        /// <returns>
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray2"/> does not intersect the <see cref="Box2"/>.
        /// </returns>
        public float? Intersects(Box2 box)
        {
            const float Epsilon = 1e-6f;

            Vector2 min = box.Min, max = box.Min + box.Size;
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

            // having a positive tMax and a negative tMin means the ray is inside the box
            // we expect the intesection distance to be 0 in that case
            if ((tMin.HasValue && tMin < 0) && tMax > 0) return 0;

            // a negative tMin means that the intersection point is behind the ray's origin
            // we discard these as not hitting the AABB
            if (tMin < 0) return null;

            return tMin;
        }

        /// <summary>
        /// Check if this <see cref="Ray2"/> intersects the specified <see cref="Box2"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box2"/> to test for intersection.</param>
        /// <param name="result">
        /// The distance along the ray of the intersection or <code>null</code> if this
        /// <see cref="Ray2"/> does not intersect the <see cref="Box2"/>.
        /// </param>
        public void Intersects(in Box2 box, out float? result)
        {
            result = Intersects(box);
        }

        /// <summary>
        /// Check if two rays are not equal.
        /// </summary>
        /// <param name="a">A ray to check for inequality.</param>
        /// <param name="b">A ray to check for inequality.</param>
        /// <returns><code>true</code> if the two rays are not equal, <code>false</code> if they are.</returns>
        public static bool operator !=(Ray2 a, Ray2 b)
        {
            return !a.Equals(b);
        }

        /// <summary>
        /// Check if two rays are equal.
        /// </summary>
        /// <param name="a">A ray to check for equality.</param>
        /// <param name="b">A ray to check for equality.</param>
        /// <returns><code>true</code> if the two rays are equals, <code>false</code> if they are not.</returns>
        public static bool operator ==(Ray2 a, Ray2 b)
        {
            return a.Equals(b);
        }

        /// <summary>
        /// Get a <see cref="String"/> representation of this <see cref="Ray2"/>.
        /// </summary>
        /// <returns>A <see cref="String"/> representation of this <see cref="Ray2"/>.</returns>
        public override string ToString()
        {
            return $"{{Origin:{Origin} Direction:{Direction}}}";
        }
    }
}