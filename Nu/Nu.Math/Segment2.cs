// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Represents a 2D segment.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v2.8/MonoGame.Framework/Ray.cs - and converted to Segment2.
    /// </summary>
    public struct Segment2 : IEquatable<Segment2>
    {
        /// <summary>
        /// The first point of this <see cref="Segment2"/>.
        /// </summary>
        public Vector2 A;

        /// <summary>
        /// The second point of this <see cref="Segment2"/>.
        /// </summary>
        public Vector2 B;

        /// <summary>
        /// The vector of the segment.
        /// </summary>
        public Vector2 Vector => B - A;

        /// <summary>
        /// The normal of the segment.
        /// </summary>
        public Vector2 Normal => Vector2.Normalize(Vector);

        /// <summary>
        /// The length of the segment.
        /// </summary>
        public float Length() => Vector.Length();

        /// <summary>
        /// The squared length of the segment.
        /// </summary>
        public float LengthSquared() => Vector.LengthSquared();

        /// <summary>
        /// Create a <see cref="Segment2"/>.
        /// </summary>
        /// <param name="a">The first point of the <see cref="Segment2"/>.</param>
        /// <param name="b">The second point of the <see cref="Segment2"/>.</param>
        public Segment2(Vector2 a, Vector2 b)
        {
            this.A = a;
            this.B = b;
        }

        /// <summary>
        /// Check if the specified <see cref="Object"/> is equal to this <see cref="Segment2"/>.
        /// </summary>
        /// <param name="obj">The <see cref="Object"/> to test for equality with this <see cref="Segment2"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Object"/> is equal to this <see cref="Segment2"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public override bool Equals(object obj)
        {
            return (obj is Segment2) && this.Equals((Segment2)obj);
        }

        /// <summary>
        /// Check if the specified <see cref="Segment2"/> is equal to this <see cref="Segment2"/>.
        /// </summary>
        /// <param name="other">The <see cref="Segment2"/> to test for equality with this <see cref="Segment2"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Segment2"/> is equal to this <see cref="Segment2"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public bool Equals(Segment2 other)
        {
            return this.A.Equals(other.A) && this.B.Equals(other.B);
        }

        /// <summary>
        /// Get a hash code for this <see cref="Segment2"/>.
        /// </summary>
        /// <returns>A hash code for this <see cref="Segment2"/>.</returns>
        public override int GetHashCode()
        {
            return A.GetHashCode() ^ B.GetHashCode();
        }

        /// <summary>
        /// Transform this <see cref="Segment2"/> by a matrix.
        /// </summary>
        public Segment2 Transform(Matrix4x4 m)
        {
            Vector2 a = Vector2.Transform(A, m);
            Vector2 b = Vector2.Transform(B, m);
            return new Segment2(a, b);
        }

        /// <summary>
        /// Check if two segments are not equal.
        /// </summary>
        /// <param name="a">A segment to check for inequality.</param>
        /// <param name="b">A segment to check for inequality.</param>
        /// <returns><code>true</code> if the two segments are not equal, <code>false</code> if they are.</returns>
        public static bool operator !=(Segment2 a, Segment2 b)
        {
            return !a.Equals(b);
        }

        /// <summary>
        /// Check if two segments are equal.
        /// </summary>
        /// <param name="a">A segment to check for equality.</param>
        /// <param name="b">A segment to check for equality.</param>
        /// <returns><code>true</code> if the two segments are equals, <code>false</code> if they are not.</returns>
        public static bool operator ==(Segment2 a, Segment2 b)
        {
            return a.Equals(b);
        }

        /// <summary>
        /// Get a <see cref="String"/> representation of this <see cref="Segment2"/>.
        /// </summary>
        /// <returns>A <see cref="String"/> representation of this <see cref="Segment2"/>.</returns>
        public override string ToString()
        {
            return $"{{A:{A} B:{B}}}";
        }
    }
}