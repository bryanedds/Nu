// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Represents a 2D segment.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v2.8/MonoGame.Framework/Ray.cs - and converted to Segment3.
    /// </summary>
    public struct Segment3 : IEquatable<Segment3>
    {
        /// <summary>
        /// The first point of this <see cref="Segment3"/>.
        /// </summary>
        public Vector3 A;

        /// <summary>
        /// The second point of this <see cref="Segment3"/>.
        /// </summary>
        public Vector3 B;

        /// <summary>
        /// The vector of the segment.
        /// </summary>
        public Vector3 Vector => B - A;

        /// <summary>
        /// The normal of the segment.
        /// </summary>
        public Vector3 Normal => Vector3.Normalize(Vector);

        /// <summary>
        /// The length of the segment.
        /// </summary>
        public float Length() => Vector.Length();

        /// <summary>
        /// The squared length of the segment.
        /// </summary>
        public float LengthSquared() => Vector.LengthSquared();

        /// <summary>
        /// Create a <see cref="Segment3"/>.
        /// </summary>
        /// <param name="a">The first point of the <see cref="Segment3"/>.</param>
        /// <param name="b">The second point of the <see cref="Segment3"/>.</param>
        public Segment3(Vector3 a, Vector3 b)
        {
            this.A = a;
            this.B = b;
        }

        /// <summary>
        /// Check if the specified <see cref="Object"/> is equal to this <see cref="Segment3"/>.
        /// </summary>
        /// <param name="obj">The <see cref="Object"/> to test for equality with this <see cref="Segment3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Object"/> is equal to this <see cref="Segment3"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public override bool Equals(object obj)
        {
            return (obj is Segment3) && this.Equals((Segment3)obj);
        }

        /// <summary>
        /// Check if the specified <see cref="Segment3"/> is equal to this <see cref="Segment3"/>.
        /// </summary>
        /// <param name="other">The <see cref="Segment3"/> to test for equality with this <see cref="Segment3"/>.</param>
        /// <returns>
        /// <code>true</code> if the specified <see cref="Segment3"/> is equal to this <see cref="Segment3"/>,
        /// <code>false</code> if it is not.
        /// </returns>
        public bool Equals(Segment3 other)
        {
            return this.A.Equals(other.A) && this.B.Equals(other.B);
        }

        /// <summary>
        /// Get a hash code for this <see cref="Segment3"/>.
        /// </summary>
        /// <returns>A hash code for this <see cref="Segment3"/>.</returns>
        public override int GetHashCode()
        {
            return A.GetHashCode() ^ B.GetHashCode();
        }

        /// <summary>
        /// Transform this <see cref="Segment3"/> by a matrix.
        /// </summary>
        public Segment3 Transform(Matrix4x4 m)
        {
            Vector3 a = Vector3.Transform(A, m);
            Vector3 b = Vector3.Transform(B, m);
            return new Segment3(a, b);
        }

        /// <summary>
        /// Check if two segments are not equal.
        /// </summary>
        /// <param name="a">A segment to check for inequality.</param>
        /// <param name="b">A segment to check for inequality.</param>
        /// <returns><code>true</code> if the two segments are not equal, <code>false</code> if they are.</returns>
        public static bool operator !=(Segment3 a, Segment3 b)
        {
            return !a.Equals(b);
        }

        /// <summary>
        /// Check if two segments are equal.
        /// </summary>
        /// <param name="a">A segment to check for equality.</param>
        /// <param name="b">A segment to check for equality.</param>
        /// <returns><code>true</code> if the two segments are equals, <code>false</code> if they are not.</returns>
        public static bool operator ==(Segment3 a, Segment3 b)
        {
            return a.Equals(b);
        }

        /// <summary>
        /// Get a <see cref="String"/> representation of this <see cref="Segment3"/>.
        /// </summary>
        /// <returns>A <see cref="String"/> representation of this <see cref="Segment3"/>.</returns>
        public override string ToString()
        {
            return $"{{A:{A} B:{B}}}";
        }
    }
}