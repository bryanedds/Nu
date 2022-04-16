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
        /// Create an oriented bounding box.
        /// </summary>
        public Box3 Orient(Quaternion rotation)
		{
            var positionOriented = Vector3.Transform(Position, rotation);
            var positionOriented2 = Vector3.Transform(Position + Size, rotation);
            return Enclose(positionOriented, positionOriented2);
        }

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
            return String.Format("{0}\n{1}", Position, Size);
        }
    }
}
