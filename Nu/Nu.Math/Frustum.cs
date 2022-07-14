// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Defines a viewing frustum for intersection operations.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v3.8/MonoGame.Framework/BoundingFrustum.cs
    /// </summary>
    public class Frustum : IEquatable<Frustum>
    {
        private Matrix4x4 _matrix;
        private readonly Vector3[] _corners = new Vector3[CornerCount];
        private readonly Plane3[] _planes = new Plane3[PlaneCount];

        /// <summary>
        /// The number of planes in the frustum.
        /// </summary>
        public const int PlaneCount = 6;

        /// <summary>
        /// The number of corner points in the frustum.
        /// </summary>
        public const int CornerCount = 8;

        /// <summary>
        /// Gets or sets the <see cref="Matrix4x4"/> of the frustum.
        /// </summary>
        public Matrix4x4 Matrix
        {
            get { return this._matrix; }
            set
            {
                this._matrix = value;
                this.CreatePlanes();    // FIXME: The odds are the planes will be used a lot more often than the matrix
                this.CreateCorners();   // is updated, so this should help performance. I hope ;)
            }
        }

        /// <summary>
        /// Gets the near plane of the frustum.
        /// </summary>
        public Plane3 Near
        {
            get { return this._planes[0]; }
        }

        /// <summary>
        /// Gets the far plane of the frustum.
        /// </summary>
        public Plane3 Far
        {
            get { return this._planes[1]; }
        }

        /// <summary>
        /// Gets the left plane of the frustum.
        /// </summary>
        public Plane3 Left
        {
            get { return this._planes[2]; }
        }

        /// <summary>
        /// Gets the right plane of the frustum.
        /// </summary>
        public Plane3 Right
        {
            get { return this._planes[3]; }
        }

        /// <summary>
        /// Gets the top plane of the frustum.
        /// </summary>
        public Plane3 Top
        {
            get { return this._planes[4]; }
        }

        /// <summary>
        /// Gets the bottom plane of the frustum.
        /// </summary>
        public Plane3 Bottom
        {
            get { return this._planes[5]; }
        }

        /// <summary>
        /// Constructs the frustum by extracting the view planes from a matrix.
        /// </summary>
        /// <param name="value">Combined matrix which usually is (View * Projection).</param>
        public Frustum(Matrix4x4 value)
        {
            this._matrix = value;
            this.CreatePlanes();
            this.CreateCorners();
        }

        /// <summary>
        /// Compares whether two <see cref="Frustum"/> instances are equal.
        /// </summary>
        /// <param name="a"><see cref="Frustum"/> instance on the left of the equal sign.</param>
        /// <param name="b"><see cref="Frustum"/> instance on the right of the equal sign.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public static bool operator ==(Frustum a, Frustum b)
        {
            if (Equals(a, null))
                return (Equals(b, null));

            if (Equals(b, null))
                return (Equals(a, null));

            return a._matrix == (b._matrix);
        }

        /// <summary>
        /// Compares whether two <see cref="Frustum"/> instances are not equal.
        /// </summary>
        /// <param name="a"><see cref="Frustum"/> instance on the left of the not equal sign.</param>
        /// <param name="b"><see cref="Frustum"/> instance on the right of the not equal sign.</param>
        /// <returns><c>true</c> if the instances are not equal; <c>false</c> otherwise.</returns>
        public static bool operator !=(Frustum a, Frustum b)
        {
            return !(a == b);
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">A <see cref="Box3"/> for testing.</param>
        /// <returns>Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Box3"/>.</returns>
        public ContainmentType Contains(Box3 box)
        {
            var result = default(ContainmentType);
            this.Contains(in box, out result);
            return result;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">A <see cref="Box3"/> for testing.</param>
        /// <param name="result">Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Box3"/> as an output parameter.</param>
        public void Contains(in Box3 box, out ContainmentType result)
        {
            var intersects = false;
            for (var i = 0; i < PlaneCount; ++i)
            {
                var planeIntersectionType = default(PlaneIntersectionType);
                box.Intersects(in this._planes[i], out planeIntersectionType);
                switch (planeIntersectionType)
                {
                    case PlaneIntersectionType.Front:
                        result = ContainmentType.Disjoint;
                        return;
                    case PlaneIntersectionType.Intersecting:
                        intersects = true;
                        break;
                }
            }
            result = intersects ? ContainmentType.Intersects : ContainmentType.Contains;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">A <see cref="Frustum"/> for testing.</param>
        /// <returns>Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Frustum"/>.</returns>
        public ContainmentType Contains(Frustum frustum)
        {
            if (this == frustum)                // We check to see if the two frustums are equal
                return ContainmentType.Contains;// If they are, there's no need to go any further.

            var intersects = false;
            for (var i = 0; i < PlaneCount; ++i)
            {
                PlaneIntersectionType planeIntersectionType;
                frustum.Intersects(in _planes[i], out planeIntersectionType);
                switch (planeIntersectionType)
                {
                    case PlaneIntersectionType.Front:
                        return ContainmentType.Disjoint;
                    case PlaneIntersectionType.Intersecting:
                        intersects = true;
                        break;
                }
            }
            return intersects ? ContainmentType.Intersects : ContainmentType.Contains;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">A <see cref="Sphere"/> for testing.</param>
        /// <returns>Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Sphere"/>.</returns>
        public ContainmentType Contains(Sphere sphere)
        {
            var result = default(ContainmentType);
            this.Contains(in sphere, out result);
            return result;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">A <see cref="Sphere"/> for testing.</param>
        /// <param name="result">Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Sphere"/> as an output parameter.</param>
        public void Contains(in Sphere sphere, out ContainmentType result)
        {
            var intersects = false;
            for (var i = 0; i < PlaneCount; ++i)
            {
                var planeIntersectionType = default(PlaneIntersectionType);

                // TODO: we might want to inline this for performance reasons
                sphere.Intersects(in this._planes[i], out planeIntersectionType);
                switch (planeIntersectionType)
                {
                    case PlaneIntersectionType.Front:
                        result = ContainmentType.Disjoint;
                        return;
                    case PlaneIntersectionType.Intersecting:
                        intersects = true;
                        break;
                }
            }
            result = intersects ? ContainmentType.Intersects : ContainmentType.Contains;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">A <see cref="Vector3"/> for testing.</param>
        /// <returns>Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Vector3"/>.</returns>
        public ContainmentType Contains(Vector3 point)
        {
            var result = default(ContainmentType);
            this.Contains(in point, out result);
            return result;
        }

        /// <summary>
        /// Containment test between this <see cref="Frustum"/> and specified <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">A <see cref="Vector3"/> for testing.</param>
        /// <param name="result">Result of testing for containment between this <see cref="Frustum"/> and specified <see cref="Vector3"/> as an output parameter.</param>
        public void Contains(in Vector3 point, out ContainmentType result)
        {
            for (var i = 0; i < PlaneCount; ++i)
            {
                // TODO: we might want to inline this for performance reasons
                if (this._planes[i].ClassifyPoint(in point) > 0)
                {
                    result = ContainmentType.Disjoint;
                    return;
                }
            }
            result = ContainmentType.Contains;
        }

        /// <summary>
        /// Compares whether current instance is equal to specified <see cref="Frustum"/>.
        /// </summary>
        /// <param name="other">The <see cref="Frustum"/> to compare.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public bool Equals(Frustum other)
        {
            return (this == other);
        }

        /// <summary>
        /// Compares whether current instance is equal to specified <see cref="Frustum"/>.
        /// </summary>
        /// <param name="obj">The <see cref="Object"/> to compare.</param>
        /// <returns><c>true</c> if the instances are equal; <c>false</c> otherwise.</returns>
        public override bool Equals(object obj)
        {
            return (obj is Frustum) && this == ((Frustum)obj);
        }

        /// <summary>
        /// Returns a copy of internal corners array.
        /// </summary>
        /// <returns>The array of corners.</returns>
        public Vector3[] GetCorners()
        {
            return (Vector3[])this._corners.Clone();
        }

        /// <summary>
        /// Returns a copy of internal corners array.
        /// </summary>
        /// <param name="corners">The array which values will be replaced to corner values of this instance. It must have size of <see cref="Frustum.CornerCount"/>.</param>
		public void GetCorners(Vector3[] corners)
        {
            if (corners == null) throw new ArgumentNullException("corners");
            if (corners.Length < CornerCount) throw new ArgumentOutOfRangeException("corners");

            this._corners.CopyTo(corners, 0);
        }

        /// <summary>
        /// Gets the hash code of this <see cref="Frustum"/>.
        /// </summary>
        /// <returns>Hash code of this <see cref="Frustum"/>.</returns>
        public override int GetHashCode()
        {
            return this._matrix.GetHashCode();
        }

        /// <summary>
        /// Gets whether or not a specified <see cref="Box3"/> intersects with this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="box">A <see cref="Box3"/> for intersection test.</param>
        /// <returns><c>true</c> if specified <see cref="Box3"/> intersects with this <see cref="Frustum"/>; <c>false</c> otherwise.</returns>
        public bool Intersects(Box3 box)
        {
            var result = false;
            this.Intersects(in box, out result);
            return result;
        }

        /// <summary>
        /// Gets whether or not a specified <see cref="Box3"/> intersects with this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="box">A <see cref="Box3"/> for intersection test.</param>
        /// <param name="result"><c>true</c> if specified <see cref="Box3"/> intersects with this <see cref="Frustum"/>; <c>false</c> otherwise as an output parameter.</param>
        public void Intersects(in Box3 box, out bool result)
        {
            var containment = default(ContainmentType);
            this.Contains(in box, out containment);
            result = containment != ContainmentType.Disjoint;
        }

        /// <summary>
        /// Gets whether or not a specified <see cref="Frustum"/> intersects with this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">An other <see cref="Frustum"/> for intersection test.</param>
        /// <returns><c>true</c> if other <see cref="Frustum"/> intersects with this <see cref="Frustum"/>; <c>false</c> otherwise.</returns>
        public bool Intersects(Frustum frustum)
        {
            return Contains(frustum) != ContainmentType.Disjoint;
        }

        /// <summary>
        /// Gets whether or not a specified <see cref="Sphere"/> intersects with this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="sphere">A <see cref="Sphere"/> for intersection test.</param>
        /// <returns><c>true</c> if specified <see cref="Sphere"/> intersects with this <see cref="Frustum"/>; <c>false</c> otherwise.</returns>
        public bool Intersects(Sphere sphere)
        {
            var result = default(bool);
            this.Intersects(in sphere, out result);
            return result;
        }

        /// <summary>
        /// Gets whether or not a specified <see cref="Sphere"/> intersects with this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="sphere">A <see cref="Sphere"/> for intersection test.</param>
        /// <param name="result"><c>true</c> if specified <see cref="Sphere"/> intersects with this <see cref="Frustum"/>; <c>false</c> otherwise as an output parameter.</param>
        public void Intersects(in Sphere sphere, out bool result)
        {
            var containment = default(ContainmentType);
            this.Contains(in sphere, out containment);
            result = containment != ContainmentType.Disjoint;
        }

        /// <summary>
        /// Gets type of intersection between specified <see cref="Plane3"/> and this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="plane">A <see cref="Plane3"/> for intersection test.</param>
        /// <returns>A plane intersection type.</returns>
        public PlaneIntersectionType Intersects(Plane3 plane)
        {
            PlaneIntersectionType result;
            Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Gets type of intersection between specified <see cref="Plane3"/> and this <see cref="Frustum"/>.
        /// </summary>
        /// <param name="plane">A <see cref="Plane3"/> for intersection test.</param>
        /// <param name="result">A plane intersection type as an output parameter.</param>
        public void Intersects(in Plane3 plane, out PlaneIntersectionType result)
        {
            plane.Intersects(in _corners[0], out result);
            for (int i = 1; i < _corners.Length; i++)
            {
                plane.Intersects(in _corners[i], out PlaneIntersectionType intersection);
                if (intersection != result)
                    result = PlaneIntersectionType.Intersecting;
            }
        }

        /// <summary>
        /// Gets the distance of intersection of <see cref="Ray3"/> and this <see cref="Frustum"/> or null if no intersection happens.
        /// </summary>
        /// <param name="ray">A <see cref="Ray3"/> for intersection test.</param>
        /// <returns>Distance at which ray intersects with this <see cref="Frustum"/> or null if no intersection happens.</returns>
        public float? Intersects(Ray3 ray)
        {
            float? result;
            Intersects(in ray, out result);
            return result;
        }

        /// <summary>
        /// Gets the distance of intersection of <see cref="Ray3"/> and this <see cref="Frustum"/> or null if no intersection happens.
        /// </summary>
        /// <param name="ray">A <see cref="Ray3"/> for intersection test.</param>
        /// <param name="result">Distance at which ray intersects with this <see cref="Frustum"/> or null if no intersection happens as an output parameter.</param>
        public void Intersects(in Ray3 ray, out float? result)
        {
            ContainmentType ctype;
            this.Contains(in ray.Position, out ctype);

            switch (ctype)
            {
                case ContainmentType.Disjoint:
                    result = null;
                    return;
                case ContainmentType.Contains:
                    result = 0.0f;
                    return;
                case ContainmentType.Intersects:
                    throw new NotImplementedException();
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        /// <summary>
        /// Returns a <see cref="String"/> representation of this <see cref="Frustum"/> in the format:
        /// {Near:[nearPlane] Far:[farPlane] Left:[leftPlane] Right:[rightPlane] Top:[topPlane] Bottom:[bottomPlane]}
        /// </summary>
        /// <returns><see cref="String"/> representation of this <see cref="Frustum"/>.</returns>
        public override string ToString()
        {
            return "{Near: " + this._planes[0] +
                   " Far:" + this._planes[1] +
                   " Left:" + this._planes[2] +
                   " Right:" + this._planes[3] +
                   " Top:" + this._planes[4] +
                   " Bottom:" + this._planes[5] +
                   "}";
        }

        private void CreateCorners()
        {
            IntersectionPoint(in this._planes[0], in this._planes[2], in this._planes[4], out this._corners[0]);
            IntersectionPoint(in this._planes[0], in this._planes[3], in this._planes[4], out this._corners[1]);
            IntersectionPoint(in this._planes[0], in this._planes[3], in this._planes[5], out this._corners[2]);
            IntersectionPoint(in this._planes[0], in this._planes[2], in this._planes[5], out this._corners[3]);
            IntersectionPoint(in this._planes[1], in this._planes[2], in this._planes[4], out this._corners[4]);
            IntersectionPoint(in this._planes[1], in this._planes[3], in this._planes[4], out this._corners[5]);
            IntersectionPoint(in this._planes[1], in this._planes[3], in this._planes[5], out this._corners[6]);
            IntersectionPoint(in this._planes[1], in this._planes[2], in this._planes[5], out this._corners[7]);
        }

        private void CreatePlanes()
        {
            this._planes[0] = new Plane3(-this._matrix.M13, -this._matrix.M23, -this._matrix.M33, -this._matrix.M43);
            this._planes[1] = new Plane3(this._matrix.M13 - this._matrix.M14, this._matrix.M23 - this._matrix.M24, this._matrix.M33 - this._matrix.M34, this._matrix.M43 - this._matrix.M44);
            this._planes[2] = new Plane3(-this._matrix.M14 - this._matrix.M11, -this._matrix.M24 - this._matrix.M21, -this._matrix.M34 - this._matrix.M31, -this._matrix.M44 - this._matrix.M41);
            this._planes[3] = new Plane3(this._matrix.M11 - this._matrix.M14, this._matrix.M21 - this._matrix.M24, this._matrix.M31 - this._matrix.M34, this._matrix.M41 - this._matrix.M44);
            this._planes[4] = new Plane3(this._matrix.M12 - this._matrix.M14, this._matrix.M22 - this._matrix.M24, this._matrix.M32 - this._matrix.M34, this._matrix.M42 - this._matrix.M44);
            this._planes[5] = new Plane3(-this._matrix.M14 - this._matrix.M12, -this._matrix.M24 - this._matrix.M22, -this._matrix.M34 - this._matrix.M32, -this._matrix.M44 - this._matrix.M42);

            this.NormalizePlane(ref this._planes[0]);
            this.NormalizePlane(ref this._planes[1]);
            this.NormalizePlane(ref this._planes[2]);
            this.NormalizePlane(ref this._planes[3]);
            this.NormalizePlane(ref this._planes[4]);
            this.NormalizePlane(ref this._planes[5]);
        }

        private static void IntersectionPoint(in Plane3 a, in Plane3 b, in Plane3 c, out Vector3 result)
        {
            // Formula used
            //                d1 ( N2 * N3 ) + d2 ( N3 * N1 ) + d3 ( N1 * N2 )
            //P =   -------------------------------------------------------------------------
            //                             N1 . ( N2 * N3 )
            //
            // Note: N refers to the normal, d refers to the displacement. '.' means dot product. '*' means cross product

            Vector3 v1, v2, v3;
            Vector3 cross = Vector3.Cross(b.Normal, c.Normal);

            float f = Vector3.Dot(a.Normal, cross);
            f *= -1.0f;

            cross = Vector3.Cross(b.Normal, c.Normal);
            v1 = Vector3.Multiply(cross, a.D);
            //v1 = (a.D * (Vector3.Cross(b.Normal, c.Normal)));


            cross = Vector3.Cross(c.Normal, a.Normal);
            v2 = Vector3.Multiply(cross, b.D);
            //v2 = (b.D * (Vector3.Cross(c.Normal, a.Normal)));


            cross = Vector3.Cross(a.Normal, b.Normal);
            v3 = Vector3.Multiply(cross, c.D);
            //v3 = (c.D * (Vector3.Cross(a.Normal, b.Normal)));

            result.X = (v1.X + v2.X + v3.X) / f;
            result.Y = (v1.Y + v2.Y + v3.Y) / f;
            result.Z = (v1.Z + v2.Z + v3.Z) / f;
        }

        private void NormalizePlane(ref Plane3 p)
        {
            float factor = 1f / p.Normal.Length();
            p.Normal.X *= factor;
            p.Normal.Y *= factor;
            p.Normal.Z *= factor;
            p.D *= factor;
        }
    }
}
