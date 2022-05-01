/* Licensed under the MIT/X11 license.
 * Copyright (c) 2006-2008 the OpenTK Team.
 * This notice may not be removed from any source distribution.
 * See license.txt for licensing detailed licensing details.
 *
 * Contributions by Andy Gill, James Talton and Georg Wächter.
 */

using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Contains common mathematical functions and constants.
    /// Copied from - https://github.com/opentk/opentk/blob/3.x/src/OpenTK/Math/MathHelper.cs
    /// Modified by BGE to add 
    /// </summary>
    public static class MathHelper
    {
        /// <summary>
        /// Defines the value of Pi as a <see cref="System.Single"/>.
        /// </summary>
        public const float Pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930382f;

        /// <summary>
        /// Defines the value of Pi divided by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float PiOver2 = Pi / 2;

        /// <summary>
        /// Defines the value of Pi divided by three as a <see cref="System.Single"/>.
        /// </summary>
        public const float PiOver3 = Pi / 3;

        /// <summary>
        /// Definesthe value of  Pi divided by four as a <see cref="System.Single"/>.
        /// </summary>
        public const float PiOver4 = Pi / 4;

        /// <summary>
        /// Defines the value of Pi divided by six as a <see cref="System.Single"/>.
        /// </summary>
        public const float PiOver6 = Pi / 6;

        /// <summary>
        /// Defines the value of Pi multiplied by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float TwoPi = 2 * Pi;

        /// <summary>
        /// Defines the value of Pi multiplied by 3 and divided by two as a <see cref="System.Single"/>.
        /// </summary>
        public const float ThreePiOver2 = 3 * Pi / 2;

        /// <summary>
        /// Defines the value of E as a <see cref="System.Single"/>.
        /// </summary>
        public const float E = 2.71828182845904523536f;

        /// <summary>
        /// Defines the base-10 logarithm of E.
        /// </summary>
        public const float Log10E = 0.434294482f;

        /// <summary>
        /// Defines the base-2 logarithm of E.
        /// </summary>
        public const float Log2E = 1.442695041f;

        /// <summary>
        /// Returns the next power of two that is greater than or equal to the specified number.
        /// </summary>
        /// <param name="n">The specified number.</param>
        /// <returns>The next power of two.</returns>
        public static long NextPowerOfTwo(long n)
        {
            if (n < 0)
            {
                throw new ArgumentOutOfRangeException("n", "Must be positive.");
            }
            return (long)System.Math.Pow(2, System.Math.Ceiling(System.Math.Log((double)n, 2)));
        }

        /// <summary>
        /// Returns the next power of two that is greater than or equal to the specified number.
        /// </summary>
        /// <param name="n">The specified number.</param>
        /// <returns>The next power of two.</returns>
        public static int NextPowerOfTwo(int n)
        {
            if (n < 0)
            {
                throw new ArgumentOutOfRangeException("n", "Must be positive.");
            }
            return (int)System.Math.Pow(2, System.Math.Ceiling(System.Math.Log((double)n, 2)));
        }

        /// <summary>
        /// Returns the next power of two that is greater than or equal to the specified number.
        /// </summary>
        /// <param name="n">The specified number.</param>
        /// <returns>The next power of two.</returns>
        public static float NextPowerOfTwo(float n)
        {
            if (n < 0)
            {
                throw new ArgumentOutOfRangeException("n", "Must be positive.");
            }
            return (float)System.Math.Pow(2, System.Math.Ceiling(System.Math.Log((double)n, 2)));
        }

        /// <summary>
        /// Returns the next power of two that is greater than or equal to the specified number.
        /// </summary>
        /// <param name="n">The specified number.</param>
        /// <returns>The next power of two.</returns>
        public static double NextPowerOfTwo(double n)
        {
            if (n < 0)
            {
                throw new ArgumentOutOfRangeException("n", "Must be positive.");
            }
            return System.Math.Pow(2, System.Math.Ceiling(System.Math.Log((double)n, 2)));
        }

        /// <summary>Calculates the factorial of a given natural number.
        /// </summary>
        /// <param name="n">The number.</param>
        /// <returns>n!</returns>
        public static long Factorial(int n)
        {
            long result = 1;

            for (; n > 1; n--)
            {
                result *= n;
            }

            return result;
        }

        /// <summary>
        /// Calculates the binomial coefficient <paramref name="n"/> above <paramref name="k"/>.
        /// </summary>
        /// <param name="n">The n.</param>
        /// <param name="k">The k.</param>
        /// <returns>n! / (k! * (n - k)!)</returns>
        public static long BinomialCoefficient(int n, int k)
        {
            return Factorial(n) / (Factorial(k) * Factorial(n - k));
        }

        /// <summary>
        /// Convert degrees to radians
        /// </summary>
        /// <param name="degrees">An angle in degrees</param>
        /// <returns>The angle expressed in radians</returns>
        public static float DegreesToRadians(float degrees)
        {
            const float degToRad = (float)System.Math.PI / 180.0f;
            return degrees * degToRad;
        }

        /// <summary>
        /// Convert radians to degrees
        /// </summary>
        /// <param name="radians">An angle in radians</param>
        /// <returns>The angle expressed in degrees</returns>
        public static float RadiansToDegrees(float radians)
        {
            const float radToDeg = 180.0f / (float)System.Math.PI;
            return radians * radToDeg;
        }

        /// <summary>
        /// Convert degrees to radians
        /// </summary>
        /// <param name="degrees">An angle in degrees</param>
        /// <returns>The angle expressed in radians</returns>
        public static double DegreesToRadians(double degrees)
        {
            const double degToRad = System.Math.PI / 180.0;
            return degrees * degToRad;
        }

        /// <summary>
        /// Convert radians to degrees
        /// </summary>
        /// <param name="radians">An angle in radians</param>
        /// <returns>The angle expressed in degrees</returns>
        public static double RadiansToDegrees(double radians)
        {
            const double radToDeg = 180.0 / System.Math.PI;
            return radians * radToDeg;
        }

        /// <summary>
        /// Swaps two double values.
        /// </summary>
        /// <param name="a">The first value.</param>
        /// <param name="b">The second value.</param>
        public static void Swap(ref double a, ref double b)
        {
            double temp = a;
            a = b;
            b = temp;
        }

        /// <summary>
        /// Swaps two float values.
        /// </summary>
        /// <param name="a">The first value.</param>
        /// <param name="b">The second value.</param>
        public static void Swap(ref float a, ref float b)
        {
            float temp = a;
            a = b;
            b = temp;
        }

        /// <summary>
        /// Clamps a number between a minimum and a maximum.
        /// </summary>
        /// <param name="n">The number to clamp.</param>
        /// <param name="min">The minimum allowed value.</param>
        /// <param name="max">The maximum allowed value.</param>
        /// <returns>min, if n is lower than min; max, if n is higher than max; n otherwise.</returns>
        public static int Clamp(int n, int min, int max)
        {
            return Math.Max(Math.Min(n, max), min);
        }

        /// <summary>
        /// Clamps a number between a minimum and a maximum.
        /// </summary>
        /// <param name="n">The number to clamp.</param>
        /// <param name="min">The minimum allowed value.</param>
        /// <param name="max">The maximum allowed value.</param>
        /// <returns>min, if n is lower than min; max, if n is higher than max; n otherwise.</returns>
        public static float Clamp(float n, float min, float max)
        {
            return Math.Max(Math.Min(n, max), min);
        }

        /// <summary>
        /// Clamps a number between a minimum and a maximum.
        /// </summary>
        /// <param name="n">The number to clamp.</param>
        /// <param name="min">The minimum allowed value.</param>
        /// <param name="max">The maximum allowed value.</param>
        /// <returns>min, if n is lower than min; max, if n is higher than max; n otherwise.</returns>
        public static double Clamp(double n, double min, double max)
        {
            return Math.Max(Math.Min(n, max), min);
        }

        /// <summary>
        /// Approximates double-precision floating point equality by an epsilon (maximum error) value.
        /// This method is designed as a "fits-all" solution and attempts to handle as many cases as possible.
        /// </summary>
        /// <param name="a">The first float.</param>
        /// <param name="b">The second float.</param>
        /// <param name="epsilon">The maximum error between the two.</param>
        /// <returns><value>true</value> if the values are approximately equal within the error margin; otherwise, <value>false</value>.</returns>
        public static bool ApproximatelyEqualEpsilon(double a, double b, double epsilon)
        {
            const double doubleNormal = (1L << 52) * double.Epsilon;
            double absA = Math.Abs(a);
            double absB = Math.Abs(b);
            double diff = Math.Abs(a - b);

            if (a == b)
            {
                // Shortcut, handles infinities
                return true;
            }

            if (a == 0.0f || b == 0.0f || diff < doubleNormal)
            {
                // a or b is zero, or both are extremely close to it.
                // relative error is less meaningful here
                return diff < (epsilon * doubleNormal);
            }

            // use relative error
            return diff / Math.Min((absA + absB), double.MaxValue) < epsilon;
        }

        /// <summary>
        /// Approximates single-precision floating point equality by an epsilon (maximum error) value.
        /// This method is designed as a "fits-all" solution and attempts to handle as many cases as possible.
        /// </summary>
        /// <param name="a">The first float.</param>
        /// <param name="b">The second float.</param>
        /// <param name="epsilon">The maximum error between the two.</param>
        /// <returns><value>true</value> if the values are approximately equal within the error margin; otherwise, <value>false</value>.</returns>
        public static bool ApproximatelyEqualEpsilon(float a, float b, float epsilon)
        {
            const float floatNormal = (1 << 23) * float.Epsilon;
            float absA = Math.Abs(a);
            float absB = Math.Abs(b);
            float diff = Math.Abs(a - b);

            if (a == b)
            {
                // Shortcut, handles infinities
                return true;
            }

            if (a == 0.0f || b == 0.0f || diff < floatNormal)
            {
                // a or b is zero, or both are extremely close to it.
                // relative error is less meaningful here
                return diff < (epsilon * floatNormal);
            }

            // use relative error
            float relativeError = diff / Math.Min((absA + absB), float.MaxValue);
            return relativeError < epsilon;
        }

        /// <summary>
        /// Approximates equivalence between two single-precision floating-point numbers on a direct human scale.
        /// It is important to note that this does not approximate equality - instead, it merely checks whether or not
        /// two numbers could be considered equivalent to each other within a certain tolerance. The tolerance is
        /// inclusive.
        /// </summary>
        /// <param name="a">The first value to compare.</param>
        /// <param name="b">The second value to compare·</param>
        /// <param name="tolerance">The tolerance within which the two values would be considered equivalent.</param>
        /// <returns>Whether or not the values can be considered equivalent within the tolerance.</returns>
        public static bool ApproximatelyEquivalent(float a, float b, float tolerance)
        {
            if (a == b)
            {
                // Early bailout, handles infinities
                return true;
            }

            float diff = Math.Abs(a - b);
            return diff <= tolerance;
        }

        /// <summary>
        /// Approximates equivalence between two double-precision floating-point numbers on a direct human scale.
        /// It is important to note that this does not approximate equality - instead, it merely checks whether or not
        /// two numbers could be considered equivalent to each other within a certain tolerance. The tolerance is
        /// inclusive.
        /// </summary>
        /// <param name="a">The first value to compare.</param>
        /// <param name="b">The second value to compare·</param>
        /// <param name="tolerance">The tolerance within which the two values would be considered equivalent.</param>
        /// <returns>Whether or not the values can be considered equivalent within the tolerance.</returns>
        public static bool ApproximatelyEquivalent(double a, double b, double tolerance)
        {
            if (a == b)
            {
                // Early bailout, handles infinities
                return true;
            }

            double diff = Math.Abs(a - b);
            return diff <= tolerance;
        }

        /// <summary>
        /// Determine if a value is negative (includning NaN).
        /// </summary>
        static bool IsNegative(double value)
        {
            return Math.Sign(value) == -1;
        }

        /// <summary>
        /// Impose the sign of a number onto a value.
        /// </summary>
        static double CopySign(double value, double sign)
        {
            return (IsNegative(value) == IsNegative(sign)) ? value : -value;
        }

        /// <summary>
        /// Convert a quaternion to euler angles.
        /// Taken from https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles.
        /// NOTE: because this use double-precision calculation, this is slower than it need be.
        /// TODO: Use MathF instead once we upgrade to .NET 5.
        /// </summary>
        public static Vector3 PitchYawRoll(in this Quaternion rotation)
        {
            Vector3 angles;

            // yaw (z-axis rotation)
            double siny_cosp = 2.0f * (rotation.W * rotation.Z + rotation.X * rotation.Y);
            double cosy_cosp = 1.0f - 2.0f * (rotation.Y * rotation.Y + rotation.Z * rotation.Z);
            angles.Y = (float)Math.Atan2(siny_cosp, cosy_cosp);

            // pitch (y-axis rotation)
            double sinp = 2.0f * (rotation.W * rotation.Y - rotation.Z * rotation.X);
            if (Math.Abs(sinp) >= 1.0f)
                angles.X = (float)CopySign(Math.PI / 2.0f, sinp); // use 90 degrees if out of range
            else
                angles.X = (float)Math.Asin(sinp);

            // roll (x-axis rotation)
            var sinr_cosp = 2.0f * (rotation.W * rotation.X + rotation.Y * rotation.Z);
            var cosr_cosp = 1.0f - 2.0f * (rotation.X * rotation.X + rotation.Y * rotation.Y);
            angles.Z = (float)Math.Atan2(sinr_cosp, cosr_cosp);

            return angles;
        }

        /// <summary>
        /// Extract scale from a matrix.
        /// </summary>
        public static Vector3 Scale(in this Matrix4x4 matrix)
		{
            return new Vector3(
                new Vector3(matrix.M11, matrix.M21, matrix.M31).Length(),
                new Vector3(matrix.M12, matrix.M22, matrix.M32).Length(),
                new Vector3(matrix.M13, matrix.M23, matrix.M33).Length());
        }

        /// <summary>
        /// Extract rotation from a matrix.
        /// </summary>
        public static Quaternion Rotation(in this Matrix4x4 matrix)
        {
            return Quaternion.CreateFromRotationMatrix(matrix);
        }

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
        public static void Intersects(in this Plane plane, ref Box3 box, out PlaneIntersectionType result)
        {
            box.Intersects(in plane, out result);
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
        public static void Intersects(in this Plane plane, in Frustum frustum, out PlaneIntersectionType result)
        {
            frustum.Intersects(in plane, out result);
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
        public static void Intersects(in this Plane plane, in Sphere sphere, out PlaneIntersectionType result)
        {
            sphere.Intersects(in plane, out result);
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
            plane.Intersects(in point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Plane"/> intersects a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane"/> with the specified <see cref="Vector3"/>.
        /// </param>
        public static void Intersects(in this Plane plane, in Vector3 point, out PlaneIntersectionType result)
        {
            float distance;
            plane.DotCoordinate(in point, out distance);

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
        public static void DotCoordinate(in this Plane plane, in Vector3 value, out float result)
        {
            result = (plane.Normal.X * value.X) + (plane.Normal.Y * value.Y) + (plane.Normal.Z * value.Z) + plane.D;
        }

        /// <summary>
        /// Returns a value indicating what side (positive/negative) of a plane a point is
        /// </summary>
        /// <param name="plane">The plane to check against</param>
        /// <param name="point">The point to check with</param>
        /// <returns>Greater than zero if on the positive side, less than zero if on the negative size, 0 otherwise</returns>
        public static float ClassifyPoint(in this Plane plane, in Vector3 point)
        {
            return point.X * plane.Normal.X + point.Y * plane.Normal.Y + point.Z * plane.Normal.Z + plane.D;
        }
    }
}