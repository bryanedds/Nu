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
    /// Contains common mathematical functions.
    /// Copied from - https://github.com/opentk/opentk/blob/3.x/src/OpenTK/Math/MathHelper.cs
    /// </summary>
    public static class Math
    {
        /// <summary>
        /// Defines the value of Pi minus a small epsilon to keep it from blowing up operations like Matrix4x4.CreatePerspectiveFieldOfView.
        /// </summary>
        public const double PI_MINUS_EPSILON = 3.14159;

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

        /// <summary>
        /// Determines if a long number is a power of two.
        /// </summary>
        public static bool PowerOfTwo(long n)
        {
            return NextPowerOfTwo(n) == n;
        }

        /// <summary>
        /// Determines if an integer number is a power of two.
        /// </summary>
        public static bool PowerOfTwo(int n)
        {
            return NextPowerOfTwo(n) == n;
        }

        /// <summary>
        /// Determines if a floating point number is a power of two.
        /// </summary>
        public static bool PowerOfTwo(float n)
        {
            return NextPowerOfTwo(n) == n;
        }

        /// <summary>
        /// Determines if a double precision floating point number is a power of two.
        /// </summary>
        public static bool PowerOfTwo(double n)
        {
            return NextPowerOfTwo(n) == n;
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
            return System.Math.Max(System.Math.Min(n, max), min);
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
            return System.Math.Max(System.Math.Min(n, max), min);
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
            return System.Math.Max(System.Math.Min(n, max), min);
        }

        /// <summary>
        /// Linearly interpolates between two values.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Destination value.</param>
        /// <param name="amount">Value between 0 and 1 indicating the weight of value2.</param>
        /// <returns>Interpolated value.</returns> 
        /// <remarks>This method performs the linear interpolation based on the following formula:
        /// <code>value1 + (value2 - value1) * amount</code>.
        /// Passing amount a value of 0 will cause value1 to be returned, a value of 1 will cause value2 to be returned.
        /// See Math.LerpPrecise for a less efficient version with more precision around edge cases.
        /// </remarks>
        public static float Lerp(float value1, float value2, float amount)
        {
            return value1 + (value2 - value1) * amount;
        }

        /// <summary>
        /// Linearly interpolates between two values.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Destination value.</param>
        /// <param name="amount">Value between 0 and 1 indicating the weight of value2.</param>
        /// <returns>Interpolated value.</returns> 
        /// <remarks>This method performs the linear interpolation based on the following formula:
        /// <code>value1 + (value2 - value1) * amount</code>.
        /// Passing amount a value of 0 will cause value1 to be returned, a value of 1 will cause value2 to be returned.
        /// See Math.LerpPrecise for a less efficient version with more precision around edge cases.
        /// </remarks>
        public static double Lerp(double value1, double value2, double amount)
        {
            return value1 + (value2 - value1) * amount;
        }

        /// <summary>
        /// Linearly interpolates between two values.
        /// This method is a less efficient, more precise version of Math.Lerp.
        /// See remarks for more info.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Destination value.</param>
        /// <param name="amount">Value between 0 and 1 indicating the weight of value2.</param>
        /// <returns>Interpolated value.</returns>
        /// <remarks>This method performs the linear interpolation based on the following formula:
        /// <code>((1 - amount) * value1) + (value2 * amount)</code>.
        /// Passing amount a value of 0 will cause value1 to be returned, a value of 1 will cause value2 to be returned.
        /// This method does not have the floating point precision issue that Math.Lerp has.
        /// i.e. If there is a big gap between value1 and value2 in magnitude (e.g. value1=10000000000000000, value2=1),
        /// right at the edge of the interpolation range (amount=1), Math.Lerp will return 0 (whereas it should return 1).
        /// This also holds for value1=10^17, value2=10; value1=10^18,value2=10^2... so on.
        /// For an in depth explanation of the issue, see below references:
        /// Relevant Wikipedia Article: https://en.wikipedia.org/wiki/Linear_interpolation#Programming_language_support
        /// Relevant StackOverflow Answer: http://stackoverflow.com/questions/4353525/floating-point-linear-interpolation#answer-23716956
        /// </remarks>
        public static float LerpPrecise(float value1, float value2, float amount)
        {
            return ((1 - amount) * value1) + (value2 * amount);
        }

        /// <summary>
        /// Linearly interpolates between two values.
        /// This method is a less efficient, more precise version of Math.Lerp.
        /// See remarks for more info.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Destination value.</param>
        /// <param name="amount">Value between 0 and 1 indicating the weight of value2.</param>
        /// <returns>Interpolated value.</returns>
        /// <remarks>This method performs the linear interpolation based on the following formula:
        /// <code>((1 - amount) * value1) + (value2 * amount)</code>.
        /// Passing amount a value of 0 will cause value1 to be returned, a value of 1 will cause value2 to be returned.
        /// This method does not have the floating point precision issue that Math.Lerp has.
        /// i.e. If there is a big gap between value1 and value2 in magnitude (e.g. value1=10000000000000000, value2=1),
        /// right at the edge of the interpolation range (amount=1), Math.Lerp will return 0 (whereas it should return 1).
        /// This also holds for value1=10^17, value2=10; value1=10^18,value2=10^2... so on.
        /// For an in depth explanation of the issue, see below references:
        /// Relevant Wikipedia Article: https://en.wikipedia.org/wiki/Linear_interpolation#Programming_language_support
        /// Relevant StackOverflow Answer: http://stackoverflow.com/questions/4353525/floating-point-linear-interpolation#answer-23716956
        /// </remarks>
        public static double LerpPrecise(double value1, double value2, double amount)
        {
            return ((1 - amount) * value1) + (value2 * amount);
        }

        /// <summary>
        /// Performs a Hermite spline interpolation.
        /// </summary>
        /// <param name="value1">Source position.</param>
        /// <param name="tangent1">Source tangent.</param>
        /// <param name="value2">Source position.</param>
        /// <param name="tangent2">Source tangent.</param>
        /// <param name="amount">Weighting factor.</param>
        /// <returns>The result of the Hermite spline interpolation.</returns>
        public static float Hermite(float value1, float tangent1, float value2, float tangent2, float amount)
        {
            // All transformed to double not to lose precission
            // Otherwise, for high numbers of param:amount the result is NaN instead of Infinity
            double v1 = value1, v2 = value2, t1 = tangent1, t2 = tangent2, s = amount, result;
            double sCubed = s * s * s;
            double sSquared = s * s;

            if (amount == 0f)
                result = value1;
            else if (amount == 1f)
                result = value2;
            else
                result = (2 * v1 - 2 * v2 + t2 + t1) * sCubed +
                    (3 * v2 - 3 * v1 - 2 * t1 - t2) * sSquared +
                    t1 * s +
                    v1;
            return (float)result;
        }

        /// <summary>
        /// Performs a Hermite spline interpolation.
        /// </summary>
        /// <param name="value1">Source position.</param>
        /// <param name="tangent1">Source tangent.</param>
        /// <param name="value2">Source position.</param>
        /// <param name="tangent2">Source tangent.</param>
        /// <param name="amount">Weighting factor.</param>
        /// <returns>The result of the Hermite spline interpolation.</returns>
        public static double Hermite(double value1, double tangent1, double value2, double tangent2, double amount)
        {
            double v1 = value1, v2 = value2, t1 = tangent1, t2 = tangent2, s = amount, result;
            double sCubed = s * s * s;
            double sSquared = s * s;

            if (amount == 0.0)
                result = value1;
            else if (amount == 1.0)
                result = value2;
            else
                result = (2 * v1 - 2 * v2 + t2 + t1) * sCubed +
                    (3 * v2 - 3 * v1 - 2 * t1 - t2) * sSquared +
                    t1 * s +
                    v1;
            return result;
        }

        /// <summary>
        /// Interpolates between two values using a cubic equation.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Source value.</param>
        /// <param name="amount">Weighting value.</param>
        /// <returns>Interpolated value.</returns>
        public static float SmoothStep(float value1, float value2, float amount)
        {
            // It is expected that 0 < amount < 1
            // If amount < 0, return value1
            // If amount > 1, return value2
            float result = Math.Clamp(amount, 0f, 1f);
            result = Math.Hermite(value1, 0f, value2, 0f, result);

            return result;
        }

        /// <summary>
        /// Interpolates between two values using a cubic equation.
        /// </summary>
        /// <param name="value1">Source value.</param>
        /// <param name="value2">Source value.</param>
        /// <param name="amount">Weighting value.</param>
        /// <returns>Interpolated value.</returns>
        public static double SmoothStep(double value1, double value2, double amount)
        {
            // It is expected that 0 < amount < 1
            // If amount < 0, return value1
            // If amount > 1, return value2
            double result = Math.Clamp(amount, 0.0, 1.0);
            result = Math.Hermite(value1, 0.0, value2, 0.0, result);

            return result;
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
            float absA = System.Math.Abs(a);
            float absB = System.Math.Abs(b);
            float diff = System.Math.Abs(a - b);

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
            float relativeError = diff / System.Math.Min((absA + absB), float.MaxValue);
            return relativeError < epsilon;
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
            double absA = System.Math.Abs(a);
            double absB = System.Math.Abs(b);
            double diff = System.Math.Abs(a - b);

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
            return diff / System.Math.Min((absA + absB), double.MaxValue) < epsilon;
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

            float diff = System.Math.Abs(a - b);
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

            double diff = System.Math.Abs(a - b);
            return diff <= tolerance;
        }

        /// <summary>
        /// Impose the sign of a number onto a value.
        /// </summary>
        public static double CopySign(float value, float sign)
        {
            return (IsNegative(value) == IsNegative(sign)) ? value : -value;
        }

        /// <summary>
        /// Impose the sign of a number onto a value.
        /// </summary>
        public static double CopySign(double value, double sign)
        {
            return (IsNegative(value) == IsNegative(sign)) ? value : -value;
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
        /// Determine if a value is negative (includning NaN).
        /// </summary>
        public static bool IsNegative(this float value)
        {
            return System.Math.Sign(value) == -1;
        }

        /// <summary>
        /// Determine if a value is negative (includning NaN).
        /// </summary>
        public static bool IsNegative(this double value)
        {
            return System.Math.Sign(value) == -1;
        }

        /// <summary>
        /// Convert degrees to radians
        /// </summary>
        /// <param name="degrees">An angle in degrees</param>
        /// <returns>The angle expressed in radians</returns>
        public static float ToRadians(this float degrees)
        {
            const float degToRad = MathF.PI / 180.0f;
            return degrees * degToRad;
        }

        /// <summary>
        /// Convert degrees to radians
        /// </summary>
        /// <param name="degrees">An angle in degrees</param>
        /// <returns>The angle expressed in radians</returns>
        public static double ToRadians(this double degrees)
        {
            const double degToRad = System.Math.PI / 180.0;
            return degrees * degToRad;
        }

        /// <summary>
        /// Convert radians to degrees
        /// </summary>
        /// <param name="radians">An angle in radians</param>
        /// <returns>The angle expressed in degrees</returns>
        public static float ToDegrees(this float radians)
        {
            const float radToDeg = 180.0f / (float)System.Math.PI;
            return radians * radToDeg;
        }

        /// <summary>
        /// Convert radians to degrees
        /// </summary>
        /// <param name="radians">An angle in radians</param>
        /// <returns>The angle expressed in degrees</returns>
        public static double ToDegrees(this double radians)
        {
            const double radToDeg = 180.0 / System.Math.PI;
            return radians * radToDeg;
        }

        /// <summary>
        /// Convert euler angles in [roll, pitch, yaw] to a quaternion.
        /// Sourced from - https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
        /// </summary>
        public static Quaternion RollPitchYaw(in this Vector3 angles)
		{
            var roll = angles.X;
            var pitch = angles.Y;
            var yaw = angles.Z;

            // Abbreviations for the various angular functions
            var cy = System.MathF.Cos(yaw * 0.5f);
            var sy = System.MathF.Sin(yaw * 0.5f);
            var cp = System.MathF.Cos(pitch * 0.5f);
            var sp = System.MathF.Sin(pitch * 0.5f);
            var cr = System.MathF.Cos(roll * 0.5f);
            var sr = System.MathF.Sin(roll * 0.5f);

            Quaternion q;
            q.W = cr * cp * cy + sr * sp * sy;
            q.X = sr * cp * cy - cr * sp * sy;
            q.Y = cr * sp * cy + sr * cp * sy;
            q.Z = cr * cp * sy - sr * sp * cy;

            return q;
        }

        /// <summary>
        /// Convert a quaternion to euler angles in [roll, pitch, yaw].
        /// Sourced from - https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
        /// NOTE: because this use double-precision calculation, it might be slower than it needs be.
        /// TODO: consider using MathF instead once we upgrade to .NET 5.
        /// </summary>
        public static Vector3 RollPitchYaw(in this Quaternion rotation)
        {
            Vector3 angles;

            // roll (x-axis rotation)
            var sinr_cosp = 2.0f * (rotation.W * rotation.X + rotation.Y * rotation.Z);
            var cosr_cosp = 1.0f - 2.0f * (rotation.X * rotation.X + rotation.Y * rotation.Y);
            angles.X = (float)System.Math.Atan2(sinr_cosp, cosr_cosp);

            // pitch (y-axis rotation)
            double sinp = 2.0f * (rotation.W * rotation.Y - rotation.Z * rotation.X);
            if (System.Math.Abs(sinp) >= 1.0f)
                angles.Y = (float)CopySign(System.Math.PI / 2.0f, sinp); // use 90 degrees if out of range
            else
                angles.Y = (float)System.Math.Asin(sinp);

            // yaw (z-axis rotation)
            double siny_cosp = 2.0f * (rotation.W * rotation.Z + rotation.X * rotation.Y);
            double cosy_cosp = 1.0f - 2.0f * (rotation.Y * rotation.Y + rotation.Z * rotation.Z);
            angles.Z = (float)System.Math.Atan2(siny_cosp, cosy_cosp);

            return angles;
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Box3"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(in this Plane3 plane, Box3 box)
        {
            return box.Intersects(plane);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Box3"/>.
        /// </summary>
        /// <param name="box">The <see cref="Box3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Box3"/>.
        /// </param>
        public static void Intersects(in this Plane3 plane, ref Box3 box, out PlaneIntersectionType result)
        {
            box.Intersects(in plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">The <see cref="Frustum"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Frustum"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(in this Plane3 plane, Frustum frustum)
        {
			PlaneIntersectionType result;
			frustum.Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Frustum"/>.
        /// </summary>
        /// <param name="frustum">The <see cref="Frustum"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Frustum"/>.
        /// </param>
        /// </returns>
        public static void Intersects(in this Plane3 plane, in Frustum frustum, out PlaneIntersectionType result)
        {
            frustum.Intersects(in plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Sphere"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(in this Plane3 plane, Sphere sphere)
        {
			PlaneIntersectionType result;
			sphere.Intersects(in plane, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Sphere"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Sphere"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Sphere"/>.
        /// </param>
        public static void Intersects(in this Plane3 plane, in Sphere sphere, out PlaneIntersectionType result)
        {
            sphere.Intersects(in plane, out result);
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="sphere">The <see cref="Vector3"/> to test for intersection.</param>
        /// <returns>
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Vector3"/>.
        /// </returns>
        public static PlaneIntersectionType Intersects(in this Plane3 plane, Vector3 point)
        {
            PlaneIntersectionType result;
            plane.Intersects(in point, out result);
            return result;
        }

        /// <summary>
        /// Check if this <see cref="Plane3"/> intersects a <see cref="Vector3"/>.
        /// </summary>
        /// <param name="point">The <see cref="Vector3"/> to test for intersection.</param>
        /// <param name="result">
        /// The type of intersection of this <see cref="Plane3"/> with the specified <see cref="Vector3"/>.
        /// </param>
        public static void Intersects(in this Plane3 plane, in Vector3 point, out PlaneIntersectionType result)
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
        /// the <see cref="Normal"/> vector of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </summary>
        /// <param name="value">The <see cref="Vector3"/> to calculate the dot product with.</param>
        /// <returns>
        /// The dot product of the specified <see cref="Vector3"/> and the normal of this <see cref="Plane3"/>
        /// plus the <see cref="D"/> value of this <see cref="Plane3"/>.
        /// </returns>
        public static float DotCoordinate(this Plane3 plane, Vector3 value)
        {
            return (plane.Normal.X * value.X) + (plane.Normal.Y * value.Y) + (plane.Normal.Z * value.Z) + plane.D;
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
        public static void DotCoordinate(in this Plane3 plane, in Vector3 value, out float result)
        {
            result = (plane.Normal.X * value.X) + (plane.Normal.Y * value.Y) + (plane.Normal.Z * value.Z) + plane.D;
        }

        /// <summary>
        /// Returns a value indicating what side (positive/negative) of a plane a point is
        /// </summary>
        /// <param name="plane">The plane to check against</param>
        /// <param name="point">The point to check with</param>
        /// <returns>Greater than zero if on the positive side, less than zero if on the negative size, 0 otherwise</returns>
        public static float ClassifyPoint(in this Plane3 plane, in Vector3 point)
        {
            return point.X * plane.Normal.X + point.Y * plane.Normal.Y + point.Z * plane.Normal.Z + plane.D;
        }
    }
}