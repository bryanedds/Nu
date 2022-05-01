/*
Copyright (c) 2006 - 2008 The Open Toolkit library.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace Nu
{
    /// <summary>
    /// Represents a 3x3 matrix containing 3D rotation and scale.
    /// Copied from - https://github.com/opentk/opentk/blob/3.x/src/OpenTK/Math/Matrix3.cs
    /// Modified by BGE to more closely conform to System.Numerics.
    /// </summary>
    [Serializable]
    [StructLayout(LayoutKind.Sequential)]
    public struct Matrix3x3 : IEquatable<Matrix3x3>
    {
        /// <summary>
        /// First row of the matrix.
        /// </summary>
        public Vector3 Row0;

        /// <summary>
        /// Second row of the matrix.
        /// </summary>
        public Vector3 Row1;

        /// <summary>
        /// Third row of the matrix.
        /// </summary>
        public Vector3 Row2;

        /// <summary>
        /// The identity matrix.
        /// </summary>
        public static readonly Matrix3x3 Identity = new Matrix3x3(Vector3.UnitX, Vector3.UnitY, Vector3.UnitZ);

        /// <summary>
        /// The zero matrix.
        /// </summary>
        public static readonly Matrix3x3 Zero = new Matrix3x3(Vector3.Zero, Vector3.Zero, Vector3.Zero);



        /// <summary>
        /// Constructs a new instance.
        /// </summary>
        /// <param name="row0">Top row of the matrix</param>
        /// <param name="row1">Second row of the matrix</param>
        /// <param name="row2">Bottom row of the matrix</param>
        public Matrix3x3(Vector3 row0, Vector3 row1, Vector3 row2)
        {
            Row0 = row0;
            Row1 = row1;
            Row2 = row2;
        }

        /// <summary>
        /// Constructs a new instance.
        /// </summary>
        /// <param name="m00">First item of the first row of the matrix.</param>
        /// <param name="m01">Second item of the first row of the matrix.</param>
        /// <param name="m02">Third item of the first row of the matrix.</param>
        /// <param name="m10">First item of the second row of the matrix.</param>
        /// <param name="m11">Second item of the second row of the matrix.</param>
        /// <param name="m12">Third item of the second row of the matrix.</param>
        /// <param name="m20">First item of the third row of the matrix.</param>
        /// <param name="m21">Second item of the third row of the matrix.</param>
        /// <param name="m22">Third item of the third row of the matrix.</param>
        public Matrix3x3(
            float m00, float m01, float m02,
            float m10, float m11, float m12,
            float m20, float m21, float m22)
        {
            Row0 = new Vector3(m00, m01, m02);
            Row1 = new Vector3(m10, m11, m12);
            Row2 = new Vector3(m20, m21, m22);
        }




        /// <summary>
        /// Gets the determinant of this matrix.
        /// </summary>
        public float Determinant
        {
            get
            {
                float m11 = Row0.X, m12 = Row0.Y, m13 = Row0.Z,
                m21 = Row1.X, m22 = Row1.Y, m23 = Row1.Z,
                m31 = Row2.X, m32 = Row2.Y, m33 = Row2.Z;

                return m11 * m22 * m33 + m12 * m23 * m31 + m13 * m21 * m32
                     - m13 * m22 * m31 - m11 * m23 * m32 - m12 * m21 * m33;
            }
        }

        /// <summary>
        /// Gets the first column of this matrix.
        /// </summary>
        public Vector3 Column0
        {
            get { return new Vector3(Row0.X, Row1.X, Row2.X); }
        }

        /// <summary>
        /// Gets the second column of this matrix.
        /// </summary>
        public Vector3 Column1
        {
            get { return new Vector3(Row0.Y, Row1.Y, Row2.Y); }
        }

        /// <summary>
        /// Gets the third column of this matrix.
        /// </summary>
        public Vector3 Column2
        {
            get { return new Vector3(Row0.Z, Row1.Z, Row2.Z); }
        }

        /// <summary>
        /// Gets or sets the value at row 1, column 1 of this instance.
        /// </summary>
        public float M11 { get { return Row0.X; } set { Row0.X = value; } }

        /// <summary>
        /// Gets or sets the value at row 1, column 2 of this instance.
        /// </summary>
        public float M12 { get { return Row0.Y; } set { Row0.Y = value; } }

        /// <summary>
        /// Gets or sets the value at row 1, column 3 of this instance.
        /// </summary>
        public float M13 { get { return Row0.Z; } set { Row0.Z = value; } }

        /// <summary>
        /// Gets or sets the value at row 2, column 1 of this instance.
        /// </summary>
        public float M21 { get { return Row1.X; } set { Row1.X = value; } }

        /// <summary>
        /// Gets or sets the value at row 2, column 2 of this instance.
        /// </summary>
        public float M22 { get { return Row1.Y; } set { Row1.Y = value; } }

        /// <summary>
        /// Gets or sets the value at row 2, column 3 of this instance.
        /// </summary>
        public float M23 { get { return Row1.Z; } set { Row1.Z = value; } }

        /// <summary>
        /// Gets or sets the value at row 3, column 1 of this instance.
        /// </summary>
        public float M31 { get { return Row2.X; } set { Row2.X = value; } }

        /// <summary>
        /// Gets or sets the value at row 3, column 2 of this instance.
        /// </summary>
        public float M32 { get { return Row2.Y; } set { Row2.Y = value; } }

        /// <summary>
        /// Gets or sets the value at row 3, column 3 of this instance.
        /// </summary>
        public float M33 { get { return Row2.Z; } set { Row2.Z = value; } }

        /// <summary>
        /// Gets or sets the values along the main diagonal of the matrix.
        /// </summary>
        public Vector3 Diagonal
        {
            get
            {
                return new Vector3(Row0.X, Row1.Y, Row2.Z);
            }
            set
            {
                Row0.X = value.X;
                Row1.Y = value.Y;
                Row2.Z = value.Z;
            }
        }

        /// <summary>
        /// Gets the trace of the matrix, the sum of the values along the diagonal.
        /// </summary>
        public float Trace { get { return Row0.X + Row1.Y + Row2.Z; } }


        /// <summary>
        /// Converts this instance into its inverse.
        /// </summary>
        public void Invert()
        {
            this = Matrix3x3.Invert(this);
        }



        /// <summary>
        /// Converts this instance into its transpose.
        /// </summary>
        public void Transpose()
        {
            this = Matrix3x3.Transpose(this);
        }


        /// <summary>
        /// Returns a normalised copy of this instance.
        /// </summary>
        public Matrix3x3 Normalized()
        {
            Matrix3x3 m = this;
            m.Normalize();
            return m;
        }

        /// <summary>
        /// Divides each element in the Matrix by the <see cref="Determinant"/>.
        /// </summary>
        public void Normalize()
        {
            var determinant = this.Determinant;
            Row0 /= determinant;
            Row1 /= determinant;
            Row2 /= determinant;
        }

        /// <summary>
        /// Returns an inverted copy of this instance.
        /// </summary>
        public Matrix3x3 Inverted()
        {
            Matrix3x3 m = this;
            if (m.Determinant != 0)
            {
                m.Invert();
            }
            return m;
        }

        /// <summary>
        /// Returns a copy of this Matrix3 without scale.
        /// </summary>
        public Matrix3x3 ClearScale()
        {
            Matrix3x3 m = this;
            m.Row0 = Vector3.Normalize(m.Row0);
            m.Row1 = Vector3.Normalize(m.Row1);
            m.Row2 = Vector3.Normalize(m.Row2);
            return m;
        }
        /// <summary>
        /// Returns a copy of this Matrix3 without rotation.
        /// </summary>
        public Matrix3x3 ClearRotation()
        {
            Matrix3x3 m = this;
            m.Row0 = new Vector3(m.Row0.Length(), 0, 0);
            m.Row1 = new Vector3(0, m.Row1.Length(), 0);
            m.Row2 = new Vector3(0, 0, m.Row2.Length());
            return m;
        }

        /// <summary>
        /// Returns the scale component of this instance.
        /// </summary>
        public Vector3 ExtractScale() { return new Vector3(Row0.Length(), Row1.Length(), Row2.Length()); }

        /// <summary>
        /// Returns the rotation component of this instance. Quite slow.
        /// </summary>
        /// <param name="row_normalise">Whether the method should row-normalise (i.e. remove scale from) the Matrix. Pass false if you know it's already normalised.</param>
        public Quaternion ExtractRotation(bool row_normalise = true)
        {
            var row0 = Row0;
            var row1 = Row1;
            var row2 = Row2;

            if (row_normalise)
            {
                row0 = Vector3.Normalize(row0);
                row1 = Vector3.Normalize(row1);
                row2 = Vector3.Normalize(row2);
            }

            // code below adapted from Blender

            Quaternion q = new Quaternion();
            double trace = 0.25 * (row0.X + row1.Y + row2.Z + 1.0);

            if (trace > 0)
            {
                double sq = Math.Sqrt(trace);

                q.W = (float)sq;
                sq = 1.0 / (4.0 * sq);
                q.X = (float)((row1.Z - row2.Y) * sq);
                q.Y = (float)((row2.X - row0.Z) * sq);
                q.Z = (float)((row0.Y - row1.X) * sq);
            }
            else if (row0.X > row1.Y && row0.X > row2.Z)
            {
                double sq = 2.0 * Math.Sqrt(1.0 + row0.X - row1.Y - row2.Z);

                q.X = (float)(0.25 * sq);
                sq = 1.0 / sq;
                q.W = (float)((row2.Y - row1.Z) * sq);
                q.Y = (float)((row1.X + row0.Y) * sq);
                q.Z = (float)((row2.X + row0.Z) * sq);
            }
            else if (row1.Y > row2.Z)
            {
                double sq = 2.0 * Math.Sqrt(1.0 + row1.Y - row0.X - row2.Z);

                q.Y = (float)(0.25 * sq);
                sq = 1.0 / sq;
                q.W = (float)((row2.X - row0.Z) * sq);
                q.X = (float)((row1.X + row0.Y) * sq);
                q.Z = (float)((row2.Y + row1.Z) * sq);
            }
            else
            {
                double sq = 2.0 * Math.Sqrt(1.0 + row2.Z - row0.X - row1.Y);

                q.Z = (float)(0.25 * sq);
                sq = 1.0 / sq;
                q.W = (float)((row1.X - row0.Y) * sq);
                q.X = (float)((row2.X + row0.Z) * sq);
                q.Y = (float)((row2.Y + row1.Z) * sq);
            }

            return Quaternion.Normalize(q);
        }



        /// <summary>
        /// Build a rotation matrix from the specified axis/angle rotation.
        /// </summary>
        /// <param name="axis">The axis to rotate about.</param>
        /// <param name="angle">Angle in radians to rotate counter-clockwise (looking in the direction of the given axis).</param>
        /// <param name="result">A matrix instance.</param>
        public static void CreateFromAxisAngle(Vector3 axis, float angle, out Matrix3x3 result)
        {
            //normalize and create a local copy of the vector.
            var axisN = Vector3.Normalize(axis);
            float axisX = axisN.X, axisY = axisN.Y, axisZ = axisN.Z;

            //calculate angles
            float cos = (float)System.Math.Cos(-angle);
            float sin = (float)System.Math.Sin(-angle);
            float t = 1.0f - cos;

            //do the conversion math once
            float tXX = t * axisX * axisX,
            tXY = t * axisX * axisY,
            tXZ = t * axisX * axisZ,
            tYY = t * axisY * axisY,
            tYZ = t * axisY * axisZ,
            tZZ = t * axisZ * axisZ;

            float sinX = sin * axisX,
            sinY = sin * axisY,
            sinZ = sin * axisZ;

            result.Row0.X = tXX + cos;
            result.Row0.Y = tXY - sinZ;
            result.Row0.Z = tXZ + sinY;
            result.Row1.X = tXY + sinZ;
            result.Row1.Y = tYY + cos;
            result.Row1.Z = tYZ - sinX;
            result.Row2.X = tXZ - sinY;
            result.Row2.Y = tYZ + sinX;
            result.Row2.Z = tZZ + cos;
        }

        /// <summary>
        /// Build a rotation matrix from the specified axis/angle rotation.
        /// </summary>
        /// <param name="axis">The axis to rotate about.</param>
        /// <param name="angle">Angle in radians to rotate counter-clockwise (looking in the direction of the given axis).</param>
        /// <returns>A matrix instance.</returns>
        public static Matrix3x3 CreateFromAxisAngle(Vector3 axis, float angle)
        {
            Matrix3x3 result;
            CreateFromAxisAngle(axis, angle, out result);
            return result;
        }



        /// <summary>
        /// Builds a rotation matrix for a rotation around the x-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <param name="result">The resulting Matrix3 instance.</param>
        public static void CreateRotationX(float angle, out Matrix3x3 result)
        {
            float cos = (float)System.Math.Cos(angle);
            float sin = (float)System.Math.Sin(angle);

            result = Identity;
            result.Row1.Y = cos;
            result.Row1.Z = sin;
            result.Row2.Y = -sin;
            result.Row2.Z = cos;
        }

        /// <summary>
        /// Builds a rotation matrix for a rotation around the x-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <returns>The resulting Matrix3 instance.</returns>
        public static Matrix3x3 CreateRotationX(float angle)
        {
            Matrix3x3 result;
            CreateRotationX(angle, out result);
            return result;
        }

        /// <summary>
        /// Builds a rotation matrix for a rotation around the y-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <param name="result">The resulting Matrix3 instance.</param>
        public static void CreateRotationY(float angle, out Matrix3x3 result)
        {
            float cos = (float)System.Math.Cos(angle);
            float sin = (float)System.Math.Sin(angle);

            result = Identity;
            result.Row0.X = cos;
            result.Row0.Z = -sin;
            result.Row2.X = sin;
            result.Row2.Z = cos;
        }

        /// <summary>
        /// Builds a rotation matrix for a rotation around the y-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <returns>The resulting Matrix3 instance.</returns>
        public static Matrix3x3 CreateRotationY(float angle)
        {
            Matrix3x3 result;
            CreateRotationY(angle, out result);
            return result;
        }

        /// <summary>
        /// Builds a rotation matrix for a rotation around the z-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <param name="result">The resulting Matrix3 instance.</param>
        public static void CreateRotationZ(float angle, out Matrix3x3 result)
        {
            float cos = (float)System.Math.Cos(angle);
            float sin = (float)System.Math.Sin(angle);

            result = Identity;
            result.Row0.X = cos;
            result.Row0.Y = sin;
            result.Row1.X = -sin;
            result.Row1.Y = cos;
        }

        /// <summary>
        /// Builds a rotation matrix for a rotation around the z-axis.
        /// </summary>
        /// <param name="angle">The counter-clockwise angle in radians.</param>
        /// <returns>The resulting Matrix3 instance.</returns>
        public static Matrix3x3 CreateRotationZ(float angle)
        {
            Matrix3x3 result;
            CreateRotationZ(angle, out result);
            return result;
        }



        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="scale">Single scale factor for the x, y, and z axes.</param>
        /// <returns>A scale matrix.</returns>
        public static Matrix3x3 CreateScale(float scale)
        {
            Matrix3x3 result;
            CreateScale(scale, out result);
            return result;
        }

        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="scale">Scale factors for the x, y, and z axes.</param>
        /// <returns>A scale matrix.</returns>
        public static Matrix3x3 CreateScale(Vector3 scale)
        {
            Matrix3x3 result;
            CreateScale(in scale, out result);
            return result;
        }

        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="x">Scale factor for the x axis.</param>
        /// <param name="y">Scale factor for the y axis.</param>
        /// <param name="z">Scale factor for the z axis.</param>
        /// <returns>A scale matrix.</returns>
        public static Matrix3x3 CreateScale(float x, float y, float z)
        {
            Matrix3x3 result;
            CreateScale(x, y, z, out result);
            return result;
        }

        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="scale">Single scale factor for the x, y, and z axes.</param>
        /// <param name="result">A scale matrix.</param>
        public static void CreateScale(float scale, out Matrix3x3 result)
        {
            result = Identity;
            result.Row0.X = scale;
            result.Row1.Y = scale;
            result.Row2.Z = scale;
        }

        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="scale">Scale factors for the x, y, and z axes.</param>
        /// <param name="result">A scale matrix.</param>
        public static void CreateScale(in Vector3 scale, out Matrix3x3 result)
        {
            result = Identity;
            result.Row0.X = scale.X;
            result.Row1.Y = scale.Y;
            result.Row2.Z = scale.Z;
        }

        /// <summary>
        /// Creates a scale matrix.
        /// </summary>
        /// <param name="x">Scale factor for the x axis.</param>
        /// <param name="y">Scale factor for the y axis.</param>
        /// <param name="z">Scale factor for the z axis.</param>
        /// <param name="result">A scale matrix.</param>
        public static void CreateScale(float x, float y, float z, out Matrix3x3 result)
        {
            result = Identity;
            result.Row0.X = x;
            result.Row1.Y = y;
            result.Row2.Z = z;
        }


        /// <summary>
        /// Adds two instances.
        /// </summary>
        /// <param name="left">The left operand of the addition.</param>
        /// <param name="right">The right operand of the addition.</param>
        /// <returns>A new instance that is the result of the addition.</returns>
        public static Matrix3x3 Add(Matrix3x3 left, Matrix3x3 right)
        {
            Matrix3x3 result;
            Add(in left, in right, out result);
            return result;
        }

        /// <summary>
        /// Adds two instances.
        /// </summary>
        /// <param name="left">The left operand of the addition.</param>
        /// <param name="right">The right operand of the addition.</param>
        /// <param name="result">A new instance that is the result of the addition.</param>
        public static void Add(in Matrix3x3 left, in Matrix3x3 right, out Matrix3x3 result)
        {
            result.Row0 = left.Row0 + right.Row0;
            result.Row1 = left.Row1 + right.Row1;
            result.Row2 = left.Row2 + right.Row2;
        }

        /// <summary>
        /// Multiplies two instances.
        /// </summary>
        /// <param name="left">The left operand of the multiplication.</param>
        /// <param name="right">The right operand of the multiplication.</param>
        /// <returns>A new instance that is the result of the multiplication</returns>
        public static Matrix3x3 Multiply(Matrix3x3 left, Matrix3x3 right)
        {
            Matrix3x3 result;
            Multiply(in left, in right, out result);
            return result;
        }

        /// <summary>
        /// Multiplies two instances.
        /// </summary>
        /// <param name="left">The left operand of the multiplication.</param>
        /// <param name="right">The right operand of the multiplication.</param>
        /// <param name="result">A new instance that is the result of the multiplication</param>
        public static void Multiply(in Matrix3x3 left, in Matrix3x3 right, out Matrix3x3 result)
        {
            float   lM11 = left.Row0.X, lM12 = left.Row0.Y, lM13 = left.Row0.Z,
            lM21 = left.Row1.X, lM22 = left.Row1.Y, lM23 = left.Row1.Z,
            lM31 = left.Row2.X, lM32 = left.Row2.Y, lM33 = left.Row2.Z,
            rM11 = right.Row0.X, rM12 = right.Row0.Y, rM13 = right.Row0.Z,
            rM21 = right.Row1.X, rM22 = right.Row1.Y, rM23 = right.Row1.Z,
            rM31 = right.Row2.X, rM32 = right.Row2.Y, rM33 = right.Row2.Z;

            result.Row0.X = ((lM11 * rM11) + (lM12 * rM21)) + (lM13 * rM31);
            result.Row0.Y = ((lM11 * rM12) + (lM12 * rM22)) + (lM13 * rM32);
            result.Row0.Z = ((lM11 * rM13) + (lM12 * rM23)) + (lM13 * rM33);
            result.Row1.X = ((lM21 * rM11) + (lM22 * rM21)) + (lM23 * rM31);
            result.Row1.Y = ((lM21 * rM12) + (lM22 * rM22)) + (lM23 * rM32);
            result.Row1.Z = ((lM21 * rM13) + (lM22 * rM23)) + (lM23 * rM33);
            result.Row2.X = ((lM31 * rM11) + (lM32 * rM21)) + (lM33 * rM31);
            result.Row2.Y = ((lM31 * rM12) + (lM32 * rM22)) + (lM33 * rM32);
            result.Row2.Z = ((lM31 * rM13) + (lM32 * rM23)) + (lM33 * rM33);
        }



        /// <summary>
        /// Calculate the inverse of the given matrix
        /// </summary>
        /// <param name="mat">The matrix to invert</param>
        /// <param name="result">The inverse of the given matrix if it has one, or the input if it is singular</param>
        /// <exception cref="InvalidOperationException">Thrown if the Matrix3 is singular.</exception>
        public static void Invert(in Matrix3x3 mat, out Matrix3x3 result)
        {
            int[] colIdx = { 0, 0, 0 };
            int[] rowIdx = { 0, 0, 0 };
            int[] pivotIdx = { -1, -1, -1 };

            float[,] inverse = {{mat.Row0.X, mat.Row0.Y, mat.Row0.Z},
                {mat.Row1.X, mat.Row1.Y, mat.Row1.Z},
                {mat.Row2.X, mat.Row2.Y, mat.Row2.Z}};

            int icol = 0;
            int irow = 0;
            for (int i = 0; i < 3; i++)
            {
                float maxPivot = 0.0f;
                for (int j = 0; j < 3; j++)
                {
                    if (pivotIdx[j] != 0)
                    {
                        for (int k = 0; k < 3; ++k)
                        {
                            if (pivotIdx[k] == -1)
                            {
                                float absVal = System.Math.Abs(inverse[j, k]);
                                if (absVal > maxPivot)
                                {
                                    maxPivot = absVal;
                                    irow = j;
                                    icol = k;
                                }
                            }
                            else if (pivotIdx[k] > 0)
                            {
                                result = mat;
                                return;
                            }
                        }
                    }
                }

                ++(pivotIdx[icol]);

                if (irow != icol)
                {
                    for (int k = 0; k < 3; ++k)
                    {
                        float f = inverse[irow, k];
                        inverse[irow, k] = inverse[icol, k];
                        inverse[icol, k] = f;
                    }
                }

                rowIdx[i] = irow;
                colIdx[i] = icol;

                float pivot = inverse[icol, icol];

                if (pivot == 0.0f)
                {
                    throw new InvalidOperationException("Matrix is singular and cannot be inverted.");
                }

                float oneOverPivot = 1.0f / pivot;
                inverse[icol, icol] = 1.0f;
                for (int k = 0; k < 3; ++k)
                {
                    inverse[icol, k] *= oneOverPivot;
                }

                for (int j = 0; j < 3; ++j)
                {
                    if (icol != j)
                    {
                        float f = inverse[j, icol];
                        inverse[j, icol] = 0.0f;
                        for (int k = 0; k < 3; ++k)
                        {
                            inverse[j, k] -= inverse[icol, k] * f;
                        }
                    }
                }
            }

            for (int j = 2; j >= 0; --j)
            {
                int ir = rowIdx[j];
                int ic = colIdx[j];
                for (int k = 0; k < 3; ++k)
                {
                    float f = inverse[k, ir];
                    inverse[k, ir] = inverse[k, ic];
                    inverse[k, ic] = f;
                }
            }

            result.Row0.X = inverse[0, 0];
            result.Row0.Y = inverse[0, 1];
            result.Row0.Z = inverse[0, 2];
            result.Row1.X = inverse[1, 0];
            result.Row1.Y = inverse[1, 1];
            result.Row1.Z = inverse[1, 2];
            result.Row2.X = inverse[2, 0];
            result.Row2.Y = inverse[2, 1];
            result.Row2.Z = inverse[2, 2];
        }

        /// <summary>
        /// Calculate the inverse of the given matrix
        /// </summary>
        /// <param name="mat">The matrix to invert</param>
        /// <returns>The inverse of the given matrix if it has one, or the input if it is singular</returns>
        /// <exception cref="InvalidOperationException">Thrown if the Matrix4 is singular.</exception>
        public static Matrix3x3 Invert(Matrix3x3 mat)
        {
            Matrix3x3 result;
            Invert(in mat, out result);
            return result;
        }



        /// <summary>
        /// Calculate the transpose of the given matrix
        /// </summary>
        /// <param name="mat">The matrix to transpose</param>
        /// <returns>The transpose of the given matrix</returns>
        public static Matrix3x3 Transpose(Matrix3x3 mat)
        {
            return new Matrix3x3(mat.Column0, mat.Column1, mat.Column2);
        }

        /// <summary>
        /// Calculate the transpose of the given matrix
        /// </summary>
        /// <param name="mat">The matrix to transpose</param>
        /// <param name="result">The result of the calculation</param>
        public static void Transpose(in Matrix3x3 mat, out Matrix3x3 result)
        {
            result.Row0.X = mat.Row0.X;
            result.Row0.Y = mat.Row1.X;
            result.Row0.Z = mat.Row2.X;
            result.Row1.X = mat.Row0.Y;
            result.Row1.Y = mat.Row1.Y;
            result.Row1.Z = mat.Row2.Y;
            result.Row2.X = mat.Row0.Z;
            result.Row2.Y = mat.Row1.Z;
            result.Row2.Z = mat.Row2.Z;
        }




        /// <summary>
        /// Matrix multiplication
        /// </summary>
        /// <param name="left">left-hand operand</param>
        /// <param name="right">right-hand operand</param>
        /// <returns>A new Matrix3d which holds the result of the multiplication</returns>
        public static Matrix3x3 operator *(Matrix3x3 left, Matrix3x3 right)
        {
            return Matrix3x3.Multiply(left, right);
        }

        /// <summary>
        /// Compares two instances for equality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left equals right; false otherwise.</returns>
        public static bool operator ==(Matrix3x3 left, Matrix3x3 right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Compares two instances for inequality.
        /// </summary>
        /// <param name="left">The first instance.</param>
        /// <param name="right">The second instance.</param>
        /// <returns>True, if left does not equal right; false otherwise.</returns>
        public static bool operator !=(Matrix3x3 left, Matrix3x3 right)
        {
            return !left.Equals(right);
        }




        /// <summary>
        /// Returns a System.String that represents the current Matrix3d.
        /// </summary>
        /// <returns>The string representation of the matrix.</returns>
        public override string ToString()
        {
            return String.Format("{0}\n{1}\n{2}", Row0, Row1, Row2);
        }



        /// <summary>
        /// Returns the hashcode for this instance.
        /// </summary>
        /// <returns>A System.Int32 containing the unique hashcode for this instance.</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = this.Row0.GetHashCode();
                hashCode = (hashCode * 397) ^ this.Row1.GetHashCode();
                hashCode = (hashCode * 397) ^ this.Row2.GetHashCode();
                return hashCode;
            }
        }



        /// <summary>
        /// Indicates whether this instance and a specified object are equal.
        /// </summary>
        /// <param name="obj">The object to compare to.</param>
        /// <returns>True if the instances are equal; false otherwise.</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Matrix3x3))
            {
                return false;
            }

            return this.Equals((Matrix3x3)obj);
        }





        /// <summary>Indicates whether the current matrix is equal to another matrix.</summary>
        /// <param name="other">A matrix to compare with this matrix.</param>
        /// <returns>true if the current matrix is equal to the matrix parameter; otherwise, false.</returns>
        public bool Equals(Matrix3x3 other)
        {
            return
                Row0 == other.Row0 &&
                    Row1 == other.Row1 &&
                    Row2 == other.Row2;
        }

        #region BGE's additional Matrix3x3 operations

        public static Matrix3x3 CreateTranslation(Vector2 v)
        {
            var m = Matrix3x3.Identity;
            m.M13 = v.X;
            m.M23 = v.Y;
            return m;
        }

        public static Matrix3x3 ExtractScaleMatrix(in Matrix3x3 m)
        {
            var scale = new Vector3(m.Row0.X, m.Row1.Y, m.Row2.Z);
            return Matrix3x3.CreateScale(scale);
        }

        public static Vector2 Multiply(in Vector2 v, in Matrix3x3 m)
        {
            var x = v.X * m.M11 + v.Y * m.M12 + m.M13;
            var y = v.X * m.M21 + v.Y * m.M22 + m.M23;
            var z = v.X * m.M31 + v.Y * m.M32 + m.M33;
            return new Vector2(x / z, y / z);
        }

        public static Vector2 operator *(Vector2 v, Matrix3x3 m)
        {
            return Multiply(in v, in m);
        }

        #endregion
    }
}