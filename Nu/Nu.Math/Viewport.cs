using System;
using System.Numerics;

namespace Nu
{
    /// <summary>
    /// Describes the bounds of a viewport.
    /// </summary>
    public struct Viewport
    {
        public float NearDistance;
        public float FarDistance;
        public Box2i Bounds;

        /// <summary>
        /// The aspect ratio.
        /// </summary>
        public float AspectRatio => (float)Bounds.Size.X / (float)Bounds.Size.Y;

        /// <summary>
        /// Construct a viewport.
        /// </summary>
        public Viewport(float nearDistance, float farDistance, Box2i bounds)
        {
            NearDistance = nearDistance;
            FarDistance = farDistance;
            Bounds = bounds;
        }

        /// <summary>
        /// Construct a viewport.
        /// </summary>
        public Viewport(float nearDistance, float farDistance, Vector2i position, Vector2i size) :
            this(nearDistance, farDistance, new Box2i(position, size))
        { }

        /// <summary>
        /// Project to the given frame.
        /// </summary>
        public Vector3 Project(Vector3 source, Matrix4x4 frame)
        {
            Vector3 vector = Vector3.Transform(source, frame);
            float a = source.X * frame.M14 + source.Y * frame.M24 + source.Z * frame.M34 + frame.M44;
            if (!WithinEpsilon(a, 1f))
            {
                vector.X /= a;
                vector.Y /= a;
                vector.Z /= a;
            }
            vector.X = (vector.X + 1f) * 0.5f * Bounds.Size.X + Bounds.Position.X;
            vector.Y = (-vector.Y + 1f) * 0.5f * Bounds.Size.Y + Bounds.Position.Y;
            vector.Z = vector.Z * (FarDistance - NearDistance) + NearDistance;
            return vector;
        }

        /// <summary>
        /// Unproject from the given frame.
        /// </summary>
        public Vector3 Unproject(Vector3 source, Matrix4x4 frame)
        {
            Matrix4x4.Invert(frame, out Matrix4x4 matrix);
            source.X = (source.X - Bounds.Position.X) / Bounds.Size.X * 2f - 1f;
            source.Y = -((source.Y - Bounds.Position.Y) / Bounds.Size.Y * 2f - 1f);
            source.Z = (source.Z - NearDistance) / (FarDistance - NearDistance);
            Vector3 vector = Vector3.Transform(source, matrix);
            float a = source.X * matrix.M14 + source.Y * matrix.M24 + source.Z * matrix.M34 + matrix.M44;
            if (!WithinEpsilon(a, 1f))
            {
                vector.X /= a;
                vector.Y /= a;
                vector.Z /= a;
            }
            return vector;
        }

        private static bool WithinEpsilon(float a, float b)
        {
            float c = a - b;
            return -float.Epsilon <= c && c <= float.Epsilon;
        }
    }
}
