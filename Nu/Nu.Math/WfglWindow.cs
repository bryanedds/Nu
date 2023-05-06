using System;

namespace Nu
{
    /// <summary>
    /// OpenGl constants.
    /// NOTE: this really doesn't belong in Nu.Math, but it's better to put it in here than to make
    /// a dependecy for Nu.Gaia.Design on Nu.
    /// </summary>
    public static class Constants
    {
        public static class OpenGl
        {
            public const int VersionMajor = 4;
            public const int VersionMinor = 1;
            public const bool CoreProfile = true;
        }
    }

    /// <summary>
    /// An interface for specifying a Windows Forms control that uses OpenGL.
    /// NOTE: this really doesn't belong in Nu.Math, but it's better to put it in here than to make
    /// a dependecy for Nu.Gaia.Design on Nu.
    /// </summary>
    public interface WfglWindow
    {
        bool TryMakeContext();
        void DeleteContext();
        void Swap();
    }
}
