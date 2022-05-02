using System;
using System.Numerics;
using System.Runtime.InteropServices;

#if USE_NUMERICS
using System.Numerics;
#endif

#if MEMORY_LOGGER
using System.Collections.Generic;
#endif

namespace Nu
{
    /// <summary>
    /// Some helper shortcuts.
    /// </summary>
    partial class Gl
    {
        #region Private Fields
        private static int version = 0;
        private static int versionMinor = 0;
        private static uint currentProgram = 0;
        #endregion

        /// <summary>
        /// Gets the current major OpenGL version (returns a cached result on subsequent calls).
        /// </summary>
        /// <returns>The current major OpenGL version, or 0 on an error.</returns>
        public static int Version()
        {
            if (version != 0) return version; // cache the version information

            try
            {
                string versionString = Gl.GetString(StringName.Version);

                version = int.Parse(versionString.Substring(0, versionString.IndexOf('.')));
                return Gl.version;
            }
            catch (Exception)
            {
                //Console.WriteLine("Error while retrieving the OpenGL version.");
                return 0;
            }
        }

        /// <summary>
        /// Gets the current minor OpenGL version (returns a cached result on subsequent calls).
        /// </summary>
        /// <returns>The current minor OpenGL version, or -1 on an error.</returns>
        public static int VersionMinor()
        {
            if (versionMinor != 0) return versionMinor; // cache the version information

            try
            {
                string versionString = Gl.GetString(StringName.Version);

                versionMinor = int.Parse(versionString.Split('.')[1]);
                return Gl.versionMinor;
            }
            catch (Exception)
            {
                //Console.WriteLine("Error while retrieving the OpenGL version.");
                return -1;
            }
        }
    }
}
