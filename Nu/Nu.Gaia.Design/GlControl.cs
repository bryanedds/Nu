using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using OpenGL;
using Nu;

namespace Nu.Gaia.Design
{
    public partial class GlControl : UserControl, WfglWindow
    {
        public GlControl()
        {
            InitializeComponent();
            SetStyle(ControlStyles.Selectable, true);
            DoubleBuffered = true;
            TabStop = true;
        }

        public bool Valid => glContext != IntPtr.Zero;

        public bool OpenGlForwardCompatible { get; set; } = false;

        public bool OpenGlRobustAccess { get; set; } = false;

        public unsafe bool TryMakeContext()
        {
            // ensure only one context is created
            if (glContext != IntPtr.Zero)
                throw new InvalidOperationException("Context already created.");

            // ensure that basic wgl extensions are available
            Trace.Assert(Wgl.CONTEXT_FLAGS_ARB == Glx.CONTEXT_FLAGS_ARB);
            Trace.Assert(Wgl.CONTEXT_MAJOR_VERSION_ARB == Glx.CONTEXT_MAJOR_VERSION_ARB);
            Trace.Assert(Wgl.CONTEXT_MINOR_VERSION_ARB == Glx.CONTEXT_MINOR_VERSION_ARB);
            Trace.Assert(Wgl.CONTEXT_PROFILE_MASK_ARB == Glx.CONTEXT_PROFILE_MASK_ARB);

            // set pixel format
            hdc = GetDC(Handle);
            Wgl.PIXELFORMATDESCRIPTOR pfd = new Wgl.PIXELFORMATDESCRIPTOR();
            pfd.nSize = (short)sizeof(Wgl.PIXELFORMATDESCRIPTOR);
            pfd.nVersion = 1;
            pfd.dwFlags = (Wgl.PixelFormatDescriptorFlags)(Wgl.PFD_DRAW_TO_WINDOW | Wgl.PFD_SUPPORT_OPENGL | Wgl.PFD_DOUBLEBUFFER);
            pfd.iPixelType = Wgl.PFD_TYPE_RGBA;
            pfd.cColorBits = 32;
            pfd.cDepthBits = 24;
            pfd.cStencilBits = 8;
            int format = Wgl.ChoosePixelFormat(hdc, ref pfd);
            Wgl.SetPixelFormat(hdc, format, ref pfd);

            // create context
            if (Gl.PlatformExtensions.CreateContext_ARB)
            {
                // initialize gl context attributes
                List<int> attributes = new List<int>();
                attributes.AddRange(new int[]
                {
                    Wgl.CONTEXT_MAJOR_VERSION_ARB, Constants.OpenGl.VersionMajor,
                    Wgl.CONTEXT_MINOR_VERSION_ARB, Constants.OpenGl.VersionMinor
                });

                // initialize gl context profile
                uint contextProfile = 0;
                if (Constants.OpenGl.CoreProfile)
                {
                    Debug.Assert(Wgl.CONTEXT_CORE_PROFILE_BIT_ARB == Glx.CONTEXT_CORE_PROFILE_BIT_ARB);
                    contextProfile |= Wgl.CONTEXT_CORE_PROFILE_BIT_ARB;
                }
                else
                {
                    Debug.Assert(Wgl.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB == Glx.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
                    contextProfile |= Wgl.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
                }
                Debug.Assert(contextProfile == 0 || Gl.PlatformExtensions.CreateContextProfile_ARB);
                if (contextProfile != 0) attributes.AddRange(new int[] { Wgl.CONTEXT_PROFILE_MASK_ARB, unchecked((int)contextProfile) });
                attributes.Add(0);

                // initialize gl context flags
                uint contextFlags = 0;
                if (OpenGlForwardCompatible)
                {
                    Debug.Assert(Wgl.CONTEXT_FORWARD_COMPATIBLE_BIT_ARB == Glx.CONTEXT_FORWARD_COMPATIBLE_BIT_ARB);
                    contextFlags |= Wgl.CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
                }
                if (OpenGlRobustAccess)
                {
                    Debug.Assert(Wgl.CONTEXT_ROBUST_ACCESS_BIT_ARB == Glx.CONTEXT_ROBUST_ACCESS_BIT_ARB);
                    contextFlags |= Wgl.CONTEXT_ROBUST_ACCESS_BIT_ARB;
                }
#if DEBUG
                Debug.Assert(Wgl.CONTEXT_DEBUG_BIT_ARB == Glx.CONTEXT_DEBUG_BIT_ARB);
                contextFlags |= Wgl.CONTEXT_DEBUG_BIT_ARB;
#endif
                if (contextFlags != 0) attributes.AddRange(new int[] { Wgl.CONTEXT_FLAGS_ARB, unchecked((int)contextFlags) });

                // create specific gl context
                glContext = Wgl.CreateContextAttribsARB(hdc, IntPtr.Zero, attributes.ToArray());
            }
            else
            {
                // create gl compatibility context
                glContext = Wgl.CreateContext(hdc);
            }

            // ensure we've got a valid context
            if (glContext != IntPtr.Zero)
            {
                // make the context current and log success
                Wgl.MakeCurrent(hdc, glContext);
                var version = Gl.GetString(StringName.Version);
                //Log.info("Initialized OpenGL " + version + ".");
                return true;
            }
            else
            {
                // failed to initialize gl
                Trace.Assert(false, "Failed to create OpenGL context.");
                return false;
            }
        }

        public void DeleteContext()
        {
            // ensure context exists
            if (glContext == IntPtr.Zero)
                throw new InvalidOperationException("Context does not exist.");

            // delete
            Wgl.DeleteContext(glContext);
        }

        public void Swap()
        {
            // ensure context exists
            if (glContext == IntPtr.Zero)
                throw new InvalidOperationException("Context does not exist.");

            // swap
            wglSwapBuffers(hdc);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            Focus();
            base.OnMouseDown(e);
        }

        protected override bool IsInputKey(Keys keyData)
        {
            if (keyData == Keys.Up || keyData == Keys.Down) return true;
            if (keyData == Keys.Left || keyData == Keys.Right) return true;
            if (keyData == Keys.W || keyData == Keys.S) return true;
            if (keyData == Keys.A || keyData == Keys.D) return true;
            return base.IsInputKey(keyData);
        }

        [DllImport("opengl32.dll")]
        private static extern bool wglSwapBuffers(IntPtr hdc);

        [DllImport("user32.dll")]
        static extern IntPtr GetDC(IntPtr hwnd);

        private nint hdc;
        private nint glContext;
    }
}
