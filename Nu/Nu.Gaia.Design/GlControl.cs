using System;
using System.Diagnostics;
using OpenGL;
using SDL2;

namespace Nu.Gaia.Design
{
    public partial class GlControl : UserControl, WfglWindow
    {
        public unsafe GlControl()
        {
            // init
            InitializeComponent();

            // configure control properties
            SetStyle(ControlStyles.Selectable, true);
            DoubleBuffered = true;
            TabStop = true;

            // set pixel format
            hdc = Wgl.UnsafeNativeMethods.GetDC(Handle);
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
        }

        public bool OpenGlForwardCompatible { get; set; } = false;

        public bool OpenGlRobustAccess { get; set; } = false;

        public void CreateSdlWindowFrom()
        {
            // ensure only one window is created
            if (sdlWindow != IntPtr.Zero)
                throw new InvalidOperationException("SDL window already created.");

            // attempt to create window
            sdlWindow = SDL.SDL_CreateWindowFrom(Handle);

            // ensure window was created
            if (sdlWindow == IntPtr.Zero)
                throw new InvalidOperationException("Failed to create SDL window.");
        }

        public void CreateWfglContext()
        {
            // ensure sdl window exists
            if (sdlWindow == IntPtr.Zero)
                throw new InvalidOperationException("SDL window does not exist.");

            // ensure only one context is created
            if (glContext != IntPtr.Zero)
                throw new InvalidOperationException("Context already created.");

            // ensure that basic wgl extensions are available
            Trace.Assert(Wgl.CONTEXT_FLAGS_ARB == Glx.CONTEXT_FLAGS_ARB);
            Trace.Assert(Wgl.CONTEXT_MAJOR_VERSION_ARB == Glx.CONTEXT_MAJOR_VERSION_ARB);
            Trace.Assert(Wgl.CONTEXT_MINOR_VERSION_ARB == Glx.CONTEXT_MINOR_VERSION_ARB);
            Trace.Assert(Wgl.CONTEXT_PROFILE_MASK_ARB == Glx.CONTEXT_PROFILE_MASK_ARB);

            // create gl context
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
#pragma warning disable CS0162 // Unreachable code detected
                    Debug.Assert(Wgl.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB == Glx.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
                    contextProfile |= Wgl.CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
#pragma warning restore CS0162 // Unreachable code detected
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
                // make the context current and write out info to console
                // TODO: log instead of write to console?
                Wgl.MakeCurrent(hdc, glContext);
                var version = Gl.GetString(StringName.Version);
                Trace.WriteLine("Initialized OpenGL " + version + ".");
            }
            else
            {
                // failed to initialize gl
                throw new InvalidOperationException("Failed to create OpenGL context.");
            }
        }

        public void Swap()
        {
            // ensure context exists
            if (glContext == IntPtr.Zero)
                throw new InvalidOperationException("Context does not exist.");

            // ensure sdl window exists
            if (sdlWindow == IntPtr.Zero)
                throw new InvalidOperationException("SDL window does not exist.");

            // swap
            Wgl.UnsafeNativeMethods.GdiSwapBuffersFast(hdc);
        }

        public void CleanUp()
        {
            // delete context if it exists
            if (glContext != IntPtr.Zero)
                Wgl.DeleteContext(glContext);

            // delete window if it exists
            if (sdlWindow != IntPtr.Zero)
                SDL.SDL_DestroyWindow(sdlWindow);
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

        private nint hdc;
        private nint sdlWindow;
        private nint glContext;
    }
}
