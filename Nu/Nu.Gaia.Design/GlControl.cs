using System;
using OpenGL;
using SDL2;
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

        public void MakeContext()
        {
            sdlWindow = SDL.SDL_CreateWindowFrom(Handle);
            Gl.Initialize();
            glContext = SDL.SDL_GL_CreateContext(sdlWindow);
            SDL.SDL_GL_SetSwapInterval(1);
            Gl.BindAPI();
            var version = Gl.GetString(StringName.Version);
            Log.info("Initialized OpenGL " + version + ".");
            Hl.Assert(0);
        }

        public void DeleteContext()
        {
            SDL.SDL_GL_DeleteContext(glContext);
            SDL.SDL_DestroyWindow(sdlWindow);
        }

        public void Swap()
        {
            Invalidate();
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

        private nint glContext;
        private nint sdlWindow;
    }
}
