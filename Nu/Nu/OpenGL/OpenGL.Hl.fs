// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

/// Force qualification of OpenGL namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System
open System.Runtime.InteropServices
open System.Text
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    let mutable private Initialized = false
    let mutable private AssertEnabled = false

    /// Initialize OpenGL assertion mechanism.
    let Init assertEnabled =
        if not Initialized then
            AssertEnabled <- assertEnabled
            Initialized <- true

    /// Assert a lack of Gl error. Has an generic parameter to enable value pass-through.
    let Assert (a : 'a) =
        if AssertEnabled then
            let error = Gl.GetError ()
            if error <> ErrorCode.NoError then
                Log.debug ("OpenGL assertion failed due to: " + string error)
        a

#if DEBUG
    let private DebugMessageListener (_ : DebugSource) (_ : DebugType) (_ : uint) (severity : DebugSeverity) (length : int) (message : nativeint) (_ : nativeint) =
        let messageBytes = Array.zeroCreate<byte> length
        Marshal.Copy (message, messageBytes, 0, length)
        let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
        match severity with
        | DebugSeverity.DebugSeverityLow -> Log.info messageStr
        | DebugSeverity.DebugSeverityMedium -> Log.debug messageStr
        | DebugSeverity.DebugSeverityHigh -> Log.trace messageStr
        | DebugSeverity.DebugSeverityNotification
        | DebugSeverity.DontCare
        | _ -> ()

    let DebugMessageProc =
        Gl.DebugProc DebugMessageListener

    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        Gl.DebugMessageCallback (DebugMessageProc, nativeint 0)
#else
    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        () // nothing to do
#endif

    /// Create an SDL OpenGL context.
    let CreateSglContext window =
        Gl.Initialize ()
        let glContext = SDL.SDL_GL_CreateContext window
        let swapInterval = if Constants.Render.Vsync then 1 else 0
        SDL.SDL_GL_SetSwapInterval swapInterval |> ignore<int>
        Gl.BindAPI ()
        let version = Gl.GetString StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        Assert ()
        glContext

    /// Begin an OpenGL frame.
    let BeginFrame (viewportOffset : Viewport, windowSize : Vector2i) =

        // set viewport to window
        Gl.Viewport (0, 0, windowSize.X, windowSize.Y)
        Assert ()

        // bind buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        Assert ()

        // clear window to designated clear color
        Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        Gl.Clear ClearBufferMask.ColorBufferBit
        Assert ()

        // set viewport to offset
        Gl.Viewport (viewportOffset.Bounds.Min.X, viewportOffset.Bounds.Min.Y, viewportOffset.Bounds.Size.X, viewportOffset.Bounds.Size.Y)
        Assert ()

        // clear offset viewport to designated clear color
        Gl.Enable EnableCap.ScissorTest
        Gl.Scissor (viewportOffset.Bounds.Min.X, viewportOffset.Bounds.Min.Y, viewportOffset.Bounds.Size.X, viewportOffset.Bounds.Size.Y)
        Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        Gl.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit)
        Gl.Disable EnableCap.ScissorTest

    /// End an OpenGL frame.
    let EndFrame () =
        () // nothing to do

    /// Save the current bound RGBA framebuffer to an image file.
    /// Only works on Windows platforms for now.
    /// TODO: make this work on non-Windows platforms!
    let SaveFramebufferRgbaToBitmap width height (filePath : string) =
        let platform = Environment.OSVersion.Platform
        if platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
            let pixelFloats = Array.zeroCreate<single> (width * height * 4)
            let handle = GCHandle.Alloc (pixelFloats, GCHandleType.Pinned)
            try let pixelDataPtr = handle.AddrOfPinnedObject ()
                Gl.ReadPixels (0, 0, width, height, PixelFormat.Rgba, PixelType.Float, pixelDataPtr)
                use bitmap = new Drawing.Bitmap (width, height, Drawing.Imaging.PixelFormat.Format32bppArgb)
                let pixelBytes = Array.init pixelFloats.Length (fun i -> byte (pixelFloats.[i] * 255.0f))
                for i in 0 .. dec (pixelBytes.Length / 4) do // swap red and blue
                    let j = i * 4
                    let temp = pixelBytes.[j]
                    pixelBytes.[j] <- pixelBytes.[j+2]
                    pixelBytes.[j+2] <- temp
                let bitmapData = bitmap.LockBits (Drawing.Rectangle (0, 0, width, height), Drawing.Imaging.ImageLockMode.WriteOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                Marshal.Copy (pixelBytes, 0, bitmapData.Scan0, pixelBytes.Length)
                bitmap.UnlockBits bitmapData
                bitmap.Save (filePath, Drawing.Imaging.ImageFormat.Bmp)
            finally handle.Free ()

    /// Save the current bound framebuffer to an image file.
    /// Only works on Windows platforms for now.
    /// TODO: make this work on non-Windows platforms!
    let SaveFramebufferDepthToBitmap width height (filePath : string) =
        let pixelFloats = Array.zeroCreate<single> (width * height)
        let handle = GCHandle.Alloc (pixelFloats, GCHandleType.Pinned)
        try
            let pixelDataPtr = handle.AddrOfPinnedObject ()
            Gl.ReadPixels (0, 0, width, height, PixelFormat.DepthComponent, PixelType.Float, pixelDataPtr)
            let pixelBytes = pixelFloats |> Array.map (fun depth -> [|0uy; 0uy; byte (depth * 255.0f); 255uy|]) |> Array.concat
            use bitmap = new Drawing.Bitmap (width, height, Drawing.Imaging.PixelFormat.Format32bppArgb)
            let bitmapData = bitmap.LockBits (Drawing.Rectangle (0, 0, width, height), Drawing.Imaging.ImageLockMode.WriteOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
            Marshal.Copy (pixelBytes, 0, bitmapData.Scan0, pixelBytes.Length)
            bitmap.UnlockBits bitmapData
            bitmap.Save (filePath, Drawing.Imaging.ImageFormat.Bmp)
        finally
            handle.Free ()