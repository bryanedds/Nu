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
    let mutable private DrawReportLock = obj ()
    let mutable private DrawCallCount = 0
    let mutable private DrawInstanceCount = 0

    /// Initialize OpenGL assertion mechanism.
    let InitAssert enabled =
        if not Initialized then
            AssertEnabled <- enabled
            Initialized <- true

    /// Assert a lack of Gl error. Has a generic parameter to enable value pass-through.
    /// Thread-safe after being initialized.
    let Assert (a : 'a) =
        if AssertEnabled then
            let error = Gl.GetError ()
            if error <> ErrorCode.NoError then
                Log.debug ("OpenGL assertion failed due to: " + string error)
        a

#if DEBUG
    let private DebugMessageListener (_ : DebugSource) (_ : DebugType) (_ : uint) (severity : DebugSeverity) (length : int) (message : nativeint) (_ : nativeint) =
        match severity with
        | DebugSeverity.DebugSeverityMedium
        | DebugSeverity.DebugSeverityHigh ->
            let messageBytes = Array.zeroCreate<byte> length
            Marshal.Copy (message, messageBytes, 0, length)
            let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
            Log.debug messageStr
        | DebugSeverity.DebugSeverityNotification
        | DebugSeverity.DebugSeverityLow
        | DebugSeverity.DontCare
        | _ -> ()

    let private DebugMessageProc =
        Gl.DebugProc DebugMessageListener

    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        Gl.Enable EnableCap.DebugOutput
        Gl.Enable EnableCap.DebugOutputSynchronous
        Gl.DebugMessageControl (DebugSource.DontCare, DebugType.DontCare, DebugSeverity.DontCare, [||], true)
        Gl.DebugMessageCallback (DebugMessageProc, nativeint 0)
#else
    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        () // nothing to do
#endif

    /// Create an SDL OpenGL context with the given window.
    let CreateSglContextInitial window =
        Gl.Initialize ()
        let glContext = SDL.SDL_GL_CreateContext window
        let swapInterval = if Constants.Render.Vsync then 1 else 0
        SDL.SDL_GL_SetSwapInterval swapInterval |> ignore<int>
        SDL.SDL_GL_MakeCurrent (window, glContext) |> ignore<int>
        Gl.BindAPI ()
        Assert ()
        let version = Gl.GetString StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        if  not (version.StartsWith "4.1") &&
            not (version.StartsWith "4.2") &&
            not (version.StartsWith "4.3") &&
            not (version.StartsWith "4.4") &&
            not (version.StartsWith "4.5") &&
            not (version.StartsWith "4.6") &&
            not (version.StartsWith "5.0") (* heaven forbid... *) then
            Log.trace "Failed to create OpenGL version 4.1 or higher. Install your system's latest graphics drivers and try again."
        glContext

    /// Create a SDL OpenGL context with the given window that shares the current context. Originating thread must wait
    /// on the given WaitOnce object before continuing processing.
    let CreateSglContextSharedWithCurrentContext (window, sharedContext) =
        SDL.SDL_GL_MakeCurrent (window, sharedContext) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1) |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        SDL.SDL_GL_MakeCurrent (window, glContext) |> ignore<int>
        Gl.BindAPI ()
        Assert ()
        glContext

    /// Initialize OpenGL context once created.
    let InitContext () =

        // listen to debug messages
        AttachDebugMessageCallback ()
        Assert ()
        
        // globally configure opengl for physically-based rendering
        Gl.Enable EnableCap.TextureCubeMapSeamless
        Assert ()

        // query extensions
        let mutable extensionsCount = 0
        let extensions = hashSetPlus StringComparer.Ordinal []
        Gl.GetInteger (GetPName.NumExtensions, &extensionsCount)
        for i in 0 .. dec extensionsCount do
            extensions.Add (Gl.GetString (StringName.Extensions, uint i)) |> ignore<bool>

        // assert that GL_ARB_bindless_texture is available
        if not (extensions.Contains "GL_ARB_bindless_texture") then
            Log.trace "Bindless textures required to run Nu."

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
            try try let pixelDataPtr = handle.AddrOfPinnedObject ()
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
                with exn -> Log.info (scstring exn)
            finally handle.Free ()

    /// Save the current bound framebuffer to an image file.
    /// Only works on Windows platforms for now.
    /// TODO: make this work on non-Windows platforms!
    let SaveFramebufferDepthToBitmap width height (filePath : string) =
        let pixelFloats = Array.zeroCreate<single> (width * height)
        let handle = GCHandle.Alloc (pixelFloats, GCHandleType.Pinned)
        try try let pixelDataPtr = handle.AddrOfPinnedObject ()
                Gl.ReadPixels (0, 0, width, height, PixelFormat.DepthComponent, PixelType.Float, pixelDataPtr)
                let pixelBytes = pixelFloats |> Array.map (fun depth -> [|0uy; 0uy; byte (depth * 255.0f); 255uy|]) |> Array.concat
                use bitmap = new Drawing.Bitmap (width, height, Drawing.Imaging.PixelFormat.Format32bppArgb)
                let bitmapData = bitmap.LockBits (Drawing.Rectangle (0, 0, width, height), Drawing.Imaging.ImageLockMode.WriteOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                Marshal.Copy (pixelBytes, 0, bitmapData.Scan0, pixelBytes.Length)
                bitmap.UnlockBits bitmapData
                bitmap.Save (filePath, Drawing.Imaging.ImageFormat.Bmp)
            with exn -> Log.info (scstring exn)
        finally handle.Free ()

    /// Report the fact that a draw call has just been made with the given number of instances.
    let ReportDrawCall drawInstances =
        lock DrawReportLock (fun () ->
            DrawCallCount <- inc DrawCallCount
            DrawInstanceCount <- DrawInstanceCount + drawInstances)

    /// Reset the running number of draw calls.
    let ResetDrawCalls () =
        lock DrawReportLock (fun () ->
            DrawCallCount <- 0
            DrawInstanceCount <- 0)

    /// Get the running number of draw calls.
    let GetDrawCallCount () =
        lock DrawReportLock (fun () -> DrawCallCount)

    /// Get the running number of draw calls.
    let GetDrawInstanceCount () =
        lock DrawReportLock (fun () -> DrawInstanceCount)