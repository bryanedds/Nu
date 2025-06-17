// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
                Log.error ("OpenGL assertion failed due to: " + string error)
        a

    let private DebugMessageListener (_ : DebugSource) (_ : DebugType) (_ : uint) (severity : DebugSeverity) (length : int) (message : nativeint) (_ : nativeint) =
        match severity with
        | DebugSeverity.DebugSeverityMedium ->
            let messageBytes = Array.zeroCreate<byte> length
            Marshal.Copy (message, messageBytes, 0, length)
            let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
            Log.warn messageStr
        | DebugSeverity.DebugSeverityHigh ->
            let messageBytes = Array.zeroCreate<byte> length
            Marshal.Copy (message, messageBytes, 0, length)
            let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
            Log.error messageStr
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

    /// Create an SDL OpenGL context with the given window.
    let CreateSglContextInitial window =
        Gl.Initialize ()
        let glContext = SDL.SDL_GL_CreateContext window
        let swapInterval = if Constants.Render.Vsync then 1 else 0
        SDL.SDL_GL_SetSwapInterval swapInterval |> ignore<int>
        if SDL.SDL_GL_MakeCurrent (window, glContext) <> 0 then Log.error "Could not make OpenGL context current when required."
        Gl.BindAPI ()
        let versionStr = Gl.GetString StringName.Version
        Log.info ("Initialized OpenGL " + versionStr + ".")
        if  not (versionStr.StartsWith "4.6") &&
            not (versionStr.StartsWith "5.0") (* heaven forbid... *) then
            Log.fail "Failed to create OpenGL version 4.6 or higher. Install your system's latest graphics drivers and try again."
        let vendorName = Gl.GetString StringName.Vendor
        let glFinishRequired =
            Constants.Render.VendorNamesExceptedFromSwapGlFinishRequirement |>
            List.notExists (fun vendorName2 -> String.Equals (vendorName, vendorName2, StringComparison.InvariantCultureIgnoreCase))
        if glFinishRequired then Log.warn "Requirement to call 'glFinish' before swapping is detected on current hardware. This will likely reduce rendering performance."
        (glFinishRequired, glContext)

    /// Create a SDL OpenGL context with the given window that shares the current context. Originating thread must wait
    /// on the given WaitOnce object before continuing processing.
    let CreateSglContextSharedWithCurrentContext (window, sharedContext) =
        if SDL.SDL_GL_MakeCurrent (window, sharedContext) <> 0 then Log.error "Could not make OpenGL context current when required."
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1) |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        if SDL.SDL_GL_MakeCurrent (window, glContext) <> 0 then Log.error "Could not make OpenGL context current when required."
        Gl.BindAPI ()
        glContext

    /// Delete an SDL-created OpenGL context.
    let DestroySglContext (glContext, sglWindow) =
        if SDL.SDL_GL_MakeCurrent (sglWindow, IntPtr.Zero) <> 0 then Log.error "Could not clear OpenGL context current when desired."
        SDL.SDL_GL_DeleteContext glContext

    /// Initialize OpenGL context once created.
    let InitContext attach =

        // potentially listen to debug messages
        if attach then AttachDebugMessageCallback ()
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

        // assert that anisotropic texture filter is available
        if not (extensions.Contains "GL_ARB_texture_filter_anisotropic") then
            Log.warn "Anisotropic texture filtering required to properly run Nu."

        // assert that at least 32 texture units are available
        let mutable imageUnits = 0
        Gl.GetInteger (GetPName.MaxTextureImageUnits, &imageUnits)
        if imageUnits < Constants.OpenGL.TextureImageUnitsRequired then
            Log.fail ("Max texture image units too low to run Nu (" + string imageUnits + " available but " + string Constants.OpenGL.TextureImageUnitsRequired + " required).")

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

    /// Begin an OpenGL frame.
    let BeginFrame (windowSize : Vector2i, outerBounds : Box2i) =

        // reset draw call counts
        ResetDrawCalls ()        

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

        // set viewport to offset bounds
        Gl.Viewport (outerBounds.Min.X, outerBounds.Min.Y, outerBounds.Size.X, outerBounds.Size.Y)
        Assert ()

        // clear outer viewport to designated clear color
        Gl.Enable EnableCap.ScissorTest
        Gl.Scissor (outerBounds.Min.X, outerBounds.Min.Y, outerBounds.Size.X, outerBounds.Size.Y)
        Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        Gl.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit)
        Gl.Disable EnableCap.ScissorTest

    /// End an OpenGL frame.
    let EndFrame () =
#if DEBUG
        match OpenGL.Gl.GetGraphicsResetStatus () with
        | OpenGL.GraphicsResetStatus.NoError -> ()
        | status -> Log.fail ("Unexpected OpenGL graphics reset (GraphicResetStatus = " + string status + ").")
#else
        ()
#endif

    /// Save the current bound RGBA framebuffer to an image file.
    /// Only works on Windows platforms for now.
    /// TODO: make this work on non-Windows platforms!
    let SaveFramebufferRgbaToBitmap (width, height, filePath : string) =
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