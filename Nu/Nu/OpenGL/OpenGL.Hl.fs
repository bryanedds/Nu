// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu

/// Force qualification of OpenGL namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System.Runtime.InteropServices
open System.Text
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    let mutable private AssertEnabled =
        false

    /// Cached buffers.
    let private CachedBuffers = System.Collections.Generic.Queue ()

    /// Cached vertex arrays.
    let private CachedVertexArrays = System.Collections.Generic.Queue ()

    /// Initialize OpenGL.Hl.
    let InitAssert assertEnabled =
        AssertEnabled <- assertEnabled

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
        | DebugSeverity.DebugSeverityMedium
        | DebugSeverity.DebugSeverityHigh -> () // Log.debug messageStr
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
        ()
#endif

    /// Allocate a vertex array, generating one via OpenGL if no cached vertex array is available.
    let AllocVertexArray () =
        if CachedVertexArrays.Count <> 0
        then CachedVertexArrays.Dequeue ()
        else Gl.GenVertexArray ()

    /// Deallocate a vertex array into the vertex array cache.
    let FreeVertexArray vertexArray =
        CachedVertexArrays.Enqueue vertexArray

    /// Allocate a buffer, generating one via OpenGL if no cached buffer is available.
    let AllocBuffer () =
        if CachedBuffers.Count <> 0
        then CachedBuffers.Dequeue ()
        else Gl.GenBuffer ()

    /// Deallocate a buffer into the buffer cache.
    let FreeBuffer buffer =
        CachedBuffers.Enqueue buffer

    /// Create an SDL OpenGL context.
    let CreateSglContext window =
        Gl.Initialize ()
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ACCELERATED_VISUAL, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, Constants.Render.OpenGlVersionMajor) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, Constants.Render.OpenGlVersionMinor) |> ignore<int>
        if Constants.Render.OpenGlCore then
            SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE) |> ignore<int>
#if DEBUG
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, int SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG) |> ignore<int>
#endif
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        SDL.SDL_GL_SetSwapInterval 1 |> ignore<int>
        Gl.BindAPI ()
        let version = Gl.GetString StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        Assert ()
        glContext

    /// Begin an OpenGL frame.
    let BeginFrame (viewportOffset : Viewport) =

        // set viewport
        Gl.Viewport
            (viewportOffset.Bounds.Min.X,
             viewportOffset.Bounds.Min.Y,
             viewportOffset.Bounds.Size.X,
             viewportOffset.Bounds.Size.Y)
        Assert ()

        // bind buffer
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        Assert ()

        // clear inner viewport
        Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        Gl.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit)
        Assert ()

        // clear drawing target
        Gl.Enable EnableCap.ScissorTest
        Gl.Scissor
            (viewportOffset.Bounds.Min.X,
             viewportOffset.Bounds.Min.Y,
             viewportOffset.Bounds.Size.X,
             viewportOffset.Bounds.Size.Y)
        Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        Gl.Clear (ClearBufferMask.ColorBufferBit)
        Gl.Disable EnableCap.ScissorTest

    /// End an OpenGL frame.
    let EndFrame () =
        () // nothing to do