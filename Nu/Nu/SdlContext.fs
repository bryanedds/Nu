namespace Nu
open System.Runtime.InteropServices
open System.Reflection
open SDL2
open OpenTK
open OpenTK.Graphics
open OpenTK.Platform

type SdlWindowInfo (win : nativeint) =
    let mutable win = win
    interface IWindowInfo with
        member this.Dispose () = win <- nativeint 0
        member this.Handle = win

type [<AllowNullLiteral>] SdlGraphicsContext (gl : nativeint, win : nativeint) as this =

    // fields
    let mutable win = win
    let handle = ContextHandle win

    // static initialization
    static let addContext = typeof<GraphicsContext>.GetMethod ("AddContext", BindingFlags.NonPublic ||| BindingFlags.Static)
    static let remContext = typeof<GraphicsContext>.GetMethod ("RemoveContext", BindingFlags.NonPublic ||| BindingFlags.Static)
    static let mutable inited = false
    do  if not inited then
            inited <- true
            let get = GraphicsContext.GetCurrentContextDelegate (fun () -> ContextHandle gl)
            let field = typeof<GraphicsContext>.GetField ("GetCurrentContext", BindingFlags.NonPublic ||| BindingFlags.Static)
            field.SetValue (null, get)
        
    // instance initialization
    do addContext.Invoke (null, [|this :> obj|]) |> ignore

    // common load all function
    member this.LoadAll () =
        let m = typeof<OpenTK.Graphics.OpenGL4.GL>.GetMethod("LoadEntryPoints", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let gl = OpenTK.Graphics.OpenGL4.GL ()
        m.Invoke(gl, null) |> ignore
        let m = typeof<OpenTK.Graphics.OpenGL.GL>.GetMethod("LoadEntryPoints", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let gl = OpenTK.Graphics.OpenGL.GL ()
        m.Invoke(gl, null) |> ignore
    
    interface IGraphicsContext with

        member this.ErrorChecking
            with get () = false and set _ = ()

        member this.GraphicsMode =
            GraphicsMode.Default

        member this.IsCurrent =
            let glCurrent = SDL.SDL_GL_GetCurrentContext ()
            glCurrent = gl

        member this.IsDisposed =
            win = nativeint 0

        member this.SwapInterval
            with get () = 0
            and set _ = ()

        member this.LoadAll () =
            this.LoadAll ()

        member this.MakeCurrent (window : IWindowInfo) =
            if isNull window then
                if OpenTK.Graphics.GraphicsContext.CurrentContextHandle <> handle then failwith "overriding context"
                SDL.SDL_GL_MakeCurrent (nativeint 0, gl) |> ignore
            else
                let handle = OpenTK.Graphics.GraphicsContext.CurrentContextHandle
                if handle.Handle <> nativeint 0 then failwith "overriding context"
                SDL.SDL_GL_MakeCurrent (win, gl) |> ignore

        member this.SwapBuffers () =
            SDL.SDL_GL_SwapWindow win

        member this.Update (_: IWindowInfo) =
            // do nothing
            ()

        member this.Dispose () =
            remContext.Invoke (null, [|this :> obj|]) |> ignore
            win <- nativeint 0

    interface IGraphicsContextInternal with
        member this.Context = handle
        member this.GetAddress (name : string) = SDL.SDL_GL_GetProcAddress name
        member this.GetAddress (nameInt : nativeint) = SDL.SDL_GL_GetProcAddress (Marshal.PtrToStringAnsi nameInt)
        member this.Implementation = this :> _
        member this.LoadAll () = this.LoadAll ()