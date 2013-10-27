module NuEditLogic.Program
open NuEditDesign
open SDL2
open System
open System.Windows.Forms
open System.Threading
open Nu.Sdl
open Nu.Simulation

[<EntryPoint; STAThread>]
let main _ =
    use form = new NuEditForm ()
    let panel = form.displayPanel
    let sdlViewConfig = ExistingWindow panel.Handle
    let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig sdlViewConfig 900 600 sdlRenderFlags 1024
    run4
        (fun sdlDeps ->
            let result = Right (createEmptyWorld sdlDeps)
            form.Show ()
            result)
        (fun world -> (not form.IsDisposed, world))
        (fun world -> let _ = panel.Invalidate () in world)
        sdlConfig