module NuEditLogic.Program
open NuEditDesign
open SDL2
open System
open System.Windows.Forms
open System.Threading

[<STAThread; EntryPoint>] // NOTE: do we need BOTH of these attributes?
let main _ =

    use form = new NuEditForm ()
    let panel = form.displayPanel

    ignore (SDL.SDL_Init SDL.SDL_INIT_EVERYTHING)
    let window = SDL.SDL_CreateWindowFrom panel.Handle
    let renderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let renderContext = SDL.SDL_CreateRenderer (window, -1, uint32 renderFlags)

    form.Paint.Add (fun args ->
        ignore (SDL.SDL_SetRenderDrawColor (renderContext, 0uy, 0uy, 179uy, 255uy))
        ignore (SDL.SDL_RenderClear renderContext)
        SDL.SDL_RenderPresent renderContext)

    Application.EnableVisualStyles ()
    Application.Run form
    0