// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Reflection
open System.Runtime.InteropServices
open FSharp.NativeInterop
open SDL
open Prime

/// SDL orientation.
type SdlOrientation =
    | LandscapeLeft
    | LandscapeRight
    | Portrait
    | PortraitUpsideDown

/// Describes the initial configuration of a window created via SDL.
type SdlWindowConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowFlags : SDL_WindowFlags }

    /// A default SdlWindowConfig.
    static member val defaultConfig =
        
        // NOTE: our use of SDL_WINDOW_HIGH_PIXEL_DENSITY only changes behavior on Apple iOS/macOS or Linux Wayland.
        // See https://wiki.libsdl.org/SDL3/README-highdpi
        let noNotificationBar = if OperatingSystem.IsIOS () || OperatingSystem.IsAndroid () then SDL_WindowFlags.SDL_WINDOW_FULLSCREEN else Unchecked.defaultof<_>
        { WindowTitle = "Nu Game"
          WindowX = int SDL3.SDL_WINDOWPOS_UNDEFINED
          WindowY = int SDL3.SDL_WINDOWPOS_UNDEFINED
          WindowFlags = SDL_WindowFlags.SDL_WINDOW_RESIZABLE ||| SDL_WindowFlags.SDL_WINDOW_VULKAN ||| SDL_WindowFlags.SDL_WINDOW_HIGH_PIXEL_DENSITY ||| noNotificationBar }

/// Describes the general configuration of SDL.
type [<ReferenceEquality>] SdlConfig =
    { Orientations : SdlOrientation Set
      WindowConfig : SdlWindowConfig }

    /// A default SdlConfig.
    static member val defaultConfig =
        { Orientations = Set.empty
          WindowConfig = SdlWindowConfig.defaultConfig }

    /// The SDL orientations as a string.
    member this.OrientationsStr =
        this.Orientations
        |> Seq.map scstring
        |> String.join " "

[<RequireQualifiedAccess>]
module SdlEvents =

    let private PolledEvents = Queue ()

    /// Accumulate SDL events. Necessary to call when you have a long-running process on the main thread to keep OS's
    /// like Windows from eco-hanging the application when it sees user input not getting processed in a timely
    /// fashion.
    let poll () =
        let mutable polledEvent = SDL_Event ()
        while (SDL3.SDL_PollEvent &&polledEvent : bool) do
            PolledEvents.Enqueue polledEvent

    /// Attempt to consume an SDL event. Usually only the engine should call this, but there might be cases where the
    /// user needs to utilize it to cancel a long-running process or something.
    let tryConsume (event : SDL_Event outref) =
        PolledEvents.TryDequeue &event

[<RequireQualifiedAccess>]
module SdlDeps =

    /// The dependencies needed to initialize SDL.
    type [<ReferenceEquality>] SdlDeps =
        private
            { WindowOpt : SDL_Window nativeptr option
              Config : SdlConfig
              Destroy : unit -> unit }
    
        interface IDisposable with
            member this.Dispose () =
                this.Destroy ()

    type private LogOutputCallback =
#nowarn 202
        [<UnmanagedCallersOnly (CallConvs = [|typeof<System.Runtime.CompilerServices.CallConvCdecl>|])>]
#warnon 202
        static member Invoke (_ : nativeint, category : int, priority : SDL_LogPriority, message : nativeptr<byte>) =
            let message = SDL3.PtrToStringUTF8 message
            let categoryStr = " (Category " + string category + ")"
            match priority with
            | SDL_LogPriority.SDL_LOG_PRIORITY_VERBOSE
            | SDL_LogPriority.SDL_LOG_PRIORITY_DEBUG
            | SDL_LogPriority.SDL_LOG_PRIORITY_INFO -> Log.info (message + categoryStr)
            | SDL_LogPriority.SDL_LOG_PRIORITY_WARN -> Log.warn (message + categoryStr)
            | SDL_LogPriority.SDL_LOG_PRIORITY_ERROR -> Log.error (message + categoryStr)
            | SDL_LogPriority.SDL_LOG_PRIORITY_CRITICAL -> Log.fail (message + categoryStr)
            | _ -> ()

    let private logOutputCallbackPtr = lazy (
        let logOutputMethod = typeof<LogOutputCallback>.GetMethod(nameof LogOutputCallback.Invoke, BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic).MethodHandle
        logOutputMethod.GetFunctionPointer ()) // requires UnmanagedCallersOnly on the function! See https://learn.microsoft.com/en-us/dotnet/api/system.runtimemethodhandle.getfunctionpointer#remarks

    let private sdlVersionToString version =
        string (SDL3.SDL_VERSIONNUM_MAJOR version) + "." +
        string (SDL3.SDL_VERSIONNUM_MINOR version) + "." +
        string (SDL3.SDL_VERSIONNUM_MICRO version)

    /// Attempt to initalize an SDL module.
    let private attemptPerformSdlInit create destroy =
        let initResult = create ()
        let error = SDL3.SDL_GetError ()
        if initResult
        then Right ((), destroy)
        else Left error

    /// Attempt to initalize an SDL resource.
    let private tryMakeSdlResource create destroy =
        let resource = create ()
        if NativePtr.isNullPtr resource
        then Left ("SDL3# resource creation failed due to '" + SDL3.SDL_GetError () + "'.")
        else Right (resource, destroy)

    /// Attempt to initalize a global SDL resource.
    let private tryMakeSdlGlobalResource create destroy =
        let resource : SDLBool = create ()
        if SDLBool.op_Implicit resource
        then Right ((), destroy)
        else Left ("SDL3# global resource creation failed due to '" + SDL3.SDL_GetError () + "'.")

    /// Get the display mode for the desktop occupied by the given window.
    let internal getDisplayModeInternal window =
        let display = SDL3.SDL_GetDisplayForWindow window
        let displayMode = SDL3.SDL_GetDesktopDisplayMode display
        if NativePtr.isNullPtr displayMode then
            Log.error ("Failed to get desktop display mode: " + SDL3.SDL_GetError ())
            Unchecked.defaultof<_>
        else NativePtr.read displayMode

    /// Get an sdlDep's optional window.
    let getWindowOpt sdlDeps =
        sdlDeps.WindowOpt

    /// Get an sdlDep's config.
    let getConfig sdlDeps =
        sdlDeps.Config

    /// Attempt to get the display mode for the desktop occupied by any current window.
    let tryGetDisplayMode sdlDeps =
        match sdlDeps.WindowOpt with
        | Some window -> Some (getDisplayModeInternal window)
        | None -> None

    /// Attempt to set the window's full screen state.
    let trySetWindowFullScreen fullScreen sdlDeps =
        match sdlDeps.WindowOpt with
        | Some window ->

            // get a snapshot of whether screen was full
            let mutable width, height = 0, 0
            SDL3.SDL_GetWindowSize (window, &&width, &&height) |> ignore<SDLBool>
            let displayMode = getDisplayModeInternal window
            let wasFullScreen = width = displayMode.w || height = displayMode.h

            // change full screen status via flags
            SDL3.SDL_SetWindowFullscreen (window, fullScreen) |> ignore<SDLBool>

            // when changing from full screen, set window to windowed size and make sure its title bar is visible
            if wasFullScreen && not fullScreen then
                let windowSizeWindowed = Constants.Render.DisplayVirtualResolution * 2
                SDL3.SDL_RestoreWindow window |> ignore<SDLBool>
                SDL3.SDL_SetWindowSize (window, windowSizeWindowed.X, windowSizeWindowed.Y) |> ignore<SDLBool>
                SDL3.SDL_SetWindowPosition (window, 100, 100) |> ignore<SDLBool>

        | None -> ()
        sdlDeps

    /// Attempt to show a desktop pre-splash screen on the given window from embedded SVG and color resources. This
    /// renders the pre-splash immediately using SDL's software surface API, before Vulkan takes over. The pre-splash
    /// render will be overwritten when Vulkan presents its first swapchain image.
    let private tryRenderPreSplash (window : SDL_Window nativeptr) =

        // attempt to utilize pre-splash graphics from manifest resource
        let assembly = Assembly.GetEntryAssembly ()
        let svgStreamOpt = assembly.GetManifestResourceStream "PreSplash"
        if notNull svgStreamOpt then

            // attempt to copy svg to memory
            use svgStream = svgStreamOpt
            let nativeMemory = NativeMemory.Alloc (unativeint svgStream.Length)
            try let nativeMemoryBytePtr = NativePtr.ofVoidPtr<byte> nativeMemory
                use nativeStream = new UnmanagedMemoryStream (nativeMemoryBytePtr, svgStream.Length, svgStream.Length, FileAccess.Write)
                svgStream.CopyTo nativeStream

                // attempt to copy memory to window surface with optional scaling
                let mutable (windowWidth, windowHeight) = (0, 0)
                SDL3.SDL_GetWindowSizeInPixels (window, &&windowWidth, &&windowHeight) |> ignore<SDLBool>
                let svgStream = SDL3.SDL_IOFromConstMem (NativePtr.toNativeInt nativeMemoryBytePtr, unativeint svgStream.Length)
                let surfacePtrOpt =
                    try if SDL3_image.IMG_isSVG svgStream |> SDLBool.op_Implicit
                        then SDL3_image.IMG_LoadSizedSVG_IO (svgStream, windowWidth, windowHeight) // maintains aspect ratio automatically
                        else
                            let rasterImage = SDL3_image.IMG_Load_IO (svgStream, true) // manually maintain aspect ratio for non-SVG images
                            try let rasterSurface = NativePtr.toByRef rasterImage
                                let (scaleX, scaleY) = (double windowWidth / double rasterSurface.w, double windowHeight / double rasterSurface.h)
                                let scale = min scaleX scaleY
                                let (newW, newH) = (int (double rasterSurface.w * scale), int (double rasterSurface.h * scale))
                                let scaledImage = SDL3.SDL_ScaleSurface (rasterImage, newW, newH, SDL_ScaleMode.SDL_SCALEMODE_LINEAR)
                                scaledImage
                            finally SDL3.SDL_DestroySurface rasterImage
                    finally SDL3.SDL_CloseIO svgStream |> ignore<SDLBool>

                // attempt to populate window background with pre-splash graphics
                try if NativePtr.notNullPtr surfacePtrOpt then

                        // attempt to fill window background with pre-splash color
                        let windowSurfacePtr = SDL3.SDL_GetWindowSurface window
                        let colorStreamOpt = assembly.GetManifestResourceStream "PreSplashColor"
                        if notNull colorStreamOpt then
                            use colorReader = new StreamReader (colorStreamOpt)
                            let colorStr = colorReader.ReadToEnd ()
                            let colorDrawing = Drawing.ColorTranslator.FromHtml colorStr // NOTE: this works fine starting on .NET Core 3.0.
                            let color = SDL3.SDL_MapSurfaceRGBA (windowSurfacePtr, byte colorDrawing.R, byte colorDrawing.G, byte colorDrawing.B, 255uy)
                            SDL3.SDL_FillSurfaceRect (windowSurfacePtr, NativePtr.nullPtr, color) |> ignore<SDLBool>

                        // display splash screen centered on window
                        let svgSurface = NativePtr.toByRef surfacePtrOpt
                        let mutable dstRect = SDL_Rect (x = (windowWidth - svgSurface.w) / 2, y = (windowHeight - svgSurface.h) / 2, w = svgSurface.w, h = svgSurface.h)
                        SDL3.SDL_BlitSurface (surfacePtrOpt, NativePtr.nullPtr, windowSurfacePtr, &&dstRect) |> ignore<SDLBool>
                        SDL3.SDL_UpdateWindowSurface window |> ignore<SDLBool>

                    // log issue
                    else Log.warn ("Failed to load splash SVG from embedded resource: " + SDL3.SDL_GetError ())

                // clean-up
                finally SDL3.SDL_DestroySurface surfacePtrOpt

            // clean-up
            finally NativeMemory.Free nativeMemory

    /// Attempt to make an SdlDeps instance.
    let tryMake (sdlConfig : SdlConfig) (accompanied : bool) (windowSize : Vector2i) =
        match attemptPerformSdlInit
            (fun () ->

                // setup SDL logging (use UnmanagedCallersOnly pointer for AOT compatibility)
                SDL3.SDL_SetLogOutputFunction (logOutputCallbackPtr.Value, 0n)

                // attempt to initialize sdl
                Log.info "Initializing SDL 3..."
                SDL3.SDL_SetHint (SDL3.SDL_HINT_WINDOWS_CLOSE_ON_ALT_F4, "0") |> ignore<SDLBool>
                SDL3.SDL_SetHint (SDL3.SDL_HINT_ORIENTATIONS, sdlConfig.OrientationsStr) |> ignore<SDLBool>
                let initConfig =
                    SDL_InitFlags.SDL_INIT_AUDIO |||
                    SDL_InitFlags.SDL_INIT_VIDEO |||
                    SDL_InitFlags.SDL_INIT_JOYSTICK |||
                    SDL_InitFlags.SDL_INIT_HAPTIC |||
                    SDL_InitFlags.SDL_INIT_GAMEPAD |||
                    SDL_InitFlags.SDL_INIT_EVENTS
                SDL3.SDL_Init initConfig)

            (fun () -> SDL3.SDL_Quit ()) with
        | Left error -> Left error
        | Right ((), destroy) ->
            match tryMakeSdlResource
                (fun () ->

                    // init sdl callback for app backgrounding on mobile devices
                    // NOTE: DJL: this happens before SDL window creation to ensure no backgrounding events are missed.
                    SDL3.SDL_SetEventFilter (Vulkan.Hl.backgroundingCallback (), 0n) // TODO: P0: pass this in as a parameter to reduce critical coupling.
                    
                    // attempt to create window
                    let windowConfig = sdlConfig.WindowConfig
                    let windowOpt = SDL3.SDL_CreateWindow (windowConfig.WindowTitle, windowSize.X, windowSize.Y, windowConfig.WindowFlags)
                    if NativePtr.notNullPtr windowOpt then

                        // set window position
                        let window = windowOpt
                        SDL3.SDL_SetWindowPosition (window, windowConfig.WindowX, windowConfig.WindowY) |> ignore<SDLBool>

                        // start text input except on platforms that would obscure the game with a virtual keyboard
                        if not (SDL3.SDL_HasScreenKeyboardSupport ()) then
                            SDL3.SDL_StartTextInput window |> ignore<SDLBool>

                        // set to full screen when window taking up entire screen and unaccompanied
                        let mutable displayMode = getDisplayModeInternal window
                        if (windowSize.X = displayMode.w || windowSize.Y = displayMode.h) && not accompanied then
                            SDL3.SDL_SetWindowFullscreen (window, true) |> ignore<SDLBool>

                        // attempt to show splash screen (software surface; will be overwritten by Vulkan)
                        tryRenderPreSplash window

                    // fin
                    windowOpt)

                (fun window -> SDL3.SDL_DestroyWindow window; destroy ()) with
            | Left error -> Left error
            | Right (window, destroy) ->
                match tryMakeSdlGlobalResource SDL3_ttf.TTF_Init (fun () -> SDL3_ttf.TTF_Quit (); destroy window) with
                | Left error -> Left error
                | Right ((), destroy) ->
                    match tryMakeSdlGlobalResource SDL3_mixer.MIX_Init (fun () -> SDL3_mixer.MIX_Quit (); destroy ()) with
                    | Left error -> Left error
                    | Right ((), destroy) ->
                        Log.info
                            ("Initialized SDL " + sdlVersionToString (SDL3.SDL_GetVersion ()) +
                             ", SDL_ttf " + sdlVersionToString (SDL3_ttf.TTF_Version ()) +
                             ", SDL_mixer " + sdlVersionToString (SDL3_mixer.MIX_Version ()) +
                             ", SDL_image " + sdlVersionToString (SDL3_image.IMG_Version ()) +
                             ".")
                        GamepadState.init ()
                        Right { WindowOpt = Some window; Config = sdlConfig; Destroy = destroy }

    /// An empty SdlDeps.
    let empty =
        { WindowOpt = None
          Config = SdlConfig.defaultConfig
          Destroy = id }

/// The dependencies needed to initialize SDL.
type SdlDeps = SdlDeps.SdlDeps