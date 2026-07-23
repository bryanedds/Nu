// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open SDL
open Prime

/// The window properties that can only be queried from SDL via the main thread.
type [<Struct>] WindowProperties =
    { WindowFlags : SDL_WindowFlags
      WindowProperties : SDL_PropertiesID
      WindowPixelDensity : single
      WindowWidth : int
      WindowHeight : int }

    /// Make a cacheable window properties record.
    static member make window =
        let mutable windowFlags = SDL3.SDL_GetWindowFlags window
        let mutable windowProperties = SDL3.SDL_GetWindowProperties window
        let mutable windowPixelDensity = SDL3.SDL_GetWindowPixelDensity window
        let mutable (windowWidth, windowHeight) = (0, 0)
        SDL3.SDL_GetWindowSizeInPixels (window, &&windowWidth, &&windowHeight) |> ignore<SDLBool>
        { WindowFlags = windowFlags
          WindowProperties = windowProperties
          WindowPixelDensity = windowPixelDensity
          WindowWidth = windowWidth
          WindowHeight = windowHeight }

    /// The empty window properties.
    static member val empty =
        Unchecked.defaultof<WindowProperties>