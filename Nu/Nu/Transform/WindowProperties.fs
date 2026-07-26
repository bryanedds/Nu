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
      PropertiesHandle : SDL_PropertiesID
      WidthPixels : int
      HeightPixels : int
      PixelDensity : single }

    /// Make a cacheable window properties record.
    static member make window =
        let windowFlags = SDL3.SDL_GetWindowFlags window
        let propertiesHandle = SDL3.SDL_GetWindowProperties window
        let mutable (widthPixels, heightPixels) = (0, 0)
        SDL3.SDL_GetWindowSizeInPixels (window, &&widthPixels, &&heightPixels) |> ignore<SDLBool>
        let pixelDensity = SDL3.SDL_GetWindowPixelDensity window
        { WindowFlags = windowFlags
          PropertiesHandle = propertiesHandle
          WidthPixels = widthPixels
          HeightPixels = heightPixels
          PixelDensity = pixelDensity }

    /// The empty window properties.
    static member val empty =
        Unchecked.defaultof<WindowProperties>