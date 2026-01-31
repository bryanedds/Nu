// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SDL2
open System
open System.Runtime.InteropServices

[<RequireQualifiedAccess>]
module SDL =

    // Missing in SDL2#? https://wiki.libsdl.org/SDL2/SDL_GetDefaultCursor?
    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint SDL_GetDefaultCursor ()