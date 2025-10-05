// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace SDL2
open System
open System.Runtime.InteropServices

[<RequireQualifiedAccess>]
module SDL =

    // Missing in SDL2#? https://wiki.libsdl.org/SDL2/SDL_GetDefaultCursor?
    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint SDL_GetDefaultCursor ()