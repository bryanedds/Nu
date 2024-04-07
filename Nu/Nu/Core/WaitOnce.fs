// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Threading
open Prime

/// Allow one thread to wait for another thread to proceed.
/// An ad-hoc synchronization primitive just used for making SDL.SDL_GL_MakeCurrent work in a threaded context -
/// https://stackoverflow.com/a/64484836
type WaitOnce () =
    let [<VolatileField>] mutable waiting = true
    member this.Wait () = while waiting do Thread.Sleep 1
    member this.Proceed () = waiting <- false