// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open FSharp.NativeInterop
open Prime

/// Abstraction for native pointer pinning for arrays.
type ArrayPin<'a when 'a : unmanaged> private (handle : Buffers.MemoryHandle, pointer : nativeptr<'a>) =

    let handle = handle
    let pointer = pointer

    new (array : 'a array) =
        let handle = array.AsMemory().Pin()
        let pointer = NativePtr.ofVoidPtr<'a> handle.Pointer
        new ArrayPin<'a> (handle, pointer)

    member this.Pointer = pointer

    interface IDisposable with
        member this.Dispose () =
            handle.Dispose ()