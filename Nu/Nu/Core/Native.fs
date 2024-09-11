// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Runtime.CompilerServices
open FSharp.NativeInterop

[<AutoOpen>]
module Native =

    /// Abstraction for native pointer pinning for arrays.
    type ArrayPin<'a when 'a : unmanaged> private (handle : Buffers.MemoryHandle, pointer : nativeptr<'a>) =
    
        let handle = handle
        let pointer = pointer
    
        new (array : 'a array) =
            let handle = array.AsMemory().Pin()
            let pointer = NativePtr.ofVoidPtr<'a> handle.Pointer
            new ArrayPin<'a> (handle, pointer)
    
        // TODO: see if implicit conversion can be used to remove the need to call this member directly.
        member this.Pointer = pointer

        // make disposal publicly available without casting
        member this.Dispose () = handle.Dispose ()
    
        interface IDisposable with
            member this.Dispose () =
                this.Dispose ()
    
    /// Null Pointer.
    let nullPtr = NativePtr.nullPtr
    
    /// Convert a managed pointer to a typed native pointer.
    let asPointer<'a when 'a : unmanaged> (managedPtr : byref<'a>) : nativeptr<'a> =
        let voidPtr = Unsafe.AsPointer &managedPtr
        NativePtr.ofVoidPtr<'a> voidPtr

    /// Derive a byte pointer from an arbitrary object.
    /// Used to access strings stored in C# fixed-size buffers, which are completely opaque in F#.
    let asBytePointer<'a> (managedPtr : byref<'a>) : nativeptr<byte> =
        let voidPtr = Unsafe.AsPointer &managedPtr
        NativePtr.ofVoidPtr<byte> voidPtr