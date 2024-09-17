// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open FSharp.NativeInterop
open Prime

[<AutoOpen>]
module Native =

    /// Convert a managed string to an unmanaged string that must be manually freed.
    let private convertToUnmanaged (str : string) =
        let lengthPlus1 = Encoding.UTF8.GetByteCount str + 1
        let voidPtr = NativeMemory.Alloc (unativeint lengthPlus1)
        let span = Span<byte> (voidPtr, lengthPlus1)
        let readOnlySpan = str.AsSpan ()
        let length = Encoding.UTF8.GetBytes (readOnlySpan, span)
        span[length] <- byte 0
        NativePtr.ofVoidPtr<byte> voidPtr

    /// Convert an unmanaged string to a managed string.
    let private convertToManaged (ptr : nativeptr<byte>) =
        let readOnlySpan = MemoryMarshal.CreateReadOnlySpanFromNullTerminated ptr
        Encoding.UTF8.GetString readOnlySpan

    /// Free an unmanaged string.
    let private freeUnmanaged ptr =
        let voidPtr = NativePtr.toVoidPtr ptr
        NativeMemory.Free voidPtr
    
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

    /// Extract the string from a C# fixed-size buffer.
    let getBufferString fixedBuffer =
        let mutable fixedBuffer = fixedBuffer
        let ptr = asBytePointer &fixedBuffer
        convertToManaged ptr
    
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

    /// A container for a pinned array of unmanaged strings.
    type StringArrayWrap private (array : nativeptr<byte> array) =
    
        let array = array
        let pin = ArrayPin array
    
        new (strs : string array) =
            let ptrs = Array.zeroCreate<nativeptr<byte>> strs.Length
            for i in [0 .. dec strs.Length] do ptrs[i] <- convertToUnmanaged strs[i]
            new StringArrayWrap (ptrs)

        // TODO: see if implicit conversion can be used to remove the need to call this member directly.
        member this.Pointer = pin.Pointer

        // make disposal publicly available without casting
        member this.Dispose () =
            for i in [0 .. dec array.Length] do freeUnmanaged array[i]
            pin.Dispose ()
    
        interface IDisposable with
            member this.Dispose () =
                this.Dispose ()
    
    