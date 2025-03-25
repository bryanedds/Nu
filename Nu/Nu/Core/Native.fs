// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open FSharp.NativeInterop
open Prime

[<RequireQualifiedAccess>]
module NativePtr =

    /// Convert a managed pointer to a typed native pointer.
    let asPointer<'a when 'a : unmanaged> (managedPtr : byref<'a>) : nativeptr<'a> =
        let voidPtr = Unsafe.AsPointer<'a> &managedPtr
        NativePtr.ofVoidPtr<'a> voidPtr

    /// Get the byte offset of a field within the unmanaged form of a managed type.
    /// This is valid for any struct that does not contain non-blittable types like bool.
    let offsetOf<'a> fieldName =
        Marshal.OffsetOf (typeof<'a>, fieldName) |> int

    /// Reinterprets the type of a managed pointer from 'a to 'b in an unsafe manner.
    /// NOTE: Because of automatic dereferencing, the result must be *re*referenced after calling.
    let reinterpretRef<'a, 'b> (source : byref<'a>) : byref<'b> =
        &(Unsafe.As<'a, 'b> &source)

    /// Convert a managed string to an unmanaged string that must be manually freed.
    let stringToUnmanaged (str : string) =
        let lengthPlus1 = Encoding.UTF8.GetByteCount str + 1
        let voidPtr = NativeMemory.Alloc (unativeint lengthPlus1)
        let span = Span<byte> (voidPtr, lengthPlus1)
        let readOnlySpan = str.AsSpan ()
        let length = Encoding.UTF8.GetBytes (readOnlySpan, span)
        span.[length] <- byte 0
        NativePtr.ofVoidPtr<byte> voidPtr

    /// Convert an unmanaged string to a managed string.
    let unmanagedToString (ptr : nativeptr<byte>) =
        let readOnlySpan = MemoryMarshal.CreateReadOnlySpanFromNullTerminated ptr
        Encoding.UTF8.GetString readOnlySpan

    /// Free an unmanaged string.
    let freeUnmanagedString ptr =
        let voidPtr = NativePtr.toVoidPtr ptr
        NativeMemory.Free voidPtr

    /// Convert nativeint to void pointer.
    let nativeintToVoidPtr nativeint =
        let ptr = NativePtr.ofNativeInt<byte> nativeint
        NativePtr.toVoidPtr ptr

    /// Convert nativeint to byte pointer.
    let nativeintToBytePtr nativeint =
        NativePtr.ofNativeInt<byte> nativeint

    /// Convert a fixed-size buffer to a string.
    let fixedBufferToString fixedBuffer =
        let mutable fixedBuffer = fixedBuffer
        let voidPtr = Unsafe.AsPointer &fixedBuffer
        let ptr = NativePtr.ofVoidPtr<byte> voidPtr
        unmanagedToString ptr

    /// Convert a fixed-size buffer to an array of given length.
    /// TODO: DJL: this function is broken.
    let fixedBufferToArray<'a when 'a : unmanaged> length fixedBuffer =
        let mutable fixedBuffer = fixedBuffer
        let voidPtr = Unsafe.AsPointer &fixedBuffer
        let ptr = NativePtr.ofVoidPtr<'a> voidPtr
        let array = Array.zeroCreate<'a> length
        for i in 0 .. dec length do array.[i] <- NativePtr.get ptr i
        array

    /// Convert a ReadOnlySpan<byte> to a string.
    let spanToString (span : ReadOnlySpan<byte>) =
        Encoding.UTF8.GetString span

    /// Copy data of the given size from source to destination at the given offset.
    let memCopy offset size source dest =
        let sourcePtr = NativePtr.ofVoidPtr<byte> source
        let destPtr = NativePtr.ofVoidPtr<byte> dest
        let offsetPtr = NativePtr.add destPtr offset
        NativePtr.copyBlock offsetPtr sourcePtr size

[<AutoOpen>]
module NativePtrOperators =

    /// Null Pointer.
    let nullPtr =
        NativePtr.nullPtr

    /// Convert a managed pointer to a typed native pointer.
    let inline asPointer<'a when 'a : unmanaged> (managedPtr : byref<'a>) : nativeptr<'a> =
        NativePtr.asPointer<'a> &managedPtr

/// Abstraction for native pointer pinning for arrays.
type ArrayPin<'a when 'a : unmanaged> private (handle : Buffers.MemoryHandle, ptr : nativeptr<'a>) =

    /// Create an ArrayPin for a given array.
    new (array : 'a array) =
        let handle = array.AsMemory().Pin()
        let ptr = NativePtr.ofVoidPtr<'a> handle.Pointer
        new ArrayPin<'a> (handle, ptr)

    /// The native pointer to the pinned array.
    member this.Pointer = ptr

    /// The void pointer to the pinned array.
    member this.VoidPtr = NativePtr.toVoidPtr ptr

    /// The nativeint to the pinned array.
    member this.NativeInt = NativePtr.toNativeInt ptr

    interface IDisposable with
        member this.Dispose () =
            handle.Dispose ()

/// Container for a single unmanaged string.
type StringWrap private (strPtr : nativeptr<byte>) =

    /// Create a StringWrap for a given string.
    new (str : string) =
        let strPtr = NativePtr.stringToUnmanaged str
        new StringWrap (strPtr)

    /// The native pointer to the unmanaged string.
    member this.Pointer = strPtr

    interface IDisposable with
        member this.Dispose () =
            NativePtr.freeUnmanagedString strPtr

/// Container for a pinned array of unmanaged strings.
type StringArrayWrap private (array : nativeptr<byte> array) =

    let pin = new ArrayPin<_> (array)

    /// Create a StringArrayWrap for a given array of strings.
    new (strs : string array) =
        let ptrs = Array.zeroCreate<nativeptr<byte>> strs.Length
        for i in 0 .. dec strs.Length do ptrs.[i] <- NativePtr.stringToUnmanaged strs.[i]
        new StringArrayWrap (ptrs)

    /// The native pointer to the pinned array of unmanaged strings.
    member this.Pointer = pin.Pointer

    interface IDisposable with
        member this.Dispose () =
            for i in 0 .. dec array.Length do NativePtr.freeUnmanagedString array.[i]
            (pin :> IDisposable).Dispose ()