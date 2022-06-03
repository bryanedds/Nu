// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Ecs
open System
open System.IO
open System.Runtime.InteropServices
open Prime
open Nu

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract Active : bool with get, set
        end

/// Allows a value to always pass as equal with another of its same type.
type [<CustomEquality; NoComparison; Struct>] 'a AlwaysEqual =
    | AlwaysEqual of 'a
    member this.Value = match this with AlwaysEqual value -> value
    override this.GetHashCode () = 0
    override this.Equals (that : obj) = that :? AlwaysEqual<'a>

/// The component that holds an entity's id.
type [<NoEquality; NoComparison; Struct>] EntityId =
    { mutable Active : bool
      mutable EntityId : uint64 }
    interface EntityId Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

/// Describes a means to store Ecs components.
type Store =
    interface
        abstract Length : int
        abstract Name : string
        abstract Item : int -> obj
        abstract SetItem : int -> obj -> unit
        abstract ZeroItem : int -> unit
        abstract Grow : unit -> unit
        abstract Read : int -> int -> FileStream -> unit
        end

/// Stores components for an Ecs.
type Store<'c when 'c : struct and 'c :> 'c Component> (name) =
    let mutable arr = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve
    member this.Length = arr.Length
    member this.Name = name
    member this.Item i = &arr.[i]
    member this.SetItem index comp = arr.[index] <- comp
    member this.ZeroItem index = arr.[index] <- Unchecked.defaultof<'c>
    member this.Grow () =
        let length = int (single (max arr.Length 2) * 1.5f)
        let arr' = Array.zeroCreate<'c> length
        Array.Copy (arr, arr', arr.Length)
        arr <- arr'
    member this.Read index count (stream : FileStream) =
        let compSize = sizeof<'c>
        let comp = Unchecked.defaultof<'c> :> obj
        let buffer = Array.zeroCreate<byte> compSize
        let gch = GCHandle.Alloc (comp, GCHandleType.Pinned)
        try 
            let mutable index = index
            for _ in 0 .. dec count do
                stream.Read (buffer, 0, compSize) |> ignore<int>
                Marshal.Copy (buffer, 0, gch.AddrOfPinnedObject (), compSize)
                if index = arr.Length then this.Grow ()
                arr.[index] <- comp :?> 'c
                index <- inc index
        finally gch.Free ()
    interface Store with
        member this.Length = this.Length
        member this.Name = this.Name
        member this.Item i = this.Item i :> obj
        member this.SetItem index compObj = this.SetItem index (compObj :?> 'c)
        member this.ZeroItem index = this.ZeroItem index
        member this.Grow () = this.Grow ()
        member this.Read index count stream = this.Read index count stream

/// A delegate for interfacing with Ecs components.
type Statement<'c, 's when
    'c : struct and 'c :> 'c Component> =
    delegate of 'c byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> =
    delegate of 'c byref * 'c2 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'c8 byref * 's -> 's

/// A delegate for interfacing with Ecs components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component and
    'c9 : struct and 'c9 :> 'c9 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'c8 byref * 'c9 byref * 's -> 's