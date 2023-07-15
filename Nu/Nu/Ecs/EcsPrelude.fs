// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Ecs
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FSharp.Quotations
open Prime
open Nu

/// Allows a value to always pass as equal with another of its same type.
/// TODO: move this elsewhere?
type [<CustomEquality; NoComparison; Struct>] 'a AlwaysEqual =
    | AlwaysEqual of 'a
    member this.Value = match this with AlwaysEqual value -> value
    override this.GetHashCode () = 0
    override this.Equals (that : obj) = that :? AlwaysEqual<'a>

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract Active : bool with get, set
        end

/// The component that holds an entity's id.
type [<StructuralEquality; NoComparison; Struct>] EntityId =
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
type Store<'c when 'c: struct and 'c :> 'c Component>(name) =
    let mutable arr = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve
    member this.Length = arr.Length
    member this.Name = name
    member this.Item i = &arr.[i]
    member this.SetItem index comp = arr.[index] <- comp
    member this.ZeroItem index = arr.[index] <- Unchecked.defaultof<'c>

    member this.Grow() =
        let length = int (single (max arr.Length 2) * 1.5f)
        let arr' = Array.zeroCreate<'c> length
        Array.Copy(arr, arr', arr.Length)
        arr <- arr'

    member this.Write(stream: FileStream) =
        let arr = Branchless.reinterpret arr
        for i = 0 to  this.Length * sizeof<'c> - 1 do
            let b = Unsafe.ReadUnaligned(&Unsafe.Add(&MemoryMarshal.GetArrayDataReference arr, i )) 
            stream.WriteByte b

        stream.Flush()
        stream.Close()
        
    member this.OpenWrite(stream: FileStream) =
        let ba: byte array = Unsafe.As &arr
        stream.Write ba

    member this.Read index count (stream: FileStream) =
        let compSize = sizeof<'c>
        let comp = Unchecked.defaultof<'c> :> obj
        let buffer = Array.zeroCreate<byte> compSize
        let gch = GCHandle.Alloc(comp, GCHandleType.Pinned)

        try
            let mutable index = index

            for _ in 0 .. dec count do
                stream.Read(buffer, 0, compSize) |> ignore<int>
                Marshal.Copy(buffer, 0, gch.AddrOfPinnedObject(), compSize)

                if index = arr.Length then
                    this.Grow()

                arr.[index] <- comp :?> 'c
                index <- inc index
        finally
            gch.Free()
                
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

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 's when
    'c : struct and 'c :> 'c Component> =
    delegate of 'c byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> =
    delegate of 'c byref * 'c2 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'c8 byref * 's -> unit

/// A delegate for interfacing with Ecs components.
type StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component and
    'c9 : struct and 'c9 :> 'c9 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'c8 byref * 'c9 byref * 's -> unit

and Term =
    | Error of string
    | Tag
    | Bool of bool
    | Int of int
    | Single of single
    | String of string
    | V3 of Vector3
    | Box3 of Box3
    | Cmp of IComparable
    | Obj of obj
    | Pair of Term * Term
    | EntityId of uint64
    | Intra of string * Type
    | Extra of string * Type * obj AlwaysEqual // only creates component when at top-level.
    | Terms of Term list
    static member equals (this : Term) (that : Term) = this.Equals that
    static member equalsMany (lefts : Map<string, Term>) (rights : Map<string, Term>) =
        if lefts.Count = rights.Count then
            let mutable result = true
            let mutable enr = (lefts :> IDictionary<_,_>).GetEnumerator ()
            while result && enr.MoveNext () do
                let current = enr.Current
                let termName = current.Key
                match rights.TryGetValue termName with
                | (true, term) -> result <- Term.equals current.Value term
                | (false, _) -> result <- false
            result
        else false

/// Exposes a Construct-Inspect-Compare mini-language for subqueries.
/// NOTE: Is NoEquality and NoComparison because I didn't feel like building a hash function. Could be tho.
and [<NoEquality; NoComparison>] Subquery =
    | V3Ctor of Subquery * Subquery * Subquery
    | Box3Ctor of Subquery * Subquery
    | PairCtor of Subquery * Subquery
    | EntityIdCtor of uint64
    | Tagged of Subquery
    | Eq of Subquery * Subquery
    | Gt of Subquery * Subquery
    | Ge of Subquery * Subquery
    | Lt of Subquery * Subquery
    | Le of Subquery * Subquery
    | If of Subquery * Subquery * Subquery
    | GetX of Subquery
    | GetY of Subquery
    | GetZ of Subquery
    | GetMin of Subquery
    | GetSize of Subquery
    | Intersects of Subquery * Subquery
    | Or of Subquery * Subquery
    | And of Subquery * Subquery
    | Not of Subquery
    | Val of Term
    | Var of string
    | Let of string * Subquery * Subquery
    | Fst of Subquery
    | Snd of Subquery
    | At of Subquery * Subquery
    | Head of Subquery
    | Tail of Subquery
    | Named of string * string
    | Typed of string * Type
    | Fun of (Map<string, Term> -> Term)
    | Subqueries of Subquery list

    static member eq term term2 =
        match (term, term2) with
        | (Bool b, Bool b2) -> Bool (b = b2)
        | (Int i, Int i2) -> Bool (i = i2)
        | (Single f, Single f2) -> Bool (f = f2)
        | (String str, String str2) -> Bool (strEq str str2)
        | (V3 v, V3 v2) -> Bool (v3Eq v v2)
        | (Box3 b, Box3 b2) -> Bool (box3Eq b b2)
        | (Cmp c, Cmp c2) -> Bool (c = c2)
        | (Obj o, Obj o2) -> Bool (objEq o o2)
        | (EntityId entityId, EntityId entityId2) -> Bool (genEq entityId entityId2)
        | (Pair (termFst, termSnd), Pair (termFst2, termSnd2)) ->
            match Subquery.eq termFst termFst2 with
            | Bool b ->
                if b then
                    match Subquery.eq termSnd termSnd2 with
                    | Bool _ as b -> b
                    | Error _ as err -> err
                    | _ -> failwithumf ()
                else Bool false
            | Error _ as err -> err
            | _ -> failwithumf ()
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length then
                let mutable errOpt = ValueNone
                let result = List.forall2 (fun term term2 -> match Subquery.eq term term2 with Bool b -> b | Error err -> (errOpt <- ValueSome err; false) | _ -> false) terms terms2
                match errOpt with ValueSome err -> Error err | ValueNone -> Bool result
            else Bool false
        | ((Error _ as err), _) -> err
        | (_, (Error _ as err)) -> err
        | _ -> Error "Checking equality on unlike terms."

    static member gt term term2 =
        match (term, term2) with
        | (Int i, Int i2) -> Bool (i > i2)
        | (Single f, Single f2) -> Bool (f > f2)
        | (String s, String s2) -> Bool (s > s2)
        | (Cmp c, Cmp c2) -> Bool (c.CompareTo c2 > 0)
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length then
                let mutable errOpt = ValueNone
                let result = List.forall2 (fun term term2 -> match Subquery.gt term term2 with Bool b -> b | Error err -> (errOpt <- ValueSome err; false) | _ -> false) terms terms2
                match errOpt with ValueSome err -> Error err | ValueNone -> Bool result
            else Bool false
        | ((Error _ as err), _) -> err
        | (_, (Error _ as err)) -> err
        | (_, _) -> Error "Comparing non-comparable terms."

    static member ge term term2 =
        match (term, term2) with
        | (Int i, Int i2) -> Bool (i >= i2)
        | (Single f, Single f2) -> Bool (f >= f2)
        | (String s, String s2) -> Bool (s >= s2)
        | (Cmp c, Cmp c2) -> Bool (c.CompareTo c2 >= 0)
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length then
                let mutable errOpt = ValueNone
                let result = List.forall2 (fun term term2 -> match Subquery.ge term term2 with Bool b -> b | Error err -> (errOpt <- ValueSome err; false) | _ -> false) terms terms2
                match errOpt with ValueSome err -> Error err | ValueNone -> Bool result
            else Bool false
        | ((Error _ as err), _) -> err
        | (_, (Error _ as err)) -> err
        | (_, _) -> Error "Comparing non-comparable terms."

    static member lt term term2 =
        match (term, term2) with
        | (Int i, Int i2) -> Bool (i < i2)
        | (Single f, Single f2) -> Bool (f < f2)
        | (String s, String s2) -> Bool (s < s2)
        | (Cmp c, Cmp c2) -> Bool (c.CompareTo c2 < 0)
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length then
                let mutable errOpt = ValueNone
                let result = List.forall2 (fun term term2 -> match Subquery.lt term term2 with Bool b -> b | Error err -> (errOpt <- ValueSome err; false) | _ -> false) terms terms2
                match errOpt with ValueSome err -> Error err | ValueNone -> Bool result
            else Bool false
        | ((Error _ as err), _) -> err
        | (_, (Error _ as err)) -> err
        | (_, _) -> Error "Comparing non-comparable terms."

    static member le term term2 =
        match (term, term2) with
        | (Int i, Int i2) -> Bool (i <= i2)
        | (Single f, Single f2) -> Bool (f <= f2)
        | (String s, String s2) -> Bool (s <= s2)
        | (Cmp c, Cmp c2) -> Bool (c.CompareTo c2 <= 0)
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length then
                let mutable errOpt = ValueNone
                let result = List.forall2 (fun term term2 -> match Subquery.le term term2 with Bool b -> b | Error err -> (errOpt <- ValueSome err; false) | _ -> false) terms terms2
                match errOpt with ValueSome err -> Error err | ValueNone -> Bool result
            else Bool false
        | ((Error _ as err), _) -> err
        | (_, (Error _ as err)) -> err
        | (_, _) -> Error "Comparing non-comparable terms."

    static member eval (terms : Map<string, Term>) (subquery : Subquery) : Term =
        match subquery with
        | V3Ctor (subquery, subquery2, subquery3) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2, Subquery.eval terms subquery3) with
            | (Single x, Single y, Single z) -> V3 (v3 x y z)
            | ((Error _ as err), _, _) -> err
            | (_, (Error _ as err), _) -> err
            | (_, _, (Error _ as err)) -> err
            | (_, _, _) -> Error "Invalid v3 call; 3 Singles required."
        | Box3Ctor (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | (V3 p, V3 s) -> Box3 (box3 p s)
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (_, _) -> Error "Invalid box3 call; 2 V3's required."
        | PairCtor (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (f, s) -> Pair (f, s)
        | EntityIdCtor entityId ->
            EntityId entityId
        | Tagged subquery ->
            match subquery with
            | Val v -> match v with Error _ as err -> err | _ -> Bool true
            | Var v -> Bool (terms.ContainsKey v)
            | _ -> Bool true
        | Eq (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (term, term2) -> Subquery.eq term term2
        | Gt (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (term, term2) -> Subquery.gt term term2
        | Ge (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (term, term2) -> Subquery.ge term term2
        | Lt (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (term, term2) -> Subquery.lt term term2
        | Le (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (term, term2) -> Subquery.le term term2
        | If (predicate, consequent, alternate) ->
            match Subquery.eval terms predicate with
            | Bool b -> if b then Subquery.eval terms consequent else Subquery.eval terms alternate
            | Error _ as err -> err
            | _ -> Error "Invalid If predicate; Bool required."
        | GetX subquery ->
            match Subquery.eval terms subquery with V3 v -> Single v.X | Error _ as err -> err | _ -> Error "Invalid GetX argument; V3 required."
        | GetY subquery ->
            match Subquery.eval terms subquery with V3 v -> Single v.Y | Error _ as err -> err | _ -> Error "Invalid GetY argument; V3 required."
        | GetZ subquery ->
            match Subquery.eval terms subquery with V3 v -> Single v.Z | Error _ as err -> err | _ -> Error "Invalid GetZ argument; V3 required."
        | GetMin subquery ->
            match Subquery.eval terms subquery with Box3 box -> V3 box.Min | Error _ as err -> err | _ -> Error "Invalid GetMin argument; Box3 required."
        | GetSize subquery ->
            match Subquery.eval terms subquery with Box3 box -> V3 box.Size | Error _ as err -> err | _ -> Error "Invalid GetSize argument; Box3 required."
        | Intersects (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | (Box3 box, Box3 box2) -> Bool (box.Intersects box2)
            | (V3 v, Box3 box) | (Box3 box, V3 v)-> Bool (box.Intersects v)
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (_, _) -> Error "Invalid Intersect arguments."
        | Or (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | (Bool b, Bool b2) -> Bool (b || b2)
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (_, _) -> Error "Invalid Or arguments; two Bools required."
        | And (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | (Bool b, Bool b2) -> Bool (b && b2)
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (_, _) -> Error "Invalid And arguments; two Bools required."
        | Not subquery ->
            match Subquery.eval terms subquery with
            | Bool b -> Bool (not b)
            | Error _ as err -> err
            | _ -> Error "Invalid Not argument; Bool required."
        | Val term ->
            term
        | Var varName ->
            match terms.TryGetValue varName with
            | (true, term) -> term
            | (false, _) -> Error "Non-existent binding."
        | Let (bindingName, subquery, subquery2) ->
            let term = Subquery.eval terms subquery
            let terms = Map.add bindingName term terms
            Subquery.eval terms subquery2
        | Fst subquery ->
            match Subquery.eval terms subquery with
            | Pair (termsFst, _) -> termsFst
            | Error _ as err -> err
            | _ -> Error "Invalid Fst argument; Pair required."
        | Snd subquery ->
            match Subquery.eval terms subquery with
            | Pair (_, termsSnd) -> termsSnd
            | Error _ as err -> err
            | _ -> Error "Invalid Fst argument; Pair required."
        | At (subquery, subquery2) ->
            match (Subquery.eval terms subquery, Subquery.eval terms subquery2) with
            | (Terms terms2, Int index) ->
                match Seq.tryItem index terms2 with
                | Some item -> item
                | None -> Bool false
            | ((Error _ as err), _) -> err
            | (_, (Error _ as err)) -> err
            | (_, _) -> Error "Invalid At arguments; Terms and Int required."
        | Head subquery ->
            match Subquery.eval terms subquery with
            | Terms terms2 ->
                match terms2 with
                | head :: _ -> head
                | _ -> Error "Invalid Head option; non-empty Terms required."
            | Error _ as err -> err
            | _ -> Error "Invalid Head argument; Terms required."
        | Tail subquery ->
            match Subquery.eval terms subquery with
            | Terms terms2 ->
                match terms2 with
                | _ :: tail -> Terms tail
                | _ -> Error "Invalid Tail option; non-empty Terms required."
            | Error _ as err -> err
            | _ -> Error "Invalid Tail argument; Terms required."
        | Named (termName, compName2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Intra (compName, _)  -> Bool (strEq compName compName2)
                | Extra (compName, _, _)  -> Bool (strEq compName compName2)
                | Error _ as err -> err
                | _ -> Error "Invalid Named argument; Intra or Extra required."
            | (false, _) -> Error "Non-existent term."
        | Typed (termName, ty2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Intra (_, ty)  -> Bool (refEq ty ty2)
                | Extra (_, ty, _) -> Bool (refEq ty ty2)
                | Error _ as err -> err
                | _ -> Error "Invalid Typed argument; Intra or Extra required."
            | (false, _) -> Error "Non-existent term."
        | Fun fn ->
            fn terms
        | Subqueries subqueries ->
            Terms (List.map (Subquery.eval terms) subqueries)

    static member evalMany (terms : Map<string, Term>) (subqueries : Subquery seq) =
        let mutable result = true
        let mutable subqueryEnr = subqueries.GetEnumerator ()
        while result && subqueryEnr.MoveNext () do
            let subquery = subqueryEnr.Current
            match Subquery.eval terms subquery with Bool true -> () | _ -> result <- false
        result

    static member unquote (quote : Expr) =
        match quote with
        | Patterns.Var var ->
            Var var.Name
        | Patterns.Value (value, ty) ->
            if ty = typeof<bool> then Val (Bool (value :?> bool))
            elif ty = typeof<int> then Val (Int (value :?> int))
            elif ty = typeof<single> then Val (Single (value :?> single))
            elif ty = typeof<Map<string, Term> -> Term> then Fun (value :?> (Map<string, Term> -> Term))
            else failwith "Unsupported Val or Fun type."
        | Patterns.Let (var, q, q2) -> 
            Let (var.Name, Subquery.unquote q, Subquery.unquote q2)
        | Patterns.IfThenElse (predicate, consequent, alternative) ->
            If (Subquery.unquote predicate, Subquery.unquote consequent, Subquery.unquote alternative)
        | Patterns.PropertyGet (None, info, args) ->
            match (info.Name, args) with
            | ("v3Zero", []) -> Val (V3 v3Zero)
            | ("v3One", []) -> Val (V3 v3One)
            | ("box3Zero", []) -> Val (Box3 (box3 v3Zero v3Zero))
            | ("box3One", []) -> Val (Box3 (box3 v3Zero v3One))
            | _ -> failwith "Unsupported call."
        | Patterns.PropertyGet (Some target, info, args) ->
            match (info.Name, args) with
            | ("Item", [arg]) -> At (Subquery.unquote arg, Subquery.unquote target)
            | _ -> failwith "Unsupported call."
        | Patterns.FieldGet (Some target, info) ->
            match info.Name with
            | "X" -> GetX (Subquery.unquote target)
            | "Y" -> GetY (Subquery.unquote target)
            | "Z" -> GetZ (Subquery.unquote target)
            | "Min" -> GetMin (Subquery.unquote target)
            | "Size" -> GetSize (Subquery.unquote target)
            | _ -> failwith "Unsupported index."
        | Patterns.NewTuple args ->
            match args with
            | [arg; arg2] -> PairCtor (Subquery.unquote arg, Subquery.unquote arg2)
            | _ -> failwith "Invalid Pair arguments; 2 Terms required (only 2-tuples are supported)."
        | Patterns.NewUnionCase (info, items) ->
            match info.Name with
            | "Cons" ->
                let subqueries = List ()
                let mutable items = items
                let mutable going = true
                while going do
                    match items with
                    | [expr; Patterns.NewUnionCase (info, items2)] when info.Name = "Cons" ->
                        subqueries.Add (Subquery.unquote expr)
                        items <- items2
                    | [expr; _] ->
                        subqueries.Add (Subquery.unquote expr)
                        going <- false
                    | _ -> failwithumf ()
                Subqueries (List.ofSeq subqueries)
            | _ -> failwith "Unsupported case expression."
        | Patterns.Call (_, info, args) ->
            match (info.Name, args) with
            | ("tagged", [arg]) -> Tagged (Subquery.unquote arg)
            | ("v3", [arg; arg2; arg3]) -> V3Ctor (Subquery.unquote arg, Subquery.unquote arg2, Subquery.unquote arg3)
            | ("box3", [arg; arg2]) -> Box3Ctor (Subquery.unquote arg, Subquery.unquote arg2)
            | ("entityId", [arg]) -> EntityIdCtor (match arg with Patterns.Value (:? uint64 as entityId, _) -> entityId | _ -> failwith "Invalid entityId; uint64 required.")
            | ("fst", [arg]) -> Fst (Subquery.unquote arg)
            | ("snd", [arg]) -> Snd (Subquery.unquote arg)
            | ("at", [arg; arg2]) -> At (Subquery.unquote arg, Subquery.unquote arg2)
            | ("head", [arg]) -> Head (Subquery.unquote arg)
            | ("tail", [arg]) -> Tail (Subquery.unquote arg)
            | ("Not", [arg]) -> Not (Subquery.unquote arg)
            | ("intersects", [arg; arg2]) -> Intersects (Subquery.unquote arg, Subquery.unquote arg2)
            | ("op_Equality", [arg; arg2]) -> Eq (Subquery.unquote arg, Subquery.unquote arg2)
            | ("op_LessThan", [arg; arg2]) -> Lt (Subquery.unquote arg, Subquery.unquote arg2)
            | ("op_LessThanOrEqual", [arg; arg2]) -> Le (Subquery.unquote arg, Subquery.unquote arg2)
            | ("op_GreaterThan", [arg; arg2]) -> Gt (Subquery.unquote arg, Subquery.unquote arg2)
            | ("op_GreaterThanOrEqual", [arg; arg2]) -> Ge (Subquery.unquote arg, Subquery.unquote arg2)
            | ("named", [arg; arg2]) ->
                match (arg, arg2) with
                | (Patterns.Value (:? string as varName, _), Patterns.Value (:? string as compName, _)) -> Named (varName, compName)
                | (_, _) -> failwith "Invalid IsType arguments."
            | ("typed", [arg]) ->
                match (arg, info.GetGenericArguments ()) with
                | (Patterns.Value (:? string as varName, _), [|ty|]) -> Typed (varName, ty)
                | (_, _) -> failwith "Invalid IsType arguments."
            | _ -> failwith "Unsupported call."
        | _ -> failwith "Unsupport Subquery expression."

[<AutoOpen>]
module Subquery =

    [<AutoOpen>]
    module Ops =

        let entityId (_ : uint64) = Unchecked.defaultof<'a>
        let tagged (_ : 'a) = Unchecked.defaultof<bool>
        let at (_ : 'a array) (_ : 'b) = Unchecked.defaultof<'a>
        let head (_ : 'a array) = Unchecked.defaultof<'a>
        let tail (_ : 'a array) = Unchecked.defaultof<'a array>
        let intersects (_ : 'a) (_ : 'a) = Unchecked.defaultof<bool>
        let named (_ : string) = Unchecked.defaultof<bool>
        let typed<'c when 'c : struct and 'c :> 'c Component> (_ : string) = Unchecked.defaultof<bool>

[<RequireQualifiedAccess>]
module Fun =
    
    type Type =
        Map<string, Term> -> Term

    let make (fn : Map<string, Term> -> Term) =
        Expr.Value (fn, typeof<Map<string, Term> -> Term>)
