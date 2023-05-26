// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime
open Nu

/// The data for a change in a simulant.
type ChangeData =
    { Name : string
      Previous : obj
      Value : obj }

/// A generalized simulant lens.
type 'w Lens =
    interface
        abstract Name : string
        abstract This : Simulant
        abstract Get : 'w -> obj
        abstract SetOpt : (obj -> 'w -> 'w) voption
        abstract TrySet : obj -> 'w -> 'w
        abstract ChangeEvent : ChangeData Address
        abstract Type : Type
        end

/// Describes a property of a simulant.
/// Similar to a Haskell lens, but specialized to simulant properties.
type [<ReferenceEquality>] Lens<'a, 's, 'w when 's :> Simulant> =
    { Name : string
      This : 's
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) voption }

    interface 'w Lens with
        member this.Name = this.Name
        member this.This = this.This :> Simulant
        member this.Get world = this.Get world :> obj
        member this.SetOpt = ValueOption.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.TrySet value world = match this.SetOpt with ValueSome set -> set (value :?> 'a) world | ValueNone -> world
        member this.ChangeEvent = this.ChangeEvent
        member this.Type = typeof<'a>

    member this.GetBy by world =
        by (this.Get world)

    member this.GetByWorld by world =
        by (this.Get world) world

    member this.TrySet value world =
        match this.SetOpt with
        | ValueSome setter -> (true, setter value world)
        | ValueNone -> (false, world)

    member this.Set value world =
        match this.TrySet value world with
        | (true, world) -> world
        | (false, _) -> failwith ("Lens for '" + this.Name + "' is readonly.")

    member this.TryUpdateWorld (updater : 'a -> 'w -> 'a) world =
        let value = this.Get world
        let value' = updater value world
        this.TrySet value' world

    member this.TryUpdateEffect (updater : 'a -> 'w -> ('a * 'w)) (world : 'w) =
        let value = this.Get world
        let (value', world) = updater value world
        this.TrySet value' world

    member this.TryUpdate (updater : 'a -> 'a) world =
        this.TryUpdateWorld (fun value _ -> updater value) world

    member this.UpdateEffect updater world =
        match this.TryUpdateEffect updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.UpdateWorld updater world =
        match this.TryUpdateWorld updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.Update updater world =
        match this.TryUpdate updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.ChangeEvent : ChangeData Address =
        let names = [|Constants.Lens.ChangeName; this.Name; Constants.Lens.EventName|]
        match box this.This with
        | null ->
            // HACK: this case is a hack to allow Nu to resolve events contextually.
            let hashCode = Constants.Lens.ChangeNameHash ^^^ hash this.Name ^^^ Constants.Lens.EventNameHash
            let changeEventAddress = { Names = names; HashCode = hashCode; Anonymous = true }
            changeEventAddress 
        | _ -> rtoa names --> this.This.SimulantAddress

    member inline this.Type =
        typeof<'a>

    (* Lensing Operators *)
    static member inline ( += ) (lens : Lens<_, _, 'w>, value) =  lens.Update (flip (+) value)
    static member inline ( -= ) (lens : Lens<_, _, 'w>, value) =  lens.Update (flip (-) value)
    static member inline ( *= ) (lens : Lens<_, _, 'w>, value) =  lens.Update (flip (*) value)
    static member inline ( /= ) (lens : Lens<_, _, 'w>, value) =  lens.Update (flip (/) value)
    static member inline ( %= ) (lens : Lens<_, _, 'w>, value) =  lens.Update (flip (%) value)
    static member inline ( ~+ ) (lens : Lens<_, _, 'w>) =         lens.Update (~+)
    static member inline ( ~- ) (lens : Lens<_, _, 'w>) =         lens.Update (~-)
    static member inline ( !+ ) (lens : Lens<_, _, 'w>) =         lens.Update inc
    static member inline ( !- ) (lens : Lens<_, _, 'w>) =         lens.Update dec

    /// Set a lensed property.
    static member inline (<--) (lens : Lens<_, _, 'w>, value) = lens.Set value

    /// Get a lensed property.
    /// TODO: see if this operator is actually useful / understandable.
    static member inline (!.) (lens : Lens<_, _, 'w>) =
        fun world -> lens.Get world

[<RequireQualifiedAccess>]
module Lens =

    let name<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.Name

    let get<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) world =
        lens.Get world

    let getBy<'a, 'b, 's, 'w when 's :> Simulant> by (lens : Lens<'a, 's, 'w>) world : 'b =
        lens.GetBy by world

    let getByWorld<'a, 'b, 's, 'w when 's :> Simulant> by (lens : Lens<'a, 's, 'w>) world : 'b =
        lens.GetByWorld by world

    let setOpt<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) world =
        match lens.SetOpt with
        | ValueSome set -> set a world
        | ValueNone -> world

    let trySet<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) world =
        lens.TrySet a world

    let set<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) world =
        lens.Set a world

    let tryUpdateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.TryUpdateEffect updater world

    let tryUpdateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.TryUpdateWorld updater world

    let tryUpdate<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.TryUpdate updater world

    let updateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.UpdateEffect updater world

    let updateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.UpdateWorld updater world

    let update<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) world =
        lens.Update updater world

    let changeEvent<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.ChangeEvent

    let ty<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.Type

    let make<'a, 's, 'w when 's :> Simulant> (name : string) (this : 's) (get : 'w -> 'a) set : Lens<'a, 's, 'w> =
        { Name = name; This = this; Get = get; SetOpt = ValueSome set }

    let makeReadOnly<'a, 's, 'w when 's :> Simulant> (name : string) (this : 's) (get : 'w -> 'a) : Lens<'a, 's, 'w> =
        { Name = name; This = this; Get = get; SetOpt = ValueNone }

[<AutoOpen>]
module LensOperators =

    /// Make a writable lens.
    let lens<'a, 's, 'w when 's :> Simulant> name (this : 's) (get : 'w -> 'a) set =
        Lens.make name this get set

    /// Make a read-only lens.
    let lensReadOnly<'a, 's, 'w when 's :> Simulant> name (this : 's) (get : 'w -> 'a) =
        Lens.makeReadOnly name this get

    /// Define a property along with its initial value.
    let define (lens : Lens<'a, 's, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    /// Define a property along with its initial value, also initializing its global attributes as non-persistent.
    let nonPersistent (lens : Lens<'a, 's, 'w>) (value : 'a) =
        Reflection.initPropertyNonPersistent true lens.Name
        define lens value

    /// Define a variable property.
    let variable (lens : Lens<'a, 's, 'w>) (var : 'w -> 'a) =
        Reflection.initPropertyNonPersistent true lens.Name
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> var (world :?> 'w) :> obj))

    /// Define a computed property.
    let computed (lens : Lens<'a, 's, 'w>) (get : 't -> 'w -> 'a) (setOpt : ('a -> 't -> 'w -> 'w) option) =
        Reflection.initPropertyNonPersistent true lens.Name
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> 'w) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> 'w) :> obj)
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)