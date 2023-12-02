// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// Describes the form of an element's presence.
[<Syntax
    ("Enclosed Exposed Imposter Omnipresent", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Presence =
    /// Inside an enclosed structure so you have to be close to see them.
    | Enclosed
    /// Outside an enclosed structure so visible near and from a distance.
    | Exposed
    /// Always visible except when as close as Exposed or Enclosed.
    | Imposter
    /// Always visible.
    | Omnipresent

    member this.EnclosedType with get () = match this with Enclosed -> true | _ -> false
    member this.ExposedType with get () = match this with Exposed -> true | _ -> false
    member this.ImposterType with get () = match this with Imposter -> true | _ -> false
    member this.OmnipresentType with get () = match this with Omnipresent -> true | _ -> false

    /// Determines if a bounds intersection is taking place in the context of the given presence configuration.
    static member intersects3d (frustumEnclosed : Frustum) (frustumExposed : Frustum) (frustumImposter : Frustum) (lightBox : Box3) (lightProbe : bool) (light : bool) (bounds : Box3) presence =
        if lightProbe then
            true
        elif not light then
            match presence with
            | Enclosed -> frustumEnclosed.Intersects bounds
            | Exposed -> frustumExposed.Intersects bounds || frustumEnclosed.Intersects bounds
            | Imposter -> frustumImposter.Intersects bounds
            | Omnipresent -> true
        else
            match presence with
            | Enclosed | Exposed | Imposter -> lightBox.Intersects bounds
            | Omnipresent -> true

[<AutoOpen>]
module PresenceOperators =
    
    /// Test two presence values for equality without allocating.
    let presenceEq left right =
        match (left, right) with
        | (Enclosed, Enclosed)
        | (Exposed, Exposed)
        | (Imposter, Imposter)
        | (Omnipresent, Omnipresent) -> true
        | (_, _) -> false

    /// Test two presence values for inequality.
    let presenceNeq left right =
        not (presenceEq left right)