// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// Describes the form of an element's presence.
type Presence =
    /// An interior element so you have to be closer to see them.
    | Interior
    /// An exterior element so you can see them from a distance.
    | Exterior
    /// Always visible except when as close as Exterior or Interior.
    | Imposter
    /// Always visible.
    | Omnipresent

    member this.InteriorType = match this with Interior -> true | _ -> false
    member this.ExteriorType = match this with Exterior -> true | _ -> false
    member this.ImposterType = match this with Imposter -> true | _ -> false
    member this.OmnipresentType = match this with Omnipresent -> true | _ -> false
    member this.DepthCutoff =
        match this with
        | Omnipresent -> Constants.Render.FarPlaneDistanceOmnipresent
        | Imposter -> -Constants.Render.NearPlaneDistanceImposter
        | Exterior | Interior -> Constants.Render.FarPlaneDistanceExterior

    /// Determines if a bounds intersection is taking place in the context of the given presence configuration.
    static member intersects3d (frustumInteriorOpt : Frustum option) (frustumExterior : Frustum) (frustumImposter : Frustum) (lightBoxOpt : Box3 option) (lightProbe : bool) (light : bool) presence (bounds : Box3) =
        if lightProbe then
            true
        elif not light then
            match presence with
            | Interior -> match frustumInteriorOpt with Some frustumInterior -> frustumInterior.Intersects bounds | None -> false
            | Exterior -> frustumExterior.Intersects bounds || (match frustumInteriorOpt with Some frustumInterior -> frustumInterior.Intersects bounds | None -> false)
            | Imposter -> frustumImposter.Intersects bounds
            | Omnipresent -> true
        else
            match presence with
            | Interior | Exterior | Imposter -> match lightBoxOpt with Some lightBox -> lightBox.Intersects bounds | None -> false
            | Omnipresent -> true

[<AutoOpen>]
module PresenceOperators =
    
    /// Test two presence values for equality without allocating.
    let presenceEq left right =
        match struct (left, right) with
        | struct (Interior, Interior)
        | struct (Exterior, Exterior)
        | struct (Imposter, Imposter)
        | struct (Omnipresent, Omnipresent) -> true
        | struct (_, _) -> false

    /// Test two presence values for inequality.
    let presenceNeq left right =
        not (presenceEq left right)