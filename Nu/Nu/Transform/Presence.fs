// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// Describes the form of an element's presence.
type [<StructuralEquality; StructuralComparison; Struct>] Presence =

    /// An interior element so you have to be closer to see them.
    | Interior

    /// An exterior element so you can see them from a distance.
    | Exterior

    /// Always visible except when as close as Exterior or Interior.
    | Imposter

    /// Always visible.
    | Omnipresent

    /// The depth cutoff for the presence, which is used to determine the far plane distance for rendering.
    member this.DepthCutoff =
        match this with
        | Omnipresent -> Constants.Render.FarPlaneDistanceOmnipresent
        | Imposter -> -Constants.Render.NearPlaneDistanceImposter // NOTE: special case where negative value inverts to near cutoff.
        | Exterior | Interior -> Constants.Render.FarPlaneDistanceExterior

    /// Determines the highest override in the context of the given presence configuration.
    static member highestOverride2 override_ (overrides : Presence voption array) =
        let mutable highest = override_
        for override_ in overrides do
            if override_ > highest then highest <- override_
        highest

    /// Determines the highest override in the context of the given presence configuration.
    static member highestOverride (overrides : Presence voption array) =
        Presence.highestOverride2 ValueNone overrides

    /// Determines if a bounds intersection is taking place in the context of the given presence configuration.
    static member intersects3d (frustumInteriorOpt : Frustum voption) (frustumExterior : Frustum) (frustumImposter : Frustum) (lightBoxOpt : Box3 voption) (lightProbe : bool) (light : bool) presence (bounds : Box3) =
        if lightProbe then
            true
        elif light then
            match presence with
            | Interior | Exterior | Imposter -> match lightBoxOpt with ValueSome lightBox -> lightBox.Intersects bounds | ValueNone -> false
            | Omnipresent -> true
        else
            match presence with
            | Interior -> match frustumInteriorOpt with ValueSome frustumInterior -> frustumInterior.Intersects bounds | ValueNone -> false
            | Exterior -> frustumExterior.Intersects bounds || (match frustumInteriorOpt with ValueSome frustumInterior -> frustumInterior.Intersects bounds | ValueNone -> false)
            | Imposter -> frustumImposter.Intersects bounds
            | Omnipresent -> true