// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gaia =

        let [<Literal>] PositionSnap2dDefault = 8.0f
        let [<Literal>] DegreesSnap2dDefault = 5.0f
        let [<Literal>] ScaleSnap2dDefault = 0.1f
        let [<Uniform>] Snaps2dDefault = (PositionSnap2dDefault, DegreesSnap2dDefault, ScaleSnap2dDefault)
        let [<Literal>] PositionSnap3dDefault = 0.1f
        let [<Literal>] DegreesSnap3dDefault = 5.0f
        let [<Literal>] ScaleSnap3dDefault = 0.1f
        let [<Uniform>] Snaps3dDefault = (PositionSnap3dDefault, DegreesSnap3dDefault, ScaleSnap3dDefault)
        let [<Literal>] CreationElevationDefault = 0.0f
        let [<Literal>] EyeSpeed = 3.0f // NOTE: might be nice to be able to configure this just like entity creation elevation in the editor.
        let [<Literal>] DragMinimumSeconds = 0.2
        let [<Literal>] PropertyValueStrMemoEvictionAge = 5.0 // NOTE: this is a somewhat arbitrary number that works well for OmniBlade, but it's unknown how well it will hold up for more complex games.
        let [<Literal>] StateFilePath = "GaiaState.txt"
        let [<Literal>] NonePick = "\"None\""
        let [<Uniform>] EventFilter =
            EventFilter.NotAny
                [EventFilter.Pattern (Rexpr "PreUpdate", [])
                 EventFilter.Pattern (Rexpr "Update", [])
                 EventFilter.Pattern (Rexpr "PostUpdate", [])
                 EventFilter.Pattern (Rexpr "Render", [])
                 EventFilter.Pattern (Rexpr "Change", [])
                 EventFilter.Pattern (Rexpr "Integration", [])
                 EventFilter.Pattern (Rexpr "BodyTransform", [])
                 EventFilter.Pattern (Rexpr "Mouse/Move", [])]
        let [<Literal>] BuildName =
#if DEBUG
            "Debug"
#else
            "Release"
#endif