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

        let [<Literal>] Position2dSnapDefault = 12.0f
        let [<Literal>] Position3dSnapDefault = 0.1f
        let [<Literal>] Degrees2dSnapDefault = 5.0f
        let [<Literal>] Degrees3dSnapDefault = 0.0f
        let [<Literal>] Scale2dSnapDefault = 0.1f
        let [<Literal>] Scale3dSnapDefault = 0.1f
        let [<Literal>] CreationElevationDefault = 0.0f
        let [<Literal>] EyeSpeed = 3.0f // NOTE: might be nice to be able to configure this just like entity creation elevation in the editor.
        let [<Literal>] DragMinimumSeconds = 0.2
        let [<Literal>] PropertyValueStrMemoCapacity = 131072
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