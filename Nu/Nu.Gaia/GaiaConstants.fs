// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Editor =
    
        let [<Literal>] Position2dSnapDefault = 12.0f
        let [<Literal>] Position3dSnapDefault = 0.1f
        let [<Literal>] Degrees2dSnapDefault = 5.0f
        let [<Literal>] Degrees3dSnapDefault = 5.0f
        let [<Literal>] Scale2dSnapDefault = 0.1f
        let [<Literal>] Scale3dSnapDefault = 0.1f
        let [<Literal>] CreationElevationDefault = 0.0f
        let [<Literal>] CameraSpeed = 3.0f // NOTE: might be nice to be able to configure this just like entity creation elevation in the editor
        let [<Literal>] SavedStateFilePath = "GaiaState.txt"
        let [<Literal>] NonePick = "\"None\""
        let (*Literal*) GroupNodeKey = string Gen.id