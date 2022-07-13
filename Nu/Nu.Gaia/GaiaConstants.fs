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
    
        let [<Literal>] PositionSnap2dDefault = 12.0f
        let [<Literal>] RotationSnap2dDefault = 5.0f
        let [<Literal>] ScaleSnap2dDefault = 0.1f
        let [<Literal>] PositionSnap3dDefault = 0.1f
        let [<Literal>] RotationSnap3dDefault = 5.0f
        let [<Literal>] ScaleSnap3dDefault = 0.1f
        let [<Literal>] CreationElevationDefault = 0.0f
        let [<Literal>] CameraSpeed = 3.0f // NOTE: might be nice to be able to configure this just like entity creation elevation in the editor
        let [<Literal>] SavedStateFilePath = "GaiaState.txt"
        let [<Literal>] NonePick = "\"None\""
        let (*Literal*) GroupNodeKey = string Gen.id