// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu.Gaia
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Editor =
    
        let [<Literal>] DefaultPositionSnap = 8
        let [<Literal>] DefaultRotationSnap = 5
        let [<Literal>] DefaultCreationDepth = 0.0f
        let [<Literal>] CameraSpeed = 4.0f // NOTE: might be nice to be able to configure this just like entity creation depth in the editor
        let [<Literal>] RefinementDir = "refinement"

    [<RequireQualifiedAccess>]
    module SubscriptionKeys =

        let RegisterEntity = makeGuid ()
        let UnregisteringEntity = makeGuid ()