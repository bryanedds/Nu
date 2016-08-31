// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Gaia
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module SubscriptionKeys =

        let AddEntity = makeGuid ()
        let RemovingEntity = makeGuid ()