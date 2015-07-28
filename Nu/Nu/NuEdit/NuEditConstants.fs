// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

namespace NuEdit
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module SubscriptionKeys =

        let AddEntity = World.makeSubscriptionKey ()
        let RemovingEntity = World.makeSubscriptionKey ()