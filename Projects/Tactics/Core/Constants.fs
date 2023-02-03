// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Tactics
open System
open Prime
open Nu
open Tactics

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gui =

        let Dissolve =
            { IncomingTime = 40L
              OutgoingTime = 60L
              DissolveImage = Assets.Default.Image8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 60L
              SlideImageOpt = Some Assets.Gui.Splash }

    [<RequireQualifiedAccess>]
    module Intro =

        let Dissolve =
            { IncomingTime = 95L
              OutgoingTime = 95L
              DissolveImage = Assets.Default.Image8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 160L
              SlideImageOpt = None }