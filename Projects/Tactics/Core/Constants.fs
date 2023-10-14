// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Tactics
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gui =

        let Dissolve =
            { IncomingTime = 40L
              OutgoingTime = 60L
              DissolveImage = Assets.Default.Black }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 60L
              SlideImageOpt = Some Assets.Gui.Splash }

    [<RequireQualifiedAccess>]
    module Intro =

        let Dissolve =
            { IncomingTime = 95L
              OutgoingTime = 95L
              DissolveImage = Assets.Default.Black }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 160L
              SlideImageOpt = None }