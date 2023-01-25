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
            { IncomingTime = FrameTime 40L
              OutgoingTime = FrameTime 60L
              DissolveImage = Assets.Default.Image8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = FrameTime 60L
              SlideImageOpt = Some Assets.Gui.Splash }

    [<RequireQualifiedAccess>]
    module Intro =

        let Dissolve =
            { IncomingTime = FrameTime 95L
              OutgoingTime = FrameTime 95L
              DissolveImage = Assets.Default.Image8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = FrameTime 160L
              SlideImageOpt = None }