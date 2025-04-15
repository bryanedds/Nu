// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Constants
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of screens.
    let Default =
        { IncomingTime = GameTime.ofSeconds 0.5f
          OutgoingTime = GameTime.ofSeconds 1.0f
          DissolveImage = Assets.Default.Black }

[<RequireQualifiedAccess>]
module Slide =

    /// The default 'slide shot' behavior of slide screens.
    let Default =
        { DissolveDescriptor = Dissolve.Default
          IdlingTime = GameTime.ofSeconds 1.0f
          SlideImageOpt = Some Assets.Default.NuSlide }