// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Constants
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of screens.
    let Default =
        { IncomingTime = GameTime.ofSeconds 0.5
          OutgoingTime = GameTime.ofSeconds 1.0
          DissolveImage = Assets.Default.Black }

[<RequireQualifiedAccess>]
module Slide =

    /// The default 'slide show' behavior of slide screens.
    let Default =
        { DissolveDescriptor = Dissolve.Default
          IdlingTime = GameTime.ofSeconds 1.0
          SlideImageOpt = Some Assets.Default.NuSlide }