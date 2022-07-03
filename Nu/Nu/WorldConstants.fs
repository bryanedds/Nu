// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of screens.
    let Default =
        { IncomingTime = 20L
          OutgoingTime = 40L
          DissolveImage = Assets.Default.Image9 }

[<RequireQualifiedAccess>]
module Splash =

    /// The default 'splashing' behavior of splash screens.
    let Default =
        { DissolveDescriptor = Dissolve.Default
          IdlingTime = 60L
          SplashImageOpt = Some Assets.Default.Image5 }