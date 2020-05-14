// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of game's screens.
    let Default =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = AssetTag.make Assets.DefaultPackageName "Image9" }

[<RequireQualifiedAccess>]
module Splash =

    /// The default 'splashing' behavior of game's splash screen.
    let Default =
        { DissolveData = Dissolve.Default
          IdlingTime = 60L
          SplashImage = AssetTag.make Assets.DefaultPackageName "Image5" }