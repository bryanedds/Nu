// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    let Game = { GameAddress = Address.empty }
    let DefaultScreen = { ScreenAddress = ntoa Constants.Engine.DefaultScreenName }
    let DefaultGroup = { GroupAddress = satoga DefaultScreen.ScreenAddress Constants.Engine.DefaultGroupName }
    let DefaultEntity = { EntityAddress = gatoea DefaultGroup.GroupAddress Constants.Engine.DefaultEntityName }