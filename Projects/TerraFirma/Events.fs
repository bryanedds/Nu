// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace TerraFirma
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Events =

    let CharacterAttackedEvent = stoa<Entity> "CharacterAttacked/Event"
    let CharacterDieEvent = stoa<unit> "CharacterDie/Event"