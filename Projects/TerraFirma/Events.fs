// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace TerraFirma
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Events =

    let CharactersAttacked = stoa<Entity Set> "CharactersAttacked/Event"
    let DieEvent = stoa<unit> "Die/Event"