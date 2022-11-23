// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Events =

    let Cancel = stoa<unit> "Cancel/Event"
    let ItemSelect = stoa<string> "Item/Select/Event"
    let TargetSelect = stoa<CharacterIndex> "Target/Select/Event"
    let Travel = stoa<Vector2> "Travel/Event"