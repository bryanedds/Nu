// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Events =

    let CancelEvent = stoa<unit> "Cancel/Event"
    let ItemSelectEvent = stoa<string> "Item/Select/Event"
    let TargetSelectEvent = stoa<CharacterIndex> "Target/Select/Event"
    let TravelEvent = stoa<Vector2> "Travel/Event"