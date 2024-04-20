// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Events =

    let CancelEvent = stoa<unit> "Cancel/Event"
    let ItemSelectEvent = stoa<string> "Item/Select/Event"
    let TargetSelectEvent = stoa<CharacterIndex> "Target/Select/Event"
    let TravelEvent = stoa<Vector2> "Travel/Event"
    let QuitFieldEvent = stoa<unit> "Quit/Field/Event"
    let CommencingBattleEvent = stoa<unit> "Commencing/Battle/Event"
    let CommenceBattleEvent = stoa<BattleData * PrizePool> "Commence/Battle/Event"
    let ConcludingBattleEvent = stoa<bool> "Concluding/Battle/Event"
    let ConcludeBattleEvent = stoa<bool * PrizePool> "Conclude/Battle/Event"