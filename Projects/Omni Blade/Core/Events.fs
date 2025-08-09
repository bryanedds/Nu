// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type BattleOutcome =
    | WinBattle
    | LoseBattle
    | RetryBattle of BattleData * PrizePool

[<RequireQualifiedAccess>]
module Events =

    let CancelEvent = stoa<unit> "Cancel/Event"
    let ItemSelectEvent = stoa<string> "Item/Select/Event"
    let TargetSelectEvent = stoa<CharacterIndex> "Target/Select/Event"
    let TravelEvent = stoa<Vector2> "Travel/Event"
    let QuitCreditsEvent = stoa<unit> "Quit/Credits/Event"
    let QuitFieldEvent = stoa<unit> "Quit/Field/Event"
    let CommencingBattleEvent = stoa<unit> "Commencing/Battle/Event"
    let CommenceBattleEvent = stoa<BattleData * PrizePool> "Commence/Battle/Event"
    let ConcludingBattleEvent = stoa<BattleOutcome> "Concluding/Battle/Event"
    let ConcludeBattleEvent = stoa<BattleOutcome * PrizePool> "Conclude/Battle/Event"