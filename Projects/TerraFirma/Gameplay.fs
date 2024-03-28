namespace TerraFirma
open System
open Prime
open Nu

// this is our MMCC message type.
type GameplayMessage =
    | Update
    | TimeUpdate
    | StartQuitting
    | FinishQuitting
    interface Message

// this is our MMCC command type.
type GameplayCommand =
    | SynchronizeNav3d
    | CharactersAttacked of Entity Set
    | TransformEye
    | PlaySound of int64 * single * Sound AssetTag
    interface Command

// this represents that state of gameplay simulation.
type GameplayState =
    | Playing
    | Quitting
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
// if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
// you could use `GameplayTime : single` instead.
type [<ReferenceEquality; SymbolicExpansion>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState }

    static member timeUpdate gameplay =
        let gameplay = { gameplay with GameplayTime = inc gameplay.GameplayTime }
        gameplay

    static member initial =
        { GameplayTime = 0L
          GameplayState = Quit }

    static member start =
        let initial = Gameplay.initial
        { initial with GameplayState = Playing }