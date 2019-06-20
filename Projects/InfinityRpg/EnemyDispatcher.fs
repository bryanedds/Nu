namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn world : Turn = this.Get Property? DesiredTurn world
        member this.SetDesiredTurn (value : Turn) world = this.Set Property? DesiredTurn value world
        member this.DesiredTurn = Lens.make this Property? DesiredTurn this.GetDesiredTurn this.SetDesiredTurn

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member PropertyDefinitions =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
             define Entity.DesiredTurn NoTurn]