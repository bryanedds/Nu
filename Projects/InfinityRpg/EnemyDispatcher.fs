namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn = this.Get<Turn> Property? DesiredTurn
        member this.SetDesiredTurn = this.Set<Turn> Property? DesiredTurn
        member this.DesiredTurn = Lens.make Property? DesiredTurn this.GetDesiredTurn this.SetDesiredTurn this

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member PropertyDefinitions =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
             define Entity.DesiredTurn NoTurn]