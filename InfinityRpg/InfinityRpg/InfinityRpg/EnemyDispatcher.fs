namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn world : Turn = this.Get Property? DesiredTurn world
        member this.SetDesiredTurn (value : Turn) world = this.Set Property? DesiredTurn value world
        member this.TagDesiredTurn = PropertyTag.make this Property? DesiredTurn this.GetDesiredTurn this.SetDesiredTurn

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member PropertyDefinitions =
            [Define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             Define? ControlType Chaos
             Define? DesiredTurn NoTurn]