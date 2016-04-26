namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn world : Turn = (this.GetXtension world)?DesiredTurn
        member this.SetDesiredTurn (value : Turn) world = this.UpdateXtension (fun xtension -> xtension?DesiredTurn <- value) world

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member PropertyDefinitions =
            [Define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             Define? ControlType Chaos
             Define? DesiredTurn NoTurn]
