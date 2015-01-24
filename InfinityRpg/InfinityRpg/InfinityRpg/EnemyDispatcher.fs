namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn world : Turn = (this.GetXtension world)?DesiredTurn
        member this.SetDesiredTurn (value : Turn) world = this.UpdateXtension (fun xtension -> xtension?DesiredTurn <- value) world

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member FieldDefinitions =
            [define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             define? ControlType Chaos
             define? DesiredTurn NoTurn]