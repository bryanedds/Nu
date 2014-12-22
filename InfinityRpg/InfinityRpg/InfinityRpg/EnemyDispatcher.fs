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

        member entity.DesiredTurn = entity?DesiredTurn : Turn
        static member setDesiredTurn (value : Turn) (entity : Entity) = entity?DesiredTurn <- value

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member FieldDefinitions =
            [define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             define? ControlType Chaos
             define? DesiredTurn NoTurn]