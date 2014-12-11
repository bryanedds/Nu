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

        member entity.DesiredTurn = entity?DesiredTurn : Turn // TODO: move into enemy dispatcher
        static member setDesiredTurn (value : Turn) (entity : Entity) = entity?DesiredTurn <- value

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member FieldDefinitions =
            [define? ControlType Chaos
             define? DesiredTurn NoTurn]