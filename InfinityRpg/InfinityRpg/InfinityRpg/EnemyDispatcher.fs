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

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member FieldDefinitions =
            [define? ControlType Chaos]