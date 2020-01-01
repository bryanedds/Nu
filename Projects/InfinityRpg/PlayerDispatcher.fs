namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PlayerDispatcherModule =

    type PlayerDispatcher () =
        inherit CharacterDispatcher ()

        static member Facets =
            [typeof<CharacterCameraFacet>]

        static member Properties =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }]