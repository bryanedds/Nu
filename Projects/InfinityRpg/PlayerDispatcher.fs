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

        static member FacetNames =
            [typeof<CharacterCameraFacet>.Name]

        static member Properties =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }]