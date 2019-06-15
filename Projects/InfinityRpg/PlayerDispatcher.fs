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

        static member IntrinsicFacetNames =
            [typeof<CharacterCameraFacet>.Name]

        static member PropertyDefinitions =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }]