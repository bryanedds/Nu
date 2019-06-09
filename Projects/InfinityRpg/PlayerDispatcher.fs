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

        static member PropertyDefinitions =
            [define Entity.HitPoints 30 // note this is an arbitrary number as hp max is calculated
             define Entity.ControlType PlayerControlled]

        static member IntrinsicFacetNames =
            [typeof<CharacterCameraFacet>.Name]