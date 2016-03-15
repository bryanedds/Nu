namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module PlayerDispatcherModule =

    type PlayerDispatcher () =
        inherit CharacterDispatcher ()

        static member FieldDefinitions =
            [Define? HitPoints 30 // note this is an arbitrary number as hp max is calculated
             Define? ControlType Player]

        static member IntrinsicFacetNames =
            [typeof<CharacterCameraFacet>.Name]