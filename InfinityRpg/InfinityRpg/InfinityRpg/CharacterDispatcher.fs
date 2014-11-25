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
module CharacterDispatcherModule =

    type ControlType =
        | Player
        | Random
        | Uncontrolled

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? ControlType Uncontrolled]

        static member IntrinsicFacetNames =
            [typeof<CharacterStateFacet>.Name
             typeof<CharacterAnimationFacet>.Name]