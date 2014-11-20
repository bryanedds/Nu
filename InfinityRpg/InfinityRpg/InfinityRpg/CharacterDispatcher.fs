namespace InfinityRpg
open System
open System.ComponentModel
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member IntrinsicFacetNames =
            [typeof<CharacterStateFacet>.Name
             typeof<CharacterAnimationFacet>.Name
             typeof<CharacterControlFacet>.Name
             typeof<CharacterCameraFacet>.Name]