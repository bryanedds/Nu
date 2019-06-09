namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member IntrinsicFacetNames =
            [typeof<CharacterStateFacet>.Name
             typeof<CharacterAnimationFacet>.Name]

        static member PropertyDefinitions =
            [define Entity.Omnipresent true
             define Entity.PublishChanges true]
