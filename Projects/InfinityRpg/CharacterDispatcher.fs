namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member IntrinsicFacetNames =
            [typeof<CharacterStateFacet>.Name
             typeof<CharacterAnimationFacet>.Name]

        static member PropertyDefinitions =
            [Define? Omnipresent true
             Define? PublishChanges true]
