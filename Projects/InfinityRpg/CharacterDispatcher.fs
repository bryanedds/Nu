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

        static member Facets =
            [typeof<CharacterStateFacet>
             typeof<CharacterAnimationFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.Omnipresent true]
