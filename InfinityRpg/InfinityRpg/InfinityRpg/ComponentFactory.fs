namespace InfinityRpg
open Prime
open Nu
open Nu.Constants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module ComponentFactoryModule =

    type InfinityRpgComponentFactory () =
        inherit UserComponentFactory ()

        override this.MakeFacets () =
            Map.ofList
                [(typeof<CharacterAnimationFacet>.Name, CharacterAnimationFacet () :> Facet)
                 (typeof<CharacterControlFacet>.Name, CharacterControlFacet () :> Facet)]

        override this.MakeEntityDispatchers () =
            Map.ofList
                [(typeof<PlayerCharacterDispatcher>.Name, PlayerCharacterDispatcher () :> EntityDispatcher)]

        override this.MakeGameDispatchers () =
            Map.ofList
                [(typeof<InfinityRpgDispatcher>.Name, InfinityRpgDispatcher () :> GameDispatcher)]