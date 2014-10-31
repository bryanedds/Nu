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
            Map.singleton typeof<CharacterAnimationFacet>.Name (CharacterAnimationFacet () :> Facet)

        override this.MakeGameDispatchers () =
            Map.singleton typeof<InfinityRpgDispatcher>.Name (InfinityRpgDispatcher () :> GameDispatcher)