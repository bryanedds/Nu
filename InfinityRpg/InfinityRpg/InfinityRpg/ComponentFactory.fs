namespace InfinityRpg
open System
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
                [(typeof<CharacterStateFacet>.Name, CharacterStateFacet () :> Facet)
                 (typeof<CharacterAnimationFacet>.Name, CharacterAnimationFacet () :> Facet)
                 (typeof<CharacterCameraFacet>.Name, CharacterCameraFacet () :> Facet)]

        override this.MakeEntityDispatchers () =
            Map.ofList
                [(typeof<FieldDispatcher>.Name, FieldDispatcher () :> EntityDispatcher)
                 (typeof<EnemyDispatcher>.Name, EnemyDispatcher () :> EntityDispatcher)
                 (typeof<CharacterDispatcher>.Name, CharacterDispatcher () :> EntityDispatcher)
                 (typeof<PlayerDispatcher>.Name, PlayerDispatcher () :> EntityDispatcher)]

        override this.MakeScreenDispatchers () =
            Map.ofList
                [(typeof<GameplayDispatcher>.Name, GameplayDispatcher () :> ScreenDispatcher)]

        override this.MakeOptGameDispatcher () =
            Some (typeof<InfinityRpgDispatcher>.Name, InfinityRpgDispatcher () :> GameDispatcher)