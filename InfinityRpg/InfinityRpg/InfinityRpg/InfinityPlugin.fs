namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type InfinityRpgPlugin () =
    inherit NuPlugin ()

    override this.MakeOptGameDispatcher () =
        Some (InfinityDispatcher () :> GameDispatcher)

    override this.MakeScreenDispatchers () =
        [GameplayDispatcher () :> ScreenDispatcher]

    override this.MakeFacets () =
        [CharacterStateFacet () :> Facet
         CharacterAnimationFacet () :> Facet
         CharacterCameraFacet () :> Facet]

    override this.MakeEntityDispatchers () =
        [FieldDispatcher () :> EntityDispatcher
         EnemyDispatcher () :> EntityDispatcher
         CharacterDispatcher () :> EntityDispatcher
         PlayerDispatcher () :> EntityDispatcher]

    override this.MakeOverlayRoutes () =
        [typeof<ButtonDispatcher>.Name, Some "InfinityButtonDispatcher"]