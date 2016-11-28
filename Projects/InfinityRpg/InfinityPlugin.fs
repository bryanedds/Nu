namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type InfinityRpgPlugin () =
    inherit NuPlugin ()

    override this.MakeGameDispatcherOpt () =
        Some (InfinityDispatcher () :> GameDispatcher)

    override this.MakeScreenDispatchers () =
        [GameplayDispatcher () :> ScreenDispatcher]

    override this.MakeEntityDispatchers () =
        [FieldDispatcher () :> EntityDispatcher
         EnemyDispatcher () :> EntityDispatcher
         CharacterDispatcher () :> EntityDispatcher
         PlayerDispatcher () :> EntityDispatcher]

    override this.MakeFacets () =
        [CharacterStateFacet () :> Facet
         CharacterAnimationFacet () :> Facet
         CharacterCameraFacet () :> Facet]

    override this.MakeOverlayRoutes () =
        [typeof<ButtonDispatcher>.Name, OverlayDescriptor.makeVanilla (Some "InfinityButtonDispatcher")]