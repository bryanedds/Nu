namespace InfinityRpg
open Nu
open InfinityRpg

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type InfinityPlugin () =
    inherit NuPlugin ()

    // specify the game dispatcher to use at run-time
    override this.GetStandAloneGameDispatcher () =
        typeof<InfinityDispatcher>

    // route overlays to specific dispatchers
    override this.MakeOverlayRoutes () =
        [typeof<ButtonDispatcher>.Name, Some "InfinityButtonDispatcher"]