namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type InfinityRpgPlugin () =
    inherit NuPlugin ()

    override this.GetStandAloneGameDispatcher () =
        typeof<InfinityDispatcher>

    override this.MakeOverlayRoutes () =
        [typeof<ButtonDispatcher>.Name, Some "InfinityButtonDispatcher"]