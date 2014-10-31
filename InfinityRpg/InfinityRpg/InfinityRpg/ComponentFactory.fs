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
        override dispatcher.MakeGameDispatchers () =
            Map.singleton typeof<InfinityRpgDispatcher>.Name (InfinityRpgDispatcher () :> GameDispatcher)