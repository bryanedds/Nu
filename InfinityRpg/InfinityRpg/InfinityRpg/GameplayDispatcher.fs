namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module GameplayDispatcherModule =

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        static let handleTouchFeeler _ world =
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            let world = World.monitor handleTouchFeeler (TouchEventAddress ->>- HudFeelerAddress) address world
            (screen, world)