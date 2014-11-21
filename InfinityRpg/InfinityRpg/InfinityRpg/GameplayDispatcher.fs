namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module GameplayDispatcherModule =

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        static let handleTick _ world =
            let player = World.getEntity PlayerAddress world
            let world = World.advanceCharacterActivity PlayerAddress player AdvanceOnly world
            (Cascade, world)

        static let handleTouchFeeler _ world =
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            let world = observe TickEventAddress address |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            (screen, world)