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
            let player = CharacterActivity.advance AdvanceOnly player world
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let touchPosition : Vector2 = World.unwrapD event world
            let player = World.getEntity PlayerAddress world
            let player = CharacterActivity.touch touchPosition player world
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            let world = observe TickEventAddress address |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            (screen, world)