namespace InfinityRpg
open System
open SDL2
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
            let advancementType =
                if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then AdvanceWithDirection North
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then AdvanceWithDirection South
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then AdvanceWithDirection East
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then AdvanceWithDirection West
                else AdvanceOnly
            let field = World.getEntity FieldAddress world
            let player = World.getEntity PlayerAddress world
            let player = CharacterActivity.advance advancementType field player
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let touchPosition : Vector2 = World.unwrapD event world
            let touchPositionW = Camera.mouseToWorld Relative touchPosition world.Camera
            let field = World.getEntity FieldAddress world
            let player = World.getEntity PlayerAddress world
            let player = CharacterActivity.touch touchPositionW field player
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let field = World.getEntity FieldAddress world
            let player = World.getEntity PlayerAddress world
            let touchPosition = player.Position + player.Size * 0.5f + Vector2.Multiply (Direction.toVector2 direction, TileSize)
            let player =
                if Math.isPointInBounds3 touchPosition field.Position (field.Position + Entity.getQuickSize field world)
                then CharacterActivity.touch touchPosition field player
                else player
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            let world = observe TickEventAddress address |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (DownEventAddress ->>- HudDetailUpAddress) address |> filter isSelected |> monitor (handleDownDetail North) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailRightAddress) address |> filter isSelected |> monitor (handleDownDetail East) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailDownAddress) address |> filter isSelected |> monitor (handleDownDetail South) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailLeftAddress) address |> filter isSelected |> monitor (handleDownDetail West) world |> snd
            (screen, world)