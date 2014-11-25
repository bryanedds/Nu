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

        static let getPlayer world =
            World.getEntity PlayerAddress world

        static let getField world =
            World.getEntity FieldAddress world

        static let getEnemies world =
            World.getEntities SceneAddress world |>
            Seq.filter (Entity.dispatchesAs typeof<EnemyDispatcher>)

        static let handleTick _ world =
            let advancementType =
                if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then AdvanceWithDirection East
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then AdvanceWithDirection West
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then AdvanceWithDirection North
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then AdvanceWithDirection South
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
            let player = CharacterActivity.detailTouch direction field player
            let world = World.setEntity PlayerAddress player world
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            if address <> GameplayAddress then failwith "Invalid address for GameplayDispatcher screen."
            let world = observe TickEventAddress address |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (DownEventAddress ->>- HudDetailUpAddress) address |> filter isSelected |> monitor (handleDownDetail North) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailRightAddress) address |> filter isSelected |> monitor (handleDownDetail East) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailDownAddress) address |> filter isSelected |> monitor (handleDownDetail South) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailLeftAddress) address |> filter isSelected |> monitor (handleDownDetail West) world |> snd
            (screen, world)