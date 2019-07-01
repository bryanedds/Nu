namespace Elmario
open Prime
open SDL2
open Nu
open Nu.Declarative
module Elmario =

    // here we create references for the entities that we are going to define for our game
    let Elmario = Default.Layer / "Elmario"
    let Ground = Default.Layer / "Ground"
    let Rock = Default.Layer / "Rock"

    // this is our Elm-style command type
    type Command =
        | MoveLeft
        | MoveRight
        | Jump

    // this is our Elm-style game dispatcher
    type ElmarioDispatcher () =
        inherit GameDispatcher<unit, unit, Command> (())

        // here we define the Bindings used to connect events to their desired commands
        override this.Bindings (_, game, _) =
            [game.UpdateEvent =|>! fun _ ->
                if KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) then Some MoveLeft
                elif KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) then Some MoveRight
                else None
             game.KeyboardKeyDownEvent =|>! fun evt ->
                if evt.Data.ScanCode = int SDL.SDL_Scancode.SDL_SCANCODE_UP && not evt.Data.Repeated
                then Some Jump
                else None]

        // here we handle the above commands
        override this.Command (command, _, _, world) =
            match command with
            | MoveLeft ->
                let physicsId = Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 -30000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 -7500.0f 0.0f) physicsId world
            | MoveRight ->
                let physicsId = Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 30000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 7500.0f 0.0f) physicsId world
            | Jump ->
                let physicsId = Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.applyBodyForce (v2 0.0f 2000000.0f) physicsId world
                    World.playSound 0.5f (AssetTag.make "Gameplay" "Jump") world
                else world

        // here we describe the content of the game including elmario and the ground he walks on.
        override this.Content (_, _, _) =
            [Content.screen Default.Screen Vanilla []
                [Content.layer Default.Layer []
                    [Content.character Elmario
                        [Entity.Position == v2 0.0f 0.0f
                         Entity.Size == v2 144.0f 144.0f]
                     Content.block Ground
                        [Entity.Position == v2 -384.0f -256.0f
                         Entity.Size == v2 768.0f 64.0f
                         Entity.StaticImage == (AssetTag.make "Gameplay" "TreeTop")]
                     Content.block Rock
                        [Entity.Position == v2 320.0f -192.0f
                         Entity.Size == v2 64.0f 64.0f
                         Entity.StaticImage == (AssetTag.make "Gameplay" "Rock")]]]]