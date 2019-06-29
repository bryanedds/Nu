namespace Elmario
open Prime
open SDL2
open Nu
open Nu.Declarative
module Elmario =

    // here we create references for the entities that we are going to define for our game
    let Elmario = Default.Layer / "Elmario"
    let Ground = Default.Layer / "Ground"

    // this is our Elm-style command type
    type Command =
        | Jump
        | MoveLeft
        | MoveRight

    // this is our Elm-style game dispatcher
    type ElmarioDispatcher () =
        inherit GameDispatcher<unit, unit, Command> (())

        // here we define the Bindings used to connect events to their desired commands
        override this.Bindings (_, game, _) =
            [game.KeyboardKeyDownEvent =|>! fun evt ->
                if evt.Data.ScanCode = int SDL.SDL_Scancode.SDL_SCANCODE_UP && not evt.Data.Repeated
                then Some Jump
                else None
             game.UpdateEvent =|>! fun _ ->
                if KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) then Some MoveLeft
                elif KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) then Some MoveRight
                else None]

        // here we handle the above commands
        override this.Command (command, _, _, world) =
            match command with
            | Jump ->
                let physicsId = Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 0.0f 100000.0f) physicsId world
                else world
            | MoveLeft ->
                let physicsId = Elmario.GetPhysicsId world
                World.applyBodyForce (v2 -1000.0f 0.0f) physicsId world
            | MoveRight ->
                let physicsId = Elmario.GetPhysicsId world
                World.applyBodyForce (v2 1000.0f 0.0f) physicsId world

        // here we describe the content of the game including elmario and the ground he walks on.
        override this.Content (_, _, _) =
            [Content.screen Default.Screen Vanilla []
                [Content.layer Default.Layer []
                    [Content.sideViewCharacter Elmario
                        [Entity.Position == v2 0.0f 0.0f]
                     Content.block Ground
                        [Entity.Position == v2 -256.0f -64.0f
                         Entity.Size == v2 512.0f 64.0f]]]]