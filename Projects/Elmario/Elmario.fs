namespace Elmario
open Prime
open SDL2
open Nu
open Nu.Declarative

module Simulants =

    // here we create an entity reference for Elmario. This is useful for simulants that you want
    // to refer to from multiple places
    let Elmario = Default.Layer / "Elmario"

module Elmario =

    // this is our Elm-style command type
    type Command =
        | Jump
        | MoveLeft
        | MoveRight
        | Nop

    // this is our Elm-style game dispatcher
    type ElmarioDispatcher () =
        inherit GameDispatcher<unit, unit, Command> (())

        // here we define the Bindings used to connect events to their desired commands
        override this.Bindings (_, game, _) =
            [game.KeyboardKeyDownEvent =|> fun evt ->
                if evt.Data.ScanCode = int SDL.SDL_Scancode.SDL_SCANCODE_UP && not evt.Data.Repeated
                then cmd Jump
                else cmd Nop
             game.UpdateEvent =|> fun _ ->
                if KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) then cmd MoveLeft
                elif KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) then cmd MoveRight
                else cmd Nop]

        // here we handle the above commands
        override this.Command (_, command, _, world) =
            let world =
                match command with
                | MoveLeft ->
                    let physicsId = Simulants.Elmario.GetPhysicsId world
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v2 -30000.0f 0.0f) physicsId world
                    else World.applyBodyForce (v2 -7500.0f 0.0f) physicsId world
                | MoveRight ->
                    let physicsId = Simulants.Elmario.GetPhysicsId world
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v2 30000.0f 0.0f) physicsId world
                    else World.applyBodyForce (v2 7500.0f 0.0f) physicsId world
                | Jump ->
                    let physicsId = Simulants.Elmario.GetPhysicsId world
                    if World.isBodyOnGround physicsId world then
                        let world = World.applyBodyForce (v2 0.0f 2000000.0f) physicsId world
                        World.playSound 0.5f (asset "Gameplay" "Jump") world
                    else world
                | Nop -> world
            just world

        // here we describe the content of the game including elmario and the ground he walks on.
        override this.Content (_, _, _) =
            [Content.screen Default.Screen.Name Vanilla []
                [Content.layer Default.Layer.Name []
                    [Content.character Simulants.Elmario.Name
                        [Entity.Position == v2 0.0f 0.0f
                         Entity.Size == v2 144.0f 144.0f]
                     Content.block "Ground"
                        [Entity.Position == v2 -384.0f -256.0f
                         Entity.Size == v2 768.0f 64.0f
                         Entity.StaticImage == asset "Gameplay" "TreeTop"]
                     Content.block "Rock"
                        [Entity.Position == v2 320.0f -192.0f
                         Entity.Size == v2 64.0f 64.0f
                         Entity.StaticImage == asset "Gameplay" "Rock"]]]]