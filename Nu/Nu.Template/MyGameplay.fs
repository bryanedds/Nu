namespace MyGame
open Prime
open SDL2
open Nu
open Nu.Declarative

// this is our Elm-style command type
type GameplayCommand =
    | Jump
    | MoveLeft
    | MoveRight
    | EyeTrack
    | Nil

// this is the screen dispatcher that defines the screen where gameplay takes place.
type MyGameplayDispatcher () =
    inherit ScreenDispatcher<unit, unit, GameplayCommand> ()

    // here we define the Bindings used to connect events to their desired commands
    override this.Bindings (_, _, _) =
        [Simulants.Game.KeyboardKeyDownEvent =|>! fun evt ->
            if evt.Data.ScanCode = int SDL.SDL_Scancode.SDL_SCANCODE_UP && not evt.Data.Repeated
            then Jump
            else Nil
         Simulants.Gameplay.UpdateEvent =|>! fun _ ->
            if KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) then MoveLeft
            elif KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) then MoveRight
            else Nil
         Simulants.Gameplay.UpdateEvent =>! EyeTrack]

    // here we handle the above commands
    override this.Command (command, _, _, world) =
        match command with
        | Jump ->
            let physicsId = Simulants.Player.GetPhysicsId world
            if World.isBodyOnGround physicsId world
            then World.applyBodyForce (v2 0.0f 2000000.0f) physicsId world
            else world
        | MoveLeft ->
            let physicsId = Simulants.Player.GetPhysicsId world
            if World.isBodyOnGround physicsId world
            then World.applyBodyForce (v2 -30000.0f 0.0f) physicsId world
            else World.applyBodyForce (v2 -7500.0f 0.0f) physicsId world
        | MoveRight ->
            let physicsId = Simulants.Player.GetPhysicsId world
            if World.isBodyOnGround physicsId world
            then World.applyBodyForce (v2 30000.0f 0.0f) physicsId world
            else World.applyBodyForce (v2 7500.0f 0.0f) physicsId world
        | EyeTrack ->
            Simulants.Game.SetEyeCenter (Simulants.Player.GetCenter world) world
        | Nil -> world

    // here we describe the content of the game including the player and the level
    override this.Content (_, _, _) =
        [Content.layer Simulants.Scene []
            [Content.character Simulants.Player
                [Entity.Position == v2 0.0f 0.0f
                 Entity.Size == v2 144.0f 144.0f]
             Content.button Simulants.Back
                [Entity.Text == "Back"
                 Entity.Position == v2 220.0f -260.0f
                 Entity.Depth == 10.0f]]
         Content.layerFromFile Simulants.Level
            "Assets/Gui/Level.nulyr"]