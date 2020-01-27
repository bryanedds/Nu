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
    | Nop

// this is the screen dispatcher that defines the screen where gameplay takes place.
type MyGameplayDispatcher () =
    inherit ScreenDispatcher<unit, unit, GameplayCommand> ()

    // here we define the Bindings used to connect events to their desired commands
    override this.Bindings (_, _, _) =
        [Simulants.Game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.ScanCode = int SDL.SDL_Scancode.SDL_SCANCODE_UP && not evt.Data.Repeated
            then cmd Jump
            else cmd Nop
         Simulants.Gameplay.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) then cmd MoveLeft
            elif KeyboardState.isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) then cmd MoveRight
            else cmd Nop
         Simulants.Gameplay.UpdateEvent => cmd EyeTrack]

    // here we handle the above commands
    override this.Command (command, _, _, world) =
        let world =
            match command with
            | Jump ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.applyBodyForce (v2 0.0f 2000000.0f) physicsId world
                    World.playSound 1.0f (asset "Gameplay" "Jump") world
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
                if World.getTickRate world <> 0L
                then Simulants.Game.SetEyeCenter (Simulants.Player.GetCenter world) world
                else world
            | Nop -> world
        just world

    // here we describe the content of the game including the player and the level
    override this.Content (_, _, _) =
        [Content.layer Simulants.Hud.Name []
            [Content.button Simulants.Back.Name
                [Entity.Text == "Back"
                 Entity.Position == v2 220.0f -260.0f
                 Entity.Depth == 10.0f]]
         Content.layerIfScreenSelected Simulants.Gameplay $ fun _ _ _ ->
            Content.layer Simulants.Scene.Name []
                [Content.character Simulants.Player.Name
                    [Entity.Position == v2 0.0f 0.0f
                     Entity.Size == v2 144.0f 144.0f]]
         Content.layerIfScreenSelected Simulants.Gameplay $ fun _ _ _ ->
            Content.layerFromFile Simulants.Level.Name "Assets/Gameplay/Level.nulyr"]