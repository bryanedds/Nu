namespace MyGame
open Prime
open Nu
open Nu.Declarative

// this is our Elm-style model type. Currently it's unit because do not yet have any state to model.
type GameplayModel =
    unit

// this is our Elm-style model type. Currently it's unit because do not yet have any messages.
type GameplayMessage =
    unit

// this is our Elm-style command type. Commands are used instead of messages when things like physics are involved.
type GameplayCommand =
    | Jump
    | MoveLeft
    | MoveRight
    | EyeTrack
    | Nop

// this is the screen dispatcher that defines the screen where gameplay takes place.
type MyGameplayDispatcher () =
    inherit ScreenDispatcher<GameplayModel, GameplayMessage, GameplayCommand> ()

    // here we define the bindings used to connect events to their desired commands
    override this.Bindings (_, _, _) =
        [Simulants.Game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated
            then cmd Jump
            else cmd Nop
         Simulants.Gameplay.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown KeyboardKey.Left then cmd MoveLeft
            elif KeyboardState.isKeyDown KeyboardKey.Right then cmd MoveRight
            else cmd Nop
         Simulants.Gameplay.UpdateEvent => cmd EyeTrack]

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        let world =
            match command with
            | Jump ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.applyBodyForce (v2 0.0f 200000.0f) physicsId world
                    World.playSound 1.0f (asset "Gameplay" "Jump") world
                else world
            | MoveLeft ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 -3000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 -750.0f 0.0f) physicsId world
            | MoveRight ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 3000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 750.0f 0.0f) physicsId world
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
         Content.layerIfScreenSelected Simulants.Gameplay $ fun _ _ ->
            Content.layer Simulants.Scene.Name []
                [Content.character Simulants.Player.Name
                    [Entity.Position == v2 0.0f 0.0f
                     Entity.Size == v2 144.0f 144.0f]]
         Content.layerIfScreenSelected Simulants.Gameplay $ fun _ _ ->
            Content.layerFromFile Simulants.Level.Name "Assets/Gameplay/Level.nulyr"]