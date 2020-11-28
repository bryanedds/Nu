namespace MyGame
open Prime
open Nu
open Nu.Declarative

// this is our Elm-style model type. Currently it's unit because do not yet have any state to model.
type Gameplay =
    unit

// this is our Elm-style model type. Currently it's unit because do not yet have any messages.
type GameplayMessage =
    unit

// this is our Elm-style command type. Commands are used instead of messages when things like physics are involved.
type GameplayCommand =
    | MoveLeft
    | MoveRight
    | Jump
    | EyeTrack
    | Back
    | Nop

// this is the screen dispatcher that defines the screen where gameplay takes place
type MyGameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> ()

    // here we channel from events to signals
    override this.Channel (_, _) =
        [Simulants.Game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated then cmd Jump
            else cmd Nop
         Simulants.Gameplay.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown KeyboardKey.Left then cmd MoveLeft
            elif KeyboardState.isKeyDown KeyboardKey.Right then cmd MoveRight
            else cmd Nop
         Simulants.Gameplay.PostUpdateEvent => cmd EyeTrack]

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        let world =
            match command with
            | MoveLeft ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 -2000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 -500.0f 0.0f) physicsId world
            | MoveRight ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 2000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 500.0f 0.0f) physicsId world
            | Jump ->
                let physicsId = Simulants.Player.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.applyBodyForce (v2 0.0f 90000.0f) physicsId world
                    World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                else world
            | EyeTrack ->
                if World.getTickRate world <> 0L
                then Simulants.Game.SetEyeCenter (Simulants.Player.GetCenter world) world
                else world
            | Back ->
                World.transitionScreen Simulants.Title world
            | Nop -> world
        just world

    // here we describe the content of the game including the level, the hud, and the player
    override this.Content (_, screen) =

        [// the level layer
         Content.layerIfScreenSelected screen $ fun _ _ ->
            Content.layerFromFile
                Simulants.Level.Name
                "Assets/Gameplay/Level.nulyr"

         // the hud layer
         Content.layerIfScreenSelected screen $ fun _ _ ->
            Content.layer "Hud" []
                [Content.button Gen.name
                    [Entity.Text == "Back"
                     Entity.Position == v2 260.0f -260.0f
                     Entity.Depth == 10.0f
                     Entity.ClickEvent ==> cmd Back]]

         // the player layer
         Content.layerIfScreenSelected screen $ fun _ _ ->
            Content.layer Simulants.Scene.Name []
                [Content.character Simulants.Player.Name
                    [Entity.Position == v2 0.0f 0.0f
                     Entity.Size == v2 108.0f 108.0f]]]