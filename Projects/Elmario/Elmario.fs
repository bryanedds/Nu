namespace Elmario
open Prime
open Nu
open Nu.Declarative
module Simulants =

    // here we create an entity reference for Elmario. This is useful for simulants that you want
    // to refer to from multiple places
    let Elmario = Default.Layer / "Elmario"

// this is our Elm-style command type
type Command =
    | Jump
    | MoveLeft
    | MoveRight
    | Nop

// this is our Elm-style game dispatcher
type ElmarioDispatcher () =
    inherit GameDispatcher<unit, unit, Command> (())

    // here we channel from events to signals
    override this.Channel (_, game, _) =
        [game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated
            then [cmd Jump]
            else [cmd Nop]
         game.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown KeyboardKey.Left then [cmd MoveLeft]
            elif KeyboardState.isKeyDown KeyboardKey.Right then [cmd MoveRight]
            else [cmd Nop]]

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        let world =
            match command with
            | MoveLeft ->
                let physicsId = Simulants.Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 -3000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 -750.0f 0.0f) physicsId world
            | MoveRight ->
                let physicsId = Simulants.Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world
                then World.applyBodyForce (v2 3000.0f 0.0f) physicsId world
                else World.applyBodyForce (v2 750.0f 0.0f) physicsId world
            | Jump ->
                let physicsId = Simulants.Elmario.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.applyBodyForce (v2 0.0f 175000.0f) physicsId world
                    World.playSound Constants.Audio.DefaultSoundVolume (asset "Gameplay" "Jump") world
                else world
            | Nop -> world
        just world

    // here we describe the content of the game including elmario, the ground he walks on, and a rock.
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