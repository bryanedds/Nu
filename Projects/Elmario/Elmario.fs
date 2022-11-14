namespace Elmario
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // here we create an entity reference for Elmario. This is useful for simulants that you want
    // to refer to from multiple places
    let Elmario = Simulants.Default.Group / "Elmario"

// this is our Elm-style command type
type Command =
    | Update
    | Jump
    | Nop

// this is our Elm-style game dispatcher
type ElmarioDispatcher () =
    inherit GameDispatcher<unit, unit, Command> (())

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        match command with
        | Update ->
            let physicsId = Simulants.Elmario.GetPhysicsId world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                let world =
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 -2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 -750.0f 0.0f 0.0f) physicsId world
                just world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then
                let world =
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 750.0f 0.0f 0.0f) physicsId world
                just world
            else just world
        | Jump ->
            let physicsId = Simulants.Elmario.GetPhysicsId world
            if World.isBodyOnGround physicsId world then
                let world = World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                let world = World.applyBodyForce (v3 0.0f 140000.0f 0.0f) physicsId world
                just world
            else just world
        | Nop -> just world

    // here we describe the content of the game including elmario, the ground he walks on, and a rock.
    override this.Content (_, _) =
        Content.game
            [Game.UpdateEvent ==> cmd Update
             Game.KeyboardKeyDownEvent ==|> fun evt -> if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated then cmd Jump else cmd Nop]
            [Content.screen Simulants.Default.Screen.Name Vanilla []
                [Content.group Simulants.Default.Group.Name []
                    [Content.sideViewCharacter Simulants.Elmario.Name
                        [Entity.Position <== v3 0.0f 0.0f 0.0f
                         Entity.Size <== v3 108.0f 108.0f 0.0f]
                     Content.block2d "Ground"
                        [Entity.Position <== v3 -384.0f -256.0f 0.0f
                         Entity.Size <== v3 768.0f 64.0f 0.0f
                         Entity.StaticImage <== asset "Gameplay" "TreeTop"]
                     Content.block2d "Rock"
                        [Entity.Position <== v3 320.0f -192.0f 0.0f
                         Entity.Size <== v3 64.0f 64.0f 0.0f
                         Entity.StaticImage <== asset "Gameplay" "Rock"]]]]