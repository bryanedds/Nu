namespace Elmario
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =
    let Screen = Game / "Screen"
    let Group = Screen / "Group"
    let Elmario = Group / "Elmario"

// this is our MMCC command type
type ElmarioCommand =
    | Update
    | Jump
    | Nop
    interface Command

// this is our MMCC game dispatcher
type ElmarioDispatcher () =
    inherit GameDispatcher<unit, Message, ElmarioCommand> (())

    // here we define the game's properties and event handling
    override this.Definitions (_, _) =
        [Game.UpdateEvent => Update
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated
            then Jump
            else Nop]

    // here we handle the MMCC commands
    override this.Command (_, command, _, world) =
        match command with
        | Update ->
            let bodyId = Simulants.Elmario.GetBodyId world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                let world =
                    if World.getBodyGrounded bodyId world
                    then World.applyBodyForce (v3 -1600.0f 0.0f 0.0f) v3Zero bodyId world
                    else World.applyBodyForce (v3 -400.0f 0.0f 0.0f) v3Zero bodyId world
                just world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then
                let world =
                    if World.getBodyGrounded bodyId world
                    then World.applyBodyForce (v3 1600.0f 0.0f 0.0f) v3Zero bodyId world
                    else World.applyBodyForce (v3 400.0f 0.0f 0.0f) v3Zero bodyId world
                just world
            else just world
        | Jump ->
            let bodyId = Simulants.Elmario.GetBodyId world
            if world.Advancing && World.getBodyGrounded bodyId world then
                let world = World.applyBodyLinearImpulse (v3 0.0f 1600.0f 0.0f) v3Zero bodyId world
                World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                just world
            else just world
        | Nop -> just world

    // here we describe the content of the game including elmario, the ground he walks on, and a rock.
    override this.Content (_, _) =
        [Content.screen Simulants.Screen.Name Vanilla []
            [Content.group Simulants.Group.Name []
                [Content.character2d Simulants.Elmario.Name
                    [Entity.Position == v3 0.0f 32.0f 0.0f
                     Entity.Size == v3 56.0f 56.0f 0.0f]
                 Content.block2d "Ground"
                    [Entity.Position == v3 0.0f -128.0f 0.0f
                     Entity.Size == v3 384.0f 32.0f 0.0f
                     Entity.StaticImage == asset "Gameplay" "TreeTop"]
                 Content.block2d "Rock"
                    [Entity.Position == v3 176.0f -96.0f 0.0f
                     Entity.Size == v3 32.0f 32.0f 0.0f
                     Entity.StaticImage == asset "Gameplay" "Rock"]]]]