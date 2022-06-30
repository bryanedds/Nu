namespace Scenery
open Prime
open Nu
open Nu.Declarative

[<RequireQualifiedAccess>]
module Simulants =

    // here we create an entity reference for SkyBox. This is useful for simulants that you want
    // to refer to from multiple places
    let SkyBox = Simulants.Default.Group / "SkyBox"

// this is our Elm-style command type
type Command =
    | MoveLeft
    | MoveRight
    | MoveForward
    | MoveBackward
    | Nop

// this is our Elm-style game dispatcher
type SceneryDispatcher () =
    inherit GameDispatcher<unit, unit, Command> (())

    // here we channel from events to signals
    override this.Channel (_, game) =
        [game.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown KeyboardKey.Left then cmd MoveLeft
            elif KeyboardState.isKeyDown KeyboardKey.Right then cmd MoveRight
            elif KeyboardState.isKeyDown KeyboardKey.Up then cmd MoveForward
            elif KeyboardState.isKeyDown KeyboardKey.Down then cmd MoveBackward
            else cmd Nop]

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        let speed = 0.5f
        let world =
            match command with
            | MoveLeft -> World.setEyePosition3d (World.getEyePosition3d world + v3Left * speed) world
            | MoveRight -> World.setEyePosition3d (World.getEyePosition3d world + v3Right * speed) world
            | MoveForward -> World.setEyePosition3d (World.getEyePosition3d world + v3Forward * speed) world
            | MoveBackward -> World.setEyePosition3d (World.getEyePosition3d world + v3Backward * speed) world
            | Nop -> world
        just world

    // here we describe the content of the game including Scenery, the ground he walks on, and a rock.
    override this.Content (_, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.skyBox Simulants.SkyBox.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 200.0f -150.0f 0.0f]]]]

    override this.Register (entity, world) =
        let world = base.Register (entity, world)
        let max = 1000.0f
        let positions = Array.init 100000 (fun _ -> v3 (Gen.randomf1 max) (Gen.randomf1 max) (Gen.randomf1 max) - v3Dup (max * 0.5f))
        Array.fold (fun world position ->
            let (staticModel, world) = World.createEntity<StaticModelDispatcher> None DefaultOverlay Simulants.Default.Group world
            staticModel.SetPosition position world)
            world positions