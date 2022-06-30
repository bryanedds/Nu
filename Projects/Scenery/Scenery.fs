namespace Scenery
open System.Collections.Generic
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
        let speed = 0.5f // ~67.1mph
        let world =
            match command with
            | MoveLeft -> World.setEyePosition3d (World.getEyePosition3d world + v3Left * speed) world
            | MoveRight -> World.setEyePosition3d (World.getEyePosition3d world + v3Right * speed) world
            | MoveForward -> World.setEyePosition3d (World.getEyePosition3d world + v3Forward * speed) world
            | MoveBackward -> World.setEyePosition3d (World.getEyePosition3d world + v3Backward * speed) world
            | Nop -> world
        just world

    // here we describe the content of the game
    override this.Content (_, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.skyBox Simulants.SkyBox.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]]]]

    // here we create the scenery in an imperative fashion
    // NOTE: performance goal: 60fps, current: 43fps.
    override this.Register (entity, world) =
        let world = base.Register (entity, world)
        let population = 60
        let spread = 12.0f
        let offset = v3Dup spread * single population * 0.5f
        let positions = List ()
        for i in 0 .. population do
            for j in 0 .. population do
                for k in 0 .. population do
                    let random = v3 (Gen.randomf1 spread) (Gen.randomf1 spread) (Gen.randomf1 spread) - v3Dup (spread * 0.5f)
                    let position = v3 (single i) (single j) (single k) * spread + random - offset
                    positions.Add position
        Seq.fold (fun world position ->
            let (staticModel, world) = World.createEntity<StaticModelDispatcher> None NoOverlay Simulants.Default.Group world
            let world = staticModel.SetPosition position world
            let world = staticModel.SetScale (v3Dup 1.5f) world
            world)
            world positions