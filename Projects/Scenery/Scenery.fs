namespace Scenery
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<RequireQualifiedAccess>]
module Simulants =

    // here we create an entity reference for SkyBox. This is useful for simulants that you want
    // to refer to from multiple places
    let Light = Simulants.Default.Group / "Light"

// this is a custom entity for performance testing
type RotatingModelDispatcher () =
    inherit EntityDispatcher3d (true, false)

    static member Facets =
        [typeof<StaticModelFacet>]

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel]

// this is our Elm-style command type
type Command =
    | MoveLeft
    | MoveRight
    | MoveForward
    | MoveBack
    | MoveUpward
    | MoveDown
    | TurnLeft
    | TurnRight
    | Nop

// this is our Elm-style game dispatcher
type SceneryDispatcher () =
    inherit GameDispatcher<unit, unit, Command> (())

    // here we channel from events to signals
    override this.Channel (_, game) =
        [game.UpdateEvent =|> fun _ ->
            if KeyboardState.isKeyDown KeyboardKey.Left then cmd TurnLeft
            elif KeyboardState.isKeyDown KeyboardKey.Right then cmd TurnRight
            elif KeyboardState.isKeyDown KeyboardKey.Up then cmd MoveForward
            elif KeyboardState.isKeyDown KeyboardKey.Down then cmd MoveBack
            elif KeyboardState.isKeyDown KeyboardKey.W then cmd MoveUpward
            elif KeyboardState.isKeyDown KeyboardKey.S then cmd MoveDown
            elif KeyboardState.isKeyDown KeyboardKey.A then cmd MoveLeft
            elif KeyboardState.isKeyDown KeyboardKey.D then cmd MoveRight
            else cmd Nop]

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        let moveSpeed = 0.1f
        let turnSpeed = 0.05f
        let rotation = World.getEyeRotation3d world
        let world =
            match command with
            | TurnLeft -> World.setEyeRotation3d (World.getEyeRotation3d world * Quaternion.CreateFromAxisAngle (v3Up, turnSpeed)) world
            | TurnRight -> World.setEyeRotation3d (World.getEyeRotation3d world * Quaternion.CreateFromAxisAngle (v3Down, turnSpeed)) world
            | MoveLeft -> World.setEyePosition3d (World.getEyePosition3d world + Vector3.Transform (v3Left, rotation) * moveSpeed) world
            | MoveRight -> World.setEyePosition3d (World.getEyePosition3d world + Vector3.Transform (v3Right, rotation) * moveSpeed) world
            | MoveForward -> World.setEyePosition3d (World.getEyePosition3d world + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
            | MoveBack -> World.setEyePosition3d (World.getEyePosition3d world + Vector3.Transform (v3Back, rotation) * moveSpeed) world
            | MoveUpward -> World.setEyePosition3d (World.getEyePosition3d world + v3Up * moveSpeed) world
            | MoveDown -> World.setEyePosition3d (World.getEyePosition3d world + v3Down * moveSpeed) world
            | Nop -> world
        just world

    // here we describe the content of the game
    override this.Content (_, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.light Simulants.Light.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Color == Color.White
                     Entity.Brightness == 100.0f
                     Entity.Intensity == 1.0f]
                 Content.skyBox Gen.name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]]]]

    // here we create the scenery in an imperative fashion
    // NOTE: performance goal: 60fps, current: 33fps.
    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (_, world) = World.createEntity<StaticSceneDispatcher> (Some [|"StaticScene"|]) DefaultOverlay Simulants.Default.Group world
#if DEBUG
        let population = 5
#else
        let population = 40
#endif
        let spread = 20.0f
        let offset = v3Dup spread * single population * 0.5f
        let positions = List ()
        for i in 0 .. population do
            for j in 0 .. population do
                for k in 0 .. population do
                    let random = v3 (Gen.randomf1 spread) (Gen.randomf1 spread) (Gen.randomf1 spread) - v3Dup (spread * 0.5f)
                    let position = v3 (single i) (single j) (single k) * spread + random - offset
                    positions.Add position
        let world =
            Seq.fold (fun world position ->
                let (staticModel, world) = World.createEntity<RotatingModelDispatcher> None NoOverlay Simulants.Default.Group world
                let world = staticModel.SetPosition position world
                //let world = staticModel.SetRenderStyle Forward world
                //let world = staticModel.SetStatic true world
                world)
                world positions
        world

    override this.PostUpdate (entity, world) =
        let world = base.PostUpdate (entity, world)
        let world = Simulants.Light.SetPosition (World.getEyePosition3d world + v3Up * 3.0f) world
        world
