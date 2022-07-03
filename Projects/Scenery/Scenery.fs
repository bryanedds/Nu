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
    let PointLight = Simulants.Default.Group / "PointLight"

// this is a custom entity for performance testing
type RotatingModelDispatcher () =
    inherit EntityDispatcher3d (true, false)

    static member Facets =
        [typeof<StaticModelFacet>]

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel]

    override this.Update (entity, world) =
        entity.SetAngles (entity.GetAngles world + v3Dup 0.01f) world

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
                [Content.pointLight Simulants.PointLight.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Color == (color 0.0f 100.0f 100.0f 1.0f)]
                 Content.skyBox Gen.name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]]]]

    // here we create the scenery in an imperative fashion
    // NOTE: performance goal: 60fps, current: 33fps.
    override this.Register (entity, world) =
        let world = base.Register (entity, world)
#if DEBUG
        let population = 10
#else
        let population = 50
#endif
        let spread = 16.0f
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
                let world = staticModel.SetScale (v3Dup 1.5f) world
                //let world = staticModel.SetRenderStyle (Forward None) world
                staticModel.SetPosition position world)
                world positions
        world

    override this.Update (entity, world) =
        let world = base.Update (entity, world)
        let rotationY = single (World.getUpdateTime world) / 60.0f / MathHelper.TwoPi
        //let world = World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, rotationY)) world
        let world = Simulants.PointLight.SetPosition (World.getEyePosition3d world) world
        world
