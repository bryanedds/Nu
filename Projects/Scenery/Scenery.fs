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

    override this.Update (entity, world) =
        entity.SetAngles (entity.GetAngles world + v3Dup 0.01f) world

// this is our Elm-style command type
type Command =
    | MoveLeft
    | MoveRight
    | MoveForward
    | MoveBackward
    | MoveUpward
    | MoveDownward
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
            elif KeyboardState.isKeyDown KeyboardKey.W then cmd MoveUpward
            elif KeyboardState.isKeyDown KeyboardKey.S then cmd MoveDownward
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
            | MoveUpward -> World.setEyePosition3d (World.getEyePosition3d world + v3Up * speed) world
            | MoveDownward -> World.setEyePosition3d (World.getEyePosition3d world + v3Down * speed) world
            | Nop -> world
        just world

    // here we describe the content of the game
    override this.Content (_, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.light Simulants.Light.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Color == Color.Wheat
                     Entity.Brightness == 10.0f
                     Entity.Intensity == 1.0f]
                 Content.skyBox Gen.name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]]]]

    // here we create the scenery in an imperative fashion
    // NOTE: performance goal: 60fps, current: 33fps.
    override this.Register (entity, world) =
        let world = base.Register (entity, world)
        let world =
            let staticModel = asset "Default" "GameObject"
            match World.tryGetStaticModelMetadata staticModel world with
            | Some staticModelMetadata ->
                // Unity Scene Export Instruction:
                //
                // 1) have FBX Exporter package installed
                // 2) be in PBR Unity Project	
                // 3) put all desired objects in empty root GameObject
                // 4) export root GameObject
                // 5) delete all fbx files except the one you exported
                // 6) instantiate entities into scene like so -
                Seq.foldi (fun surfaceIndex world (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                    let (staticModelSurface, world) = World.createEntity<StaticModelSurfaceDispatcher> None NoOverlay Simulants.Default.Group world
                    let bounds = surface.SurfaceBounds
                    let boundsExtended = bounds.Combine bounds.Mirror
                    let world = staticModelSurface.SetSurfaceIndex surfaceIndex world
                    let world = staticModelSurface.SetStaticModel staticModel world
                    let world = staticModelSurface.SetSize boundsExtended.Size world
                    let transform = surface.SurfaceMatrix
                    let position = transform.Translation
                    let mutable rotation = transform
                    rotation.Translation <- v3Zero
                    let rotation = Quaternion.CreateFromRotationMatrix rotation
                    let world = staticModelSurface.SetPosition position world
                    let world = staticModelSurface.SetRotation rotation world
                    world)
                    world staticModelMetadata.Surfaces
            | None -> world
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
                //let world = staticModel.SetRenderStyle Forward world
                let world = staticModel.SetPosition position world
                world)
                world positions
        world

    override this.Update (entity, world) =
        let world = base.Update (entity, world)
        let rotationY = single (World.getUpdateTime world) / 60.0f / MathHelper.TwoPi
        let world = World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, rotationY)) world
        let world = Simulants.Light.SetPosition (World.getEyePosition3d world) world
        world
