namespace SandBox3d
open System
open System.Numerics
open Prime
open Nu
open Nu.BlockMap
open SandBox3d

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type SandBox3dPlugin () =
    inherit NuPlugin ()

    let WallIndex = 0

    let createWallColumnModel corner (affine : Affine) (parent : Entity) world =
        let name = "Wall" + string Gen.id64
        let surnames = Array.add name parent.Surnames
        let entity = World.createEntity<RigidModelDispatcher> (Some Address.parent) NoOverlay (Some surnames) parent.Group world
        let translation =
            match corner with
            | 0 -> v3 -0.1f 0.5f 0.1f
            | 1 -> v3 -0.1f 0.5f -0.1f
            | 2 -> v3 0.1f 0.5f -0.1f
            | _ -> v3 0.1f 0.5f 0.1f
        let scale = v3 0.5f 4.0f 0.5f
        entity.SetPositionLocal (translation + affine.Translation) world
        entity.SetRotationLocal affine.Rotation world
        entity.SetScaleLocal (scale * affine.Scale) world
        entity.SetStaticModel Assets.Default.StaticModel world

    let createWallModel lateral halfDirectionOpt (affine : Affine) (parent : Entity) world =
        let name = "Wall" + string Gen.id64
        let surnames = Array.add name parent.Surnames
        let entity = World.createEntity<RigidModelDispatcher> (Some Address.parent) NoOverlay (Some surnames) parent.Group world
        let (translation, scale) =
            if lateral then
                match halfDirectionOpt with
                | Some false -> (v3 -0.25f 0.5f 0.0f, v3 0.5f 4.0f 0.5f)
                | Some true -> (v3 0.25f 0.5f 0.0f, v3 0.5f 4.0f 0.5f)
                | None -> (v3 0.0f 0.5f 0.0f, v3 1.0f 4.0f 0.5f)
            else
                match halfDirectionOpt with
                | Some false -> (v3 0.0f 0.5f -0.25f, v3 0.5f 4.0f 0.5f)
                | Some true -> (v3 0.0f 0.5f 0.25f, v3 0.5f 4.0f 0.5f)
                | None -> (v3 0.0f 0.5f 0.0f, v3 0.5f 4.0f 1.0f)
        entity.SetPositionLocal (translation + affine.Translation) world
        entity.SetRotationLocal affine.Rotation world
        entity.SetScaleLocal (scale * affine.Scale) world
        entity.SetStaticModel Assets.Default.StaticModel world

    let wallSlice (bottom : Vector3i) consumerOpt chunk =
        match chunk.Blocks.TryGetValue bottom with
        | (true, block0) when block0.StyleIndex = WallIndex && Block.getAvailable consumerOpt block0 ->
            match chunk.Blocks.TryGetValue (bottom + v3iUp) with
            | (true, block1) when block1.StyleIndex = WallIndex && Block.getAvailable consumerOpt block1 ->
                match chunk.Blocks.TryGetValue (bottom + v3iUp * 2) with
                | (true, block2) when block2.StyleIndex = WallIndex && Block.getAvailable consumerOpt block2 ->
                    match chunk.Blocks.TryGetValue (bottom + v3iUp * 3) with
                    | (true, block3) when block3.StyleIndex = WallIndex && Block.getAvailable consumerOpt block3 ->
                        Some [|block0; block1; block2; block3|]
                    | (_, _) -> None
                | (_, _) -> None
            | (_, _) -> None
        | (_, _) -> None

    let wall _ affine _ consumer chunk =

        // determine wall
        let bottom = v3i 1 0 1
        match wallSlice bottom (Some consumer) chunk with
        | Some centerWall ->

            // 4-way
            let forwardWallOpt = wallSlice (bottom + v3iForward) None chunk
            let rightWallOpt = wallSlice (bottom + v3iRight) None chunk
            let backWallOpt = wallSlice (bottom + v3iBack) None chunk
            let leftWallOpt = wallSlice (bottom + v3iLeft) None chunk
            if forwardWallOpt.IsSome && rightWallOpt.IsSome && backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true None affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)

            // 3-ways
            elif forwardWallOpt.IsSome && backWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true (Some true) affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif forwardWallOpt.IsSome && backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true (Some false) affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome && forwardWallOpt.IsSome then
                let effect parent world =
                    createWallModel true None affine parent world
                    createWallModel false (Some false) affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome && backWallOpt.IsSome then
                let effect parent world =
                    createWallModel true None affine parent world
                    createWallModel false (Some true) affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)

            // corners
            elif forwardWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some false) affine parent world
                    createWallModel true (Some true) affine parent world
                    createWallColumnModel 0 affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif backWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some true) affine parent world
                    createWallModel true (Some true) affine parent world
                    createWallColumnModel 1 affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some true) affine parent world
                    createWallModel true (Some false) affine parent world
                    createWallColumnModel 2 affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif forwardWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some false) affine parent world
                    createWallModel true (Some false) affine parent world
                    createWallColumnModel 3 affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)

            // flats
            elif forwardWallOpt.IsSome && backWallOpt.IsSome then
                let effect parent world = createWallModel false None affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world = createWallModel true None affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)

            // termins
            elif forwardWallOpt.IsSome || backWallOpt.IsSome then
                let effect parent world = createWallModel false None affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)
            elif rightWallOpt.IsSome || leftWallOpt.IsSome then
                let effect parent world = createWallModel true None affine parent world
                Some (effect, Chunk.consumeBlocks centerWall consumer chunk)

            // no determinable wall
            else None

        // not wall
        | Some _ | None -> None

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetGameState Splash world)
             ("Title", fun world -> Game.SetGameState Title world)
             ("Credits", fun world -> Game.SetGameState Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplayState Playing world
                Game.SetGameState Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]

    override this.ProcessFns =
        let fns = base.ProcessFns
        let fns = Map.add "Wall" (v3i 3 4 3, wall) fns
        fns