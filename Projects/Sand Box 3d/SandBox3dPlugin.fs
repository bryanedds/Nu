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

    let WallColor = Palette.BaseColorValues.[0]

    let createWallColumnModel corner (affine : Affine) (parent : Entity) world =
        let name = "Wall" + string Gen.id64
        let surnames = Array.add name parent.Surnames
        let entity = World.createEntity<RigidModelDispatcher> (Some Address.parent) NoOverlay (Some surnames) parent.Group world
        let translation =
            match corner with
            | 0 -> v3 -0.1f 0.0f 0.1f
            | 1 -> v3 -0.1f 0.0f -0.1f
            | 2 -> v3 0.1f 0.0f -0.1f
            | _ -> v3 0.1f 0.0f 0.1f
        let scale = v3 0.5f 3.0f 0.5f
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
                | Some false -> (v3 -0.25f 0.0f 0.0f, v3 0.5f 3.0f 0.5f)
                | Some true -> (v3 0.25f 0.0f 0.0f, v3 0.5f 3.0f 0.5f)
                | None -> (v3Zero, v3 1.0f 3.0f 0.5f)
            else
                match halfDirectionOpt with
                | Some false -> (v3 0.0f 0.0f -0.25f, v3 0.5f 3.0f 0.5f)
                | Some true -> (v3 0.0f 0.0f 0.25f, v3 0.5f 3.0f 0.5f)
                | None -> (v3Zero, v3 0.5f 3.0f 1.0f)
        entity.SetPositionLocal (translation + affine.Translation) world
        entity.SetRotationLocal affine.Rotation world
        entity.SetScaleLocal (scale * affine.Scale) world
        entity.SetStaticModel Assets.Default.StaticModel world

    let wallSlice (center : Vector3i) chunk =
        match chunk.Blocks.TryGetValue center with
        | (true, middleBlock) when middleBlock.Color = WallColor ->
            match chunk.Blocks.TryGetValue (center + v3iUp) with
            | (true, topBlock) when topBlock.Color = WallColor ->
                match chunk.Blocks.TryGetValue (v3iOne + v3iDown) with
                | (true, bottomBlock) when bottomBlock.Color = WallColor -> Some (bottomBlock, middleBlock, topBlock)
                | (_, _) -> None
            | (_, _) -> None
        | (_, _) -> None

    let wall _ affine _ chunk =

        // determine wall
        let center = v3iOne
        let centerWallOpt = wallSlice center chunk
        if centerWallOpt.IsSome then

            // 4-way
            let forwardWallOpt = wallSlice (center + v3iForward) chunk
            let rightWallOpt = wallSlice (center + v3iRight) chunk
            let backWallOpt = wallSlice (center + v3iBack) chunk
            let leftWallOpt = wallSlice (center + v3iLeft) chunk
            if forwardWallOpt.IsSome && rightWallOpt.IsSome && backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true None affine parent world
                Some (effect, chunk)

            // 3-ways
            elif forwardWallOpt.IsSome && backWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true (Some true) affine parent world
                Some (effect, chunk)
            elif forwardWallOpt.IsSome && backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false None affine parent world
                    createWallModel true (Some false) affine parent world
                Some (effect, chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome && forwardWallOpt.IsSome then
                let effect parent world =
                    createWallModel true None affine parent world
                    createWallModel false (Some false) affine parent world
                Some (effect, chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome && backWallOpt.IsSome then
                let effect parent world =
                    createWallModel true None affine parent world
                    createWallModel false (Some true) affine parent world
                Some (effect, chunk)

            // corners
            elif forwardWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some false) affine parent world
                    createWallModel true (Some true) affine parent world
                    createWallColumnModel 0 affine parent world
                Some (effect, chunk)
            elif backWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some true) affine parent world
                    createWallModel true (Some true) affine parent world
                    createWallColumnModel 1 affine parent world
                Some (effect, chunk)
            elif backWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some true) affine parent world
                    createWallModel true (Some false) affine parent world
                    createWallColumnModel 2 affine parent world
                Some (effect, chunk)
            elif forwardWallOpt.IsSome && leftWallOpt.IsSome then
                let effect parent world =
                    createWallModel false (Some false) affine parent world
                    createWallModel true (Some false) affine parent world
                    createWallColumnModel 3 affine parent world
                Some (effect, chunk)

            // flats
            elif forwardWallOpt.IsSome && backWallOpt.IsSome then
                let effect parent world = createWallModel false None affine parent world
                Some (effect, chunk)
            elif leftWallOpt.IsSome && rightWallOpt.IsSome then
                let effect parent world = createWallModel true None affine parent world
                Some (effect, chunk)

            // termins
            elif forwardWallOpt.IsSome || backWallOpt.IsSome then
                let effect parent world = createWallModel false None affine parent world
                Some (effect, chunk)
            elif rightWallOpt.IsSome || leftWallOpt.IsSome then
                let effect parent world = createWallModel true None affine parent world
                Some (effect, chunk)

            // no determinable wall
            else None

        // not wall
        else None

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
        let fns = Map.add "Wall" (v3iDup 3, wall) fns
        fns