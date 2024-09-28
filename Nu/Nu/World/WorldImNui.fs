// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// Describes an ImNui screen result.
type ScreenResult =
    | Select
    | IncomingStart
    | IncomingFinish
    | OutgoingStart
    | OutgoingFinish
    | Deselecting

/// Describes an ImNui physics body result.
type BodyResult =
    | BodyPenetration of BodyPenetrationData
    | BodySeparationExplicit of BodySeparationExplicitData
    | BodySeparationImplicit of BodySeparationImplicitData
    | BodyTransform of BodyTransformData

[<AutoOpen>]
module WorldImNui =

    /// Instructs ImNui static equality (.=) to act as dynamic equality (@=) for a frame.
    let mutable internal Reinitializing = false

    /// Specifies a dynamic ImNui argument.
    let
#if !DEBUG
        inline
#endif
        (@=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = false; ArgLens = lens; ArgValue = value } : 's ArgImNui

    /// Specifies a static ImNui argument.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = true; ArgLens = lens; ArgValue = value } : 's ArgImNui

    type World with

        ///
        static member initBodyResult mapResult (entity : Entity) world =
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyPenetration event.Data) world)) entity.BodyPenetrationEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationExplicit event.Data) world)) entity.BodySeparationExplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationImplicit event.Data) world)) entity.BodySeparationImplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyTransform event.Data) world)) entity.BodyTransformEvent entity world
            world

        /// Clear the current ImNui context.
        static member scopeWorld world =
            World.setContext Address.empty world

        /// Begin the ImNui declaration of a game with the given arguments.
        static member beginGamePlus<'r> (zero : 'r) init (args : Game ArgImNui seq) (world : World) : 'r * World =
            if world.ContextImNui.Names.Length > 0 then raise (InvalidOperationException "ImNui game declared outside of valid ImNui context (must be called in World context).")
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ContextImNui.Names)
            let world = World.setContext gameAddress world
            let game = Nu.Game gameAddress
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue game with
                | (true, gameImNui) -> (false, World.utilizeSimulantImNui game gameImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui game { Utilized = true; Result = zero } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapGameImNui gameImNui = { gameImNui with Result = mapper (gameImNui.Result :?> 'r) }
                        World.tryMapSimulantImNui mapGameImNui game world
                    (true, init mapResult game world)
            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if initializing || not arg.ArgStatic
                        then game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let result = (World.getSimulantImNui game world).Result :?> 'r
            let world = World.mapSimulantImNui (fun simulantImNui -> { simulantImNui with Result = zero }) game world
            (result, world)

        /// Begin the ImNui declaration of a game with the given arguments.
        static member beginGame world args =
            World.beginGamePlus<unit> () (fun _ _ world -> world) args world |> snd

        /// End the ImNui declaration of a group with the given arguments.
        static member endGame (world : World) =
            match world.ContextImNui with
            | :? (Game Address) -> World.setContext Address.empty world
            | _ -> raise (InvalidOperationException "World.beginGame mismatch.")

        /// Make the game the current ImNui context.
        static member scopeGame (args : Game ArgImNui seq) world =
            let game = Game
            let world = World.setContext game.GameAddress world
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c')
                world args

        static member internal beginScreenPlus10<'d, 'r when 'd :> ScreenDispatcher> (zero : 'r) init transitionScreen setScreenSlide name select behavior groupFilePathOpt (args : Screen ArgImNui seq) (world : World) : ScreenResult FQueue * 'r * World =
            if world.ContextImNui.Names.Length < 1 then raise (InvalidOperationException "ImNui screen declared outside of valid ImNui context (must be called in a Game context).")
            let screenAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext screenAddress world
            let screen = Nu.Screen screenAddress
            let world =
                if not (screen.GetExists world) then
                    let world = World.createScreen<'d> (Some name) world |> snd
                    let world = World.setScreenProtected true screen world |> snd'
                    match groupFilePathOpt with
                    | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> snd
                    | None -> world
                else world
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue screen with
                | (true, screenImNui) -> (false, World.utilizeSimulantImNui screen screenImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui screen { Utilized = true; Result = (FQueue.empty<ScreenResult>, zero) } world
                    let mapFstResult (mapper : ScreenResult FQueue -> ScreenResult FQueue) world =
                        let mapScreenImNui screenImNui =
                            let (screenResult, userResult) = screenImNui.Result :?> ScreenResult FQueue * 'r
                            { screenImNui with Result = (mapper screenResult, userResult) }
                        World.tryMapSimulantImNui mapScreenImNui screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj Select) world)) screen.SelectEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj IncomingStart) world)) screen.IncomingStartEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj IncomingFinish) world)) screen.IncomingFinishEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj OutgoingStart) world)) screen.OutgoingStartEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj OutgoingFinish) world)) screen.OutgoingFinishEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapFstResult (FQueue.conj Deselecting) world)) screen.DeselectingEvent screen world
                    let mapSndResult (mapper : 'r -> 'r) world =
                        let mapScreenImNui screenImNui =
                            let (screenResult, userResult) = screenImNui.Result :?> ScreenResult FQueue * 'r
                            { screenImNui with Result = (screenResult, mapper userResult) }
                        World.tryMapSimulantImNui mapScreenImNui screen world
                    (true, init mapSndResult screen world)
            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && screen.GetExists world
                        then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let world = if screen.GetExists world then World.applyScreenBehavior setScreenSlide behavior screen world else world
            let world =
                if screen.GetExists world && select then
                    if world.Accompanied && world.Halted // special case to quick cut when halted in the editor
                    then World.setSelectedScreen screen world
                    else transitionScreen screen world
                else world
            let (screenResult, userResult) = (World.getSimulantImNui screen world).Result :?> ScreenResult FQueue * 'r
            let world = World.mapSimulantImNui (fun simulantImNui -> { simulantImNui with Result = (FQueue.empty<ScreenResult>, zero) }) screen world
            (screenResult, userResult, world)

        static member internal beginScreen8<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name select behavior groupFilePathOpt args world : ScreenResult FQueue * World =
            World.beginScreenPlus10<'d, unit> () (fun _ _ world -> world) transitionScreen setScreenSlide name select behavior groupFilePathOpt args world |> a_c

        /// End the ImNui declaration of a screen.
        static member endScreen (world : World) =
            match world.ContextImNui with
            | :? (Screen Address) -> World.setContext Game.GameAddress world
            | _ -> raise (InvalidOperationException "World.beginScreen mismatch.")

        /// Make a screen the current ImNui context.
        static member scopeScreen (screen : Screen) (args : Screen ArgImNui seq) world =
            let world = World.setContext screen.ScreenAddress world
            Seq.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        static member private beginGroupPlus6<'d, 'r when 'd :> GroupDispatcher> (zero : 'r) init name groupFilePathOpt (args : Group ArgImNui seq) (world : World) : 'r * World =
            if world.ContextImNui.Names.Length < 2 then raise (InvalidOperationException "ImNui group declared outside of valid ImNui context (must be called in a Screen context).")
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext groupAddress world
            let group = Nu.Group groupAddress
            let world =
                if not (group.GetExists world) then
                    let world =
                        match groupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some name) group.Screen world |> snd
                        | None -> World.createGroup<'d> (Some name) group.Screen world |> snd
                    World.setGroupProtected true group world |> snd'
                else world
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue group with
                | (true, groupImNui) -> (false, World.utilizeSimulantImNui group groupImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui group { Utilized = true; Result = () } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapGroupImNui groupImNui = { groupImNui with Result = mapper (groupImNui.Result :?> 'r) }
                        World.tryMapSimulantImNui mapGroupImNui group world
                    (true, init mapResult group world)
            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && group.GetExists world
                        then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let result = (World.getSimulantImNui group world).Result :?> 'r
            let world = World.mapSimulantImNui (fun simulantImNui -> { simulantImNui with Result = zero }) group world
            (result, world)

        static member private beginGroup4<'d> name groupFilePathOpt args world =
            World.beginGroupPlus6 () (fun _ _ world -> world) name groupFilePathOpt args world |> snd

        /// Begin the ImNui declaration of a group read from the given file path with the given arguments.
        static member beginGroupFromFilePlus<'d, 'r when 'd :> GroupDispatcher> zero init name groupFilePath args world =
            World.beginGroupPlus6<'d, 'r> zero init name (Some groupFilePath) args world

        /// Begin the ImNui declaration of a group read from the given file path with the given arguments.
        static member beginGroupFromFile<'d when 'd :> GroupDispatcher> name groupFilePath args world =
            World.beginGroup4<'d> name (Some groupFilePath) args world

        /// Begin the ImNui declaration of a group with the given arguments.
        static member beginGroupPlus<'d, 'r when 'd :> GroupDispatcher> zero init name args world =
            World.beginGroupPlus6<'d, 'r> zero init name None args world

        /// Begin the ImNui declaration of a group with the given arguments.
        static member beginGroup<'d when 'd :> GroupDispatcher> name args world =
            World.beginGroup4<'d> name None args world

        /// End the ImNui declaration of a group.
        static member endGroup (world : World) =
            match world.ContextImNui with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setContext currentAddress world
            | _ -> raise (InvalidOperationException "World.beginGroup mismatch.")

        /// Make a group the current ImNui context.
        static member scopeGroup (group : Group) (args : Group ArgImNui seq) world =
            let world = World.setContext group.GroupAddress world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Begin the ImNui declaration of an entity with the given arguments.
        /// TODO: P1: optimize this for large-scale use.
        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> (zero : 'r) init name (args : Entity ArgImNui seq) (world : World) : 'r * World =
            if world.ContextImNui.Names.Length < 3 then raise (InvalidOperationException "ImNui entity declared outside of valid ImNui context (must be called in either Group or Entity context).")
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let world = 
                if not (entity.GetExists world) then
                    let world = World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                    let world = World.setEntityProtected true entity world |> snd'
                    if entity.Surnames.Length > 1 then entity.SetMountOpt (Some (Relation.makeParent ())) world else world
                else world
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue entity with
                | (true, entityImNui) -> (false, World.utilizeSimulantImNui entity entityImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui entity { Utilized = true; Result = zero } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapEntityImNui entityImNui = { entityImNui with Result = mapper (entityImNui.Result :?> 'r) }
                        World.tryMapSimulantImNui mapEntityImNui entity world
                    (true, init mapResult entity world)
            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && entity.GetExists world
                        then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let result = (World.getSimulantImNui entity world).Result :?> 'r
            let world = World.mapSimulantImNui (fun simulantImNui -> { simulantImNui with Result = zero }) entity world
            (result, world)

        /// Begin the ImNui declaration of an entity with the given arguments.
        static member beginEntity<'d when 'd :> EntityDispatcher> name args world =
            World.beginEntityPlus<'d, unit> () (fun _ _ world -> world) name args world |> snd

        /// End the ImNui declaration of an entity.
        static member endEntity (world : World) =
            match world.ContextImNui with
            | :? (Entity Address) as entityAddress when entityAddress.Length >= 4 ->
                let currentNames = Array.take (dec entityAddress.Length) entityAddress.Names
                let currentAddress =
                    if currentNames.Length = 3
                    then Address.makeFromArray<Group> currentNames :> Address
                    else Address.makeFromArray<Entity> currentNames
                World.setContext currentAddress world
            | _ -> raise (InvalidOperationException "World.beginEntity mismatch.")

        /// ImNui declare an entity with the given arguments.
        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> zero init name args world =
            let (result, world) = World.beginEntityPlus<'d, 'r> zero init name args world
            let world = World.endEntity world
            (result, world)

        /// ImNui declare an entity with the given arguments.
        static member doEntity<'d when 'd :> EntityDispatcher> name args world =
            let world = World.beginEntity<'d> name args world
            World.endEntity world

        /// Make an entity the current ImNui context.
        static member scopeEntity (entity : Entity) (args : Entity ArgImNui seq) world =
            let world = World.setContext entity.EntityAddress world
            Seq.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Begin the ImNui declaration of associated gui entities with the given arguments.
        static member beginAssociation name args world = World.beginEntity<GuiDispatcher> name args world

        /// End the ImNui declaration of associated gui entities.
        static member endAssociation world = World.endEntity world

        /// ImNui declare an empty association of gui entities with the given arguments.
        static member doAssociation name args world = World.doEntity<GuiDispatcher> name args world

        /// ImNui declare a 2d effect with the given arguments.
        static member doEffect2d name args world = World.doEntity<Effect2dDispatcher> name args world

        /// ImNui declare a static sprite with the given arguments.
        static member doStaticSprite name args world = World.doEntity<StaticSpriteDispatcher> name args world

        /// ImNui declare an animated sprite with the given arguments.
        static member doAnimatedSprite name args world = World.doEntity<AnimatedSpriteDispatcher> name args world

        /// ImNui declare a basic static sprite emitter with the given arguments.
        static member doBasicStaticSpriteEmitter name args world = World.doEntity<BasicStaticSpriteEmitterDispatcher> name args world

        /// ImNui declare a text entity with the given arguments.
        static member doText name args world = World.doEntity<TextDispatcher> name args world

        /// ImNui declare a label with the given arguments.
        static member doLabel name args world = World.doEntity<LabelDispatcher> name args world

        /// ImNui declare a button with the given arguments.
        static member doButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult (fun _ -> true) world)) entity.ClickEvent entity world
            World.doEntityPlus<ButtonDispatcher, _> false init name args world

        /// ImNui declare a toggle button with the given arguments.
        static member doToggleButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun evt world -> (Cascade, mapResult (fun _ -> evt.Data) world)) entity.ToggleEvent entity world
            World.doEntityPlus<ToggleButtonDispatcher, _> false init name args world

        /// ImNui declare a radio button with the given arguments.
        static member doRadioButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun evt world -> (Cascade, mapResult (fun _ -> evt.Data) world)) entity.DialEvent entity world
            World.doEntityPlus<RadioButtonDispatcher, _> false init name args world

        /// ImNui declare a fill bar with the given arguments.
        static member doFillBar name args world = World.doEntity<FillBarDispatcher> name args world

        /// ImNui declare a feeler with the given arguments.
        static member doFeeler name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult (fun _ -> true) world)) entity.TouchEvent entity world
            World.doEntityPlus<ButtonDispatcher, _> false init name args world

        /// ImNui declare an fps entity with the given arguments.
        static member doFps name args world = World.doEntity<FpsDispatcher> name args world

        /// ImNui declare the beginning of a panel with the given arguments.
        static member beginPanel name args world = World.beginEntity<PanelDispatcher> name args world

        /// ImNui declare the end of a panel.
        static member endPanel world = World.endEntity world

        /// ImNui declare a panel with the given arguments.
        static member doPanel name args world = World.doEntity<PanelDispatcher> name args world

        /// ImNui declare a 2d block with the given arguments.
        static member doBlock2d name args world = World.doEntityPlus<Block2dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 2d box with the given arguments.
        static member doBox2d name args world = World.doEntityPlus<Box2dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 2d character with the given arguments.
        static member doCharacter2d name args world = World.doEntityPlus<Character2dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a tile map with the given arguments.
        static member doTileMap name args world = World.doEntityPlus<TileMapDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 3d light probe with the given arguments.
        static member doLightProbe3d name args world = World.doEntity<LightProbe3dDispatcher> name args world

        /// ImNui declare a 3d light with the given arguments.
        static member doLight3d name args world = World.doEntity<Light3dDispatcher> name args world

        /// ImNui declare a sky box with the given arguments.
        static member doSkyBox name args world = World.doEntity<SkyBoxDispatcher> name args world

        /// ImNui declare a basic static billboard emitter with the given arguments.
        static member doBasicStaticBillboardEmitter name args world = World.doEntity<BasicStaticBillboardEmitterDispatcher> name args world

        /// ImNui declare a 3d effect with the given arguments.
        static member doEffect3d name args world = World.doEntity<Effect3dDispatcher> name args world

        /// ImNui declare a 3d block with the given arguments.
        static member doBlock3d name args world = World.doEntityPlus<Block3dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 3d box with the given arguments.
        static member doBox3d name args world = World.doEntityPlus<Box3dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a static billboard with the given arguments.
        static member doStaticBillboard name args world = World.doEntity<StaticBillboardDispatcher> name args world

        /// ImNui declare a static model with the given arguments.
        static member doStaticModel name args world = World.doEntity<StaticModelDispatcher> name args world

        /// ImNui declare a static model surface with the given arguments.
        static member doStaticModelSurface name args world = World.doEntity<StaticModelSurfaceDispatcher> name args world

        /// ImNui declare a rigid model with the given arguments.
        static member doRigidModel name args world = World.doEntityPlus<RigidModelDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a rigid model with the given arguments.
        static member doRigidModelSurface name args world = World.doEntityPlus<RigidModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a animated model with the given arguments.
        static member doAnimatedModel name args world = World.doEntity<AnimatedModelDispatcher> name args world

        /// ImNui declare a 3d character with the given arguments.
        static member doCharacter3d name args world = World.doEntityPlus<Character3dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 3d body joint with the given arguments.
        static member doBodyJoint3d name args world = World.doEntityPlus<BodyJoint3dDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a terrain with the given arguments.
        static member doTerrain name args world = World.doEntityPlus<TerrainDispatcher, _> FQueue.empty World.initBodyResult name args world

        /// ImNui declare a 3d nav config with the given arguments.
        static member doNav3dConfig name args world = World.doEntity<Nav3dConfigDispatcher> name args world

        /// ImNui declare a 3d light config with the given arguments.
        static member doLighting3dConfig name args world = World.doEntity<Lighting3dConfigDispatcher> name args world

        /// ImNui declare a static model hierarchy with the given arguments.
        static member doStaticModelHierarchy name args world = World.doEntity<StaticModelHierarchyDispatcher> name args world

        /// ImNui declare a rigid model hierarchy with the given arguments.
        static member doRigidModelHierarchy name args world = World.doEntity<RigidModelHierarchyDispatcher> name args world