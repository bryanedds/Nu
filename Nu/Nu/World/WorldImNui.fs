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

    /// Specify a dynamic ImNui argument.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = false; ArgLens = lens; ArgValue = value } : 's ArgImNui

    /// Specify a static ImNui argument.
    let
#if !DEBUG
        inline
#endif
        (@=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = true; ArgLens = lens; ArgValue = value } : 's ArgImNui

    type World with

        ///
        static member initBoolResult mapResult (button : Entity) world =
            World.monitor (fun _ world -> (Cascade, mapResult (fun _ -> true) world)) button.ClickEvent button world

        ///
        static member initBodyResult mapResult (entity : Entity) world =
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyPenetration event.Data) world)) entity.BodyPenetrationEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationExplicit event.Data) world)) entity.BodySeparationExplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationImplicit event.Data) world)) entity.BodySeparationImplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyTransform event.Data) world)) entity.BodyTransformEvent entity world
            world

        ///
        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> (zero : 'r) init name (world : World) (args : Entity ArgImNui seq) : 'r * World =
            // TODO: optimize this for large-scale use.
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContextImNui entityAddress world
            let entity = Nu.Entity entityAddress
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue entity with
                | (true, entityImNui) -> (false, World.utilizeSimulantImNui entity entityImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui entity { Utilized = true; Result = zero } world
                    let world = World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                    let world = World.setEntityProtected true entity world |> snd'
                    let world = if entity.Surnames.Length > 1 then entity.SetMountOpt (Some (Relation.makeParent ())) world else world
                    let mapResult = fun (mapper : 'r -> 'r) world -> World.mapSimulantImNui (fun entityImNui -> { entityImNui with Result = mapper (entityImNui.Result :?> 'r) }) entity world
                    (true, init mapResult entity world)
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

        ///
        static member beginEntity<'d when 'd :> EntityDispatcher> name world args =
            World.beginEntityPlus<'d, unit> () (fun _ _ world -> world) name world args |> snd

        ///
        static member endEntity (world : World) =
            match world.ContextImNui with
            | :? (Entity Address) as entityAddress when entityAddress.Length >= 4 ->
                let currentNames = Array.take (dec entityAddress.Length) entityAddress.Names
                let currentAddress =
                    if currentNames.Length = 3
                    then Address.makeFromArray<Group> currentNames :> Address
                    else Address.makeFromArray<Entity> currentNames
                World.setContextImNui currentAddress world
            | _ -> raise (new InvalidOperationException "World.beginEntity mismatch.")

        ///
        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> zero init name world args =
            let (result, world) = World.beginEntityPlus<'d, 'r> zero init name world args
            let world = World.endEntity world
            (result, world)

        ///
        static member doEntity<'d when 'd :> EntityDispatcher> name world args =
            let world = World.beginEntity<'d> name world args
            World.endEntity world

        /// Begin the declaration of associated gui entities with the given arguments.
        static member beginAssociation name world args = World.beginEntity<GuiDispatcher> name world args

        /// End the declaration of associated gui entities.
        static member endAssociation world = World.endEntity world

        /// Declare an empty association of gui entities with the given arguments.
        static member doAssociation name world args = World.doEntity<GuiDispatcher> name world args

        /// Declare a 2d effect with the given arguments.
        static member doEffect2d name world args = World.doEntity<Effect2dDispatcher> name world args

        /// Declare a static sprite with the given arguments.
        static member doStaticSprite name world args = World.doEntity<StaticSpriteDispatcher> name world args

        /// Declare an animated sprite with the given arguments.
        static member doAnimatedSprite name world args = World.doEntity<AnimatedSpriteDispatcher> name world args

        /// Declare a basic static sprite emitter with the given arguments.
        static member doBasicStaticSpriteEmitter name world args = World.doEntity<BasicStaticSpriteEmitterDispatcher> name world args

        /// Declare a text entity with the given arguments.
        static member doText name world args = World.doEntity<TextDispatcher> name world args

        /// Declare a label with the given arguments.
        static member doLabel name world args = World.doEntity<LabelDispatcher> name world args

        /// Declare a button with the given arguments.
        static member doButton name world args = World.doEntityPlus<ButtonDispatcher, _> false World.initBoolResult name world args

        /// Declare a toggle button with the given arguments.
        static member doToggleButton name world args = World.doEntityPlus<ToggleButtonDispatcher, _> false World.initBoolResult name world args

        /// Declare a radio button with the given arguments.
        static member doRadioButton name world args = World.doEntityPlus<ToggleButtonDispatcher, _> false World.initBoolResult name world args

        /// Declare a fill bar with the given arguments.
        static member doFillBar name world args = World.doEntity<FillBarDispatcher> name world args

        /// Declare a feeler with the given arguments.
        static member doFeeler name world args = World.doEntityPlus<FeelerDispatcher, _> false World.initBoolResult name world args

        /// Declare an fps entity with the given arguments.
        static member doFps name world args = World.doEntity<FpsDispatcher> name world args

        /// Declare the beginning of a panel with the given arguments.
        static member beginPanel name world args = World.beginEntity<PanelDispatcher> name world args

        /// Declare the end of a panel.
        static member endPanel world = World.endEntity world

        /// Declare a panel with the given arguments.
        static member doPanel name world args = World.doEntity<PanelDispatcher> name world args

        /// Declare a 2d block with the given arguments.
        static member doBlock2d name world args = World.doEntityPlus<Block2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 2d box with the given arguments.
        static member doBox2d name world args = World.doEntityPlus<Box2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 2d character with the given arguments.
        static member doCharacter2d name world args = World.doEntityPlus<Character2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a tile map with the given arguments.
        static member doTileMap name world args = World.doEntityPlus<TileMapDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d light probe with the given arguments.
        static member doLightProbe3d name world args = World.doEntityPlus<LightProbe3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d light with the given arguments.
        static member doLight3d name world args = World.doEntityPlus<Light3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a sky box with the given arguments.
        static member doSkyBox name world args = World.doEntityPlus<SkyBoxDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a basic static billboard emitter with the given arguments.
        static member doBasicStaticBillboardEmitter name world args = World.doEntityPlus<BasicStaticBillboardEmitterDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d effect with the given arguments.
        static member doEffect3d name world args = World.doEntityPlus<Effect3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d block with the given arguments.
        static member doBlock3d name world args = World.doEntityPlus<Block3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d box with the given arguments.
        static member doBox3d name world args = World.doEntityPlus<Box3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a static billboard with the given arguments.
        static member doStaticBillboard name world args = World.doEntityPlus<StaticBillboardDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a static model with the given arguments.
        static member doStaticModel name world args = World.doEntityPlus<StaticModelDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a static model surface with the given arguments.
        static member doStaticModelSurface name world args = World.doEntityPlus<StaticModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a rigid model with the given arguments.
        static member doRigidModel name world args = World.doEntityPlus<RigidModelDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a rigid model with the given arguments.
        static member doRigidModelSurface name world args = World.doEntityPlus<RigidModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a animated model with the given arguments.
        static member doAnimatedModel name world args = World.doEntityPlus<AnimatedModelDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d character with the given arguments.
        static member doCharacter3d name world args = World.doEntityPlus<Character3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d body joint with the given arguments.
        static member doBodyJoint3d name world args = World.doEntityPlus<BodyJoint3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a terrain with the given arguments.
        static member doTerrain name world args = World.doEntityPlus<TerrainDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d nav config with the given arguments.
        static member doNav3dConfig name world args = World.doEntityPlus<Nav3dConfigDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a 3d light config with the given arguments.
        static member doLighting3dConfig name world args = World.doEntityPlus<Lighting3dConfigDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a static model hierarchy with the given arguments.
        static member doStaticModelHierarchy name world args = World.doEntityPlus<StaticModelHierarchyDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// Declare a rigid model hierarchy with the given arguments.
        static member doRigidModelHierarchy name world args = World.doEntityPlus<RigidModelHierarchyDispatcher, _> FQueue.empty World.initBodyResult name world args

        ///
        static member internal beginGroup4<'d when 'd :> GroupDispatcher> name (groupFilePathOpt : string option) (world : World) (args : Group ArgImNui seq) =
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContextImNui groupAddress world
            let group = Nu.Group groupAddress
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue group with
                | (true, groupImNui) -> (false, World.utilizeSimulantImNui group groupImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui group { Utilized = true; Result = () } world
                    let world =
                        match groupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some name) group.Screen world |> snd
                        | None -> World.createGroup<'d> (Some name) group.Screen world |> snd
                    let world = World.setGroupProtected true group world |> snd'
                    (true, world)
            Seq.fold
                (fun world arg ->
                    if (initializing || not arg.ArgStatic) && group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        ///
        static member internal beginGroupFromFile<'d when 'd :> GroupDispatcher> name groupFilePath world args =
            World.beginGroup4<'d> name (Some groupFilePath) world args

        ///
        static member internal beginGroup<'d when 'd :> GroupDispatcher> name world args =
            World.beginGroup4<'d> name None world args
            
        ///
        static member endGroup (world : World) =
            match world.ContextImNui with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setContextImNui currentAddress world
            | _ -> raise (new InvalidOperationException "World.beginGroup mismatch.")

        ///
        static member doGroup<'d when 'd :> GroupDispatcher> name world args =
            let world = World.beginGroup<'d> name world args
            World.endGroup world

        static member internal beginScreenInternal<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name behavior select (world : World) (args : Screen ArgImNui seq) =
            let screenAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContextImNui screenAddress world
            let screen = Nu.Screen screenAddress
            let (initializing, world) =
                let simulantImNuis = world.SimulantImNuis
                match simulantImNuis.TryGetValue screen with
                | (true, screenImNui) -> (false, World.utilizeSimulantImNui screen screenImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui screen { Utilized = true; Result = FQueue.empty<ScreenResult> } world
                    let world = if not (screen.GetExists world) then World.createScreen<'d> (Some name) world |> snd else world // NOTE: special-case when Gaia has already created the screen.
                    let world = World.setScreenProtected true screen world |> snd'
                    let mapResult = fun (mapper : 'r -> 'r) world -> World.mapSimulantImNui (fun screenImNui -> { screenImNui with Result = mapper (screenImNui.Result :?> 'r) }) screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj Select) world)) screen.SelectEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj IncomingStart) world)) screen.IncomingStartEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj IncomingFinish) world)) screen.IncomingFinishEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj OutgoingStart) world)) screen.OutgoingStartEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj OutgoingFinish) world)) screen.OutgoingFinishEvent screen world
                    let world = World.monitor (fun _ world -> (Cascade, mapResult (FQueue.conj Deselecting) world)) screen.DeselectingEvent screen world
                    (true, world)
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && screen.GetExists world
                        then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let world = if screen.GetExists world then World.applyScreenBehavior setScreenSlide behavior screen world else world
            let world = if screen.GetExists world && select then transitionScreen screen world else world
            let result = (World.getSimulantImNui screen world).Result :?> ScreenResult FQueue
            (result, world)

        ///
        static member endScreen (world : World) =
            match world.ContextImNui with
            | :? (Screen Address) ->
                World.setContextImNui Game.GameAddress world
            | _ -> raise (new InvalidOperationException "World.beginScreen mismatch.")

        static member doScreenInternal<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name behavior select world args =
            let (result, world) = World.beginScreenInternal<'d> transitionScreen setScreenSlide name behavior select world args
            let world = World.endScreen world
            (result, world)

        ///
        static member beginGame (world : World) (args : Game ArgImNui seq) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ContextImNui.Names)
            let world = World.setContextImNui gameAddress world
            let game = Nu.Game gameAddress
            Seq.fold
                (fun world arg ->
                    if not arg.ArgStatic
                    then game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        ///
        static member endGame (world : World) =
            match world.ContextImNui with
            | :? (Game Address) ->
                World.setContextImNui Address.empty world
            | _ -> raise (new InvalidOperationException "World.beginGame mismatch.")

        ///
        static member doGame world args =
            let world = World.beginGame world args
            World.endGame world

        ///
        static member scopeEntity (entity : Entity) world (args : Entity ArgImNui seq) =
            let world = World.setContextImNui entity.EntityAddress world
            Seq.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        ///
        static member scopeGroup (group : Group) world (args : Group ArgImNui seq) =
            let world = World.setContextImNui group.GroupAddress world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        ///
        static member scopeScreen (screen : Screen) world (args : Screen ArgImNui seq) =
            let world = World.setContextImNui screen.ScreenAddress world
            Seq.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        ///
        static member scopeGame (game : Game) world (args : Game ArgImNui seq) =
            let world = World.setContextImNui game.GameAddress world
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c')
                world args

        ///
        static member scopeWorld world =
            World.setContextImNui Address.empty world

        static member internal updateImNui (world : World) =
            if world.Advancing then
                OMap.fold (fun world simulant simulantImNui ->
                    if not simulantImNui.Utilized then
                        let world = World.destroy simulant world
                        World.setSimulantImNuis (OMap.remove simulant world.SimulantImNuis) world
                    else
                        if world.Imperative then
                            simulantImNui.Utilized <- false
                            world
                        else
                            let world = World.setSimulantImNuis (OMap.add simulant { simulantImNui with Utilized = false } world.SimulantImNuis) world
                            world)
                    world world.SimulantImNuis
            else world