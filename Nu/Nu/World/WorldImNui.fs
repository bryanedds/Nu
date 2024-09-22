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
        static member initBoolResult mapResult (entity : Entity) world =
            World.monitor (fun _ world -> (Cascade, mapResult (fun _ -> true) world)) entity.ClickEvent entity world

        ///
        static member initBodyResult mapResult (entity : Entity) world =
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyPenetration event.Data) world)) entity.BodyPenetrationEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationExplicit event.Data) world)) entity.BodySeparationExplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationImplicit event.Data) world)) entity.BodySeparationImplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyTransform event.Data) world)) entity.BodyTransformEvent entity world
            world

        /// Begin the ImNui declaration of an entity with the given arguments.
        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> (zero : 'r) init name (world : World) (args : Entity ArgImNui seq) : 'r * World =
            // TODO: optimize this for large-scale use.
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue entity with
                | (true, entityImNui) -> (false, World.utilizeSimulantImNui entity entityImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui entity { Utilized = true; Result = zero } world
                    let world = if not (entity.GetExists world) then World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd else world
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

        /// Begin the ImNui declaration of an entity with the given arguments.
        static member beginEntity<'d when 'd :> EntityDispatcher> name world args =
            World.beginEntityPlus<'d, unit> () (fun _ _ world -> world) name world args |> snd

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
            | _ -> raise (new InvalidOperationException "World.beginEntity mismatch.")

        /// ImNui declare an entity with the given arguments.
        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> zero init name world args =
            let (result, world) = World.beginEntityPlus<'d, 'r> zero init name world args
            let world = World.endEntity world
            (result, world)

        /// ImNui declare an entity with the given arguments.
        static member doEntity<'d when 'd :> EntityDispatcher> name world args =
            let world = World.beginEntity<'d> name world args
            World.endEntity world

        /// Begin the ImNui declaration of associated gui entities with the given arguments.
        static member beginAssociation name world args = World.beginEntity<GuiDispatcher> name world args

        /// End the ImNui declaration of associated gui entities.
        static member endAssociation world = World.endEntity world

        /// ImNui declare an empty association of gui entities with the given arguments.
        static member doAssociation name world args = World.doEntity<GuiDispatcher> name world args

        /// ImNui declare a 2d effect with the given arguments.
        static member doEffect2d name world args = World.doEntity<Effect2dDispatcher> name world args

        /// ImNui declare a static sprite with the given arguments.
        static member doStaticSprite name world args = World.doEntity<StaticSpriteDispatcher> name world args

        /// ImNui declare an animated sprite with the given arguments.
        static member doAnimatedSprite name world args = World.doEntity<AnimatedSpriteDispatcher> name world args

        /// ImNui declare a basic static sprite emitter with the given arguments.
        static member doBasicStaticSpriteEmitter name world args = World.doEntity<BasicStaticSpriteEmitterDispatcher> name world args

        /// ImNui declare a text entity with the given arguments.
        static member doText name world args = World.doEntity<TextDispatcher> name world args

        /// ImNui declare a label with the given arguments.
        static member doLabel name world args = World.doEntity<LabelDispatcher> name world args

        /// ImNui declare a button with the given arguments.
        static member doButton name world args = World.doEntityPlus<ButtonDispatcher, _> false World.initBoolResult name world args

        /// ImNui declare a toggle button with the given arguments.
        static member doToggleButton name world args = World.doEntityPlus<ToggleButtonDispatcher, _> false World.initBoolResult name world args

        /// ImNui declare a radio button with the given arguments.
        static member doRadioButton name world args = World.doEntityPlus<ToggleButtonDispatcher, _> false World.initBoolResult name world args

        /// ImNui declare a fill bar with the given arguments.
        static member doFillBar name world args = World.doEntity<FillBarDispatcher> name world args

        /// ImNui declare a feeler with the given arguments.
        static member doFeeler name world args = World.doEntityPlus<FeelerDispatcher, _> false World.initBoolResult name world args

        /// ImNui declare an fps entity with the given arguments.
        static member doFps name world args = World.doEntity<FpsDispatcher> name world args

        /// ImNui declare the beginning of a panel with the given arguments.
        static member beginPanel name world args = World.beginEntity<PanelDispatcher> name world args

        /// ImNui declare the end of a panel.
        static member endPanel world = World.endEntity world

        /// ImNui declare a panel with the given arguments.
        static member doPanel name world args = World.doEntity<PanelDispatcher> name world args

        /// ImNui declare a 2d block with the given arguments.
        static member doBlock2d name world args = World.doEntityPlus<Block2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 2d box with the given arguments.
        static member doBox2d name world args = World.doEntityPlus<Box2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 2d character with the given arguments.
        static member doCharacter2d name world args = World.doEntityPlus<Character2dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a tile map with the given arguments.
        static member doTileMap name world args = World.doEntityPlus<TileMapDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 3d light probe with the given arguments.
        static member doLightProbe3d name world args = World.doEntity<LightProbe3dDispatcher> name world args

        /// ImNui declare a 3d light with the given arguments.
        static member doLight3d name world args = World.doEntity<Light3dDispatcher> name world args

        /// ImNui declare a sky box with the given arguments.
        static member doSkyBox name world args = World.doEntity<SkyBoxDispatcher> name world args

        /// ImNui declare a basic static billboard emitter with the given arguments.
        static member doBasicStaticBillboardEmitter name world args = World.doEntity<BasicStaticBillboardEmitterDispatcher> name world args

        /// ImNui declare a 3d effect with the given arguments.
        static member doEffect3d name world args = World.doEntity<Effect3dDispatcher> name world args

        /// ImNui declare a 3d block with the given arguments.
        static member doBlock3d name world args = World.doEntityPlus<Block3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 3d box with the given arguments.
        static member doBox3d name world args = World.doEntityPlus<Box3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a static billboard with the given arguments.
        static member doStaticBillboard name world args = World.doEntity<StaticBillboardDispatcher> name world args

        /// ImNui declare a static model with the given arguments.
        static member doStaticModel name world args = World.doEntity<StaticModelDispatcher> name world args

        /// ImNui declare a static model surface with the given arguments.
        static member doStaticModelSurface name world args = World.doEntity<StaticModelSurfaceDispatcher> name world args

        /// ImNui declare a rigid model with the given arguments.
        static member doRigidModel name world args = World.doEntityPlus<RigidModelDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a rigid model with the given arguments.
        static member doRigidModelSurface name world args = World.doEntityPlus<RigidModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a animated model with the given arguments.
        static member doAnimatedModel name world args = World.doEntity<AnimatedModelDispatcher> name world args

        /// ImNui declare a 3d character with the given arguments.
        static member doCharacter3d name world args = World.doEntityPlus<Character3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 3d body joint with the given arguments.
        static member doBodyJoint3d name world args = World.doEntityPlus<BodyJoint3dDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a terrain with the given arguments.
        static member doTerrain name world args = World.doEntityPlus<TerrainDispatcher, _> FQueue.empty World.initBodyResult name world args

        /// ImNui declare a 3d nav config with the given arguments.
        static member doNav3dConfig name world args = World.doEntity<Nav3dConfigDispatcher> name world args

        /// ImNui declare a 3d light config with the given arguments.
        static member doLighting3dConfig name world args = World.doEntity<Lighting3dConfigDispatcher> name world args

        /// ImNui declare a static model hierarchy with the given arguments.
        static member doStaticModelHierarchy name world args = World.doEntity<StaticModelHierarchyDispatcher> name world args

        /// ImNui declare a rigid model hierarchy with the given arguments.
        static member doRigidModelHierarchy name world args = World.doEntity<RigidModelHierarchyDispatcher> name world args

        static member private beginGroup4<'d when 'd :> GroupDispatcher> name groupFilePathOpt (world : World) (args : Group ArgImNui seq) =
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext groupAddress world
            let group = Nu.Group groupAddress
            let (initializing, world) =
                match world.SimulantImNuis.TryGetValue group with
                | (true, groupImNui) -> (false, World.utilizeSimulantImNui group groupImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui group { Utilized = true; Result = () } world
                    let world =
                        if not (group.GetExists world) then
                            match groupFilePathOpt with
                            | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some name) group.Screen world |> snd
                            | None -> World.createGroup<'d> (Some name) group.Screen world |> snd
                        else world
                    let world = World.setGroupProtected true group world |> snd'
                    (true, world)
            Seq.fold
                (fun world arg ->
                    if (initializing || not arg.ArgStatic) && group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Begin the ImNui declaration of a group read from the given file path with the given arguments.
        static member beginGroupFromFile<'d when 'd :> GroupDispatcher> name groupFilePath world args =
            World.beginGroup4<'d> name (Some groupFilePath) world args

        /// Begin the ImNui declaration of a group with the given arguments.
        static member beginGroup<'d when 'd :> GroupDispatcher> name world args =
            World.beginGroup4<'d> name None world args
            
        /// End the ImNui declaration of a group.
        static member endGroup (world : World) =
            match world.ContextImNui with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setContext currentAddress world
            | _ -> raise (new InvalidOperationException "World.beginGroup mismatch.")

        /// ImNui declare a group with the given arguments.
        static member doGroup<'d when 'd :> GroupDispatcher> name world args =
            let world = World.beginGroup<'d> name world args
            World.endGroup world

        static member internal beginScreen8<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name select behavior groupFilePathOpt (world : World) (args : Screen ArgImNui seq) =
            let screenAddress = Address.makeFromArray (Array.add name world.ContextImNui.Names)
            let world = World.setContext screenAddress world
            let screen = Nu.Screen screenAddress
            let (initializing, world) =
                let simulantImNuis = world.SimulantImNuis
                match simulantImNuis.TryGetValue screen with
                | (true, screenImNui) -> (false, World.utilizeSimulantImNui screen screenImNui world)
                | (false, _) ->
                    let world = World.addSimulantImNui screen { Utilized = true; Result = FQueue.empty<ScreenResult> } world
                    let world =
                        if not (screen.GetExists world)
                        then World.createScreen<'d> (Some name) world |> snd
                        else world
                    let world = World.setScreenProtected true screen world |> snd'
                    let world =
                        match groupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> snd
                        | None -> world
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

        /// End the ImNui declaration of a screen.
        static member endScreen (world : World) =
            match world.ContextImNui with
            | :? (Screen Address) ->
                World.setContext Game.GameAddress world
            | _ -> raise (new InvalidOperationException "World.beginScreen mismatch.")

        static member internal doScreen8<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name select behavior groupFilePathOpt world args =
            let (result, world) = World.beginScreen8<'d> transitionScreen setScreenSlide name select behavior groupFilePathOpt world args
            let world = World.endScreen world
            (result, world)

        /// Begin the ImNui declaration of a game with the given arguments.
        static member beginGame (world : World) (args : Game ArgImNui seq) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ContextImNui.Names)
            let world = World.setContext gameAddress world
            let game = Nu.Game gameAddress
            Seq.fold
                (fun world arg ->
                    if not arg.ArgStatic
                    then game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// End the ImNui declaration of a group with the given arguments.
        static member endGame (world : World) =
            match world.ContextImNui with
            | :? (Game Address) ->
                World.setContext Address.empty world
            | _ -> raise (new InvalidOperationException "World.beginGame mismatch.")

        /// ImNui declare a group with the given arguments.
        static member doGame world args =
            let world = World.beginGame world args
            World.endGame world

        /// Make an entity the current ImNui context.
        static member scopeEntity (entity : Entity) world (args : Entity ArgImNui seq) =
            let world = World.setContext entity.EntityAddress world
            Seq.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Make a group the current ImNui context.
        static member scopeGroup (group : Group) world (args : Group ArgImNui seq) =
            let world = World.setContext group.GroupAddress world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Make a screen the current ImNui context.
        static member scopeScreen (screen : Screen) world (args : Screen ArgImNui seq) =
            let world = World.setContext screen.ScreenAddress world
            Seq.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Make the game the current ImNui context.
        static member scopeGame world (args : Game ArgImNui seq) =
            let game = Game
            let world = World.setContext game.GameAddress world
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c')
                world args

        /// Clear the current ImNui context.
        static member scopeWorld world =
            World.setContext Address.empty world

        static member internal collectImNui (world : World) =
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