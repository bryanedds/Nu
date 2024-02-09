// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Reflection
open System.Threading
open SDL2
open Prime

[<AbstractClass; Sealed>]
type Nu () =

    static let mutable Initialized = false

    /// Initialize the Nu game engine.
    static member init () =

        // init only if needed
        if not Initialized then

            // ensure the current culture is invariate
            Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init (Some "Log.txt")

            // init reflection module
            Reflection.init ()

            // init math module
            Math.Init ()

            // init vsync
            Vsync.Init Constants.Engine.RunSynchronously

            // init OpenGL assert mechanism
            OpenGL.Hl.InitAssert
#if DEBUG
                Constants.OpenGL.HlAssert
#else
                false
#endif

            // init simulant modules
            WorldModuleGame.init ()
            WorldModuleScreen.init ()
            WorldModuleGroup.init ()
            WorldModuleEntity.init ()

            // init simulant types
            Nu.Entity.init ()

            // init content variables
            WorldTypes.EmptyGameContent <- GameContent.empty
            WorldTypes.EmptyScreenContent <- ScreenContent.empty
            WorldTypes.EmptyGroupContent <- GroupContent.empty
            WorldTypes.EmptyEntityContent <- EntityContent.empty

            // init debug view F# reach-arounds
            WorldTypes.viewGame <- fun game world -> World.viewGameProperties (game :?> Game) (world :?> World)
            WorldTypes.viewScreen <- fun screen world -> World.viewScreenProperties (screen :?> Screen) (world :?> World)
            WorldTypes.viewGroup <- fun group world -> World.viewGroupProperties (group :?> Group) (world :?> World)
            WorldTypes.viewEntity <- fun entity world -> World.viewEntityProperties (entity :?> Entity) (world :?> World)

            // init handleSubscribeAndUnsubscribeEvent F# reach-around
            WorldTypes.getSelectedScreenIdling <- fun worldObj -> World.getSelectedScreenIdling (worldObj :?> World)
            WorldTypes.getSelectedScreenTransitioning <- fun worldObj -> World.getSelectedScreenTransitioning (worldObj :?> World)
            WorldTypes.handleSubscribeAndUnsubscribeEvent <- fun subscribing eventAddress _ worldObj ->
                // here we need to update the event publish flags for entities based on whether there are subscriptions to
                // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
                // that these optimizations do not support, and warn the developer if they are invoked. Additionally, we
                // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
                let world = worldObj :?> World
                let eventNames = Address.getNames eventAddress
                let eventNamesLength = Array.length eventNames
                let world =
                    if eventNamesLength >= 6 then
                        let eventFirstName = eventNames.[0]
                        match eventFirstName with
#if !DISABLE_ENTITY_PRE_UPDATE
                        | "PreUpdate" ->
    #if DEBUG
                            if  Array.contains Address.WildcardName eventNames ||
                                Array.contains Address.EllipsisName eventNames then
                                Log.debug
                                    ("Subscribing to entity pre-update events with a wildcard or ellipsis is not supported. " +
                                     "This will cause a bug where some entity pre-update events are not published.")
    #endif
                            let entity = Nu.Entity (Array.skip 2 eventNames)
                            World.updateEntityPublishPreUpdateFlag entity world |> snd'
#endif
                        | "Update" ->
#if DEBUG
                            if  Array.contains Address.WildcardName eventNames ||
                                Array.contains Address.EllipsisName eventNames then
                                Log.debug
                                    ("Subscribing to entity update events with a wildcard or ellipsis is not supported. " +
                                     "This will cause a bug where some entity update events are not published.")
#endif
                            let entity = Nu.Entity (Array.skip 2 eventNames)
                            World.updateEntityPublishUpdateFlag entity world |> snd'
#if !DISABLE_ENTITY_POST_UPDATE
                        | "PostUpdate" ->
    #if DEBUG
                            if  Array.contains Address.WildcardName eventNames ||
                                Array.contains Address.EllipsisName eventNames then
                                Log.debug
                                    ("Subscribing to entity post-update events with a wildcard or ellipsis is not supported. " +
                                     "This will cause a bug where some entity post-update events are not published.")
    #endif
                            let entity = Nu.Entity (Array.skip 2 eventNames)
                            World.updateEntityPublishPostUpdateFlag entity world |> snd'
#endif
                        | "BodyCollision" | "BodySeparationExplicit" ->
                            let entity = Nu.Entity (Array.skip 2 eventNames)
                            World.updateBodyObservable subscribing entity world
                        | _ -> world
                    else world
                let world =
                    if eventNamesLength >= 4 then
                        match eventNames.[0] with
                        | "Change" ->
                            let world =
                                if eventNamesLength >= 6 then
                                    let entityAddress = rtoa (Array.skip 3 eventNames)
                                    let entity = Nu.Entity entityAddress
                                    match World.tryGetKeyedValueFast<Guid, UMap<Entity Address, int>> (EntityChangeCountsId, world) with
                                    | (true, entityChangeCounts) ->
                                        match entityChangeCounts.TryGetValue entityAddress with
                                        | (true, entityChangeCount) ->
                                            let entityChangeCount = if subscribing then inc entityChangeCount else dec entityChangeCount
                                            let entityChangeCounts =
                                                if entityChangeCount = 0
                                                then UMap.remove entityAddress entityChangeCounts
                                                else UMap.add entityAddress entityChangeCount entityChangeCounts
                                            let world =
                                                if entity.Exists world then
                                                    if entityChangeCount = 0 then World.setEntityPublishChangeEvents false entity world |> snd'
                                                    elif entityChangeCount = 1 then World.setEntityPublishChangeEvents true entity world |> snd'
                                                    else world
                                                else world
                                            World.addKeyedValue EntityChangeCountsId entityChangeCounts world
                                        | (false, _) ->
                                            if not subscribing then failwithumf ()
                                            let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                            World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                                    | (false, _) ->
                                        if not subscribing then failwithumf ()
                                        let config = World.getCollectionConfig world
                                        let entityChangeCounts = UMap.makeEmpty HashIdentity.Structural config
                                        let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                        World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                                else world
                            if  Array.contains Address.WildcardName eventNames ||
                                Array.contains Address.EllipsisName eventNames then
                                Log.debug "Subscribing to change events with a wildcard or ellipsis is not supported."
                            world
                        | _ -> world
                    else world
                world :> obj

            // init getEntityIs2d F# reach-around
            WorldTypes.getEntityIs2d <- fun entityObj worldObj ->
                World.getEntityIs2d (entityObj :?> Entity) (worldObj :?> World)

            // init getSelected F# reach-around
            WorldModule.getSelected <- fun simulant world ->
                World.getSelected simulant world

            // init sortSubscriptionByElevation F# reach-around
            WorldModule.sortSubscriptionsByElevation <- fun subscriptions worldObj ->
                let world = worldObj :?> World
                EventGraph.sortSubscriptionsBy
                    (fun (simulant : Simulant) _ ->
                        match simulant with
                        | :? Entity as entity -> { SortElevation = entity.GetElevation world; SortHorizon = 0.0f; SortTarget = entity } :> IComparable
                        | :? Group as group -> { SortElevation = Constants.Engine.GroupSortPriority; SortHorizon = 0.0f; SortTarget = group } :> IComparable
                        | :? Screen as screen -> { SortElevation = Constants.Engine.ScreenSortPriority; SortHorizon = 0.0f; SortTarget = screen } :> IComparable
                        | :? Game | :? GlobalSimulantGeneralized -> { SortElevation = Constants.Engine.GameSortPriority; SortHorizon = 0.0f; SortTarget = Game } :> IComparable
                        | _ -> failwithumf ())
                    subscriptions
                    world

            // init admitScreenElements F# reach-around
            WorldModule.admitScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat |> SList.ofSeq
                let (entities2d, entities3d) = SList.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let quadtree = World.getQuadtree world
                for entity in entities2d do
                    let entityState = World.getEntityState entity world
                    let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entity
                    Quadtree.addElement entityState.Presence entityState.Bounds.Box2 element quadtree
                let octree = World.getOctree world
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                    Octree.addElement entityState.Presence entityState.Bounds element octree
                world
                
            // init evictScreenElements F# reach-around
            WorldModule.evictScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat |> SArray.ofSeq
                let (entities2d, entities3d) = SArray.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let quadtree = World.getQuadtree world
                for entity in entities2d do
                    let entityState = World.getEntityState entity world
                    let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entity
                    Quadtree.removeElement entityState.Presence entityState.Bounds.Box2 element quadtree
                let octree = World.getOctree world
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                    Octree.removeElement entityState.Presence entityState.Bounds element octree
                world

            // init registerScreenPhysics F# reach-around
            WorldModule.registerScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntities world) |>
                    Seq.concat |>
                    SList.ofSeq
                SList.fold (fun world (entity : Entity) ->
                    World.registerEntityPhysics entity world)
                    world entities

            // init unregisterScreenPhysics F# reach-around
            WorldModule.unregisterScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntities world) |>
                    Seq.concat |>
                    SList.ofSeq
                SList.fold (fun world (entity : Entity) ->
                    World.unregisterEntityPhysics entity world)
                    world entities

            // init signal F# reach-around
            WorldModule.signal <- fun signalObj simulant world ->
                match simulant with
                | :? Entity as entity -> (entity.GetDispatcher world).Signal (signalObj, entity, world)
                | :? Group as group -> (group.GetDispatcher world).Signal (signalObj, group, world)
                | :? Screen as screen -> (screen.GetDispatcher world).Signal (signalObj, screen, world)
                | :? Game as game -> (game.GetDispatcher world).Signal (signalObj, game, world)
                | _ -> failwithumf ()

            // init life-cycle F# reach-arounds
            WorldModule.register <- fun simulant world -> World.register simulant world
            WorldModule.unregister <- fun simulant world -> World.unregister simulant world
            WorldModule.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModule.destroy <- fun simulant world -> World.destroy simulant world

            // init miscellaneous F# reach-arounds
            WorldModule.getEmptyEffect <- fun () -> Effect.empty :> obj

            // init event world caching
            EventGraph.setEventAddressCaching true

            // mark init flag
            Initialized <- true

[<AutoOpen>]
module WorldModule3 =

    type World with

        static member private pairWithName source =
            (getTypeName source, source)

        static member private makeDefaultGameDispatcher () =
            World.pairWithName (GameDispatcher ())

        static member private makeDefaultScreenDispatchers () =
            Map.ofList [World.pairWithName (ScreenDispatcher ())]

        static member private makeDefaultGroupDispatchers () =
            Map.ofList [World.pairWithName (GroupDispatcher ())]

        static member private makeDefaultEntityDispatchers () =
            // TODO: consider if we should reflectively generate these.
            Map.ofListBy World.pairWithName $
                [EntityDispatcher (true, false, false)
                 Entity2dDispatcher false
                 Entity3dDispatcher false
                 StaticSpriteDispatcher ()
                 AnimatedSpriteDispatcher ()
                 GuiDispatcher ()
                 TextDispatcher ()
                 LabelDispatcher ()
                 ButtonDispatcher ()
                 ToggleButtonDispatcher ()
                 RadioButtonDispatcher ()
                 FillBarDispatcher ()
                 FeelerDispatcher ()
                 FpsDispatcher ()
                 BasicStaticSpriteEmitterDispatcher ()
                 Effect2dDispatcher ()
                 Block2dDispatcher ()
                 Box2dDispatcher ()
                 Character2dDispatcher ()
                 TileMapDispatcher ()
                 TmxMapDispatcher ()
                 LightProbe3dDispatcher ()
                 Light3dDispatcher ()
                 SkyBoxDispatcher ()
                 StaticBillboardDispatcher ()
                 StaticModelSurfaceDispatcher ()
                 RigidModelSurfaceDispatcher ()
                 StaticModelDispatcher ()
                 AnimatedModelDispatcher ()
                 RigidModelDispatcher ()
                 Effect3dDispatcher ()
                 Block3dDispatcher ()
                 Box3dDispatcher ()
                 Character3dDispatcher ()
                 TerrainDispatcher ()
                 StaticModelHierarchyDispatcher ()
                 RigidModelHierarchyDispatcher ()]

        static member private makeDefaultFacets () =
            // TODO: consider if we should reflectively generate these.
            Map.ofListBy World.pairWithName $
                [Facet false
                 StaticSpriteFacet ()
                 AnimatedSpriteFacet ()
                 TextFacet ()
                 BackdroppableFacet ()
                 LabelFacet ()
                 ButtonFacet ()
                 ToggleButtonFacet ()
                 RadioButtonFacet ()
                 FillBarFacet ()
                 FeelerFacet ()
                 BasicStaticSpriteEmitterFacet ()
                 EffectFacet ()
                 RigidBodyFacet ()
                 TileMapFacet ()
                 TmxMapFacet ()
                 LayoutFacet ()
                 LightProbe3dFacet ()
                 Light3dFacet ()
                 SkyBoxFacet ()
                 StaticBillboardFacet ()
                 StaticModelSurfaceFacet ()
                 StaticModelFacet ()
                 AnimatedModelFacet ()
                 TerrainFacet ()
                 FreezerFacet ()]

        /// Update late bindings internally stored by the engine from types found in the given assemblies.
        static member updateLateBindings (assemblies : Assembly array) world =
            Content.UpdateLateBindingsCount <- inc Content.UpdateLateBindingsCount
            World.clearClipboard world // HACK: clear what's on the clipboard rather than changing its dispatcher instance.
            let pluginType =
                assemblies |>
                Array.map (fun assembly -> assembly.GetTypes ()) |>
                Array.concat |>
                Array.filter (fun ty -> ty.IsSubclassOf typeof<NuPlugin>) |>
                Array.filter (fun ty -> not ty.IsAbstract) |>
                Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0)) |>
                Array.head
            let plugin = Activator.CreateInstance pluginType :?> NuPlugin
            let pluginFacets = plugin.Birth<Facet> assemblies
            let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> assemblies
            let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> assemblies
            let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> assemblies
            let pluginGameDispatchers = plugin.Birth<GameDispatcher> assemblies
            let world =
                { world with WorldExtension = { world.WorldExtension with Plugin = plugin }}
            let world =
                Array.fold (fun world (facetName, facet) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with Facets = Map.add facetName facet world.WorldExtension.Dispatchers.Facets }}})
                    world pluginFacets
            let world =
                Array.fold (fun world (entityDispatcherName, entityDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with EntityDispatchers = Map.add entityDispatcherName entityDispatcher world.WorldExtension.Dispatchers.EntityDispatchers }}})
                    world pluginEntityDispatchers
            let world =
                Array.fold (fun world (groupDispatcherName, groupDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with GroupDispatchers = Map.add groupDispatcherName groupDispatcher world.WorldExtension.Dispatchers.GroupDispatchers }}})
                    world pluginGroupDispatchers
            let world =
                Array.fold (fun world (screenDispatcherName, screenDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with ScreenDispatchers = Map.add screenDispatcherName screenDispatcher world.WorldExtension.Dispatchers.ScreenDispatchers }}})
                    world pluginScreenDispatchers
            let world =
                Array.fold (fun world (gameDispatcherName, gameDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with GameDispatchers = Map.add gameDispatcherName gameDispatcher world.WorldExtension.Dispatchers.GameDispatchers }}})
                    world pluginGameDispatchers
            let lateBindingses =
                [|Array.map (snd >> cast<LateBindings>) pluginFacets
                  Array.map (snd >> cast<LateBindings>) pluginEntityDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginGroupDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginScreenDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginGameDispatchers|] |>
                Array.concat
            let world =
                UMap.fold (fun world simulant _ ->
                    Array.fold (fun world lateBindings -> World.updateLateBindings3 lateBindings simulant world) world lateBindingses)
                    world (World.getSimulants world)
            let world =
                UMap.fold
                    (fun world simulant _ -> World.trySynchronize true simulant world)
                    world (World.getSimulants world)
            world

        /// Make the world.
        static member make plugin eventGraph jobSystem dispatchers quadtree octree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer activeGameDispatcher =
            Nu.init () // ensure game engine is initialized
            let config = AmbientState.getConfig ambientState
            let entityStates = SUMap.makeEmpty HashIdentity.Structural config
            let groupStates = UMap.makeEmpty HashIdentity.Structural config
            let screenStates = UMap.makeEmpty HashIdentity.Structural config
            let gameState = GameState.make activeGameDispatcher
            let subsystems = { ImGui = imGui; PhysicsEngine2d = physicsEngine2d; PhysicsEngine3d = physicsEngine3d; RendererProcess = rendererProcess; AudioPlayer = audioPlayer }
            let simulants = UMap.singleton HashIdentity.Structural config (Game :> Simulant) None
            let worldExtension = { JobSystem = jobSystem; DestructionListRev = []; Dispatchers = dispatchers; Plugin = plugin }
            let world =
                { ChooseCount = 0
                  EventGraph = eventGraph
                  EntityStates = entityStates
                  GroupStates = groupStates
                  ScreenStates = screenStates
                  GameState = gameState
                  EntityMounts = UMap.makeEmpty HashIdentity.Structural config
                  Quadtree = quadtree
                  Octree = octree
                  AmbientState = ambientState
                  Subsystems = subsystems
                  Simulants = simulants
                  WorldExtension = worldExtension }
            let world = { world with GameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState world }
            World.choose world

        /// Make an empty world.
        static member makeEmpty config (plugin : NuPlugin) =

            // make the world's event delegate
            let eventGraph =
                let eventTracing = Constants.Engine.EventTracing
                let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None // NOTE: lambda expression is duplicated in multiple places...
                let eventFilter = Constants.Engine.EventFilter
                let globalSimulantGeneralized = { GsgAddress = atoa Game.GameAddress }
                let eventConfig = if config.Imperative then Imperative else Functional
                EventGraph.make eventTracerOpt eventFilter globalSimulantGeneralized eventConfig

            // make the default game dispatcher
            let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

            // make the default job system
            let jobSystem = JobSystemInline ()

            // make the world's dispatchers
            let dispatchers =
                { Facets = World.makeDefaultFacets ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  GroupDispatchers = World.makeDefaultGroupDispatchers ()
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  GameDispatchers = Map.ofList [defaultGameDispatcher] }

            // make the world's subsystems
            let imGui = ImGui (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
            let physicsEngine2d = StubPhysicsEngine.make ()
            let physicsEngine3d = StubPhysicsEngine.make ()
            let rendererProcess = RendererInline () :> RendererProcess
            rendererProcess.Start imGui.Fonts None // params implicate stub renderers
            let audioPlayer = StubAudioPlayer.make ()

            // make the world's ambient state
            let ambientState =
                let overlayRouter = OverlayRouter.empty
                let symbolics = Symbolics.makeEmpty ()
                AmbientState.make config.Imperative config.Accompanied true symbolics Overlayer.empty overlayRouter None

            // make the world's quadtree
            let quadtree = World.makeQuadtree ()

            // make the world's octree
            let octree = World.makeOctree ()

            // make the world
            let world = World.make plugin eventGraph jobSystem dispatchers quadtree octree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer (snd defaultGameDispatcher)

            // finally, register the game
            World.registerGame Game world

        /// Attempt to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake sdlDeps config (plugin : NuPlugin) =

            // attempt to create asset graph
            match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
            | Right assetGraph ->

                // populate metadata
                Metadata.generateMetadata config.Imperative assetGraph

                // make the world's event graph
                let eventGraph =
                    let eventTracing = Constants.Engine.EventTracing
                    let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None
                    let eventFilter = Constants.Engine.EventFilter
                    let globalSimulant = Game
                    let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                    let eventConfig = if config.Imperative then Imperative else Functional
                    EventGraph.make eventTracerOpt eventFilter globalSimulantGeneralized eventConfig
                    
                // make plug-in facets and dispatchers
                let pluginAssemblies = [|plugin.GetType().Assembly|]
                let pluginFacets = plugin.Birth<Facet> pluginAssemblies
                let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> pluginAssemblies
                let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> pluginAssemblies
                let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> pluginAssemblies
                let pluginGameDispatchers = plugin.Birth<GameDispatcher> pluginAssemblies

                // make the default game dispatcher
                let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

                // make the job system
                let jobSystem =
                    if Constants.Engine.RunSynchronously
                    then JobSystemInline () :> JobSystem
                    else JobSystemParallel (TimeSpan.FromSeconds 0.5) :> JobSystem

                // make the world's dispatchers
                let dispatchers =
                    { Facets = Map.addMany pluginFacets (World.makeDefaultFacets ())
                      EntityDispatchers = Map.addMany pluginEntityDispatchers (World.makeDefaultEntityDispatchers ())
                      GroupDispatchers = Map.addMany pluginGroupDispatchers (World.makeDefaultGroupDispatchers ())
                      ScreenDispatchers = Map.addMany pluginScreenDispatchers (World.makeDefaultScreenDispatchers ())
                      GameDispatchers = Map.addMany pluginGameDispatchers (Map.ofList [defaultGameDispatcher]) }

                // get the first game dispatcher
                let activeGameDispatcher =
                    match Array.tryHead pluginGameDispatchers with
                    | Some (_, dispatcher) -> dispatcher
                    | None -> GameDispatcher ()

                // make the world's subsystems
                let imGui = ImGui (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
                let physicsEngine2d = PhysicsEngine2d.make Constants.Physics.Gravity2dDefault
                let physicsEngine3d = PhysicsEngine3d.make Constants.Physics.Gravity3dDefault Metadata.tryGetFilePath Metadata.tryGetStaticModelMetadata
                let rendererProcess =
                    if Constants.Engine.RunSynchronously
                    then RendererInline () :> RendererProcess
                    else RendererThread () :> RendererProcess
                rendererProcess.Start imGui.Fonts (SdlDeps.getWindowOpt sdlDeps)
                rendererProcess.EnqueueMessage2d (LoadRenderPackage2d Assets.Default.PackageName) // enqueue default package hint
                let audioPlayer =
                    if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                    then SdlAudioPlayer.make () :> AudioPlayer
                    else StubAudioPlayer.make () :> AudioPlayer
                audioPlayer.EnqueueMessage (LoadAudioPackageMessage Assets.Default.PackageName) // enqueue default package hint

                // attempt to make the overlayer
                let intrinsicOverlays = World.makeIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays Assets.Global.OverlayerFilePath with
                | Right overlayer ->

                    // make the world's ambient state
                    let ambientState =
                        let overlays = Overlayer.getIntrinsicOverlays overlayer @ Overlayer.getExtrinsicOverlays overlayer
                        let overlayRoutes =
                            overlays |>
                            List.map (fun overlay -> overlay.OverlaidTypeNames |> List.map (fun typeName -> (typeName, overlay.OverlayName))) |>
                            List.concat
                        let overlayRouter = OverlayRouter.make overlayRoutes
                        let symbolics = Symbolics.makeEmpty ()
                        AmbientState.make config.Imperative config.Accompanied config.Advancing symbolics overlayer overlayRouter (Some sdlDeps)

                    // make the world's quadtree
                    let quadtree = World.makeQuadtree ()

                    // make the world's octree
                    let octree = World.makeOctree ()

                    // make the world
                    let world = World.make plugin eventGraph jobSystem dispatchers quadtree octree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer activeGameDispatcher

                    // add the keyed values
                    let (kvps, world) = plugin.MakeKeyedValues world
                    let world = List.fold (fun world (key, value) -> World.addKeyedValue key value world) world kvps

                    // register the game
                    let world = World.registerGame Game world
                    Right world

                // forward error messages
                | Left error -> Left error
            | Left error -> Left error

        /// Run the game engine, initializing dependencies as indicated by WorldConfig, and returning exit code upon
        /// termination.
        static member run worldConfig plugin =
            match SdlDeps.tryMake worldConfig.SdlConfig with
            | Right sdlDeps ->
                use sdlDeps = sdlDeps // bind explicitly to dispose automatically
                match World.tryMake sdlDeps worldConfig plugin with
                | Right world -> World.runWithCleanUp tautology id id id id id Live true world
                | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure