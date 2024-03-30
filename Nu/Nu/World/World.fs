// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open System.Reflection
open System.Threading
open SDL2
open Prime

[<AbstractClass; Sealed>]
type Nu () =

    static let mutable Initialized = false

    // NOTE: extracted from Nu.initPlus to shorten stack trace.
    [<DebuggerHidden>]
    static member private worldModuleSignal (signalObj : obj) (simulant : Simulant) world =
        World.signal (signalObj :?> Signal) simulant world

    /// Initialize the Nu game engine, allowing for additional user-defined initialization after setting up logging
    /// and function / lens references but before performing initialization involving values stored in constants.
    static member initPlus userInit =

        // init only if needed
        if not Initialized then

            // ensure the current culture is invariate
            Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init (Some "Log.txt")

            // init math module
            Math.Init ()

            // init reflection module
            Reflection.init ()

            // init simulant modules
            WorldModuleGame.init ()
            WorldModuleScreen.init ()
            WorldModuleGroup.init ()
            WorldModuleEntity.init ()

            // init simulant types
            Nu.Entity.init ()

            // init WorldTypes variables
            WorldTypes.EmptyGameContent <- GameContent.empty
            WorldTypes.EmptyScreenContent <- ScreenContent.empty
            WorldTypes.EmptyGroupContent <- GroupContent.empty
            WorldTypes.EmptyEntityContent <- EntityContent.empty

            // init WorldTypes F# reach-arounds
            WorldTypes.viewGame <- fun game world -> World.viewGameProperties (game :?> Game) (world :?> World)
            WorldTypes.viewScreen <- fun screen world -> World.viewScreenProperties (screen :?> Screen) (world :?> World)
            WorldTypes.viewGroup <- fun group world -> World.viewGroupProperties (group :?> Group) (world :?> World)
            WorldTypes.viewEntity <- fun entity world -> World.viewEntityProperties (entity :?> Entity) (world :?> World)
            WorldTypes.getSelectedScreenIdling <- fun worldObj -> World.getSelectedScreenIdling (worldObj :?> World)
            WorldTypes.getSelectedScreenTransitioning <- fun worldObj -> World.getSelectedScreenTransitioning (worldObj :?> World)
            WorldTypes.handleSubscribeAndUnsubscribeEvent <- fun subscribing eventAddress subscriber worldObj -> World.handleSubscribeAndUnsubscribeEvent subscribing eventAddress subscriber (worldObj :?> World)
            WorldTypes.getEntityIs2d <- fun entityObj worldObj -> World.getEntityIs2d (entityObj :?> Entity) (worldObj :?> World)

            // init WorldModule F# reach-arounds
            WorldModule.getSelected <- fun simulant world -> World.getSelected simulant world
            WorldModule.sortSubscriptionsByElevation <- fun subscriptions worldObj -> World.sortSubscriptionsByElevation subscriptions (worldObj :?> World)
            WorldModule.admitScreenElements <- fun screen world -> World.admitScreenElements screen world
            WorldModule.evictScreenElements <- fun screen world -> World.evictScreenElements screen world
            WorldModule.registerScreenPhysics <- fun screen world -> World.registerScreenPhysics screen world
            WorldModule.unregisterScreenPhysics <- fun screen world -> World.unregisterScreenPhysics screen world
            WorldModule.register <- fun simulant world -> World.register simulant world
            WorldModule.unregister <- fun simulant world -> World.unregister simulant world
            WorldModule.signal <- Nu.worldModuleSignal
            WorldModule.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModule.destroy <- fun simulant world -> World.destroy simulant world
            WorldModule.getEmptyEffect <- fun () -> Effect.empty :> obj

            // init user-defined initialization process
            let result = userInit ()

            // init vsync
            Vsync.Init Constants.Engine.RunSynchronously

            // init OpenGL assert mechanism
            OpenGL.Hl.InitAssert
#if DEBUG
                Constants.OpenGL.HlAssert
#else
                false
#endif

            // mark init flag
            Initialized <- true

            // fin
            result

        // already init'd
        else userInit ()

    /// Initialize the Nu game engine.
    static member init () =
        Nu.initPlus (fun () -> ())

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
                 NavBodyFacet ()
                 Nav3dConfigFacet ()
                 FollowerFacet ()
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
        static member make plugin eventGraph jobGraph dispatchers quadtree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer activeGameDispatcher =
            Nu.init () // ensure game engine is initialized
            let config = AmbientState.getConfig ambientState
            let entityStates = SUMap.makeEmpty HashIdentity.Structural config
            let groupStates = UMap.makeEmpty HashIdentity.Structural config
            let screenStates = UMap.makeEmpty HashIdentity.Structural config
            let gameState = GameState.make activeGameDispatcher
            let subsystems = { ImGui = imGui; PhysicsEngine2d = physicsEngine2d; PhysicsEngine3d = physicsEngine3d; RendererProcess = rendererProcess; AudioPlayer = audioPlayer }
            let simulants = UMap.singleton HashIdentity.Structural config (Game :> Simulant) None
            let worldExtension =
                { DestructionListRev = []
                  Dispatchers = dispatchers
                  Plugin = plugin
                  PropagationTargets = UMap.makeEmpty HashIdentity.Structural config }
            let world =
                { ChooseCount = 0
                  EventGraph = eventGraph
                  EntityStates = entityStates
                  GroupStates = groupStates
                  ScreenStates = screenStates
                  GameState = gameState
                  EntityMounts = UMap.makeEmpty HashIdentity.Structural config
                  Quadtree = quadtree
                  OctreeOpt = None
                  AmbientState = ambientState
                  Subsystems = subsystems
                  Simulants = simulants
                  JobGraph = jobGraph
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

            // make the default job graph
            let jobGraph = JobGraphInline ()

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
            let symbolics = Symbolics.makeEmpty ()
            let ambientState = AmbientState.make config.Imperative config.Accompanied true false symbolics Overlayer.empty None

            // make the world's quadtree
            let quadtree = Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize

            // make the world
            let world = World.make plugin eventGraph jobGraph dispatchers quadtree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer (snd defaultGameDispatcher)

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

                // make the job graph
                let jobGraph =
                    if Constants.Engine.RunSynchronously
                    then JobGraphInline () :> JobGraph
                    else JobGraphParallel (TimeSpan.FromSeconds 0.5) :> JobGraph

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
                    let symbolics = Symbolics.makeEmpty ()
                    let ambientState = AmbientState.make config.Imperative config.Accompanied config.Advancing config.FramePacing symbolics overlayer (Some sdlDeps)

                    // make the world's quadtree
                    let quadtree = Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize

                    // make the world
                    let world = World.make plugin eventGraph jobGraph dispatchers quadtree ambientState imGui physicsEngine2d physicsEngine3d rendererProcess audioPlayer activeGameDispatcher

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