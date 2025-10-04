// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Tracing
open System.Numerics
open System.Reflection
open System.Threading
open SDL2
open Prime

/// GC event listener. Currently just logs whenever an object larger than 85k is allocated to notify user of possible
/// LOH churn.
type private GcEventListener () =
    inherit EventListener ()

    static let mutable InstanceOpt = null

    override this.OnEventSourceCreated (eventSource : EventSource) =
        if eventSource.Name = "Microsoft-Windows-DotNETRuntime" then
            let gcEventsKeyword = Branchless.reinterpret 0x1L
            base.EnableEvents (eventSource, EventLevel.Verbose, gcEventsKeyword)

    override this.OnEventWritten(eventData: EventWrittenEventArgs) =
        if eventData.EventName = "GCAllocationTick_V4" && notNull eventData.Payload && eventData.Payload.Count >= 9 then
            match eventData.Payload.[8] with
            | :? uint64 as allocSize when allocSize >= uint64 Constants.Runtime.LohSize ->
                match eventData.Payload.[5] with
                | :? string as typeName ->
                    Log.info ("Allocated object of type '" + typeName + "' of size " + string allocSize + " on the LOH.")
                | _ -> ()
            | _ -> ()

    /// Initialize listener when gcDebug is true.
    static member init gcDebug =
        if gcDebug && isNull InstanceOpt then
            InstanceOpt <- new GcEventListener ()

/// Nu initialization functions.
/// NOTE: this is a type in order to avoid creating a module name that may clash with the namespace name in an
/// interactive environment.
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
            Log.init (Some Constants.Paths.LogFilePath)

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
            WorldModule.tryProcessGame <- fun game world -> World.tryProcessGame game world
            WorldModule.tryProcessScreen <- fun screen world -> World.tryProcessScreen screen world
            WorldModule.tryProcessGroup <- fun group world -> World.tryProcessGroup group world
            WorldModule.tryProcessEntity <- fun entity world -> World.tryProcessEntity entity world
            WorldModule.signal <- Nu.worldModuleSignal
            WorldModule.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModule.destroy <- fun simulant world -> World.destroy simulant world
            WorldModule.getEmptyEffect <- fun () -> Effect.empty :> obj

            // init entity module
            WorldModuleEntity.LayoutFacetType <- typeof<LayoutFacet>

            // init user-defined initialization process
            let result = userInit ()

            // init GC event listener
            GcEventListener.init Constants.Runtime.GcDebug

            // init vsync
            Vsync.Init Constants.Engine.RunSynchronously

            // init OpenGL assert mechanism
            OpenGL.Hl.InitAssert Constants.OpenGL.HlDebug

            // mark init flag
            Initialized <- true

            // fin
            result

        // already init'd
        else userInit ()

    /// Initialize the Nu game engine.
    static member init () =
        Nu.initPlus (fun () -> ())

/// Universal function definitions for the world (4/4).
[<AutoOpen>]
module WorldModule4 =

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
            // TODO: consider if we should reflectively generate most of these.
            Map.ofListBy World.pairWithName $
                [EntityDispatcher (true, false, false, false)
                 Entity2dDispatcher (false, false, false)
                 Entity3dDispatcher (false, false, false)
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
                 TextBoxDispatcher ()
                 FpsDispatcher ()
                 PanelDispatcher ()
                 CursorDispatcher ()
                 BasicStaticSpriteEmitterDispatcher ()
                 Effect2dDispatcher ()
                 Block2dDispatcher ()
                 Box2dDispatcher ()
                 Sphere2dDispatcher ()
                 Ball2dDispatcher ()
                 Character2dDispatcher ()
                 BodyJoint2dDispatcher ()
                 FluidEmitter2dDispatcher ()
                 TileMapDispatcher ()
                 TmxMapDispatcher ()
                 SpineSkeletonDispatcher ()
                 Lighting3dConfigDispatcher ()
                 LightProbe3dDispatcher ()
                 Light3dDispatcher ()
                 SkyBoxDispatcher ()
                 StaticBillboardDispatcher ()
                 AnimatedBillboardDispatcher ()
                 StaticModelDispatcher ()
                 AnimatedModelDispatcher ()
                 SensorModelDispatcher ()
                 RigidModelDispatcher ()
                 StaticModelSurfaceDispatcher ()
                 SensorModelSurfaceDispatcher ()
                 RigidModelSurfaceDispatcher ()
                 BasicStaticBillboardEmitterDispatcher ()
                 Effect3dDispatcher ()
                 Block3dDispatcher ()
                 Box3dDispatcher ()
                 Sphere3dDispatcher ()
                 Ball3dDispatcher ()
                 Character3dDispatcher ()
                 BodyJoint3dDispatcher ()
                 TerrainDispatcher ()
                 Nav3dConfigDispatcher ()
                 EditVolumeDispatcher ()
                 Permafreezer3dDispatcher ()
                 StaticModelHierarchyDispatcher ()
                 RigidModelHierarchyDispatcher ()]

        static member private makeDefaultFacets () =
            // TODO: consider if we should reflectively generate most of these.
            Map.ofListBy World.pairWithName $
                [Facet (false, false, false)
                 StaticSpriteFacet ()
                 AnimatedSpriteFacet ()
                 TextFacet ()
                 BackdroppableFacet ()
                 ButtonFacet ()
                 ToggleButtonFacet ()
                 RadioButtonFacet ()
                 FillBarFacet ()
                 FeelerFacet ()
                 TextBoxFacet ()
                 BasicStaticSpriteEmitterFacet ()
                 EffectFacet ()
                 RigidBodyFacet ()
                 BodyJointFacet ()
                 FluidEmitter2dFacet ()
                 TileMapFacet ()
                 TmxMapFacet ()
                 SpineSkeletonFacet ()
                 LayoutFacet ()
                 LightProbe3dFacet ()
                 Light3dFacet ()
                 SkyBoxFacet ()
                 StaticBillboardFacet ()
                 AnimatedBillboardFacet ()
                 BasicStaticBillboardEmitterFacet ()
                 StaticModelFacet ()
                 StaticModelSurfaceFacet ()
                 AnimatedModelFacet ()
                 TerrainFacet ()
                 EditVolumeFacet ()
                 TraversalInterpolatedFacet ()
                 NavBodyFacet ()
                 FollowerFacet ()
                 Freezer3dFacet ()]

        /// Update late bindings internally stored by the engine from types found in the given assemblies.
        static member updateLateBindings (assemblies : Assembly array) world =

            // prepare for late-bound type updating
            WorldImSim.Reinitializing <- true
            Content.UpdateLateBindingsCount <- inc Content.UpdateLateBindingsCount
            World.clearEntityFromClipboard world // HACK: clear what's on the clipboard rather than changing its dispatcher instance.
            world.WorldExtension.Plugin.CleanUp ()

            // update late-bound types
            let pluginType =
                assemblies
                |> Array.map (fun assembly -> assembly.GetTypes ())
                |> Array.concat
                |> Array.filter (fun ty -> ty.IsSubclassOf typeof<NuPlugin>)
                |> Array.filter (fun ty -> not ty.IsAbstract)
                |> Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0))
                |> Array.head
            let plugin = Activator.CreateInstance pluginType :?> NuPlugin
            let pluginFacets = plugin.Birth<Facet> assemblies
            let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> assemblies
            let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> assemblies
            let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> assemblies
            let pluginGameDispatchers = plugin.Birth<GameDispatcher> assemblies
            let worldExtension = world.WorldExtension
            let worldExtension = { worldExtension with Plugin = plugin }
            let worldExtension =
                Array.fold (fun worldExtension (facetName, facet) ->
                    { worldExtension with Dispatchers = { worldExtension.Dispatchers with Facets = Map.add facetName facet worldExtension.Dispatchers.Facets }})
                    worldExtension pluginFacets
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with Dispatchers = { worldExtension.Dispatchers with EntityDispatchers = Map.add dispatcherName dispatcher worldExtension.Dispatchers.EntityDispatchers }})
                    worldExtension pluginEntityDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with Dispatchers = { worldExtension.Dispatchers with GroupDispatchers = Map.add dispatcherName dispatcher worldExtension.Dispatchers.GroupDispatchers }})
                    worldExtension pluginGroupDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with Dispatchers = { worldExtension.Dispatchers with ScreenDispatchers = Map.add dispatcherName dispatcher worldExtension.Dispatchers.ScreenDispatchers }})
                    worldExtension pluginScreenDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with Dispatchers = { worldExtension.Dispatchers with GameDispatchers = Map.add dispatcherName dispatcher worldExtension.Dispatchers.GameDispatchers }})
                    worldExtension pluginGameDispatchers
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

            // update late bindings for all simulants
            let lateBindingses =
                Array.concat
                    [|Array.map (snd >> cast<LateBindings>) pluginFacets
                      Array.map (snd >> cast<LateBindings>) pluginEntityDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginGroupDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginScreenDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginGameDispatchers|]
            for (simulant, _) in world.Simulants do
                for lateBindings in lateBindingses do
                    World.updateLateBindings3 lateBindings simulant world
            for (simulant, _) in world.Simulants do
                World.trySynchronize false true simulant world

        /// Make the world.
        static member makePlus
            plugin eventGraph jobGraph geometryViewport rasterViewport outerViewport dispatchers quadtree octree worldConfig sdlDepsOpt
            imGui physicsEngine2d physicsEngine3d rendererPhysics3dOpt rendererProcess audioPlayer cursorClient activeGameDispatcher =
            Nu.init () // ensure game engine is initialized
            let symbolics = Symbolics.makeEmpty ()
            let intrinsicOverlays = World.makeIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
            let overlayer = Overlayer.makeFromFileOpt intrinsicOverlays Assets.Global.OverlayerFilePath
            let timers = Timers.make ()
            let ambientState = AmbientState.make worldConfig.Imperative worldConfig.Accompanied worldConfig.Advancing worldConfig.FramePacing symbolics overlayer timers sdlDepsOpt
            let config = AmbientState.getConfig ambientState
            let entityStates = SUMap.makeEmpty HashIdentity.Structural config
            let groupStates = UMap.makeEmpty HashIdentity.Structural config
            let screenStates = UMap.makeEmpty HashIdentity.Structural config
            let gameState = GameState.make activeGameDispatcher
            let subsystems =
                { ImGui = imGui
                  PhysicsEngine2d = physicsEngine2d
                  PhysicsEngine3d = physicsEngine3d
                  RendererProcess = rendererProcess
                  RendererPhysics3dOpt = rendererPhysics3dOpt
                  AudioPlayer = audioPlayer
                  CursorClient = cursorClient }
            let simulants = UMap.singleton HashIdentity.Structural config (Game :> Simulant) None
            let entitiesIndexed = UMap.makeEmpty HashIdentity.Structural config
            let worldExtension =
                { ContextImSim = Address.empty
                  DeclaredImSim = Address.empty
                  SimulantsImSim = SUMap.makeEmpty HashIdentity.Structural config
                  SubscriptionsImSim = SUMap.makeEmpty HashIdentity.Structural config
                  JobGraph = jobGraph
                  GeometryViewport = geometryViewport
                  RasterViewport = rasterViewport
                  OuterViewport = outerViewport
                  DestructionListRev = []
                  Dispatchers = dispatchers
                  Plugin = plugin
                  PropagationTargets = UMap.makeEmpty HashIdentity.Structural config }
            let worldState =
                { EventGraph = eventGraph
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Unchecked.defaultof<Entity>, entityStates)) Unchecked.defaultof<EntityState>
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
                  EntitiesIndexed = entitiesIndexed
                  WorldExtension = worldExtension }
            let worldState =
                { worldState with
                    GameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState worldState }
            let world = { WorldState = worldState }
            WorldTypes.WorldForDebug <- world
            world

        /// Make a world with stub dependencies.
        static member makeStub worldConfig (plugin : NuPlugin) =

            // make the world's event delegate
            let eventGraph =
                let eventTracing = Constants.Engine.EventTracing
                let eventTracerOpt = if eventTracing then Some (Log.custom "Event") else None // NOTE: lambda expression is duplicated in multiple places...
                let eventFilter = Constants.Engine.EventFilter
                let globalSimulantGeneralized = { GsgAddress = atoa Game.GameAddress }
                let eventConfig = if worldConfig.Imperative then Imperative else Functional
                EventGraph.make eventTracerOpt eventFilter globalSimulantGeneralized eventConfig

            // make the default game dispatcher
            let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

            // make the default job graph
            let jobGraph = JobGraphInline ()

            // make the default viewports
            let outerViewport = Viewport.makeOuter Constants.Render.DisplayVirtualResolution
            let rasterViewport = Viewport.makeRaster outerViewport.Inset outerViewport.Bounds
            let geometryViewport = Viewport.makeGeometry outerViewport.Bounds.Size

            // make the world's dispatchers
            let dispatchers =
                { Facets = World.makeDefaultFacets ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  GroupDispatchers = World.makeDefaultGroupDispatchers ()
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  GameDispatchers = Map.ofList [defaultGameDispatcher] }

            // make the world's subsystems
            let imGui = ImGui (true, outerViewport.Bounds.Size)
            let physicsEngine2d = StubPhysicsEngine.make ()
            let physicsEngine3d = StubPhysicsEngine.make ()
            let rendererProcess = RendererInline () :> RendererProcess
            rendererProcess.Start imGui.Fonts None geometryViewport rasterViewport outerViewport // params implicate stub renderers
            let audioPlayer = StubAudioPlayer.make ()
            let cursorClient = StubCursorClient.make ()

            // make the world's spatial trees
            let quadtree = Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize
            let octree = Octree.make Constants.Engine.OctreeDepth Constants.Engine.OctreeSize

            // make the world
            let world =
                World.makePlus
                    plugin eventGraph jobGraph geometryViewport rasterViewport outerViewport dispatchers quadtree octree worldConfig None
                    imGui physicsEngine2d physicsEngine3d None rendererProcess audioPlayer cursorClient (snd defaultGameDispatcher)

            // register the game
            World.registerGame Game world

            // fin
            world

        /// Make the world with the given dependencies.
        static member make sdlDeps config geometryViewport rasterViewport (outerViewport : Viewport) (plugin : NuPlugin) =

            // create asset graph
            let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath

            // compute initial packages
            let initialPackages = Assets.Default.PackageName :: plugin.InitialPackages

            // initialize metadata and load initial package
            Metadata.init assetGraph
            for package in initialPackages do
                Metadata.loadMetadataPackage package

            // make the world's event graph
            let eventGraph =
                let eventTracing = Constants.Engine.EventTracing
                let eventTracerOpt = if eventTracing then Some (Log.custom "Event") else None
                let eventFilter = Constants.Engine.EventFilter
                let globalSimulant = Game
                let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                let eventConfig = if config.Imperative then Imperative else Functional
                EventGraph.make eventTracerOpt eventFilter globalSimulantGeneralized eventConfig

            // collect plugin assemblies
            let pluginAssemblyNamePredicate =
                fun (assemblyName : AssemblyName) ->
                    not (assemblyName.Name.StartsWith "System.") && // OPTIMIZATION: skip known irrelevant assemblies.
                    not (assemblyName.Name.StartsWith "FSharp.") &&
                    not (assemblyName.Name.StartsWith "Prime.") &&
                    not (assemblyName.Name.StartsWith "Nu.") &&
                    assemblyName.Name <> "Prime" &&
                    assemblyName.Name <> "Nu" &&
                    assemblyName.Name <> "netstandard" &&
                    assemblyName.Name <> "SDL2-CS"
            let pluginAssembly = plugin.GetType().Assembly
            let pluginAssembliesReferenced = Reflection.loadReferencedAssembliesTransitively pluginAssemblyNamePredicate pluginAssembly
            let pluginAssemblies = Array.cons pluginAssembly pluginAssembliesReferenced

            // make plug-in facets and dispatchers
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

            // make the world's subsystems, loading initial packages where applicable
            let imGui = ImGui (false, outerViewport.Bounds.Size)
            let physicsEngine2d = PhysicsEngine2d.make (Constants.Physics.GravityDefault * Constants.Engine.Meter2d)
            let physicsEngine3d = PhysicsEngine3d.make Constants.Physics.GravityDefault
            let joltDebugRendererImGuiOpt = new JoltDebugRendererImGui ()
            let rendererProcess =
                if Constants.Engine.RunSynchronously
                then RendererInline () :> RendererProcess
                else RendererThread () :> RendererProcess
            rendererProcess.Start imGui.Fonts (SdlDeps.getWindowOpt sdlDeps) geometryViewport rasterViewport outerViewport
            for package in initialPackages do
                rendererProcess.EnqueueMessage2d (LoadRenderPackage2d package)
            for package in initialPackages do
                rendererProcess.EnqueueMessage3d (LoadRenderPackage3d package)
            let audioPlayer =
                if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                then SdlAudioPlayer.make () :> AudioPlayer
                else StubAudioPlayer.make () :> AudioPlayer
            for package in initialPackages do
                audioPlayer.EnqueueMessage (LoadAudioPackageMessage package)
            let cursorClient = SdlCursorClient.make () :> CursorClient
            for package in initialPackages do
                cursorClient.LoadCursorPackage package

            // make the world's spatial trees
            let quadtree = Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize
            let octree = Octree.make Constants.Engine.OctreeDepth Constants.Engine.OctreeSize

            // make the world
            let world =
                World.makePlus
                    plugin eventGraph jobGraph geometryViewport rasterViewport outerViewport dispatchers quadtree octree config (Some sdlDeps)
                    imGui physicsEngine2d physicsEngine3d (Some joltDebugRendererImGuiOpt) rendererProcess audioPlayer cursorClient activeGameDispatcher

            // add the keyed values
            for (key, value) in plugin.MakeKeyedValues world do
                World.addKeyedValue key value world

            // register the game
            World.registerGame Game world
            world

        /// Run the game engine, initializing dependencies as indicated by WorldConfig, and returning exit code upon
        /// termination.
        static member runPlus runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess worldConfig windowSize geometryViewport rasterViewport outerViewport plugin =
            match SdlDeps.tryMake worldConfig.SdlConfig worldConfig.Accompanied windowSize with
            | Right sdlDeps ->
                use sdlDeps = sdlDeps // bind explicitly to dispose automatically
                let world = World.make sdlDeps worldConfig geometryViewport rasterViewport outerViewport plugin
                World.runWithCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess true world
            | Left error -> Log.error error; Constants.Engine.ExitCodeFailure

        /// Run the game engine, initializing dependencies as indicated by WorldConfig, and returning exit code upon
        /// termination.
        static member run worldConfig plugin =
            let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
            let outerViewport = Viewport.makeOuter windowSize
            let rasterViewport = Viewport.makeRaster outerViewport.Inset outerViewport.Bounds
            let geometryViewport = Viewport.makeGeometry outerViewport.Bounds.Size
            World.runPlus tautology ignore ignore ignore ignore ignore worldConfig outerViewport.Bounds.Size geometryViewport rasterViewport outerViewport plugin