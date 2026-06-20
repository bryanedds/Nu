// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Tracing
open System.Numerics
open System.Reflection
open System.Threading
open SDL
open Prime

module NativeLibraryLoading =

    open System.IO
    open System.Diagnostics.CodeAnalysis
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    let tryLoadNativeLibraryOpt libraryPath =
        let mutable handle = 0n
        if File.Exists libraryPath && NativeLibrary.TryLoad (libraryPath, &handle)
        then Some handle
        else None

    let tryLoadNativeLibraryByNameOpt libraryName =
        let mutable handle = 0n
        if NativeLibrary.TryLoad (libraryName, &handle)
        then Some handle
            else None

    module Apple =
        let frameworkPath frameworkName =
            Path.Combine (AppContext.BaseDirectory, "Frameworks", frameworkName + ".framework", frameworkName)

        let trySetDllImportResolver (assembly : Assembly) mappedLibraries =
            let resolver =
                DllImportResolver (fun libraryName _ _ ->
                    mappedLibraries
                    |> List.tryPick (fun (candidateName, frameworkName) ->
                        if String.Equals (libraryName, candidateName, StringComparison.Ordinal) then
                            if frameworkName = "__Internal" then
                                tryLoadNativeLibraryByNameOpt frameworkName
                            else tryLoadNativeLibraryOpt (frameworkPath frameworkName)
                        else None)
                    |> Option.defaultValue 0n)
            try NativeLibrary.SetDllImportResolver (assembly, resolver)
            with :? InvalidOperationException -> ()

        let private loadAssimpFramework () =
            let assimpLibrary = global.Assimp.Unmanaged.AssimpLibrary.Instance
            if not assimpLibrary.IsLibraryLoaded then
                let libraryPath = frameworkPath "assimp"
                let libraryType = typeof<global.Assimp.Unmanaged.UnmanagedLibrary>
                let getField name =
                    let field = libraryType.GetField (name, BindingFlags.Instance ||| BindingFlags.NonPublic)
                    if isNull field then failwith ("Could not find AssimpNet field '" + name + "'.")
                    field
                let impl = (getField "m_impl").GetValue assimpLibrary
                let loadLibraryMethod = impl.GetType().GetMethod ("LoadLibrary", BindingFlags.Instance ||| BindingFlags.Public)
                if isNull loadLibraryMethod then failwith "Could not find AssimpNet implementation LoadLibrary method."
                let loaded = loadLibraryMethod.Invoke (impl, [| box libraryPath |]) :?> bool
                if not loaded then failwith ("Could not load Assimp framework from '" + libraryPath + "'.")
                (getField "m_libraryPath").SetValue (assimpLibrary, libraryPath)
                (getField "m_checkNeedsLoading").SetValue (assimpLibrary, false)
                // AssimpNet's public LoadLibrary appends ".dylib" to extensionless paths,
                // but framework executables are intentionally extensionless.
                let onLibraryLoadedMethod = libraryType.GetMethod ("OnLibraryLoaded", BindingFlags.Instance ||| BindingFlags.NonPublic)
                if isNull onLibraryLoadedMethod then failwith "Could not find AssimpNet OnLibraryLoaded method."
                onLibraryLoadedMethod.Invoke (assimpLibrary, [||]) |> ignore

        [<DynamicDependency ("JoltDllImporterResolver", "JoltPhysicsSharp.JoltApi", "JoltPhysicsSharp")>] // iOS release mode linker will drop the event without this attribute
        let private configureJoltFramework () =
            let joltApiType = typeof<JoltPhysicsSharp.Jolt>.Assembly.GetType ("JoltPhysicsSharp.JoltApi", true)
            RuntimeHelpers.RunClassConstructor joltApiType.TypeHandle
            let joltResolverEvent = joltApiType.GetEvent ("JoltDllImporterResolver", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            if isNull joltResolverEvent then failwith "Could not find JoltPhysicsSharp JoltDllImporterResolver event."
            let resolver =
                DllImportResolver (fun libraryName _ _ ->
                    assert (libraryName = "joltc")
                    tryLoadNativeLibraryOpt (frameworkPath "joltc") |> Option.defaultValue 0n)
            joltResolverEvent.AddEventHandler (null, resolver)

        let private configureVmaFramework () =
            let vmaType = typeof<Vortice.Vulkan.Vma>
            RuntimeHelpers.RunClassConstructor vmaType.TypeHandle
            let vmaResolverEvent = vmaType.GetEvent ("VmaDllImporterResolver", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            if isNull vmaResolverEvent then failwith "Could not find Vortice Vulkan VmaDllImporterResolver event."
            let resolver =
                DllImportResolver (fun libraryName _ _ ->
                    assert (libraryName = "vma")
                    tryLoadNativeLibraryOpt (frameworkPath "vma") |> Option.defaultValue 0n)
            vmaResolverEvent.AddEventHandler (null, resolver)

        let configureFrameworkNativeLibraries () =
            trySetDllImportResolver typeof<World>.Assembly
                ["cimgui", "cimgui"]
            trySetDllImportResolver typeof<ImGuiNET.ImGui>.Assembly
                ["cimgui", "cimgui"]
            trySetDllImportResolver typeof<BulletSharp.BulletObject>.Assembly
                ["libbulletc", "bulletc"]
            loadAssimpFramework ()
            configureJoltFramework ()
            configureVmaFramework ()

    module iOS =

        let setupSdl () = // unfortunately we need to expose this to user code before iOS SDL is initialized

            Apple.trySetDllImportResolver typeof<SDL.SDL3>.Assembly
                ["SDL3", "SDL3"]
            Apple.trySetDllImportResolver typeof<SDL.SDL3_image>.Assembly
                ["SDL3_image", "SDL3_image"]
            Apple.trySetDllImportResolver typeof<SDL.SDL3_ttf>.Assembly
                ["SDL3_ttf", "SDL3_ttf"]
            Apple.trySetDllImportResolver typeof<SDL.SDL3_mixer>.Assembly
                ["SDL3_mixer", "SDL3_mixer"]

        [<DynamicDependency ("ResolveLibrary", "Vortice.ShaderCompiler.Native", "Vortice.ShaderCompiler")>] // iOS release mode linker will drop the setter without this attribute
        let private configureShaderCompilerLibrary () =
            let shaderCompilerAssembly = typeof<Vortice.ShaderCompiler.ShaderMacro>.Assembly
            let nativeType = shaderCompilerAssembly.GetType ("Vortice.ShaderCompiler.Native", true)
            RuntimeHelpers.RunClassConstructor nativeType.TypeHandle
            let resolveLibraryProperty = nativeType.GetProperty ("ResolveLibrary", BindingFlags.Static ||| BindingFlags.Public)
            if isNull resolveLibraryProperty then failwith "Could not find Vortice.ShaderCompiler.Native.ResolveLibrary property."
            let resolver =
                DllImportResolver (fun libraryName _ _ ->
                    assert (libraryName = "shaderc_shared")
                    let hasShadercInitSymbol handle =
                        let mutable symbol = 0n
                        NativeLibrary.TryGetExport (handle, "shaderc_compiler_initialize", &symbol)
                    [Apple.frameworkPath "shaderc_shared"; "shaderc_shared"; "@rpath/shaderc_shared.framework/shaderc_shared"]
                    |> List.tryPick (fun candidate ->
                        tryLoadNativeLibraryOpt candidate
                        |> Option.filter hasShadercInitSymbol)
                    |> Option.defaultValue 0n)
            resolveLibraryProperty.SetValue (null, resolver)

        let configureIosNativeLibraries () =
            configureShaderCompilerLibrary ()
            Apple.configureFrameworkNativeLibraries ()

            let moltenVkPath = Apple.frameworkPath "MoltenVK"
            let result = Vortice.Vulkan.Vulkan.vkInitialize moltenVkPath
            if result <> Vortice.Vulkan.VkResult.Success then
                failwith ("Could not initialize Vulkan from '" + moltenVkPath + "' due to: " + string result)
            SDL.SDL3.SDL_SetHint (SDL.SDL3.SDL_HINT_VULKAN_LIBRARY, moltenVkPath) |> ignore<SDL.SDLBool>

    module Android =

        // We need this for Release mode with full AOT, but not necessary for Debug mode.
        let configureAndroidNativeLibraries () =
            let trySetAndroidDllImportResolver (assembly : Assembly) mappedLibraries =
                let resolver =
                    DllImportResolver (fun libraryName _ _ ->
                        mappedLibraries
                        |> List.tryPick (fun (requestedName, candidateName) ->
                            if String.Equals (libraryName, requestedName, StringComparison.Ordinal)
                            then tryLoadNativeLibraryByNameOpt candidateName
                            else None)
                        |> Option.orElseWith (fun () -> tryLoadNativeLibraryByNameOpt libraryName)
                        |> Option.defaultValue 0n)
                try NativeLibrary.SetDllImportResolver (assembly, resolver)
                with :? InvalidOperationException -> ()

            // Normalize desktop-oriented sonames requested by some native bindings.
            let androidAliases =
                ["libc.so.6", "libc"
                 "libc.so", "libc"
                 "libdl.so", "libdl"
                 "libvulkan.so.1", "libvulkan"
                 "libvulkan.so", "libvulkan"
                 "liblog", "liblog"
                 "liblog.so", "liblog"]
            trySetAndroidDllImportResolver typeof<global.Assimp.Unmanaged.AssimpLibrary>.Assembly androidAliases
            trySetAndroidDllImportResolver typeof<Vortice.Vulkan.Vulkan>.Assembly androidAliases
            trySetAndroidDllImportResolver typeof<World>.Assembly androidAliases

/// GC event listener. Currently just logs whenever an object larger than 85k is allocated to notify user of possible
/// LOH churn.
type private GcEventListener () =
    inherit EventListener ()

    static let mutable InstanceOpt = null

    override this.OnEventSourceCreated (eventSource : EventSource) =
        if eventSource.Name = "Microsoft-Windows-DotNETRuntime" then
            let gcEventsKeyword = Branchless.reinterpret 0x1L
            base.EnableEvents (eventSource, EventLevel.Verbose, gcEventsKeyword)

    override this.OnEventWritten (eventData : EventWrittenEventArgs) =
        if eventData.EventName = "GCAllocationTick_V4" && notNull eventData.Payload && eventData.Payload.Count >= 9 then
            match eventData.Payload[8] with
            | :? uint64 as allocSize when allocSize >= uint64 Constants.Runtime.LohSize ->
                match eventData.Payload[5] with
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

            if OperatingSystem.IsIOS () then
                NativeLibraryLoading.iOS.configureIosNativeLibraries ()
                Log.init None // disable Nu's default file log because the iOS app bundle is read-only.
            elif OperatingSystem.IsAndroid () then
                NativeLibraryLoading.Android.configureAndroidNativeLibraries ()
                 // disable Nu's default file log because the Android asset pack directory should be treated as read-only for incremental updates to work:
                 // https://developer.android.com/reference/com/google/android/play/core/assetpacks/AssetPackManager#getpacklocation
                Log.init None
            elif OperatingSystem.IsMacOS () then
                NativeLibraryLoading.Apple.configureFrameworkNativeLibraries ()

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

            // init WorldTypes F# reach functions
            WorldTypes.viewGame <- fun game world -> World.viewGameProperties (game :?> Game) (world :?> World)
            WorldTypes.viewScreen <- fun screen world -> World.viewScreenProperties (screen :?> Screen) (world :?> World)
            WorldTypes.viewGroup <- fun group world -> World.viewGroupProperties (group :?> Group) (world :?> World)
            WorldTypes.viewEntity <- fun entity world -> World.viewEntityProperties (entity :?> Entity) (world :?> World)
            WorldTypes.getSelectedScreenIdling <- fun worldObj -> World.getSelectedScreenIdling (worldObj :?> World)
            WorldTypes.getSelectedScreenTransitioning <- fun worldObj -> World.getSelectedScreenTransitioning (worldObj :?> World)
            WorldTypes.handleSubscribeAndUnsubscribeEvent <- fun subscribing eventAddress subscriber worldObj -> World.handleSubscribeAndUnsubscribeEvent subscribing eventAddress subscriber (worldObj :?> World)
            WorldTypes.getEntityIs2d <- fun entityObj worldObj -> World.getEntityIs2d (entityObj :?> Entity) (worldObj :?> World)

            // init WorldModuleInternal F# reach functions
            WorldModuleInternal.getSelected <- fun simulant world -> World.getSelected simulant world
            WorldModuleInternal.sortSubscriptionsByElevation <- fun subscriptions worldObj -> World.sortSubscriptionsByElevation subscriptions (worldObj :?> World)
            WorldModuleInternal.admitScreenElements <- fun screen world -> World.admitScreenElements screen world
            WorldModuleInternal.evictScreenElements <- fun screen world -> World.evictScreenElements screen world
            WorldModuleInternal.registerScreenPhysics <- fun screen world -> World.registerScreenPhysics screen world
            WorldModuleInternal.unregisterScreenPhysics <- fun screen world -> World.unregisterScreenPhysics screen world
            WorldModuleInternal.register <- fun simulant world -> World.register simulant world
            WorldModuleInternal.unregister <- fun simulant world -> World.unregister simulant world
            WorldModuleInternal.tryProcessGame <- fun game world -> World.tryProcessGame game world
            WorldModuleInternal.tryProcessScreen <- fun screen world -> World.tryProcessScreen screen world
            WorldModuleInternal.tryProcessGroup <- fun group world -> World.tryProcessGroup group world
            WorldModuleInternal.tryProcessEntity <- fun entity world -> World.tryProcessEntity entity world
            WorldModuleInternal.signal <- Nu.worldModuleSignal
            WorldModuleInternal.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModuleInternal.destroy <- fun simulant world -> World.destroy simulant world
            WorldModuleInternal.getEmptyEffect <- fun () -> Effect.empty :> obj

            // init entity module
            WorldModuleEntity.LayoutFacetType <- typeof<LayoutFacet>

            // init user-defined initialization process
            let result = userInit ()

            // init GC event listener
            GcEventListener.init Constants.Runtime.GcDebug

            // init vsync
            Vsync.Init Constants.Engine.RunSynchronously



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
            Assembly.GetExecutingAssembly().GetTypes()
            |> Array.filter (fun ty -> ty.IsSubclassOf typeof<EntityDispatcher>)
            |> Array.filter (fun ty -> not ty.IsAbstract)
            |> Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0))
            |> Array.map (fun ty -> Activator.CreateInstance ty :?> EntityDispatcher)
            |> flip Array.append // TODO: utilize Array.prepend if it becomes available.
                [|EntityDispatcher (true, false, false, false)
                  Entity2dDispatcher (false, false, false)
                  Entity3dDispatcher (false, false, false)|]
            |> Map.ofArrayBy World.pairWithName

        static member private makeDefaultFacets () =
            Assembly.GetExecutingAssembly().GetTypes()
            |> Array.filter (fun ty -> ty.IsSubclassOf typeof<Facet>)
            |> Array.filter (fun ty -> not ty.IsAbstract)
            |> Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0))
            |> Array.map (fun ty -> Activator.CreateInstance ty :?> Facet)
            |> Array.cons (Facet (false, false, false))
            |> Map.ofArrayBy World.pairWithName

        /// Update late bindings internally stored by the engine from types found in the given assemblies.
        static member updateLateBindings initializing (assemblies : Assembly array) world =

            // prepare for late-bound type updating
            WorldImSim.Initializing <- initializing
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
                    { worldExtension with LateBindingsInstances = { worldExtension.LateBindingsInstances with Facets = Map.add facetName facet worldExtension.LateBindingsInstances.Facets }})
                    worldExtension pluginFacets
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with LateBindingsInstances = { worldExtension.LateBindingsInstances with EntityDispatchers = Map.add dispatcherName dispatcher worldExtension.LateBindingsInstances.EntityDispatchers }})
                    worldExtension pluginEntityDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with LateBindingsInstances = { worldExtension.LateBindingsInstances with GroupDispatchers = Map.add dispatcherName dispatcher worldExtension.LateBindingsInstances.GroupDispatchers }})
                    worldExtension pluginGroupDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with LateBindingsInstances = { worldExtension.LateBindingsInstances with ScreenDispatchers = Map.add dispatcherName dispatcher worldExtension.LateBindingsInstances.ScreenDispatchers }})
                    worldExtension pluginScreenDispatchers
            let worldExtension =
                Array.fold (fun worldExtension (dispatcherName, dispatcher) ->
                    { worldExtension with LateBindingsInstances = { worldExtension.LateBindingsInstances with GameDispatchers = Map.add dispatcherName dispatcher worldExtension.LateBindingsInstances.GameDispatchers }})
                    worldExtension pluginGameDispatchers
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

            // update late bindings for all simulants
            let lateBindingsInstances =
                Array.concat
                    [|Array.map (snd >> cast<LateBindings>) pluginFacets
                      Array.map (snd >> cast<LateBindings>) pluginEntityDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginGroupDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginScreenDispatchers
                      Array.map (snd >> cast<LateBindings>) pluginGameDispatchers|]
            for (simulant, _) in world.Simulants do
                for lateBindings in lateBindingsInstances do
                    World.updateLateBindings3 lateBindings simulant world
            for (simulant, _) in world.Simulants do
                World.trySynchronize initializing true simulant world

        /// Make the world.
        static member makePlus
            tryMakeEditContext plugin eventGraph jobGraph geometryViewport windowViewport lateBindingsInstances quadtree octree worldConfig sdlDepsOpt
            imGui physicsEngine2d physicsEngine3d rendererPhysics3dOpt rendererProcess audioPlayer cursorClient activeGameDispatcher =
            Nu.init () // ensure game engine is initialized
            let symbolics = Symbolics.makeEmpty ()
            let intrinsicOverlays = World.makeIntrinsicOverlays lateBindingsInstances.Facets lateBindingsInstances.EntityDispatchers
            let overlayer = Overlayer.makeFromFileOpt intrinsicOverlays Assets.Global.OverlayerFilePath
            let timers = Timers.make ()
            let ambientState = AmbientState.make worldConfig.Imperative worldConfig.Accompanied worldConfig.Advancing worldConfig.FramePacing symbolics overlayer timers sdlDepsOpt
            let collectionConfig = AmbientState.getCollectionConfig ambientState
            let entityStates = SUMap.makeEmpty HashIdentity.Structural collectionConfig
            let groupStates = UMap.makeEmpty HashIdentity.Structural collectionConfig
            let screenStates = UMap.makeEmpty HashIdentity.Structural collectionConfig
            let gameState = GameState.make activeGameDispatcher
            let subsystems =
                { ImGui = imGui
                  PhysicsEngine2d = physicsEngine2d
                  PhysicsEngine3d = physicsEngine3d
                  RendererProcess = rendererProcess
                  RendererPhysics3dOpt = rendererPhysics3dOpt
                  AudioPlayer = audioPlayer
                  CursorClient = cursorClient }
            let simulants = UMap.singleton HashIdentity.Structural collectionConfig (Game :> Simulant) None
            let entitiesIndexed = UMap.makeEmpty HashIdentity.Structural collectionConfig
            let worldExtension =
                { ContextImSim = Address.empty
                  DeclaredImSim = Address.empty
                  SimulantJournals = SUMap.makeEmpty HashIdentity.Structural collectionConfig
                  SubscriptionJournals = SUMap.makeEmpty HashIdentity.Structural collectionConfig
                  JobGraph = jobGraph
                  GeometryViewport = geometryViewport
                  WindowViewport = windowViewport
                  DestructionListRev = []
                  LateBindingsInstances = lateBindingsInstances
                  TryMakeEditContext = tryMakeEditContext
                  Plugin = plugin
                  PropagationTargets = UMap.makeEmpty HashIdentity.Structural collectionConfig
                  EditDeferrals = UMap.makeEmpty HashIdentity.Structural collectionConfig }
            let worldState =
                { EventGraph = eventGraph
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Unchecked.defaultof<Entity>, entityStates)) Unchecked.defaultof<EntityState>
                  EntityStates = entityStates
                  GroupStates = groupStates
                  ScreenStates = screenStates
                  GameState = gameState
                  EntityMounts = UMap.makeEmpty HashIdentity.Structural collectionConfig
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
        static member makeStub tryMakeEditContext worldConfig (plugin : NuPlugin) =

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
            let windowViewport = Viewport.makeWindow1 Constants.Render.DisplayVirtualResolution
            let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size

            // make the world's late-bindings instances
            let lateBindingsInstances =
                { Facets = World.makeDefaultFacets ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  GroupDispatchers = World.makeDefaultGroupDispatchers ()
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  GameDispatchers = Map.ofList [defaultGameDispatcher] }

            // make the world's subsystems
            let imGui = ImGui (true, windowViewport.Bounds.Size)
            let physicsEngine2d = StubPhysicsEngine.make ()
            let physicsEngine3d = StubPhysicsEngine.make ()
            let rendererProcess = RendererInline () :> RendererProcess
            rendererProcess.Start imGui.Fonts None geometryViewport windowViewport // params implicate stub renderers
            let audioPlayer = StubAudioPlayer.make ()
            let cursorClient = StubCursorClient.make ()

            // make the world's spatial trees
            let quadtree = Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize
            let octree = Octree.make Constants.Engine.OctreeDepth Constants.Engine.OctreeSize

            // make the world
            let world =
                World.makePlus
                    tryMakeEditContext plugin eventGraph jobGraph geometryViewport windowViewport lateBindingsInstances quadtree octree worldConfig None
                    imGui physicsEngine2d physicsEngine3d None rendererProcess audioPlayer cursorClient (snd defaultGameDispatcher)

            // register the game
            World.registerGame Game world

            // fin
            world

        /// Make the world with the given dependencies.
        static member make tryMakeEditContext sdlDeps config geometryViewport (windowViewport : Viewport) (plugin : NuPlugin) =

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
                    not (assemblyName.Name.StartsWith "ppy.SDL3")
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

            // make the world's late-bindings instances
            let lateBindingsInstances =
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
            let imGui = ImGui (false, windowViewport.Bounds.Size)
            let physicsEngine2d = plugin.MakePhysicsEngine2d ()
            let physicsEngine3d = JoltPhysicsEngine.make Constants.Physics.GravityDefault
            let joltDebugRendererImGuiOpt = new JoltDebugRendererImGui ()
            let rendererProcess =
                if Constants.Engine.RunSynchronously
                then RendererInline () :> RendererProcess
                else RendererThread () :> RendererProcess
            rendererProcess.Start imGui.Fonts (SdlDeps.getWindowOpt sdlDeps) geometryViewport windowViewport
            for package in initialPackages do
                rendererProcess.EnqueueMessage2d (LoadRenderPackage2d package)
            for package in initialPackages do
                rendererProcess.EnqueueMessage3d (LoadRenderPackage3d package)
            let audioPlayer =
                if SDL3.SDL_WasInit SDL_InitFlags.SDL_INIT_AUDIO <> LanguagePrimitives.EnumOfValue 0u
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
                    tryMakeEditContext plugin eventGraph jobGraph geometryViewport windowViewport lateBindingsInstances quadtree octree config (Some sdlDeps)
                    imGui physicsEngine2d physicsEngine3d (Some joltDebugRendererImGuiOpt) rendererProcess audioPlayer cursorClient activeGameDispatcher

            // add the keyed values
            for (key, value) in plugin.MakeKeyedValues world do
                World.addKeyedValue key value world

            // register the game
            World.registerGame Game world
            world

        /// Run the game engine, initializing dependencies as indicated by WorldConfig, and returning exit code upon
        /// termination.
        static member runPlus tryMakeEditContext runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess worldConfig windowSize geometryViewport windowViewport plugin =
            match SdlDeps.tryMake worldConfig.SdlConfig worldConfig.Accompanied windowSize with
            | Right sdlDeps ->
                use sdlDeps = sdlDeps // bind explicitly to dispose automatically
                let world = World.make tryMakeEditContext sdlDeps worldConfig geometryViewport windowViewport plugin
                if World.getWindowSize world <> windowSize then
                    World.processWindowResized world // synchronize window size with actual size, e.g. fullscreen for mobile or a display with size smaller than the configured DisplayScalar
                World.runWithCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess worldConfig.FirstFrameReady world
            | Left error -> Log.error error; Constants.Engine.ExitCodeFailure

        /// Run the game engine, initializing dependencies as indicated by WorldConfig, and returning exit code upon
        /// termination.
        static member run worldConfig plugin =
            let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
            let windowViewport = Viewport.makeWindow1 windowSize
            let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size
            World.runPlus (constant None) tautology ignore ignore ignore ignore ignore worldConfig windowViewport.Outer.Size geometryViewport windowViewport plugin