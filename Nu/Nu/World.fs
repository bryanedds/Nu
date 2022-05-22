// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Reflection
open System.Threading
open SDL2
open Prime
open Nu

// HACK: I had to remove the [<RequireQualifiedAccess>] attribute from here because it was being interpreted in an
// ambiguous way by F# Interactive.
module Nu =

    let mutable private Initialized = false

    let private tryPropagateByLens (left : World Lens) (right : World Lens) world =
        if right.Validate world then
            let value = right.GetWithoutValidation world
            left.TrySet value world
        else world

    let private tryPropagateByName simulant leftName (right : World Lens) world =
        if right.Validate world then
            let value = right.GetWithoutValidation world
            let property = { PropertyType = right.Type; PropertyValue = value }
            World.trySetPropertyFast leftName property simulant world
        else world

    let private tryPropagate simulant (left : World Lens) (right : World Lens) world =
        if notNull (left.This :> obj)
        then tryPropagateByLens left right world
        else tryPropagateByName simulant left.Name right world

    let internal unbind propertyBindingKey propertyAddress world =
        let world = World.removePropertyBinding propertyBindingKey propertyAddress world
        let world = World.decreaseBindingCount propertyAddress.PASimulant world
        world

    /// Initialize the Nu game engine.
    let init nuConfig =

        // init only if needed
        if not Initialized then

            // process loading assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add (fun args ->
                Reflection.AssembliesLoaded.Assign (args.LoadedAssembly.FullName, args.LoadedAssembly))
            AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
                snd (Reflection.AssembliesLoaded.TryGetValue args.Name)))

            // process existing assemblies
            for assembly in AppDomain.CurrentDomain.GetAssemblies () do
                Reflection.AssembliesLoaded.Assign (assembly.FullName, assembly)

            // ensure the current culture is invariate
            Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init (Some "Log.txt")

            // init math module
            Math.init ()

            // init simulant modules
            WorldModuleGame.init ()
            WorldModuleScreen.init ()
            WorldModuleGroup.init ()
            WorldModuleEntity.init ()

            // init handleUserDefinedCallback F# reach-around
            WorldTypes.handleUserDefinedCallback <- fun userDefined _ worldObj ->
                let world = worldObj :?> World
                let (simulant, left, right) = userDefined :?> Simulant * World Lens * World Lens
                let world =
                    if notNull (left.This :> obj)
                    then tryPropagateByLens left right world
                    else tryPropagateByName simulant left.Name right world
                (Cascade, world :> obj)

            // init handleSubscribeAndUnsubscribeEventHook F# reach-around
            WorldTypes.handleSubscribeAndUnsubscribeEventHook <- fun subscribing eventAddress _ worldObj ->
                // here we need to update the event publish flags for entities based on whether there are subscriptions to
                // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
                // that these optimizations do not support, and warn the developer if they are invoked. Additionally, we
                // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
                let world = worldObj :?> World
                let eventNames = Address.getNames eventAddress
                let eventNamesLength = Array.length eventNames
                let world =
                    if eventNamesLength >= 5 then
                        let eventFirstName = eventNames.[0]
                        let entity = Entity (Array.skip 2 eventNames)
                        match eventFirstName with
                        | "Update" ->
#if DEBUG
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug
                                    ("Subscribing to entity update events with a wildcard is not supported. " +
                                     "This will cause a bug where some entity update events are not published.")
#endif
                            World.updateEntityPublishUpdateFlag entity world |> snd'
#if !DISABLE_ENTITY_POST_UPDATE
                        | "PostUpdate" ->
    #if DEBUG
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug
                                    ("Subscribing to entity post-update events with a wildcard is not supported. " +
                                     "This will cause a bug where some entity post-update events are not published.")
    #endif
                            World.updateEntityPublishPostUpdateFlag entity world |> snd'
#endif
                        | _ -> world
                    else world
                let world =
                    if eventNamesLength >= 3 then
                        match eventNames.[0] with
                        | "Change" ->
                            let world =
                                if eventNamesLength >= 6 then
                                    let entityAddress = rtoa (Array.skip 3 eventNames)
                                    let entity = Entity entityAddress
                                    match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityChangeCountsId, world) with
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
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug "Subscribing to change events with a wildcard is not supported."
                            world
                        | _ -> world
                    else world
                world :> obj

            // init getEntityIs2d F# reach-around
            WorldTypes.getEntityIs2d <- fun entityObj worldObj ->
                World.getEntityIs2d (entityObj :?> Entity) (worldObj :?> World)

            // init eval F# reach-around
            // TODO: remove duplicated code with the following 4 functions...
            WorldModule.eval <- fun expr localFrame scriptContext world ->
                match expr with
                | Scripting.Unit ->
                    // OPTIMIZATION: don't bother evaluating unit
                    struct (Scripting.Unit, world)
                | _ ->
                    let oldLocalFrame = World.getLocalFrame world
                    let oldScriptContext = World.getScriptContext world
                    World.setLocalFrame localFrame world
                    let world = World.setScriptContext scriptContext world
                    ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                    let struct (evaled, world) = World.evalInternal expr world
                    ScriptingSystem.removeProceduralBindings world
                    let world = World.setScriptContext oldScriptContext world
                    World.setLocalFrame oldLocalFrame world
                    struct (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalMany <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                let struct (evaleds, world) = World.evalManyInternal exprs world
                ScriptingSystem.removeProceduralBindings world
                let world = World.setScriptContext oldScriptContext world
                World.setLocalFrame oldLocalFrame world
                struct (evaleds, world)

            // init evalWithLogging F# reach-around
            WorldModule.evalWithLogging <- fun expr localFrame scriptContext world ->
                match expr with
                | Scripting.Unit ->
                    // OPTIMIZATION: don't bother evaluating unit
                    struct (Scripting.Unit, world)
                | _ ->
                    let oldLocalFrame = World.getLocalFrame world
                    let oldScriptContext = World.getScriptContext world
                    World.setLocalFrame localFrame world
                    let world = World.setScriptContext scriptContext world
                    ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                    let struct (evaled, world) = World.evalWithLoggingInternal expr world
                    ScriptingSystem.removeProceduralBindings world
                    let world = World.setScriptContext oldScriptContext world
                    World.setLocalFrame oldLocalFrame world
                    struct (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalManyWithLogging <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                let struct (evaleds, world) = World.evalManyWithLoggingInternal exprs world
                ScriptingSystem.removeProceduralBindings world
                let world = World.setScriptContext oldScriptContext world
                World.setLocalFrame oldLocalFrame world
                struct (evaleds, world)

            // init isSelected F# reach-around
            WorldModule.isSelected <- fun simulant world ->
                World.isSelected simulant world

            // init ignorePropertyBindings F# reach-around
            WorldModule.ignorePropertyBindings <- fun simulant world ->
                World.ignorePropertyBindings simulant world

            // init getScreenEcs F# reach-around
            WorldModule.getScreenEcs <- 
                World.getScreenEcs

            // init sortSubscriptionByElevation F# reach-around
            WorldModule.sortSubscriptionsByElevation <- fun subscriptions worldObj ->
                let world = worldObj :?> World
                EventSystem.sortSubscriptionsBy
                    (fun (simulant : Simulant) _ ->
                        match simulant with
                        | :? Entity as entity -> { SortElevation = entity.GetElevation world; SortHorizon = 0.0f; SortTarget = entity } :> IComparable
                        | :? Group as group -> { SortElevation = Constants.Engine.GroupSortPriority; SortHorizon = 0.0f; SortTarget = group } :> IComparable
                        | :? Screen as screen -> { SortElevation = Constants.Engine.ScreenSortPriority; SortHorizon = 0.0f; SortTarget = screen } :> IComparable
                        | :? Game | :? GlobalSimulantGeneralized -> { SortElevation = Constants.Engine.GameSortPriority; SortHorizon = 0.0f; SortTarget = Simulants.Game } :> IComparable
                        | _ -> failwithumf ())
                    subscriptions
                    world

            // init admitScreenElements F# reach-around
            WorldModule.admitScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat |> Seq.toArray
                let (entities2d, entities3d) = Array.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let oldWorld = world
                let quadtree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                        (fun quadtree ->
                            for entity in entities2d do
                                let entityState = World.getEntityState entity world
                                let entityBounds = entityState.Bounds
                                let entityOmnipresent = entityState.Omnipresent || entityState.Absolute
                                Quadtree.addElement entityOmnipresent entityBounds.Box2 entity quadtree
                            quadtree)
                        (World.getQuadtree world)
                let world = World.setQuadtree quadtree world
                let octree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                        (fun octree ->
                            for entity in entities3d do
                                let entityState = World.getEntityState entity world
                                let entityBounds = entityState.Bounds
                                let entityOmnipresent = entityState.Omnipresent || entityState.Absolute
                                let element = Octelement.make false false entity // TODO: 3D:  populate flags correctly.
                                Octree.addElement entityOmnipresent entityBounds element octree
                            octree)
                        (World.getOctree world)
                let world = World.setOctree octree world
                world
                
            // init evictScreenElements F# reach-around
            WorldModule.evictScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat |> Seq.toArray
                let (entities2d, entities3d) = Array.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let oldWorld = world
                let quadtree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                        (fun quadtree ->
                            for entity in entities2d do
                                let entityState = World.getEntityState entity world
                                let entityBounds = entityState.Bounds
                                let entityOmnipresent = entityState.Omnipresent || entityState.Absolute
                                Quadtree.removeElement entityOmnipresent entityBounds.Box2 entity quadtree
                            quadtree)
                        (World.getQuadtree world)
                let world = World.setQuadtree quadtree world
                let octree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                        (fun octree ->
                            for entity in entities3d do
                                let entityState = World.getEntityState entity world
                                let entityBounds = entityState.Bounds
                                let entityOmnipresent = entityState.Omnipresent || entityState.Absolute
                                let element = Octelement.make false false entity // TODO: 3D:  populate flags correctly.
                                Octree.removeElement entityOmnipresent entityBounds element octree
                            octree)
                        (World.getOctree world)
                let world = World.setOctree octree world
                world

            // init registerScreenPhysics F# reach-around
            WorldModule.registerScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntitiesFlattened world) |>
                    Seq.concat |>
                    Seq.toArray
                Array.fold (fun world (entity : Entity) ->
                    World.registerEntityPhysics entity world)
                    world entities

            // init unregisterScreenPhysics F# reach-around
            WorldModule.unregisterScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntitiesFlattened world) |>
                    Seq.concat |>
                    Seq.toArray
                Array.fold (fun world (entity : Entity) ->
                    World.unregisterEntityPhysics entity world)
                    world entities

            // init bind5 F# reach-around
            WorldModule.bind5 <- fun simulant left right world ->
                let leftFixup =
                    if isNull (left.This :> obj) then
                        Lens.make
                            left.Name
                            (fun world ->
                                match World.tryGetProperty (left.Name, simulant, world) with
                                | (true, property) -> property.PropertyValue
                                | (false, _) -> failwithumf ())
                            (fun propertyValue world ->
                                let property = { PropertyType = left.Type; PropertyValue = propertyValue }
                                World.trySetPropertyFast left.Name property simulant world)
                            simulant
                    else Lens.make left.Name left.GetWithoutValidation (Option.get left.SetOpt) simulant
                let rightFixup = Lens.makePlus right.Name right.ParentOpt right.ValidateOpt right.GetWithoutValidation None right.This
                let world =
                    // propagate immediately to start things out synchronized if possible.
                    if World.getExists rightFixup.This world
                    then tryPropagateByLens leftFixup rightFixup world
                    else world
                let propertyBindingKey = Gen.id
                let propertyAddress = PropertyAddress.make rightFixup.Name rightFixup.This
                let world = World.monitor (fun _ world -> (Cascade, unbind propertyBindingKey propertyAddress world)) (Events.Unregistering --> simulant.SimulantAddress) simulant world
                let world = World.monitor (fun _ world -> (Cascade, if not (World.ignorePropertyBindings leftFixup.This world) then tryPropagate simulant leftFixup rightFixup world else world)) (Events.Register --> right.This.SimulantAddress) simulant world
                let world = World.increaseBindingCount right.This world
                World.addPropertyBinding propertyBindingKey propertyAddress leftFixup rightFixup world

            // init miscellaneous reach-arounds
            WorldModule.register <- fun simulant world -> World.register simulant world
            WorldModule.unregister <- fun simulant world -> World.unregister simulant world
            WorldModule.expandContent <- fun setScreenSplash content origin owner parent world -> World.expandContent setScreenSplash content origin owner parent world
            WorldModule.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModule.destroy <- fun simulant world -> World.destroy simulant world
            WorldModule.trySignalFacet <- fun signalObj facetName simulant world -> World.trySignalFacet signalObj facetName simulant world
            WorldModule.trySignal <- fun signalObj simulant world -> World.trySignal signalObj simulant world

            // init debug view F# reach-arounds
            Debug.World.viewGame <- fun world -> Debug.Game.view (world :?> World)
            Debug.World.viewScreen <- fun screen world -> Debug.Screen.view (screen :?> Screen) (world :?> World)
            Debug.World.viewGroup <- fun group world -> Debug.Group.view (group :?> Group) (world :?> World)
            Debug.World.viewEntity <- fun entity world -> Debug.Entity.view (entity :?> Entity) (world :?> World)

            // init scripting
            World.initScripting ()
            WorldBindings.initBindings ()

            // init vsync
            Vsync.Init nuConfig.RunSynchronously

            // init event world caching
            EventSystemDelegate.setEventAddressCaching true

            // mark init flag
            Initialized <- true

[<AutoOpen; ModuleBinding>]
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
            // TODO: consider if we should reflectively generate these
            Map.ofListBy World.pairWithName $
                [EntityDispatcher2d (false, true) :> EntityDispatcher
                 EntityDispatcher3d (false, false) :> EntityDispatcher
                 StaticSpriteDispatcher () :> EntityDispatcher
                 AnimatedSpriteDispatcher () :> EntityDispatcher
                 GuiDispatcher () :> EntityDispatcher
                 ButtonDispatcher () :> EntityDispatcher
                 LabelDispatcher () :> EntityDispatcher
                 TextDispatcher () :> EntityDispatcher
                 ToggleButtonDispatcher () :> EntityDispatcher
                 RadioButtonDispatcher () :> EntityDispatcher
                 FpsDispatcher () :> EntityDispatcher
                 FeelerDispatcher () :> EntityDispatcher
                 FillBarDispatcher () :> EntityDispatcher
                 BasicEmitter2dDispatcher () :> EntityDispatcher
                 Effect2dDispatcher () :> EntityDispatcher
                 Block2dDispatcher () :> EntityDispatcher
                 Box2dDispatcher () :> EntityDispatcher
                 SideViewCharacterDispatcher () :> EntityDispatcher
                 TileMapDispatcher () :> EntityDispatcher
                 TmxMapDispatcher () :> EntityDispatcher
                 ConcatenatedSceneryDispatcher () :> EntityDispatcher]

        static member private makeDefaultFacets () =
            // TODO: consider if we should reflectively generate these
            Map.ofListBy World.pairWithName $
                [ScriptFacet () :> Facet
                 StaticSpriteFacet () :> Facet
                 AnimatedSpriteFacet () :> Facet
                 TextFacet () :> Facet
                 BasicEmitter2dFacet () :> Facet
                 Effect2dFacet () :> Facet
                 RigidBody2dFacet () :> Facet
                 Joint2dFacet () :> Facet
                 TileMapFacet () :> Facet
                 TmxMapFacet () :> Facet]

        /// Make an empty world.
        static member makeEmpty (config : WorldConfig) =

            // ensure game engine is initialized
            Nu.init config.NuConfig

            // make the default plug-in
            let plugin = NuPlugin ()

            // make the world's event delegate
            let eventDelegate =
                let eventTracing = Constants.Engine.EventTracing
                let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None // NOTE: lambda expression is duplicated in multiple places...
                let eventFilter = Constants.Engine.EventFilter
                let globalSimulant = Simulants.Game
                let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                let eventConfig = if config.Imperative then Imperative else Functional
                EventSystemDelegate.make eventTracerOpt eventFilter globalSimulant globalSimulantGeneralized eventConfig

            // make the default game dispatcher
            let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

            // make the world's dispatchers
            let dispatchers =
                { GameDispatchers = Map.ofList [defaultGameDispatcher]
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  GroupDispatchers = World.makeDefaultGroupDispatchers ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  Facets = World.makeDefaultFacets ()
                  TryGetExtrinsic = World.tryGetExtrinsic
                  UpdateEntityInEntityTree = World.updateEntityInEntityTree
                  RebuildQuadtree = World.rebuildQuadtree
                  RebuildOctree = World.rebuildOctree }

            // make the world's subsystems
            let subsystems =
                { PhysicsEngine2d = MockPhysicsEngine.make ()
                  RendererProcess2d = RendererInline2d (fun () -> MockRenderer2d.make () :> Renderer2d)
                  AudioPlayer = MockAudioPlayer.make () }

            // make the world's scripting environment
            let scriptingEnv = Scripting.Env.make ()

            // make the world's ambient state
            let ambientState =
                let overlayRouter = OverlayRouter.empty
                let symbolStore = SymbolStore.makeEmpty ()
                AmbientState.make config.Imperative config.StandAlone 1L (Metadata.makeEmpty config.Imperative) symbolStore Overlayer.empty overlayRouter None

            // make the world's quadtree
            let quadtree = World.makeQuadtree ()

            // make the world's octree
            let octree = World.makeOctree ()

            // make the world
            let world = World.make plugin eventDelegate dispatchers subsystems scriptingEnv ambientState quadtree octree (snd defaultGameDispatcher)

            // finally, register the game
            World.registerGame world

        /// Make a default world with a default screen, group, and entity, such as for testing.
        static member makeDefault () =
            let worldConfig = WorldConfig.defaultConfig
            let world = World.makeEmpty worldConfig
            let world = World.createScreen (Some Simulants.DefaultScreen.Name) world |> snd
            let world = World.createGroup (Some Simulants.DefaultGroup.Name) Simulants.DefaultScreen world |> snd
            let world = World.createEntity (Some Simulants.DefaultEntity.Surnames) DefaultOverlay Simulants.DefaultGroup world |> snd
            world

        /// Attempt to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake (sdlDeps : SdlDeps) config (plugin : NuPlugin) =

            // ensure game engine is initialized
            Nu.init config.NuConfig

            // attempt to create asset graph
            match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
            | Right assetGraph ->

                // make the world's event system
                let eventSystem =
                    let eventTracing = Constants.Engine.EventTracing
                    let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None
                    let eventFilter = Constants.Engine.EventFilter
                    let globalSimulant = Simulants.Game
                    let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                    let eventConfig = if config.Imperative then Imperative else Functional
                    EventSystemDelegate.make eventTracerOpt eventFilter globalSimulant globalSimulantGeneralized eventConfig
                    
                // make plug-in facets and dispatchers
                let pluginFacets = plugin.Birth<Facet> ()
                let pluginGameDispatchers = plugin.Birth<GameDispatcher> ()
                let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> ()
                let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> ()
                let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> ()

                // make the default game dispatcher
                let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

                // make the world's dispatchers
                let dispatchers =
                    { GameDispatchers = Map.addMany pluginGameDispatchers (Map.ofList [defaultGameDispatcher])
                      ScreenDispatchers = Map.addMany pluginScreenDispatchers (World.makeDefaultScreenDispatchers ())
                      GroupDispatchers = Map.addMany pluginGroupDispatchers (World.makeDefaultGroupDispatchers ())
                      EntityDispatchers = Map.addMany pluginEntityDispatchers (World.makeDefaultEntityDispatchers ())
                      Facets = Map.addMany pluginFacets (World.makeDefaultFacets ())
                      TryGetExtrinsic = World.tryGetExtrinsic
                      UpdateEntityInEntityTree = World.updateEntityInEntityTree
                      RebuildQuadtree = World.rebuildQuadtree
                      RebuildOctree = World.rebuildOctree }

                // look up the active game dispather
                let activeGameDispatcherType = if config.StandAlone then plugin.StandAloneConfig else typeof<GameDispatcher>
                let activeGameDispatcher = Map.find activeGameDispatcherType.Name dispatchers.GameDispatchers

                // make the world's subsystems
                let subsystems =
                    let physicsEngine2d =
                        AetherPhysicsEngine.make config.Imperative Constants.Physics.GravityDefault
                    let createRenderer2d =
                        fun () ->
                            match SdlDeps.getWindowOpt sdlDeps with
                            | Some window -> GlRenderer2d.make window :> Renderer2d
                            | None -> MockRenderer2d.make () :> Renderer2d
                    let rendererProcess2d =
                        if config.StandAlone
                        then RendererThread2d createRenderer2d :> RendererProcess2d
                        else RendererInline2d createRenderer2d :> RendererProcess2d
                    rendererProcess2d.Start ()
                    rendererProcess2d.EnqueueMessage (HintRenderPackageUseMessage2d Assets.Default.PackageName) // enqueue default package hint
                    let audioPlayer =
                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                        then SdlAudioPlayer.make () :> AudioPlayer
                        else MockAudioPlayer.make () :> AudioPlayer
                    audioPlayer.EnqueueMessage (HintAudioPackageUseMessage Assets.Default.PackageName) // enqueue default package hint
                    { PhysicsEngine2d = physicsEngine2d
                      RendererProcess2d = rendererProcess2d
                      AudioPlayer = audioPlayer }

                // attempt to make the overlayer
                let intrinsicOverlays = World.makeIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays Assets.Global.OverlayerFilePath with
                | Right overlayer ->

                    // make the world's scripting environment
                    let scriptingEnv = Scripting.Env.make ()

                    // make the world's ambient state
                    let ambientState =
                        let assetMetadataMap = Metadata.make config.Imperative assetGraph
                        let overlays = Overlayer.getIntrinsicOverlays overlayer @ Overlayer.getExtrinsicOverlays overlayer
                        let overlayRoutes =
                            overlays |>
                            List.map (fun overlay -> overlay.OverlaidTypeNames |> List.map (fun typeName -> (typeName, overlay.OverlayName))) |>
                            List.concat
                        let overlayRouter = OverlayRouter.make overlayRoutes
                        let symbolStore = SymbolStore.makeEmpty ()
                        AmbientState.make config.Imperative config.StandAlone config.UpdateRate assetMetadataMap symbolStore overlayer overlayRouter (Some sdlDeps)

                    // make the world's quadtree
                    let quadtree = World.makeQuadtree ()

                    // make the world's octree
                    let octree = World.makeOctree ()

                    // make the world
                    let world = World.make plugin eventSystem dispatchers subsystems scriptingEnv ambientState quadtree octree activeGameDispatcher

                    // add the keyed values
                    let (kvps, world) = plugin.MakeKeyedValues world
                    let world = List.fold (fun world (key, value) -> World.addKeyedValue key value world) world kvps

                    // try to load the prelude for the scripting language
                    match World.tryEvalPrelude world with
                    | Right struct (_, world) ->

                        // register the game
                        let world = World.registerGame world

#if DEBUG
                        // attempt to hookup the console if debugging
                        let world = WorldConsole.tryHookUp world |> snd
#endif

                        // fin
                        Right world
                    
                    // forward error messages
                    | Left struct (error, _) -> Left error
                | Left error -> Left error
            | Left error -> Left error

        /// Run the game engine as a stand-alone application.
        static member run worldConfig plugin =
            match SdlDeps.tryMake worldConfig.SdlConfig with
            | Right sdlDeps ->
                use sdlDeps = sdlDeps // bind explicitly to dispose automatically
                match World.tryMake sdlDeps worldConfig plugin with
                | Right world -> World.run4 tautology sdlDeps Live world
                | Left error -> Log.trace error; Constants.Engine.FailureExitCode
            | Left error -> Log.trace error; Constants.Engine.FailureExitCode