// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Nu =

    let mutable private Initialized = false
    let private LoadedAssemblies = Dictionary<string, Assembly> HashIdentity.Structural

    /// Initialize the Nu game engine.
    let init runSynchronously =

        // init only if needed
        if not Initialized then

            // make types load reflectively from pathed (non-static) assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add
                (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler
                (fun _ args -> snd (LoadedAssemblies.TryGetValue args.Name)))

            // ensure the current culture is invariate
            Threading.Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init (Some "Log.txt")

            // init math module
            Math.init ()

            // init simulant modules
            WorldModuleGame.init ()
            WorldModuleScreen.init ()
            WorldModuleLayer.init ()
            WorldModuleEntity.init ()

            // init getPropertyOpt F# reach-around
            WorldTypes.getPropertyOpt <- fun propertyName propertied world ->
                match propertied with
                | :? Simulant as simulant ->
                    match World.tryGetProperty propertyName simulant (world :?> World) with
                    | Some property -> Some property.PropertyValue
                    | None -> None
                | _ -> None

            // init setPropertyOpt F# reach-around
            WorldTypes.setPropertyOpt <- fun propertyName propertied valueOpt ty world ->
                let world = world :?> World
                match propertied with
                | :? Simulant as simulant ->
                    match simulant with
                    | :? Game ->
                        match (valueOpt, WorldModuleGame.Setters.TryGetValue propertyName) with
                        | (Some value, (true, setter)) ->
                            let property = { PropertyValue = value; PropertyType = ty }
                            setter property world |> snd |> box
                        | (Some value, (false, _)) ->
                            let property = { PropertyValue = value; PropertyType = ty }
                            World.attachGameProperty propertyName property world |> box
                        | (None, (true, _)) ->
                            World.exit world |> box
                        | (None, (false, _)) ->
                            World.detachGameProperty propertyName world |> box
                    | :? Screen as screen ->
                        match (valueOpt, WorldModuleScreen.Setters.TryGetValue propertyName) with
                        | (Some value, (true, setter)) ->
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let property = { PropertyValue = value; PropertyType = ty }
                            setter property screen world |> snd |> box
                        | (Some value, (false, _)) ->
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let property = { PropertyValue = value; PropertyType = ty }
                            World.attachScreenProperty propertyName property screen world |> box
                        | (None, (true, _)) ->
                            World.destroyScreen screen world |> box
                        | (None, (false, _)) ->
                            World.detachScreenProperty propertyName screen world |> box
                    | :? Layer as layer ->
                        match (valueOpt, WorldModuleLayer.Setters.TryGetValue propertyName) with
                        | (Some value, (true, setter)) ->
                            let screen = ltos layer
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let world = if not (World.getLayerExists layer world) then World.createLayer None screen world |> snd else world
                            let property = { PropertyValue = value; PropertyType = ty }
                            setter property layer world |> snd |> box
                        | (Some value, (false, _)) ->
                            let screen = ltos layer
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let world = if not (World.getLayerExists layer world) then World.createLayer None screen world |> snd else world
                            let property = { PropertyValue = value; PropertyType = ty }
                            World.attachLayerProperty propertyName property layer world |> box
                        | (None, (true, _)) ->
                            World.destroyLayer layer world |> box
                        | (None, (false, _)) ->
                            World.detachLayerProperty propertyName layer world |> box
                    | :? Entity as entity ->
                        match (valueOpt, WorldModuleEntity.Setters.TryGetValue propertyName) with
                        | (Some value, (true, setter)) ->
                            let layer = etol entity
                            let screen = ltos layer
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let world = if not (World.getLayerExists layer world) then World.createLayer None screen world |> snd else world
                            let world = if not (World.getEntityExists entity world) then World.createEntity None DefaultOverlay layer world |> snd else world
                            let property = { PropertyValue = value; PropertyType = ty }
                            setter property entity world |> snd |> box
                        | (Some value, (false, _)) ->
                            let layer = etol entity
                            let screen = ltos layer
                            let world = if not (World.getScreenExists screen world) then World.createScreen None world |> snd else world
                            let world = if not (World.getLayerExists layer world) then World.createLayer None screen world |> snd else world
                            let world = if not (World.getEntityExists entity world) then World.createEntity None DefaultOverlay layer world |> snd else world
                            let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                            let nonPersistent = not (Reflection.isPropertyPersistentByName propertyName)
                            let property = { PropertyValue = value; PropertyType = ty }
                            World.attachEntityProperty propertyName alwaysPublish nonPersistent property entity world |> box
                        | (None, (true, _)) ->
                            World.destroyEntity entity world |> box
                        | (None, (false, _)) ->
                            World.detachEntityProperty propertyName entity world |> box
                    | _ -> failwithumf ()
                | _ -> box world

            // init handlePropertyChange F# reach-around
            WorldTypes.handlePropertyChange <- fun propertyName propertied handler world ->
                let participant = propertied :?> Participant
                let handler = handler :?> PropertyChangeHandler<World>
                let (unsubscribe, world) =
                    World.subscribePlus
                        (makeGuid ())
                        (fun (event : Event<obj, _>) world ->
                            let data = event.Data :?> ChangeData
                            let world = handler data.OldWorld world
                            (Cascade, world))
                        (Address.makeFromList ("Change" :: propertyName :: "Event" :: Address.getNames participant.ParticipantAddress))
                        (Default.Game :> Participant)
                        (world :?> World)
                (box unsubscribe, box world)

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

            // init addSimulantScriptUnsubscription F# reach-around
            WorldModule.addSimulantScriptUnsubscription <- fun unsubscription (simulant : Simulant) world ->
                match simulant with
                | :? Game as game -> game.ScriptUnsubscriptions.Update (List.cons unsubscription) world
                | :? Screen as screen -> screen.ScriptUnsubscriptions.Update (List.cons unsubscription) world
                | :? Layer as layer -> layer.ScriptUnsubscriptions.Update (List.cons unsubscription) world
                | :? Entity as entity -> entity.ScriptUnsubscriptions.Update (List.cons unsubscription) world
                | _ -> world

            // init unsubscribeSimulantScripts F# reach-around
            WorldModule.unsubscribeSimulantScripts <- fun (simulant : Simulant) world ->
                let propertyOpt =
                    match simulant with
                    | :? Game as game -> Some game.ScriptUnsubscriptions
                    | :? Screen as screen -> Some screen.ScriptUnsubscriptions
                    | :? Layer as layer -> Some layer.ScriptUnsubscriptions
                    | :? Entity as entity -> Some entity.ScriptUnsubscriptions
                    | _ -> None
                match propertyOpt with
                | Some property ->
                    let unsubscriptions = property.Get world
                    let world = List.foldBack apply unsubscriptions world
                    property.Set [] world
                | None -> world

            // init equate5 F# reach-around
            WorldModule.equate5 <- fun name (participant : Participant) (lens : World Lens) breaking world ->
                let nonPersistent = not (Reflection.isPropertyPersistentByName name)
                let alwaysPublish = Reflection.isPropertyAlwaysPublishByName name
                let propagate (_ : Event<obj, Participant>) world =
                    let property = { PropertyType = lens.Type; PropertyValue = lens.Get world }
                    World.setProperty name nonPersistent alwaysPublish property (participant :?> Simulant) world
                let breaker = if breaking then Stream.noMoreThanOncePerUpdate else Stream.id
                let world = Stream.make (atooa Events.Register --> lens.This.ParticipantAddress) |> breaker |> Stream.optimize |> Stream.monitor propagate participant $ world
                Stream.make (atooa (Events.Change lens.Name) --> lens.This.ParticipantAddress) |> breaker |> Stream.optimize |> Stream.monitor propagate participant $ world

            // init scripting
            World.initScripting ()
            WorldBindings.initBindings ()

            // init debug view F# reach-arounds
#if DEBUG
            Debug.World.viewGame <- fun world -> Debug.Game.view (world :?> World)
            Debug.World.viewScreen <- fun screen world -> Debug.Screen.view (screen :?> Screen) (world :?> World)
            Debug.World.viewLayer <- fun layer world -> Debug.Layer.view (layer :?> Layer) (world :?> World)
            Debug.World.viewEntity <- fun entity world -> Debug.Entity.view (entity :?> Entity) (world :?> World)
#endif

            // init Vsync with incoming parameter
            Vsync.init runSynchronously

            // init event world caching
            EventSystem.setEventAddressCaching true

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

        static member private makeDefaultLayerDispatchers () =
            Map.ofList [World.pairWithName (LayerDispatcher ())]

        static member private makeDefaultEntityDispatchers () =
            // TODO: consider if we shoud reflectively generate these
            Map.ofListBy World.pairWithName $
                [EntityDispatcher ()
                 NodeDispatcher () :> EntityDispatcher
                 EffectDispatcher () :> EntityDispatcher
                 GuiDispatcher () :> EntityDispatcher
                 ButtonDispatcher () :> EntityDispatcher
                 LabelDispatcher () :> EntityDispatcher
                 TextDispatcher () :> EntityDispatcher
                 ToggleDispatcher () :> EntityDispatcher
                 FeelerDispatcher () :> EntityDispatcher
                 FillBarDispatcher () :> EntityDispatcher
                 BlockDispatcher () :> EntityDispatcher
                 BoxDispatcher () :> EntityDispatcher
                 CharacterDispatcher () :> EntityDispatcher
                 TileMapDispatcher () :> EntityDispatcher]

        static member private makeDefaultFacets () =
            // TODO: consider if we shoud reflectively generate these
            Map.ofList
                [(typeof<NodeFacet>.Name, NodeFacet () :> Facet)
                 (typeof<EffectFacet>.Name, EffectFacet () :> Facet)
                 (typeof<ScriptFacet>.Name, ScriptFacet () :> Facet)
                 (typeof<TextFacet>.Name, TextFacet () :> Facet)
                 (typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet)
                 (typeof<StaticSpriteFacet>.Name, StaticSpriteFacet () :> Facet)
                 (typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet)]

        /// Make an empty world.
        static member makeEmpty userState =

            // ensure game engine is initialized
            // TODO: P1: parameterize hard-coded boolean
            Nu.init false

            // make the world's event delegate
            let eventDelegate =
                let eventTracer = Log.remark "Event"
                let eventTracing = Core.getEventTracing ()
                let eventFilter = Core.getEventFilter ()
                let globalParticipant = Default.Game
                let globalParticipantGeneralized = { GpgAddress = atoa globalParticipant.GameAddress }
                EventSystemDelegate.make eventTracer eventTracing eventFilter globalParticipant globalParticipantGeneralized

            // make the game dispatcher
            let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

            // make the world's dispatchers
            let dispatchers =
                { GameDispatchers = Map.ofList [defaultGameDispatcher]
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  LayerDispatchers = World.makeDefaultLayerDispatchers ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  Facets = World.makeDefaultFacets ()
                  TryGetExtrinsic = World.tryGetExtrinsic
                  UpdateEntityInEntityTree = World.updateEntityInEntityTree
                  RebuildEntityTree = World.rebuildEntityTree }

            // make the world's subsystems
            let subsystems =
                let subsystemMap =
                    UMap.makeFromSeq
                        Constants.Engine.SubsystemMapConfig
                        [(Constants.Engine.PhysicsEngineSubsystemName, PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockPhysicsEngine.make ()) :> World Subsystem)
                         (Constants.Engine.RendererSubsystemName, RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockRenderer.make ()) :> World Subsystem)
                         (Constants.Engine.AudioPlayerSubsystemName, AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockAudioPlayer.make ()) :> World Subsystem)]
                Subsystems.make subsystemMap

            // make the world's scripting environment
            let scriptingEnv = Scripting.Env.Env.make ()

            // make the world's ambient state
            let ambientState =
                let overlayRoutes = World.dispatchersToOverlayRoutes dispatchers.EntityDispatchers
                let overlayRouter = OverlayRouter.make overlayRoutes
                AmbientState.make 1L (Metadata.makeEmpty ()) overlayRouter Overlayer.empty SymbolStore.empty userState

            // make the world
            let world = World.make eventDelegate dispatchers subsystems scriptingEnv ambientState (snd defaultGameDispatcher)
            
            // subscribe to subscribe and unsubscribe events
            let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Subscribe Default.Game world
            let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Unsubscribe Default.Game world

            // finally, register the game
            World.registerGame world

        /// Make a default world with a default screen, layer, and entity, such as for testing.
        static member makeDefault () =
            let world = World.makeEmpty ()
            let world = World.createScreen (Some Default.Screen.ScreenName) world |> snd
            let world = World.createLayer (Some Default.Layer.LayerName) Default.Screen world |> snd
            let world = World.createEntity (Some Default.Entity.EntityName) DefaultOverlay Default.Layer world |> snd
            world

        /// Attempt to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake standAlone tickRate userState (plugin : NuPlugin) sdlDeps =

            // ensure game engine is initialized
            // TODO: P1: parameterize hard-coded boolean
            Nu.init false

            // attempt to create asset graph
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->

                // make the world's event system
                let eventSystem =
                    let eventTracer = Log.remark "Event"
                    let eventTracing = Core.getEventTracing ()
                    let eventFilter = Core.getEventFilter ()
                    let globalParticipant = Default.Game
                    let globalParticipantGeneralized = { GpgAddress = atoa globalParticipant.GameAddress }
                    EventSystemDelegate.make eventTracer eventTracing eventFilter globalParticipant globalParticipantGeneralized

                // make plug-in dispatchers
                let pluginFacets = plugin.MakeFacets () |> List.map World.pairWithName
                let pluginGameDispatchers = plugin.MakeGameDispatchers () |> List.map World.pairWithName
                let pluginScreenDispatchers = plugin.MakeScreenDispatchers () |> List.map World.pairWithName
                let pluginLayerDispatchers = plugin.MakeLayerDispatchers () |> List.map World.pairWithName
                let pluginEntityDispatchers = plugin.MakeEntityDispatchers () |> List.map World.pairWithName

                // make the default game dispatcher
                let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

                // make the world's dispatchers
                let dispatchers =
                    { GameDispatchers = Map.addMany pluginGameDispatchers (Map.ofList [defaultGameDispatcher])
                      ScreenDispatchers = Map.addMany pluginScreenDispatchers (World.makeDefaultScreenDispatchers ())
                      LayerDispatchers = Map.addMany pluginLayerDispatchers (World.makeDefaultLayerDispatchers ())
                      EntityDispatchers = Map.addMany pluginEntityDispatchers (World.makeDefaultEntityDispatchers ())
                      Facets = Map.addMany pluginFacets (World.makeDefaultFacets ())
                      TryGetExtrinsic = World.tryGetExtrinsic
                      UpdateEntityInEntityTree = World.updateEntityInEntityTree
                      RebuildEntityTree = World.rebuildEntityTree }

                // look up the active game dispather
                let activeGameDispatcherName = if standAlone then plugin.GetStandAloneGameDispatcherName () else plugin.GetEditorGameDispatcherName ()
                let activeGameDispatcher = Map.find activeGameDispatcherName dispatchers.GameDispatchers

                // make the world's subsystems
                let subsystems =
                    let userSubsystems = plugin.MakeSubsystems ()
                    let physicsEngine = FarseerPhysicsEngine.make Constants.Physics.Gravity
                    let physicsEngineSubsystem = PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder physicsEngine :> World Subsystem
                    let renderer =
                        match SdlDeps.getRenderContextOpt sdlDeps with
                        | Some renderContext -> SdlRenderer.make renderContext :> Renderer
                        | None -> MockRenderer.make () :> Renderer
                    let renderer = renderer.EnqueueMessage (HintRenderPackageUseMessage Assets.DefaultPackage)
                    let rendererSubsystem = RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder renderer :> World Subsystem
                    let audioPlayer =
                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                        then SdlAudioPlayer.make () :> IAudioPlayer
                        else MockAudioPlayer.make () :> IAudioPlayer
                    let audioPlayerSubsystem = AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder audioPlayer :> World Subsystem
                    let defaultSubsystemMap =
                        UMap.makeFromSeq
                            Constants.Engine.SubsystemMapConfig
                            [(Constants.Engine.PhysicsEngineSubsystemName, physicsEngineSubsystem)
                             (Constants.Engine.RendererSubsystemName, rendererSubsystem)
                             (Constants.Engine.AudioPlayerSubsystemName, audioPlayerSubsystem)]
                    let subsystemMap = UMap.addMany userSubsystems defaultSubsystemMap
                    Subsystems.make subsystemMap

                // attempt to make the overlayer
                let intrinsicOverlays = World.makeIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays Assets.OverlayerFilePath with
                | Right overlayer ->

                    // make the world's scripting environment
                    let scriptingEnv = Scripting.Env.Env.make ()
            
                    // make the world's ambient state
                    let ambientState =
                        let assetMetadataMap = Metadata.make assetGraph
                        let intrinsicOverlayRoutes = World.dispatchersToOverlayRoutes dispatchers.EntityDispatchers
                        let userOverlayRoutes = plugin.MakeOverlayRoutes ()
                        let overlayRoutes = intrinsicOverlayRoutes @ userOverlayRoutes
                        let overlayRouter = OverlayRouter.make overlayRoutes
                        AmbientState.make tickRate assetMetadataMap overlayRouter overlayer SymbolStore.empty userState

                    // make the world
                    let world = World.make eventSystem dispatchers subsystems scriptingEnv ambientState activeGameDispatcher

                    // subscribe to subscribe and unsubscribe events
                    let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Subscribe Default.Game world
                    let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Unsubscribe Default.Game world

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