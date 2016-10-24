// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Reflection
open OpenTK
open Prime
open Nu

/// Describes the information needed to sort entities.
/// OPTIMIZATION: implemented as a struct and carries related entity to avoid GC pressure.
/// TODO: see if there's a better file to place this in.
type [<CustomEquality; CustomComparison>] EntitySortPriority =
    { SortDepth : single
      SortPositionY : single
      TargetEntity : Entity }

    static member equals left right =
        left.SortDepth = right.SortDepth &&
        left.SortPositionY = right.SortPositionY &&
        left.TargetEntity = right.TargetEntity

    static member compare left right =
        if left.SortDepth < right.SortDepth then 1
        elif left.SortDepth > right.SortDepth then -1
        elif left.SortPositionY < right.SortPositionY then -1
        elif left.SortPositionY > right.SortPositionY then 1
        else 0

    override this.GetHashCode () =
        this.SortDepth.GetHashCode () ^^^ (this.SortPositionY.GetHashCode () * 13)

    override this.Equals that =
        match that with
        | :? EntitySortPriority as that -> EntitySortPriority.equals this that
        | _ -> failwithumf ()

    interface IComparable<EntitySortPriority> with
        member this.CompareTo that =
            EntitySortPriority.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? EntitySortPriority as that -> (this :> IComparable<EntitySortPriority>).CompareTo that
            | _ -> failwithumf ()

[<AutoOpen>]
module WorldModule =

    // Mutable clipboard that allows its state to persist beyond undo / redo.
    let mutable private Clipboard : obj option = None

    type World with

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

    type World with

        /// Update an entity in the entity tree.
        static member internal updateEntityInEntityTree entity oldWorld world =
            world.Dispatchers.UpdateEntityInEntityTree entity oldWorld world

        /// Rebuild the entity tree if needed.
        static member internal rebuildEntityTree screen world =
            world.Dispatchers.RebuildEntityTree screen world

    type World with

        /// Get event subscriptions.
        static member getSubscriptions world =
            EventWorld.getSubscriptions<Game, World> world

        /// Get event unsubscriptions.
        static member getUnsubscriptions world =
            EventWorld.getUnsubscriptions<Game, World> world

        /// Add event state to the world.
        static member addEventState key state world =
            EventWorld.addEventState<'a, Game, World> key state world

        /// Remove event state from the world.
        static member removeEventState key world =
            EventWorld.removeEventState<Game, World> key world

        /// Get event state from the world.
        static member getEventState<'a> key world =
            EventWorld.getEventState<'a, Game, World> key world

        /// Get whether events are being traced.
        static member getEventTracing (world : World) =
            EventWorld.getEventTracing world

        /// Set whether events are being traced.
        static member setEventTracing tracing (world : World) =
            EventWorld.setEventTracing tracing world

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventWorld.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            EventWorld.setEventFilter filter world

        /// Get the context of the event system.
        static member getEventContext (world : World) =
            EventWorld.getEventContext world

        /// Set the context of the event system.
#if DEBUG
        static member internal withEventContext operation context (world : World) =
            let oldContext = World.getEventContext world
            let world = EventWorld.setEventContext context world
            let world = operation world
            let world = EventWorld.setEventContext oldContext world
            world
#else
        static member inline internal withEventContext operation _ (world : World) =
            // NOTE: inlined in debug to hopefully get rid of the lambda
            operation world
#endif

        /// Qualify the context of the event system.
        static member qualifyEventContext address (world : World) =
            EventWorld.qualifyEventContext address world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsBy by subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions (world : World) =
            // OPTIMIZATION: priority boxed up front to decrease GC pressure.
            let priorityBoxed = Constants.Engine.EntityPublishingPriority :> IComparable
            World.sortSubscriptionsBy (fun _ _ -> priorityBoxed) subscriptions world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsNone subscriptions world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publish7<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) allowWildcard world =
            EventWorld.publish7<'a, 'p, Game, World> publishSorter eventData eventAddress eventTrace publisher allowWildcard world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publish6<'a, 'p when 'p :> Simulant> (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) allowWildcard world =
            EventWorld.publish7<'a, 'p, Game, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher allowWildcard world

        /// Publish an event.
        static member publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish7<'a, 'p, Game, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher true world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            EventWorld.unsubscribe<Game, World> subscriptionKey world

        /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
        static member subscribePlus5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus5<'a, 's, Game, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus<'a, 's, Game, World> subscription eventAddress subscriber world

        /// Subscribe to an event using the given subscriptionKey.
        static member subscribe5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe5<'a, 's, Game, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe<'a, 's, Game, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitorPlus<'a, 's, Game, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitor<'a, 's, Game, World> subscription eventAddress subscriber world

    type World with

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.Dispatchers.GameDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.Dispatchers.ScreenDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.Dispatchers.GroupDispatchers

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.Dispatchers.EntityDispatchers

        /// Get the facets of the world.
        static member getFacets world =
            world.Dispatchers.Facets

    type World with

        static member internal getSubsystemMap world =
            Subsystems.getSubsystemMap world.Subsystems

        static member internal getSubsystem<'s when 's :> World Subsystem> name world : 's =
            Subsystems.getSubsystem name world.Subsystems

        static member internal getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
            Subsystems.getSubsystemBy by name world.Subsystems

        // NOTE: it'd be nice to get rid of this function to improve encapsulation, but I can't seem to do so in practice...
        static member internal setSubsystem<'s when 's :> World Subsystem> (subsystem : 's) name world =
            World.choose { world with Subsystems = Subsystems.setSubsystem subsystem name world.Subsystems }

        static member internal updateSubsystem<'s when 's :> World Subsystem> (updater : 's -> World -> 's) name world =
            World.choose { world with Subsystems = Subsystems.updateSubsystem updater name world.Subsystems world }

        static member internal updateSubsystems (updater : World Subsystem -> World -> World Subsystem) world =
            World.choose { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

        static member internal clearSubsystemsMessages world =
            World.choose { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

    type World with

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }
    
        /// Get the tick rate.
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate at the end of the current update.
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState (AmbientState.setTickRateImmediate tickRate) world }}) world

        /// Reset the tick time to 0 at the end of the current update.
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState AmbientState.resetTickTimeImmediate world }}) world

        /// Get the world's tick time.
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Check that the world is ticking.
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        static member internal updateTickTime world =
            World.updateAmbientState AmbientState.updateTickTime world

        /// Get the world's update count.
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        static member internal incrementUpdateCount world =
            World.updateAmbientState AmbientState.incrementUpdateCount world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current update.
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientState (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientState (AmbientState.addTasklets tasklets) world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            AmbientState.getAssetMetadataMap ^ World.getAmbientState world

        static member internal setAssetMetadataMap assetMetadataMap world =
            World.updateAmbientState (AmbientState.setAssetMetadataMap assetMetadataMap) world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get intrinsic overlays.
        static member getIntrinsicOverlays world =
            World.getOverlayerBy Overlayer.getIntrinsicOverlays world

        /// Get extrinsic overlays.
        static member getExtrinsicOverlays world =
            World.getOverlayerBy Overlayer.getExtrinsicOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols assetTags symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Reload all the symbols in the symbol store.
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            World.getAmbientStateBy AmbientState.getUserState world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            World.updateAmbientState (AmbientState.updateUserState updater) world

        /// Make 
        static member dispatchersToOverlayRoutes entityDispatchers =
            entityDispatchers |>
            Map.toValueListBy getTypeName |>
            List.map (fun typeName -> (typeName, OverlayDescriptor.makeVanilla (Some typeName)))

    type World with

        /// Get the opt entity cache.
        static member internal getOptEntityCache world =
            world.OptEntityCache

        /// Set the opt entity cache.
        static member internal setOptEntityCache optEntityCache world =
            World.choose { world with OptEntityCache = optEntityCache }

    type World with

        /// Get the screen directory.
        static member internal getScreenDirectory world =
            world.ScreenDirectory

    type World with

        /// View the member properties of some SimulantState.
        static member internal getMemberProperties (state : SimulantState) =
            state |>
            getType |>
            getProperties |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state)) |>
            Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getXProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            List.ofSeq |>
            List.sortBy fst |>
            List.map (fun (name, property) -> (name, property.PropertyValue))

        /// Provides a full view of all the properties of some SimulantState.
        static member internal getProperties state =
            List.append
                (World.getMemberProperties state)
                (World.getXProperties state)

    type World with

        static member private publishGameChange (propertyName : string) oldWorld world =
            let game = Game.proxy Address.empty
            let changeEventAddress = ltoa [!!"Game"; !!"Change"; !!propertyName; !!"Event"] ->>- game.GameAddress
            let eventTrace = EventTrace.record "World" "publishGameChange" EventTrace.empty
            World.publish { Participant = game; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace game world

        static member private getGameState world =
            world.GameState

        static member private setGameState gameState world =
#if DEBUG
            if not ^ World.qualifyEventContext Address.empty world then
                failwith ^ "Cannot set the state of a game in an unqualifed event context."
#endif
            World.choose { world with GameState = gameState }

        static member private updateGameStateWithoutEvent updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

        static member private updateGameState updater propertyName world =
            let oldWorld = world
            let world = World.updateGameStateWithoutEvent updater world
            World.publishGameChange propertyName oldWorld world

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameXtension world = (World.getGameState world).Xtension // TODO: try to get rid of this
        static member internal getGameDispatcherNp world = (World.getGameState world).DispatcherNp
        static member internal getGameSpecialization world = (World.getGameState world).Specialization
        static member internal getGameCreationTimeStampNp world = (World.getGameState world).CreationTimeStampNp

        /// Get the current eye center.
        static member getEyeCenter world =
            (World.getGameState world).EyeCenter

        /// Set the current eye center.
        static member setEyeCenter value world =
            World.updateGameState (fun groupState -> { groupState with EyeCenter = value }) Property? EyeCenter world

        /// Get the current eye size.
        static member getEyeSize world =
            (World.getGameState world).EyeSize

        /// Set the current eye size.
        static member setEyeSize value world =
            World.updateGameState (fun groupState -> { groupState with EyeSize = value }) Property? EyeSize world

        /// Get the currently selected screen, if any.
        static member getOptSelectedScreen world =
            (World.getGameState world).OptSelectedScreen
        
        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member setOptSelectedScreen value world =
            World.updateGameState (fun groupState -> { groupState with OptSelectedScreen = value }) Property? OptSelectedScreen world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        static member getSelectedScreen world =
            Option.get ^ World.getOptSelectedScreen world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreen screen world =
            World.setOptSelectedScreen (Some screen) world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            (World.getGameState world).OptScreenTransitionDestination

        /// Set the current destination screen or None. Be careful using this function as calling
        /// it is predicated that no screen transition is currently underway.
        /// TODO: consider asserting such predication here.
        static member internal setOptScreenTransitionDestination destination world =
            World.updateGameState (fun gameState -> { gameState with OptScreenTransitionDestination = destination }) Property? OptScreenTransitionDestination world

        /// Get the view of the eye in absolute terms (world space).
        static member getViewAbsolute world =
            Math.getViewAbsolute (World.getEyeCenter world) (World.getEyeSize world)
        
        /// Get the view of the eye in absolute terms (world space) with translation sliced on
        /// integers.
        static member getViewAbsoluteI world =
            Math.getViewAbsolute (World.getEyeCenter world) (World.getEyeSize world)

        /// The relative view of the eye with original single values. Due to the problems with
        /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
        /// coordinates is very, very bad for rendering.
        static member getViewRelative world =
            Math.getViewRelative (World.getEyeCenter world) (World.getEyeSize world)

        /// The relative view of the eye with translation sliced on integers. Good for rendering.
        static member getViewRelativeI world =
            Math.getViewRelativeI (World.getEyeCenter world) (World.getEyeSize world)

        /// Get the bounds of the eye's sight relative to its position.
        static member getViewBoundsRelative world =
            let gameState = World.getGameState world
            Vector4
                (gameState.EyeCenter.X - gameState.EyeSize.X * 0.5f,
                 gameState.EyeCenter.Y - gameState.EyeSize.Y * 0.5f,
                 gameState.EyeCenter.X + gameState.EyeSize.X * 0.5f,
                 gameState.EyeCenter.Y + gameState.EyeSize.Y * 0.5f)

        /// Get the bounds of the eye's sight not relative to its position.
        static member getViewBoundsAbsolute world =
            let gameState = World.getGameState world
            Vector4
                (gameState.EyeSize.X * -0.5f,
                 gameState.EyeSize.Y * -0.5f,
                 gameState.EyeSize.X * 0.5f,
                 gameState.EyeSize.Y * 0.5f)

        /// Get the bounds of the eye's sight.
        static member getViewBounds viewType world =
            match viewType with
            | Relative -> World.getViewBoundsRelative world
            | Absolute -> World.getViewBoundsAbsolute world

        /// Check that the given bounds is within the eye's sight.
        static member inView viewType (bounds : Vector4) world =
            let viewBounds = World.getViewBounds viewType world
            Math.isBoundsInBounds bounds viewBounds

        /// Transform the given mouse position to screen space.
        static member mouseToScreen (mousePosition : Vector2) world =
            let gameState = World.getGameState world
            let positionScreen =
                Vector2
                    (mousePosition.X - gameState.EyeSize.X * 0.5f,
                     -(mousePosition.Y - gameState.EyeSize.Y * 0.5f)) // negation for right-handedness
            positionScreen

        /// Transform the given mouse position to world space.
        static member mouseToWorld viewType mousePosition world =
            let positionScreen = World.mouseToScreen mousePosition world
            let view =
                match viewType with
                | Relative -> World.getViewRelative world
                | Absolute -> World.getViewAbsolute world
            let positionWorld = positionScreen * view
            positionWorld

        /// Transform the given mouse position to entity space.
        static member mouseToEntity viewType entityPosition mousePosition world =
            let mousePositionWorld = World.mouseToWorld viewType mousePosition world
            entityPosition - mousePositionWorld

        static member internal getGameProperty propertyName world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getGameId world :> obj, typeof<Guid>)
            | "Xtension" -> (World.getGameXtension world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getGameDispatcherNp world :> obj, typeof<GameDispatcher>)
            | "Specialization" -> (World.getGameSpecialization world :> obj, typeof<string>)
            | "CreationTimeStampNp" -> (World.getGameCreationTimeStampNp world :> obj, typeof<int64>)
            | "OptSelectedScreen" -> (World.getOptSelectedScreen world :> obj, typeof<Screen option>)
            | "OptScreenTransitionDestination" -> (World.getOptScreenTransitionDestination world :> obj, typeof<Screen option>)
            | "EyeCenter" -> (World.getEyeCenter world :> obj, typeof<Vector2>)
            | "EyeSize" -> (World.getEyeSize world :> obj, typeof<Vector2>)
            | _ -> GameState.getProperty propertyName (World.getGameState world)

        static member internal setGameProperty propertyName (property : obj * Type) world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change group id."
            | "Xtension" -> failwith "Cannot change group xtension."
            | "DispatcherNp" -> failwith "Cannot change group dispatcher."
            | "Specialization" -> failwith "Cannot change group specialization."
            | "CreationTimeStampNp" -> failwith "Cannot change group creation time stamp."
            | "OptOptSelectedScreen" -> World.setOptSelectedScreen (property |> fst :?> Screen option) world
            | "OptScreenTransitionDestination" -> World.setOptScreenTransitionDestination (property |> fst :?> Screen option) world
            | "EyeCenter" -> World.setEyeCenter (property |> fst :?> Vector2) world
            | "EyeSize" -> World.setEyeSize (property |> fst :?> Vector2) world
            | _ -> World.updateGameState (GameState.setProperty propertyName property) propertyName world

        static member internal makeGameState optSpecialization dispatcher =
            let gameState = GameState.make optSpecialization dispatcher
            Reflection.attachProperties GameState.copy dispatcher gameState

        static member internal writeGame3 writeScreens gameDescriptor world =
            let gameState = World.getGameState world
            let gameDispatcherName = getTypeName gameState.DispatcherNp
            let gameDescriptor = { gameDescriptor with GameDispatcher = gameDispatcherName }
            let viewGameProperties = Reflection.writePropertiesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = viewGameProperties }
            writeScreens gameDescriptor world

        static member internal readGame3 readScreens gameDescriptor world =

            // create the dispatcher
            let dispatcherName = gameDescriptor.GameDispatcher
            let dispatchers = World.getGameDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find GameDispatcher '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the bare game state
            let gameState = World.makeGameState None dispatcher

            // read the game state's value
            let gameState = Reflection.readPropertiesToTarget GameState.copy gameDescriptor.GameProperties gameState

            // set the game's state in the world
            let world = World.setGameState gameState world
            
            // read the game's screens
            let world = readScreens gameDescriptor world |> snd
            
            // choose the world
            World.choose world

        /// View all of the properties of a game.
        static member viewGameProperties world =
            let state = World.getGameState world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member private screenStateAdder screenState screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] ->
                    match Umap.tryFind screenName world.ScreenDirectory with
                    | Some (_, groupDirectory) ->
                        // NOTE: this is logically a redundant operation...
                        Umap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                    | None ->
                        let groupDirectory = Umap.makeEmpty None
                        Umap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Umap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] -> Umap.remove screenName world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Umap.remove screen.ScreenAddress world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateSetter screenState screen world =
#if DEBUG
            if not ^ Umap.containsKey screen.ScreenAddress world.ScreenStates then
                failwith ^ "Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'"
            if not ^ World.qualifyEventContext (atooa screen.ScreenAddress) world then
                failwith ^ "Cannot set the state of a screen in an unqualifed event context."
#endif
            let screenStates = Umap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member private removeScreenState screen world =
            World.screenStateRemover screen world

        static member private publishScreenChange (propertyName : string) (screen : Screen) oldWorld world =
            let changeEventAddress = ltoa [!!"Screen"; !!"Change"; !!propertyName; !!"Event"] ->>- screen.ScreenAddress
            let eventTrace = EventTrace.record "World" "publishScreenChange" EventTrace.empty
            World.publish { Participant = screen; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace screen world

        static member private getOptScreenState screen world =
            Umap.tryFind screen.ScreenAddress world.ScreenStates

        static member private getScreenState screen world =
            match World.getOptScreenState screen world with
            | Some screenState -> screenState
            | None -> failwith ^ "Could not find screen with address '" + scstring screen.ScreenAddress + "'."

        static member private setScreenState screenState screen world =
            World.screenStateSetter screenState screen world

        static member private updateScreenStateWithoutEvent updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        static member private updateScreenState updater propertyName screen world =
            let oldWorld = world
            let world = World.updateScreenStateWithoutEvent updater screen world
            World.publishScreenChange propertyName screen oldWorld world

        static member private publishScreenChanges screen oldWorld world =
            let screenState = World.getScreenState screen world
            let properties = World.getProperties screenState
            List.fold (fun world (propertyName, _) -> World.publishScreenChange propertyName screen oldWorld world) world properties

        /// Check that the world contains the proxied screen.
        static member containsScreen screen world =
            Option.isSome ^ World.getOptScreenState screen world

        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name
        static member internal getScreenXtension screen world = (World.getScreenState screen world).Xtension // TODO: try to get rid of this
        static member internal getScreenDispatcherNp screen world = (World.getScreenState screen world).DispatcherNp
        static member internal getScreenSpecialization screen world = (World.getScreenState screen world).Specialization
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) Property? Persistent screen world
        static member internal getScreenCreationTimeStampNp screen world = (World.getScreenState screen world).CreationTimeStampNp
        static member internal getScreenEntityTreeNp screen world = (World.getScreenState screen world).EntityTreeNp
        static member internal setScreenEntityTreeNp value screen world = World.updateScreenState (fun screenState -> { screenState with EntityTreeNp = value }) Property? EntityTreeNp screen world
        static member internal getScreenTransitionStateNp screen world = (World.getScreenState screen world).TransitionStateNp
        static member internal setScreenTransitionStateNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionStateNp = value }) Property? TransitionStateNp screen world
        static member internal getScreenTransitionTicksNp screen world = (World.getScreenState screen world).TransitionTicksNp
        static member internal setScreenTransitionTicksNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionTicksNp = value }) Property? TransitionTicksNp screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) Property? Incoming screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) Property? Outgoing screen world

        static member internal getScreenProperty propertyName screen world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getScreenId screen world :> obj, typeof<Guid>)
            | "Name" -> (World.getScreenName screen world :> obj, typeof<Name>)
            | "Xtension" -> (World.getScreenXtension screen world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getScreenDispatcherNp screen world :> obj, typeof<ScreenDispatcher>)
            | "Specialization" -> (World.getScreenSpecialization screen world :> obj, typeof<string>)
            | "Persistent" -> (World.getScreenPersistent screen world :> obj, typeof<bool>)
            | "CreationTimeStampNp" -> (World.getScreenCreationTimeStampNp screen world :> obj, typeof<int64>)
            | "EntityTreeNp" -> (World.getScreenEntityTreeNp screen world :> obj, typeof<Entity QuadTree MutantCache>)
            | "TransitionStateNp" -> (World.getScreenTransitionStateNp screen world :> obj, typeof<TransitionState>)
            | "TransitionTicksNp" -> (World.getScreenTransitionTicksNp screen world :> obj, typeof<int64>)
            | "Incoming" -> (World.getScreenIncoming screen world :> obj, typeof<Transition>)
            | "Outgoing" -> (World.getScreenOutgoing screen world :> obj, typeof<Transition>)
            | _ -> ScreenState.getProperty propertyName (World.getScreenState screen world)

        static member internal setScreenProperty propertyName (property : obj * Type) screen world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change screen id."
            | "Name" -> failwith "Cannot change screen name."
            | "Xtension" -> failwith "Cannot change group xtension."
            | "DispatcherNp" -> failwith "Cannot change screen dispatcher."
            | "Specialization" -> failwith "Cannot change screen specialization."
            | "Persistent" -> World.setScreenPersistent (property |> fst :?> bool) screen world
            | "CreationTimeStampNp" -> failwith "Cannot change screen creation time stamp."
            | "EntityTreeNp" -> failwith "Cannot change screen entity tree."
            | "TransitionStateNp" -> World.setScreenTransitionStateNp (property |> fst :?> TransitionState) screen world
            | "TransitionTicksNp" -> World.setScreenTransitionTicksNp (property |> fst :?> int64) screen world
            | "Incoming" -> World.setScreenIncoming (property |> fst :?> Transition) screen world
            | "Outgoing" -> World.setScreenOutgoing (property |> fst :?> Transition) screen world
            | _ -> World.updateScreenState (ScreenState.setProperty propertyName property) propertyName screen world

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not ^ World.containsScreen screen world
            if isNew || mayReplace then
                let oldWorld = world
                let world = World.addScreenState screenState screen world
                let world =
                    if isNew then
                        let dispatcher = World.getScreenDispatcherNp screen world
                        let world = World.withEventContext (fun world -> dispatcher.Register (screen, world)) (atooa screen.ScreenAddress) world
                        let eventTrace = EventTrace.record "World" "addScreen" EventTrace.empty
                        World.publish () (ltoa<unit> [!!"Screen"; !!"Add"; !!"Event"] ->- screen) eventTrace screen world
                    else world
                World.publishScreenChanges screen oldWorld world
            else failwith ^ "Adding a screen that the world already contains at address '" + scstring screen.ScreenAddress + "'."

        static member internal removeScreen3 removeGroups screen world =
            let eventTrace = EventTrace.record "World" "removeScreen" EventTrace.empty
            let world = World.publish () (ltoa<unit> [!!"Screen"; !!"Removing"; !!"Event"] ->- screen) eventTrace screen world
            if World.containsScreen screen world then
                let dispatcher = World.getScreenDispatcherNp screen world
                let world = World.withEventContext (fun world -> dispatcher.Unregister (screen, world)) (atooa screen.ScreenAddress) world
                let world = removeGroups screen world
                World.removeScreenState screen world
            else world

        static member internal writeScreen4 writeGroups screen screenDescriptor world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.DispatcherNp
            let screenDescriptor = { screenDescriptor with ScreenDispatcher = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            writeGroups screen screenDescriptor world

        static member internal readScreen4 readGroups screenDescriptor optName world =
            
            // create the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcher
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find ScreenDispatcher '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the bare screen state with name as id
            let screenState = ScreenState.make None None dispatcher

            // attach the screen state's instrinsic properties from its dispatcher if any
            let screenState = Reflection.attachProperties ScreenState.copy screenState.DispatcherNp screenState

            // read the screen state's value
            let screenState = Reflection.readPropertiesToTarget ScreenState.copy screenDescriptor.ScreenProperties screenState

            // apply the name if one is provided
            let screenState =
                match optName with
                | Some name -> { screenState with Name = name }
                | None -> screenState
            
            // add the screen's state to the world
            let screen = screenState.Name |> ntoa |> Screen.proxy
            let screenState =
                if World.containsScreen screen world
                then { screenState with EntityTreeNp = World.getScreenEntityTreeNp screen world }
                else screenState
            let world = World.addScreen true screenState screen world
            
            // read the screen's groups
            let world = readGroups screenDescriptor screen world |> snd
            (screen, world)

        /// View all of the properties of a screen.
        static member viewScreenProperties screen world =
            let state = World.getScreenState screen world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member private groupStateAdder groupState group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Umap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Umap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let groupDirectory = Umap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Umap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None ->
                            let entityDirectory = Umap.makeEmpty None
                            let groupDirectory = Umap.add groupName (group.GroupAddress, entityDirectory) groupDirectory
                            Umap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add group '" + scstring group.GroupAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Umap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member private groupStateRemover group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Umap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        let groupDirectory = Umap.remove groupName groupDirectory
                        Umap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove group '" + scstring group.GroupAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Umap.remove group.GroupAddress world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member private groupStateSetter groupState group world =
#if DEBUG
            if not ^ Umap.containsKey group.GroupAddress world.GroupStates then
                failwith ^ "Cannot set the state of a non-existent group '" + scstring group.GroupAddress + "'"
            if not ^ World.qualifyEventContext (atooa group.GroupAddress) world then
                failwith ^ "Cannot set the state of a group in an unqualifed event context."
#endif
            let groupStates = Umap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with GroupStates = groupStates }

        static member private addGroupState groupState group world =
            World.groupStateAdder groupState group world

        static member private removeGroupState group world =
            World.groupStateRemover group world

        static member private publishGroupChange (propertyName : string) (group : Group) oldWorld world =
            let changeEventAddress = ltoa [!!"Group"; !!"Change"; !!propertyName; !!"Event"] ->>- group.GroupAddress
            let eventTrace = EventTrace.record "World" "publishGroupChange" EventTrace.empty
            World.publish { Participant = group; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace group world

        static member private getOptGroupState group world =
            Umap.tryFind group.GroupAddress world.GroupStates

        static member private getGroupState group world =
            match World.getOptGroupState group world with
            | Some groupState -> groupState
            | None -> failwith ^ "Could not find group with address '" + scstring group.GroupAddress + "'."

        static member private setGroupState groupState group world =
            World.groupStateSetter groupState group world

        static member private updateGroupStateWithoutEvent updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

        static member private updateGroupState updater propertyName group world =
            let oldWorld = world
            let world = World.updateGroupStateWithoutEvent updater group world
            World.publishGroupChange propertyName group oldWorld world

        static member private publishGroupChanges group oldWorld world =
            let groupState = World.getGroupState group world
            let properties = World.getProperties groupState
            List.fold (fun world (propertyName, _) -> World.publishGroupChange propertyName group oldWorld world) world properties

        /// Check that the world contains a group.
        static member containsGroup group world =
            Option.isSome ^ World.getOptGroupState group world

        static member internal getGroupId group world = (World.getGroupState group world).Id
        static member internal getGroupName group world = (World.getGroupState group world).Name
        static member internal getGroupXtension group world = (World.getGroupState group world).Xtension // TODO: try to get rid of this
        static member internal getGroupDispatcherNp group world = (World.getGroupState group world).DispatcherNp
        static member internal getGroupSpecialization group world = (World.getGroupState group world).Specialization
        static member internal getGroupPersistent group world = (World.getGroupState group world).Persistent
        static member internal setGroupPersistent value group world = World.updateGroupState (fun groupState -> { groupState with Persistent = value }) Property? Persistent group world
        static member internal getGroupCreationTimeStampNp group world = (World.getGroupState group world).CreationTimeStampNp

        static member internal getGroupProperty propertyName group world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getGroupId group world :> obj, typeof<Guid>)
            | "Name" -> (World.getGroupName group world :> obj, typeof<Name>)
            | "Xtension" -> (World.getGroupXtension group world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getGroupDispatcherNp group world :> obj, typeof<GroupDispatcher>)
            | "Specialization" -> (World.getGroupSpecialization group world :> obj, typeof<string>)
            | "Persistent" -> (World.getGroupPersistent group world :> obj, typeof<bool>)
            | "CreationTimeStampNp" -> (World.getGroupCreationTimeStampNp group world :> obj, typeof<int64>)
            | _ -> GroupState.getProperty propertyName (World.getGroupState group world)

        static member internal setGroupProperty propertyName (property : obj * Type) group world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change group id."
            | "Name" -> failwith "Cannot change group name."
            | "Xtension" -> failwith "Cannot change group xtension."
            | "DispatcherNp" -> failwith "Cannot change group dispatcher."
            | "Specialization" -> failwith "Cannot change group specialization."
            | "Persistent" -> World.setGroupPersistent (property |> fst :?> bool) group world
            | "CreationTimeStampNp" -> failwith "Cannot change group creation time stamp."
            | _ -> World.updateGroupState (GroupState.setProperty propertyName property) propertyName group world

        static member private addGroup mayReplace groupState group world =
            let isNew = not ^ World.containsGroup group world
            if isNew || mayReplace then
                let oldWorld = world
                let world = World.addGroupState groupState group world
                let world =
                    if isNew then
                        let dispatcher = World.getGroupDispatcherNp group world
                        let world = World.withEventContext (fun world -> dispatcher.Register (group, world)) (atooa group.GroupAddress) world
                        let eventTrace = EventTrace.record "World" "addGroup" EventTrace.empty
                        World.publish () (ltoa<unit> [!!"Group"; !!"Add"; !!"Event"] ->- group) eventTrace group world
                    else world
                World.publishGroupChanges group oldWorld world
            else failwith ^ "Adding a group that the world already contains at address '" + scstring group.GroupAddress + "'."

        static member internal removeGroup3 removeEntities group world =
            let eventTrace = EventTrace.record "World" "removeGroup" EventTrace.empty
            let world = World.publish () (ltoa<unit> [!!"Group"; !!"Removing"; !!"Event"] ->- group) eventTrace group world
            if World.containsGroup group world then
                let dispatcher = World.getGroupDispatcherNp group world
                let world = World.withEventContext (fun world -> dispatcher.Unregister (group, world)) (atooa group.GroupAddress) world
                let world = removeEntities group world
                World.removeGroupState group world
            else world

        /// Create a group and add it to the world.
        static member createGroup5 dispatcherName optSpecialization optName screen world =
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ^ "Could not find a GroupDispatcher named '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"
            let groupState = GroupState.make optSpecialization optName dispatcher
            let groupState = Reflection.attachProperties GroupState.copy dispatcher groupState
            let group = screen.ScreenAddress -<<- ntoa<Group> groupState.Name |> Group.proxy
            let world = World.addGroup false groupState group world
            (group, world)

        /// Create a group and add it to the world.
        static member createGroup<'d when 'd :> GroupDispatcher> optSpecialization optName screen world =
            World.createGroup5 typeof<'d>.Name optSpecialization optName screen world

        static member internal writeGroup4 writeEntities group groupDescriptor world =
            let groupState = World.getGroupState group world
            let groupDispatcherName = getTypeName groupState.DispatcherNp
            let groupDescriptor = { groupDescriptor with GroupDispatcher = groupDispatcherName }
            let getGroupProperties = Reflection.writePropertiesFromTarget tautology3 groupDescriptor.GroupProperties groupState
            let groupDescriptor = { groupDescriptor with GroupProperties = getGroupProperties }
            writeEntities group groupDescriptor world

        static member internal readGroup5 readEntities groupDescriptor optName screen world =

            // create the dispatcher
            let dispatcherName = groupDescriptor.GroupDispatcher
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find GroupDispatcher '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the bare group state with name as id
            let groupState = GroupState.make None None dispatcher

            // attach the group state's instrinsic properties from its dispatcher if any
            let groupState = Reflection.attachProperties GroupState.copy groupState.DispatcherNp groupState

            // read the group state's value
            let groupState = Reflection.readPropertiesToTarget GroupState.copy groupDescriptor.GroupProperties groupState

            // apply the name if one is provided
            let groupState =
                match optName with
                | Some name -> { groupState with Name = name }
                | None -> groupState

            // add the group's state to the world
            let group = screen.ScreenAddress -<<- ntoa<Group> groupState.Name |> Group.proxy
            let world = World.addGroup true groupState group world

            // read the group's entities
            let world = readEntities groupDescriptor group world |> snd
            (group, world)

        /// View all of the properties of a group.
        static member viewGroupProperties group world =
            let state = World.getGroupState group world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member private optEntityStateKeyEquality 
            (entityAddress : Entity Address, world : World)
            (entityAddress2 : Entity Address, world2 : World) =
            refEq entityAddress entityAddress2 && refEq world world2

        static member private optEntityGetFreshKeyAndValue entity world =
            let optEntityState = Umap.tryFind entity.EntityAddress ^ world.EntityStates
            ((entity.EntityAddress, world), optEntityState)

        static member private optEntityStateFinder entity world =
            KeyedCache.getValue
                World.optEntityStateKeyEquality
                (fun () -> World.optEntityGetFreshKeyAndValue entity world)
                (entity.EntityAddress, world)
                (World.getOptEntityCache world)

        static member private entityStateAdder entityState entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Umap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Umap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Umap.add entityName entity.EntityAddress entityDirectory
                            let groupDirectory = Umap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Umap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent group."
                    | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Umap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateRemover entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Umap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Umap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Umap.remove entityName entityDirectory
                            let groupDirectory = Umap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Umap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent group."
                    | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Umap.remove entity.EntityAddress world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateSetter entityState entity world =
#if DEBUG
            if not ^ Umap.containsKey entity.EntityAddress world.EntityStates then
                failwith ^ "Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'"
            if not ^ World.qualifyEventContext (atooa entity.EntityAddress) world then
                failwith ^ "Cannot set the state of an entity in an unqualifed event context."
#endif
            let entityStates = Umap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private addEntityState entityState entity world =
            World.entityStateAdder entityState entity world

        static member private removeEntityState entity world =
            World.entityStateRemover entity world

        static member private shouldPublishEntityChange entity world =
            let entityState = World.getEntityState entity world
            entityState.PublishChanges

        static member private publishEntityChange propertyName entity oldWorld world =
            if World.shouldPublishEntityChange entity world then
                let changeEventAddress = ltoa [!!"Entity"; !!"Change"; !!propertyName; !!"Event"] ->>- entity.EntityAddress
                let eventTrace = EventTrace.record "World" "publishEntityChange" EventTrace.empty
                World.publish { Participant = entity; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace entity world
            else world

        static member private getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member private getEntityState entity world =
            match World.getOptEntityState entity world with
            | Some entityState -> entityState
            | None -> failwith ^ "Could not find entity with address '" + scstring entity.EntityAddress + "'."

        static member private setEntityState entityState entity world =
            World.entityStateSetter entityState entity world

        static member private updateEntityStateWithoutEvent updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

        static member private updateEntityState updater propertyName entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent updater entity world
            World.publishEntityChange propertyName entity oldWorld world

        static member private updateEntityStatePlus updater propertyName entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent updater entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            World.publishEntityChange propertyName entity oldWorld world

        static member private publishEntityChanges entity oldWorld world =
            if World.shouldPublishEntityChange entity world then
                let entityState = World.getEntityState entity world
                let properties = World.getProperties entityState
                List.fold (fun world (propertyName, _) -> World.publishEntityChange propertyName entity oldWorld world) world properties
            else world

        /// Check that the world contains an entity.
        static member containsEntity entity world =
            Option.isSome ^ World.getOptEntityState entity world

        static member private getEntityStateBoundsMax entityState =
            // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
            match entityState.Rotation with
            | 0.0f ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                boundsOverflow // no need to transform is unrotated
            | _ ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                let position = boundsOverflow.Xy
                let size = Vector2 (boundsOverflow.Z, boundsOverflow.W) - position
                let center = position + size * 0.5f
                let corner = position + size
                let centerToCorner = corner - center
                let quaternion = Quaternion.FromAxisAngle (Vector3.UnitZ, Constants.Math.DegreesToRadiansF * 45.0f)
                let newSizeOver2 = Vector2 (Vector2.Transform (centerToCorner, quaternion)).Y
                let newPosition = center - newSizeOver2
                let newSize = newSizeOver2 * 2.0f
                Vector4 (newPosition.X, newPosition.Y, newPosition.X + newSize.X, newPosition.Y + newSize.Y)

        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntityName entity world = (World.getEntityState entity world).Name
        static member internal getEntityXtension entity world = (World.getEntityState entity world).Xtension // TODO: try to get rid of this
        static member internal getEntityDispatcherNp entity world = (World.getEntityState entity world).DispatcherNp
        static member internal getEntitySpecialization entity world = (World.getEntityState entity world).Specialization
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> { entityState with Persistent = value }) Property? Persistent entity world
        static member internal getEntityCreationTimeStampNp entity world = (World.getEntityState entity world).CreationTimeStampNp
        static member internal getEntityOptOverlayName entity world = (World.getEntityState entity world).OptOverlayName
        static member internal setEntityOptOverlayName value entity world = World.updateEntityState (fun entityState -> { entityState with OptOverlayName = value }) Property? OptOverlayName entity world
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal setEntityPosition value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with EntityState.Position = value }) Property? Position entity world
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal setEntitySize value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Size = value }) Property? Size entity world
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal setEntityRotation value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Rotation = value }) Property? Rotation entity world
        static member internal getEntityDepth entity world = (World.getEntityState entity world).Depth
        static member internal setEntityDepth value entity world = World.updateEntityState (fun entityState -> { entityState with Depth = value }) Property? Depth entity world
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Overflow
        static member internal setEntityOverflow value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Overflow = value }) Property? Overflow entity world
        static member internal getEntityViewType entity world = (World.getEntityState entity world).ViewType
        static member internal setEntityViewType value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with ViewType = value }) Property? ViewType entity world
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal setEntityVisible value entity world = World.updateEntityState (fun entityState -> { entityState with Visible = value }) Property? Visible entity world
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal setEntityEnabled value entity world = World.updateEntityState (fun entityState -> { entityState with Enabled = value }) Property? Enabled entity world
        static member internal getEntityOmnipresent entity world = (World.getEntityState entity world).Omnipresent
        static member internal setEntityOmnipresent value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Omnipresent = value }) Property? Omnipresent entity world
        static member internal getEntityPublishChanges entity world = (World.getEntityState entity world).PublishChanges
        static member internal setEntityPublishChanges value entity world = World.updateEntityState (fun entityState -> { entityState with PublishChanges = value }) Property? PublishChanges entity world
        static member internal getEntityPublishUpdatesNp entity world = (World.getEntityState entity world).PublishUpdatesNp
        static member internal setEntityPublishUpdatesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishUpdatesNp = value }) Property? PublishUpdatesNp entity world
        static member internal getEntityPublishPostUpdatesNp entity world = (World.getEntityState entity world).PublishPostUpdatesNp
        static member internal setEntityPublishPostUpdatesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishPostUpdatesNp = value }) Property? PublishPostUpdatesNp entity world
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityFacetsNp entity world = (World.getEntityState entity world).FacetsNp

        static member internal getEntityTransform entity world =
            EntityState.getTransform (World.getEntityState entity world)
        
        static member internal setEntityTransform value entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent (EntityState.setTransform value) entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            if World.shouldPublishEntityChange entity world then
                let world = World.publishEntityChange Property? Position entity oldWorld world
                let world = World.publishEntityChange Property? Size entity oldWorld world
                let world = World.publishEntityChange Property? Rotation entity oldWorld world
                World.publishEntityChange Property? Depth entity oldWorld world
            else world

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ^ "Invalid facet name '" + facetName + "'."

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Xtension.tryGetProperty definition.PropertyName entityState.Xtension with
                        | Some property -> property.PropertyType <> definition.PropertyType
                        | None -> false)
                    facetPropertyDefinitions
            else false

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let propertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts propertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name ^ Reflection.getPropertyDefinitions facetType
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts facetPropertyDefinitions

            // compute the difference of the counts
            let finalPropertyDefinitionNameCounts =
                Map.map
                    (fun propertyName propertyCount ->
                        match Map.tryFind propertyName facetPropertyDefinitionNameCounts with
                        | Some facetPropertyCount -> propertyCount - facetPropertyCount
                        | None -> propertyCount)
                    propertyDefinitionNameCounts

            // build a set of all property names where the final counts are negative
            Map.fold
                (fun propertyNamesToDetach propertyName propertyCount ->
                    if propertyCount = 0
                    then Set.add propertyName propertyNamesToDetach
                    else propertyNamesToDetach)
                Set.empty
                finalPropertyDefinitionNameCounts

        /// Get an entity's intrinsic facet names.
        static member getIntrinsicFacetNames entityState =
            let intrinsicFacetNames = entityState.DispatcherNp |> getType |> Reflection.getIntrinsicFacetNames
            Set.ofList intrinsicFacetNames

        /// Get an entity's facet names via reflection.
        static member getFacetNamesReflectively entityState =
            let facetNames = List.map getTypeName entityState.FacetsNp
            Set.ofList facetNames

        static member private tryRemoveFacet facetName entityState optEntity world =
            match List.tryFind (fun facet -> getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match optEntity with
                    | Some entity ->
                        let world = World.withEventContext (fun world -> facet.Unregister (entity, world)) entity.ObjAddress world
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.copy propertyNames entityState
                let entityState = { entityState with FacetNames = Set.remove facetName entityState.FacetNames }
                let entityState = { entityState with FacetsNp = List.remove ((=) facet) entityState.FacetsNp }
                match optEntity with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let entityState = { entityState with FacetNames = Set.add facetName entityState.FacetNames }
                    let entityState = { entityState with FacetsNp = facet :: entityState.FacetsNp }
                    let entityState = Reflection.attachProperties EntityState.copy facet entityState
                    match optEntity with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityState entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.withEventContext (fun world -> facet.Register (entity, world)) entity.ObjAddress world
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState optEntity world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState optEntity world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member internal trySetFacetNames facetNames entityState optEntity world =
            let intrinsicFacetNames = World.getIntrinsicFacetNames entityState
            let extrinsicFacetNames = Set.fold (flip Set.remove) facetNames intrinsicFacetNames
            let facetNamesToRemove = Set.difference entityState.FacetNames extrinsicFacetNames
            let facetNamesToAdd = Set.difference extrinsicFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState optEntity world =
            let facetNamesToRemove = Set.difference oldFacetNames entityState.FacetNames
            let facetNamesToAdd = Set.difference entityState.FacetNames oldFacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.copy entityDispatchers facets entityState.DispatcherNp entityState

        static member internal applyEntityOverlay oldOverlayer overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OptOverlayName with
            | Some overlayName ->
                let oldFacetNames = entityState.FacetNames
                let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy overlayName overlayName entityState oldOverlayer overlayer
                match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                | Right (entityState, world) ->
                    let oldWorld = world
                    let facetNames = World.getFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.copy overlayName overlayName facetNames entityState oldOverlayer overlayer
                    let world = World.setEntityState entityState entity world
                    World.updateEntityInEntityTree entity oldWorld world
                | Left error -> Log.info ^ "There was an issue in applying a reloaded overlay: " + error; world
            | None -> world

        static member internal getEntityProperty propertyName entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getEntityId entity world :> obj, typeof<Guid>)
            | "Name" -> (World.getEntityName entity world :> obj, typeof<Name>)
            | "Xtension" -> (World.getEntityXtension entity world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getEntityDispatcherNp entity world :> obj, typeof<EntityDispatcher>)
            | "Persistent" -> (World.getEntityPersistent entity world :> obj, typeof<bool>)
            | "Specialization" -> (World.getEntitySpecialization entity world :> obj, typeof<string>)
            | "CreationTimeStampNp" -> (World.getEntityCreationTimeStampNp entity world :> obj, typeof<int64>)
            | "OptOverlayName" -> (World.getEntityOptOverlayName entity world :> obj, typeof<string option>)
            | "Position" -> (World.getEntityPosition entity world :> obj, typeof<Vector2>)
            | "Size" -> (World.getEntitySize entity world :> obj, typeof<Vector2>)
            | "Rotation" -> (World.getEntityRotation entity world :> obj, typeof<single>)
            | "Depth" -> (World.getEntityDepth entity world :> obj, typeof<single>)
            | "Overflow" -> (World.getEntityOverflow entity world :> obj, typeof<Vector2>)
            | "ViewType" -> (World.getEntityViewType entity world :> obj, typeof<ViewType>)
            | "Visible" -> (World.getEntityVisible entity world :> obj, typeof<bool>)
            | "Enabled" -> (World.getEntityEnabled entity world :> obj, typeof<bool>)
            | "Omnipresent" -> (World.getEntityOmnipresent entity world :> obj, typeof<bool>)
            | "PublishChanges" -> (World.getEntityPublishChanges entity world :> obj, typeof<bool>)
            | "PublishUpdatesNp" -> (World.getEntityPublishUpdatesNp entity world :> obj, typeof<bool>)
            | "PublishPostUpdatesNp" -> (World.getEntityPublishPostUpdatesNp entity world :> obj, typeof<bool>)
            | "FacetNames" -> (World.getEntityFacetNames entity world :> obj, typeof<string Set>)
            | "FacetsNp" -> (World.getEntityFacetsNp entity world :> obj, typeof<Facet list>)
            | _ -> EntityState.getProperty propertyName (World.getEntityState entity world)

        static member internal setEntityProperty propertyName (property : obj * Type) entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change entity id."
            | "Name" -> failwith "Cannot change entity name."
            | "Xtension" -> failwith "Cannot change group xtension."
            | "DispatcherNp" -> failwith "Cannot change entity dispatcher."
            | "Specialization" -> failwith "Cannot change entity specialization."
            | "Persistent" -> World.setEntityPersistent (property |> fst :?> bool) entity world
            | "CreationTimeStampNp" -> failwith "Cannot change entity creation time stamp."
            | "Position" -> World.setEntityPosition (property |> fst :?> Vector2) entity world
            | "Size" -> World.setEntitySize (property |> fst :?> Vector2) entity world
            | "Rotation" -> World.setEntityRotation (property |> fst :?> single) entity world
            | "Depth" -> World.setEntityDepth (property |> fst :?> single) entity world
            | "Overflow" -> World.setEntityOverflow (property |> fst :?> Vector2) entity world
            | "ViewType" -> World.setEntityViewType (property |> fst :?> ViewType) entity world
            | "Visible" -> World.setEntityVisible (property |> fst :?> bool) entity world
            | "Enabled" -> World.setEntityEnabled (property |> fst :?> bool) entity world
            | "Omnipresent" -> World.setEntityOmnipresent (property |> fst :?> bool) entity world
            | "PublishChanges" -> World.setEntityPublishChanges (property |> fst :?> bool) entity world
            | "PublishUpdatesNp" -> failwith "Cannot change entity publish updates."
            | "PublishPostUpdatesNp" -> failwith "Cannot change entity post-publish updates."
            | "FacetNames" -> failwith "Cannot change entity facet names with a property setter."
            | "FacetsNp" -> failwith "Cannot change entity facets with a property setter."
            | _ -> World.updateEntityState (EntityState.setProperty propertyName property) propertyName entity world

        /// Get the maxima bounds of the entity as determined by size, position, rotation, and overflow.
        static member getEntityBoundsMax entity world =
            let entityState = World.getEntityState entity world
            World.getEntityStateBoundsMax entityState

        /// Get the quick size of an entity (the appropriate user-defined size for an entity).
        static member getEntityQuickSize (entity : Entity) world =
            let dispatcher = World.getEntityDispatcherNp entity world
            let facets = World.getEntityFacetsNp entity world
            let quickSize = dispatcher.GetQuickSize (entity, world)
            List.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector2
                        (Math.Max (quickSize.X, maxSize.X),
                         Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                facets

        /// Get an entity's sorting priority.
        static member getEntitySortingPriority entity world =
            let entityState = World.getEntityState entity world
            { SortDepth = entityState.Depth; SortPositionY = entityState.Position.Y; TargetEntity = entity }

        static member private updateEntityPublishEventFlag setFlag entity eventAddress world =
            let publishUpdates =
                match Umap.tryFind eventAddress ^ World.getSubscriptions world with
                | Some [] -> failwithumf () // NOTE: event system is defined to clean up all empty subscription entries
                | Some (_ :: _) -> true
                | None -> false
            if World.containsEntity entity world
            then setFlag publishUpdates entity world
            else world

        static member internal updateEntityPublishUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishUpdatesNp entity (atooa entity.UpdateAddress) world

        static member internal updateEntityPublishPostUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishPostUpdatesNp entity (atooa entity.PostUpdateAddress) world

        static member internal updateEntityPublishFlags entity world =
            let world = World.updateEntityPublishUpdateFlag entity world
            let world = World.updateEntityPublishPostUpdateFlag entity world
            world

        static member private addEntity mayReplace entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            let isNew = not ^ World.containsEntity entity world
            if isNew || mayReplace then

                // get old world for entity tree rebuild and change events
                let oldWorld = world
                
                // adding entity to world
                let world = World.addEntityState entityState entity world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity world
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.addElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let world = World.setScreenEntityTreeNp entityTree screen world

                // register entity if needed
                let world =
                    if isNew then
                        World.withEventContext (fun world ->
                            let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                            let facets = World.getEntityFacetsNp entity world
                            let world = dispatcher.Register (entity, world)
                            let world = List.fold (fun world (facet : Facet) -> facet.Register (entity, world)) world facets
                            let world = World.updateEntityPublishFlags entity world
                            let eventTrace = EventTrace.record "World" "addEntity" EventTrace.empty
                            World.publish () (ltoa<unit> [!!"Entity"; !!"Add"; !!"Event"] ->- entity) eventTrace entity world)
                            entity.ObjAddress
                            world
                    else world

                // publish change event for every property
                World.publishEntityChanges entity oldWorld world

            // handle failure
            else failwith ^ "Adding an entity that the world already contains at address '" + scstring entity.EntityAddress + "'."

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world = World.removeEntity entity world

        /// Create an entity and add it to the world.
        static member createEntity5 dispatcherName optSpecialization optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ^ "Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"

            // try to compute the routed overlay name
            let classification = Classification.make dispatcherName ^ Option.getOrDefault Constants.Engine.VanillaSpecialization optSpecialization
            let optOverlayName = OverlayRouter.findOptOverlayName classification overlayRouter

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make optSpecialization optName optOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let entityState =
                match optOverlayName with
                | Some routedOverlayName ->

                    // apply overlay to facets
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy dispatcherName routedOverlayName entityState overlayer overlayer

                    // synchronize the entity's facets (and attach their properties)
                    match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> Log.debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState

            // apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something
                    if dispatcherName <> overlayName then
                        let facetNames = World.getFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // add entity's state to world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity false entityState entity world
            (entity, world)

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> optSpecialization optName group world =
            World.createEntity5 typeof<'d>.Name optSpecialization optName group world

        static member private removeEntity entity world =
            
            // ensure entity exists in the world
            if World.containsEntity entity world then
                
                // publish event and unregister entity
                let world =
                    World.withEventContext (fun world ->
                        let eventTrace = EventTrace.record "World" "removeEntity" EventTrace.empty
                        let world = World.publish () (ltoa<unit> [!!"Entity"; !!"Removing"; !!"Event"] ->- entity) eventTrace entity world
                        let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                        let facets = World.getEntityFacetsNp entity world
                        let world = dispatcher.Unregister (entity, world)
                        List.fold (fun world (facet : Facet) -> facet.Unregister (entity, world)) world facets)
                        entity.ObjAddress
                        world

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity oldWorld
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.removeElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // remove the entity from the world
                World.removeEntityState entity world

            // pass
            else world

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // create the dispatcher
            let dispatcherName = entityDescriptor.EntityDispatcher
            let dispatchers = World.getEntityDispatchers world
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher =
                        match Map.tryFind dispatcherName dispatchers with
                        | Some dispatcher -> dispatcher
                        | None -> failwith ^ "Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?"
                    (dispatcherName, dispatcher)

            // try to compute the routed overlay name
            let classification = Classification.makeVanilla dispatcherName
            let optOverlayName = OverlayRouter.findOptOverlayName classification overlayRouter

            // make the bare entity state with name as id
            let entityState = EntityState.make None None optOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let entityState = Reflection.tryReadOptOverlayNameToTarget EntityState.copy entityDescriptor.EntityProperties entityState
            let entityState =
                match (optOverlayName, entityState.OptOverlayName) with
                | (Some routedOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames EntityState.copy routedOverlayName overlayName entityState overlayer overlayer
                | (_, _) -> entityState

            // read the entity state's facet names
            let entityState = Reflection.readFacetNamesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState
            
            // synchronize the entity state's facets (and attach their properties)
            let entityState =
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> Log.debug error; entityState

            // attempt to apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: applying overlay only when it will change something
                    if dispatcherName <> overlayName then
                        let facetNames = World.getFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // read the entity state's values
            let entityState = Reflection.readPropertiesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // apply the name if one is provided
            let entityState =
                match optName with
                | Some name -> { entityState with Name = name }
                | None -> entityState

            // add entity state to the world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity true entityState entity world
            (entity, world)

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.DispatcherNp
            let entityDescriptor = { entityDescriptor with EntityDispatcher = entityDispatcherName }
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let overlayRouter = World.getOverlayRouter world
                    let classification = Classification.make entityDispatcherName entityState.Specialization
                    let defaultOptOverlayName = OverlayRouter.findOptOverlayName classification overlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let overlayer = World.getOverlayer world
                    let facetNames = World.getFacetNamesReflectively entityState
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entityState overlayer
            let getEntityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            { entityDescriptor with EntityProperties = getEntityProperties }

        /// Reassign an entity's identity and / or group. Note that since this destroys the reassigned entity
        /// immediately, you should not call this inside an event handler that involves the reassigned entity itself.
        static member reassignEntityImmediate entity optName group world =
            let entityState = World.getEntityState entity world
            let world = World.removeEntity entity world
            let (id, name) = Reflection.deriveIdAndName optName
            let entityState = { entityState with Id = id; Name = name }
            let transmutedEntity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
            let world = World.addEntity false entityState transmutedEntity world
            (transmutedEntity, world)

        /// Reassign an entity's identity and / or group.
        static member reassignEntity entity optName group world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.reassignEntityImmediate entity optName group world |> snd }}
            World.addTasklet tasklet world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOptOverlayName optOverlayName entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOptOverlayName = oldEntityState.OptOverlayName
            let entityState = { oldEntityState with OptOverlayName = optOverlayName }
            match (oldOptOverlayName, optOverlayName) with
            | (Some oldOverlayName, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let (entityState, world) =
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy oldOverlayName overlayName entityState overlayer overlayer
                    match World.trySynchronizeFacetsToNames entityState.FacetNames entityState (Some entity) world with
                    | Right (entityState, world) -> (entityState, world)
                    | Left error -> Log.debug error; (entityState, world)
                let facetNames = World.getFacetNamesReflectively entityState
                let entityState = Overlayer.applyOverlay EntityState.copy oldOverlayName overlayName facetNames entityState overlayer
                let oldWorld = world
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChanges entity oldWorld world
                Right world
            | (_, _) ->
                World.choose world |> ignore
                Left "Could not set the entity's overlay name because setting an overlay to or from None is currently unimplemented."

        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            match World.trySetFacetNames facetNames entityState (Some entity) world with
            | Right (entityState, world) ->
                let oldWorld = world
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChanges entity oldWorld world
                Right world
            | Left error -> Left error

        /// View all of the properties of an entity.
        static member viewEntityProperties entity world =
            let state = World.getEntityState entity world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member internal updateEntityInEntityTreeImpl entity oldWorld world =
            // OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease address hashing
            // OPTIMIZATION: assumes a valid entity address with List.head on its names
            let screen =
                match (World.getGameState world).OptSelectedScreen with
                | Some screen when Address.getName screen.ScreenAddress = List.head ^ Address.getNames entity.EntityAddress -> screen
                | Some _ | None -> entity.EntityAddress |> Address.getNames |> List.head |> ntoa<Screen> |> Screen.proxy

            // proceed with updating entity in entity tree
            let screenState = World.getScreenState screen world
            let entityTree =
                MutantCache.mutateMutant
                    (fun () -> World.rebuildEntityTree screen oldWorld)
                    (fun entityTree ->
                        let oldEntityState = World.getEntityState entity oldWorld
                        let oldEntityBoundsMax = World.getEntityStateBoundsMax oldEntityState
                        let entityState = World.getEntityState entity world
                        let entityBoundsMax = World.getEntityStateBoundsMax entityState
                        QuadTree.updateElement
                            (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityBoundsMax
                            (entityState.Omnipresent || entityState.ViewType = Absolute) entityBoundsMax
                            entity entityTree
                        entityTree)
                    screenState.EntityTreeNp
            let screenState = { screenState with EntityTreeNp = entityTree }
            World.setScreenState screenState screen world

    type World with

        /// Copy an entity to the clipboard.
        static member copyToClipboard entity world =
            let entityState = World.getEntityState entity world
            Clipboard <- Some (entityState :> obj)

        /// Cut an entity to the clipboard.
        static member cutToClipboard entity world =
            World.copyToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap group world =
            match Clipboard with
            | Some entityStateObj ->
                let entityState = entityStateObj :?> EntityState
                let id = makeGuid ()
                let name = !!(scstring id)
                let entityState = { entityState with Id = id; Name = name }
                let position =
                    if atMouse
                    then World.mouseToWorld entityState.ViewType rightClickPosition world
                    else World.mouseToWorld entityState.ViewType (World.getEyeSize world * 0.5f) world
                let transform = { EntityState.getTransform entityState with Position = position }
                let transform = Math.snapTransform positionSnap rotationSnap transform
                let entityState = EntityState.setTransform transform entityState
                let entity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

    type World with

        // Make the world.
        static member internal make eventSystem dispatchers subsystems ambientState optGameSpecialization activeGameDispatcher =
            let gameState = GameState.make optGameSpecialization activeGameDispatcher
            let world =
                { EventSystem = eventSystem
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  OptEntityCache = Unchecked.defaultof<KeyedCache<Entity Address * World, EntityState option>>
                  ScreenDirectory = Umap.makeEmpty None
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = Umap.makeEmpty None
                  GroupStates = Umap.makeEmpty None
                  EntityStates = Umap.makeEmpty None }
            World.choose world