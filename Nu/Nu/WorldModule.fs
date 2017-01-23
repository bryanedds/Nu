// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Reflection
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldModule =

    /// Mutable clipboard that allows its state to persist beyond undo / redo.
    let mutable private Clipboard : obj option = None

    /// F# reach-around for evaluating a script expression.
    let mutable internal eval : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World =
        Unchecked.defaultof<Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalMany : Scripting.Expr list -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr list * World =
        Unchecked.defaultof<Scripting.Expr list -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr list * World>

    /// Evaluate a script expression, with logging on violation result.
    let evalWithLogging expr localFrame simulant world =
        let (evaled, world) = eval expr localFrame simulant world
        Scripting.log evaled
        (evaled, world)

    /// Evaluate a series of script expressions, with logging on violation result.
    let evalManyWithLogging exprs localFrame simulant world =
        let (evaleds, world) = evalMany exprs localFrame simulant world
        List.iter Scripting.log evaleds
        (evaleds, world)

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
            // OPTIMIZATION: entity priority boxed up front to decrease GC pressure.
            let entityPriorityBoxed = Constants.Engine.EntitySortPriority :> IComparable
            World.sortSubscriptionsBy
                (fun (participant : Participant) _ ->
                    match participant with
                    | :? Game -> Constants.Engine.GameSortPriority :> IComparable
                    | :? Screen -> Constants.Engine.ScreenSortPriority :> IComparable
                    | :? Layer -> Constants.Engine.LayerSortPriority :> IComparable
                    | :? Entity -> entityPriorityBoxed
                    | _ -> failwithumf ())
                subscriptions
                world

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

        /// Get the layer dispatchers of the world.
        static member getLayerDispatchers world =
            world.Dispatchers.LayerDispatchers

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
        static member tryLoadSymbolStorePackage implicitDlimiters packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage implicitDlimiters packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol implicitDlimiters assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol implicitDlimiters assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols implicitDlimiters assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols implicitDlimiters assetTags symbolStore
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

        /// TODO: document. 
        static member dispatchersToOverlayRoutes entityDispatchers =
            entityDispatchers |>
            Map.toValueListBy getTypeName |>
            List.map (fun typeName -> (typeName, OverlayDescriptor.makeVanilla (Some typeName)))

    type World with

        /// Get the optional cached screen.
        static member internal getScreenCachedOpt world =
            world.ScreenCachedOpt

        /// Get the optional cached layer.
        static member internal getLayerCachedOpt world =
            world.LayerCachedOpt

        /// Get the optional cached entity.
        static member internal getEntityCachedOpt world =
            world.EntityCachedOpt

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
            World.publish6 { Participant = game; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace game false world

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
        static member internal getGameDispatcherNp world = (World.getGameState world).DispatcherNp
        static member internal getGameSpecialization world = (World.getGameState world).Specialization
        static member internal getGameCreationTimeStampNp world = (World.getGameState world).CreationTimeStampNp
        static member internal getGameImperative world = Xtension.getImperative (World.getGameState world).Xtension
        static member internal getGameScriptOpt world = (World.getGameState world).ScriptOpt
        static member internal setGameScriptOpt value world = World.updateGameState (fun gameState -> { gameState with ScriptOpt = value }) Property? ScriptOpt world
        static member internal getGameScript world = (World.getGameState world).Script
        static member internal setGameScript value world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateGameState (fun gameState -> { gameState with Script = value }) Property? Script world
            let world = World.setGameScriptFrameNp scriptFrame world
            evalManyWithLogging value scriptFrame (Game.proxy Address.empty) world |> snd
        static member internal getGameScriptFrameNp world = (World.getGameState world).ScriptFrameNp
        static member internal setGameScriptFrameNp value world = World.updateGameState (fun gameState -> { gameState with ScriptFrameNp = value }) Property? ScriptFrameNp world
        static member internal getGameOnRegister world = (World.getGameState world).OnRegister
        static member internal setGameOnRegister value world = World.updateGameState (fun gameState -> { gameState with OnRegister = value }) Property? OnRegister world
        static member internal getGameOnUnregister world = (World.getGameState world).OnUnregister
        static member internal setGameOnUnregister value world = World.updateGameState (fun gameState -> { gameState with OnUnregister = value }) Property? OnUnregister world
        static member internal getGameOnUpdate world = (World.getGameState world).OnUpdate
        static member internal setGameOnUpdate value world = World.updateGameState (fun gameState -> { gameState with OnUpdate = value }) Property? OnUpdate world
        static member internal getGameOnPostUpdate world = (World.getGameState world).OnPostUpdate
        static member internal setGameOnPostUpdate value world = World.updateGameState (fun gameState -> { gameState with OnPostUpdate = value }) Property? OnPostUpdate world
        static member internal getGameOnActualize world = (World.getGameState world).OnActualize
        static member internal setGameOnActualize value world = World.updateGameState (fun gameState -> { gameState with OnActualize = value }) Property? OnActualize world

        /// Get the current eye center.
        static member getEyeCenter world =
            (World.getGameState world).EyeCenter

        /// Set the current eye center.
        static member setEyeCenter value world =
            World.updateGameState (fun layerState -> { layerState with EyeCenter = value }) Property? EyeCenter world

        /// Get the current eye size.
        static member getEyeSize world =
            (World.getGameState world).EyeSize

        /// Set the current eye size.
        static member setEyeSize value world =
            World.updateGameState (fun layerState -> { layerState with EyeSize = value }) Property? EyeSize world

        /// Get the currently selected screen, if any.
        static member getSelectedScreenOpt world =
            (World.getGameState world).SelectedScreenOpt
        
        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreenOpt value world =
            World.updateGameState (fun layerState -> { layerState with SelectedScreenOpt = value }) Property? SelectedScreenOpt world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        static member getSelectedScreen world =
            Option.get ^ World.getSelectedScreenOpt world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreen screen world =
            World.setSelectedScreenOpt (Some screen) world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getScreenTransitionDestinationOpt world =
            (World.getGameState world).ScreenTransitionDestinationOpt

        /// Set the current destination screen or None. Be careful using this function as calling
        /// it is predicated that no screen transition is currently underway.
        /// TODO: consider asserting such predication here.
        static member internal setScreenTransitionDestinationOpt destination world =
            World.updateGameState (fun gameState -> { gameState with ScreenTransitionDestinationOpt = destination }) Property? ScreenTransitionDestinationOpt world

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
            Math.isBoundsIntersectingBounds bounds viewBounds

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

        /// Try to convert an asset tag to a script.
        static member assetTagToScriptOpt assetTag world =
            let (symbolOpt, world) = World.tryFindSymbol true assetTag world
            let scriptOpt =
                match symbolOpt with
                | Some symbol ->
                    try let script = valueize<Scripting.Expr list> symbol in Some script
                    with exn -> Log.info ^ "Failed to convert symbol '" + scstring symbol + "' to Script due to: " + scstring exn; None
                | None -> None
            (scriptOpt, world)

        static member internal tryGetGameProperty propertyName world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> Some (World.getGameId world :> obj, typeof<Guid>)
            | "DispatcherNp" -> Some (World.getGameDispatcherNp world :> obj, typeof<GameDispatcher>)
            | "Specialization" -> Some (World.getGameSpecialization world :> obj, typeof<string>)
            | "CreationTimeStampNp" -> Some (World.getGameCreationTimeStampNp world :> obj, typeof<int64>)
            | "Imperative" -> Some (World.getGameImperative world :> obj, typeof<bool>)
            | "ScriptOpt" -> Some (World.getGameScriptOpt world :> obj, typeof<AssetTag option>)
            | "Script" -> Some (World.getGameScript world :> obj, typeof<Scripting.Expr list>)
            | "ScriptFrameNp" -> Some (World.getGameScript world :> obj, typeof<Scripting.ProceduralFrame list>)
            | "OnRegister" -> Some (World.getGameOnRegister world :> obj, typeof<Scripting.Expr>)
            | "OnUnregister" -> Some (World.getGameOnUnregister world :> obj, typeof<Scripting.Expr>)
            | "OnUpdate" -> Some (World.getGameOnUpdate world :> obj, typeof<Scripting.Expr>)
            | "OnPostUpdate" -> Some (World.getGameOnPostUpdate world :> obj, typeof<Scripting.Expr>)
            | "OnActualize" -> Some (World.getGameOnActualize world :> obj, typeof<Scripting.Expr>)
            | "SelectedScreenOpt" -> Some (World.getSelectedScreenOpt world :> obj, typeof<Screen option>)
            | "ScreenTransitionDestinationOpt" -> Some (World.getScreenTransitionDestinationOpt world :> obj, typeof<Screen option>)
            | "EyeCenter" -> Some (World.getEyeCenter world :> obj, typeof<Vector2>)
            | "EyeSize" -> Some (World.getEyeSize world :> obj, typeof<Vector2>)
            | _ -> GameState.tryGetProperty propertyName (World.getGameState world)

        static member internal getGameProperty propertyName world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (World.getGameId world :> obj, typeof<Guid>)
            | "DispatcherNp" -> (World.getGameDispatcherNp world :> obj, typeof<GameDispatcher>)
            | "Specialization" -> (World.getGameSpecialization world :> obj, typeof<string>)
            | "CreationTimeStampNp" -> (World.getGameCreationTimeStampNp world :> obj, typeof<int64>)
            | "Imperative" -> (World.getGameImperative world :> obj, typeof<bool>)
            | "ScriptOpt" -> (World.getGameScriptOpt world :> obj, typeof<AssetTag option>)
            | "Script" -> (World.getGameScript world :> obj, typeof<Scripting.Expr list>)
            | "OnRegister" -> (World.getGameOnRegister world :> obj, typeof<Scripting.Expr>)
            | "OnUnregister" -> (World.getGameOnUnregister world :> obj, typeof<Scripting.Expr>)
            | "OnUpdate" -> (World.getGameOnUpdate world :> obj, typeof<Scripting.Expr>)
            | "OnPostUpdate" -> (World.getGameOnPostUpdate world :> obj, typeof<Scripting.Expr>)
            | "OnActualize" -> (World.getGameOnActualize world :> obj, typeof<Scripting.Expr>)
            | "SelectedScreenOpt" -> (World.getSelectedScreenOpt world :> obj, typeof<Screen option>)
            | "ScreenTransitionDestinationOpt" -> (World.getScreenTransitionDestinationOpt world :> obj, typeof<Screen option>)
            | "EyeCenter" -> (World.getEyeCenter world :> obj, typeof<Vector2>)
            | "EyeSize" -> (World.getEyeSize world :> obj, typeof<Vector2>)
            | _ -> GameState.getProperty propertyName (World.getGameState world)

        static member internal trySetGameProperty propertyName (property : obj * Type) world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (false, world)
            | "DispatcherNp" -> (false, world)
            | "Specialization" -> (false, world)
            | "CreationTimeStampNp" -> (false, world)
            | "Imperative" -> (false, world)
            | "ScriptOpt" -> (false, world)
            | "Script" -> (false, world)
            | "ScriptFrameNp" -> (false, world)
            | "OnRegister" -> (false, world)
            | "OnUnregister" -> (false, world)
            | "OnUpdate" -> (false, world)
            | "OnPostUpdate" -> (false, world)
            | "OnActualize" -> (false, world)
            | "SelectedScreenOpt" -> (true, World.setSelectedScreenOpt (property |> fst :?> Screen option) world)
            | "ScreenTransitionDestinationOpt" -> (true, World.setScreenTransitionDestinationOpt (property |> fst :?> Screen option) world)
            | "EyeCenter" -> (true, World.setEyeCenter (property |> fst :?> Vector2) world)
            | "EyeSize" -> (true, World.setEyeSize (property |> fst :?> Vector2) world)
            | _ ->
                // HACK: needed to mutate a flag to get the success state out of an updateGameState callback...
                let mutable success = false
                let world =
                    World.updateGameState (fun gameState ->
                        let (successInner, gameState) = GameState.trySetProperty propertyName property gameState
                        success <- successInner
                        gameState)
                        propertyName
                        world
                (success, world)

        static member internal setGameProperty propertyName (property : obj * Type) world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change game " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change game " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change game " + propertyName + "."
            | "CreationTimeStampNp" -> failwith ^ "Cannot change game " + propertyName + "."
            | "Imperative" -> failwith ^ "Cannot change game " + propertyName + "."
            | "ScriptOpt" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "Script" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "ScriptFrameNp" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "OnRegister" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "OnUnregister" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "OnUpdate" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "OnPostUpdate" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "OnActualize" -> failwith ^ "Cannot change game " + propertyName + " dynamically."
            | "SelectedScreenOpt" -> World.setSelectedScreenOpt (property |> fst :?> Screen option) world
            | "ScreenTransitionDestinationOpt" -> World.setScreenTransitionDestinationOpt (property |> fst :?> Screen option) world
            | "EyeCenter" -> World.setEyeCenter (property |> fst :?> Vector2) world
            | "EyeSize" -> World.setEyeSize (property |> fst :?> Vector2) world
            | _ -> World.updateGameState (GameState.setProperty propertyName property) propertyName world

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
                    Log.info ^ "Could not find GameDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the game state and populate its properties
            let gameState = GameState.make None dispatcher
            let gameState = Reflection.attachProperties GameState.copy dispatcher gameState
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

        static member private screenStateKeyEquality
            (screenAddress : Screen Address, screenStates : UMap<Screen Address, ScreenState>)
            (screenAddress2 : Screen Address, screenStates2 : UMap<Screen Address, ScreenState>) =
            refEq screenAddress screenAddress2 && refEq screenStates screenStates2

        static member private screenGetFreshKeyAndValue screen world =
            let screenStateOpt = UMap.tryFind screen.ScreenAddress world.ScreenStates
            ((screen.ScreenAddress, world.ScreenStates), screenStateOpt)

        static member private screenStateFinder screen world =
            KeyedCache.getValue
                World.screenStateKeyEquality
                (fun () -> World.screenGetFreshKeyAndValue screen world)
                (screen.ScreenAddress, world.ScreenStates)
                (World.getScreenCachedOpt world)

        static member private screenStateAdder screenState screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (_, layerDirectory) ->
                        // NOTE: this is logically a redundant operation...
                        UMap.add screenName (screen.ScreenAddress, layerDirectory) world.ScreenDirectory
                    | None ->
                        let layerDirectory = UMap.makeEmpty None
                        UMap.add screenName (screen.ScreenAddress, layerDirectory) world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = UMap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] -> UMap.remove screenName world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = UMap.remove screen.ScreenAddress world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateSetter screenState screen world =
#if DEBUG
            if not ^ UMap.containsKey screen.ScreenAddress world.ScreenStates then
                failwith ^ "Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'"
            if not ^ World.qualifyEventContext (atooa screen.ScreenAddress) world then
                failwith ^ "Cannot set the state of a screen in an unqualifed event context."
#endif
            let screenStates = UMap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member private removeScreenState screen world =
            World.screenStateRemover screen world

        static member private publishScreenChange (propertyName : string) (screen : Screen) oldWorld world =
            let changeEventAddress = ltoa [!!"Screen"; !!"Change"; !!propertyName; !!"Event"] ->>- screen.ScreenAddress
            let eventTrace = EventTrace.record "World" "publishScreenChange" EventTrace.empty
            World.publish6 { Participant = screen; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace screen false world

        static member private getScreenStateOpt screen world =
             World.screenStateFinder screen world

        static member private getScreenState screen world =
            match World.getScreenStateOpt screen world with
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

        /// Check that the world contains the proxied screen.
        static member containsScreen screen world =
            Option.isSome ^ World.getScreenStateOpt screen world

        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name
        static member internal getScreenDispatcherNp screen world = (World.getScreenState screen world).DispatcherNp
        static member internal getScreenSpecialization screen world = (World.getScreenState screen world).Specialization
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) Property? Persistent screen world
        static member internal getScreenCreationTimeStampNp screen world = (World.getScreenState screen world).CreationTimeStampNp
        static member internal getScreenImperative screen world = Xtension.getImperative (World.getScreenState screen world).Xtension
        static member internal getScreenScriptOpt screen world = (World.getScreenState screen world).ScriptOpt
        static member internal setScreenScriptOpt value screen world = World.updateScreenState (fun screenState -> { screenState with ScriptOpt = value }) Property? ScriptOpt screen world
        static member internal getScreenScript screen world = (World.getScreenState screen world).Script
        static member internal setScreenScript value screen world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateScreenState (fun screenState -> { screenState with Script = value }) Property? Script screen world
            let world = World.setScreenScriptFrameNp scriptFrame screen world
            evalManyWithLogging value scriptFrame screen world |> snd
        static member internal getScreenScriptFrameNp screen world = (World.getScreenState screen world).ScriptFrameNp
        static member internal setScreenScriptFrameNp value screen world = World.updateScreenState (fun screenState -> { screenState with ScriptFrameNp = value }) Property? ScriptFrameNp screen world
        static member internal getScreenOnRegister screen world = (World.getScreenState screen world).OnRegister
        static member internal setScreenOnRegister value screen world = World.updateScreenState (fun screenState -> { screenState with OnRegister = value }) Property? OnRegister screen world
        static member internal getScreenOnUnregister screen world = (World.getScreenState screen world).OnUnregister
        static member internal setScreenOnUnregister value screen world = World.updateScreenState (fun screenState -> { screenState with OnUnregister = value }) Property? OnUnregister screen world
        static member internal getScreenOnUpdate screen world = (World.getScreenState screen world).OnUpdate
        static member internal setScreenOnUpdate value screen world = World.updateScreenState (fun screenState -> { screenState with OnUpdate = value }) Property? OnUpdate screen world
        static member internal getScreenOnPostUpdate screen world = (World.getScreenState screen world).OnPostUpdate
        static member internal setScreenOnPostUpdate value screen world = World.updateScreenState (fun screenState -> { screenState with OnPostUpdate = value }) Property? OnPostUpdate screen world
        static member internal getScreenOnActualize screen world = (World.getScreenState screen world).OnActualize
        static member internal setScreenOnActualize value screen world = World.updateScreenState (fun screenState -> { screenState with OnActualize = value }) Property? OnActualize screen world
        static member internal getScreenEntityTreeNp screen world = (World.getScreenState screen world).EntityTreeNp
        static member internal setScreenEntityTreeNpNoEvent value screen world = World.updateScreenStateWithoutEvent (fun screenState -> { screenState with EntityTreeNp = value }) screen world
        static member internal getScreenTransitionStateNp screen world = (World.getScreenState screen world).TransitionStateNp
        static member internal setScreenTransitionStateNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionStateNp = value }) Property? TransitionStateNp screen world
        static member internal getScreenTransitionTicksNp screen world = (World.getScreenState screen world).TransitionTicksNp
        static member internal setScreenTransitionTicksNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionTicksNp = value }) Property? TransitionTicksNp screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) Property? Incoming screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) Property? Outgoing screen world

        static member internal tryGetScreenProperty propertyName screen world =
            if World.containsScreen screen world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> Some (World.getScreenId screen world :> obj, typeof<Guid>)
                | "Name" -> Some (World.getScreenName screen world :> obj, typeof<Name>)
                | "DispatcherNp" -> Some (World.getScreenDispatcherNp screen world :> obj, typeof<ScreenDispatcher>)
                | "Specialization" -> Some (World.getScreenSpecialization screen world :> obj, typeof<string>)
                | "Persistent" -> Some (World.getScreenPersistent screen world :> obj, typeof<bool>)
                | "CreationTimeStampNp" -> Some (World.getScreenCreationTimeStampNp screen world :> obj, typeof<int64>)
                | "Imperative" -> Some (World.getScreenImperative screen world :> obj, typeof<bool>)
                | "ScriptOpt" -> Some (World.getScreenScriptOpt screen world :> obj, typeof<AssetTag option>)
                | "Script" -> Some (World.getScreenScript screen world :> obj, typeof<Scripting.Expr list>)
                | "ScriptFrameNp" -> Some (World.getScreenScriptFrameNp screen world :> obj, typeof<Scripting.ProceduralFrame list>)
                | "OnRegister" -> Some (World.getScreenOnRegister screen world :> obj, typeof<Scripting.Expr>)
                | "OnUnregister" -> Some (World.getScreenOnUnregister screen world :> obj, typeof<Scripting.Expr>)
                | "OnUpdate" -> Some (World.getScreenOnUpdate screen world :> obj, typeof<Scripting.Expr>)
                | "OnPostUpdate" -> Some (World.getScreenOnPostUpdate screen world :> obj, typeof<Scripting.Expr>)
                | "OnActualize" -> Some (World.getScreenOnActualize screen world :> obj, typeof<Scripting.Expr>)
                | "EntityTreeNp" -> Some (World.getScreenEntityTreeNp screen world :> obj, typeof<Entity SpatialTree MutantCache>)
                | "TransitionStateNp" -> Some (World.getScreenTransitionStateNp screen world :> obj, typeof<TransitionState>)
                | "TransitionTicksNp" -> Some (World.getScreenTransitionTicksNp screen world :> obj, typeof<int64>)
                | "Incoming" -> Some (World.getScreenIncoming screen world :> obj, typeof<Transition>)
                | "Outgoing" -> Some (World.getScreenOutgoing screen world :> obj, typeof<Transition>)
                | _ -> ScreenState.tryGetProperty propertyName (World.getScreenState screen world)
            else None

        static member internal getScreenProperty propertyName screen world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (World.getScreenId screen world :> obj, typeof<Guid>)
            | "Name" -> (World.getScreenName screen world :> obj, typeof<Name>)
            | "DispatcherNp" -> (World.getScreenDispatcherNp screen world :> obj, typeof<ScreenDispatcher>)
            | "Specialization" -> (World.getScreenSpecialization screen world :> obj, typeof<string>)
            | "Persistent" -> (World.getScreenPersistent screen world :> obj, typeof<bool>)
            | "CreationTimeStampNp" -> (World.getScreenCreationTimeStampNp screen world :> obj, typeof<int64>)
            | "Imperative" -> (World.getScreenImperative screen world :> obj, typeof<bool>)
            | "ScriptOpt" -> (World.getScreenScriptOpt screen world :> obj, typeof<AssetTag option>)
            | "Script" -> (World.getScreenScript screen world :> obj, typeof<Scripting.Expr list>)
            | "ScriptFrameNp" -> (World.getScreenScriptFrameNp screen world :> obj, typeof<Scripting.ProceduralFrame list>)
            | "OnRegister" -> (World.getScreenOnRegister screen world :> obj, typeof<Scripting.Expr>)
            | "OnUnregister" -> (World.getScreenOnUnregister screen world :> obj, typeof<Scripting.Expr>)
            | "OnUpdate" -> (World.getScreenOnUpdate screen world :> obj, typeof<Scripting.Expr>)
            | "OnPostUpdate" -> (World.getScreenOnPostUpdate screen world :> obj, typeof<Scripting.Expr>)
            | "OnActualize" -> (World.getScreenOnActualize screen world :> obj, typeof<Scripting.Expr>)
            | "EntityTreeNp" -> (World.getScreenEntityTreeNp screen world :> obj, typeof<Entity SpatialTree MutantCache>)
            | "TransitionStateNp" -> (World.getScreenTransitionStateNp screen world :> obj, typeof<TransitionState>)
            | "TransitionTicksNp" -> (World.getScreenTransitionTicksNp screen world :> obj, typeof<int64>)
            | "Incoming" -> (World.getScreenIncoming screen world :> obj, typeof<Transition>)
            | "Outgoing" -> (World.getScreenOutgoing screen world :> obj, typeof<Transition>)
            | _ -> ScreenState.getProperty propertyName (World.getScreenState screen world)

        static member internal trySetScreenProperty propertyName (property : obj * Type) screen world =
            if World.containsScreen screen world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> (false, world)
                | "Name" -> (false, world)
                | "DispatcherNp" -> (false, world)
                | "Specialization" -> (false, world)
                | "Persistent" -> (true, World.setScreenPersistent (property |> fst :?> bool) screen world)
                | "CreationTimeStampNp" -> (false, world)
                | "Imperative" -> (false, world)
                | "ScriptOpt" -> (false, world)
                | "Script" -> (false, world)
                | "ScriptFrameNp" -> (false, world)
                | "OnRegister" -> (false, world)
                | "OnUnregister" -> (false, world)
                | "OnUpdate" -> (false, world)
                | "OnPostUpdate" -> (false, world)
                | "OnActualize" -> (false, world)
                | "EntityTreeNp" -> (false, world)
                | "TransitionStateNp" -> (true, World.setScreenTransitionStateNp (property |> fst :?> TransitionState) screen world)
                | "TransitionTicksNp" -> (true, World.setScreenTransitionTicksNp (property |> fst :?> int64) screen world)
                | "Incoming" -> (true, World.setScreenIncoming (property |> fst :?> Transition) screen world)
                | "Outgoing" -> (true, World.setScreenOutgoing (property |> fst :?> Transition) screen world)
                | _ ->
                    // HACK: needed to mutate a flag to get the success state out of an updateScreenState callback...
                    let mutable success = false
                    let world =
                        World.updateScreenState (fun screenState ->
                            let (successInner, screenState) = ScreenState.trySetProperty propertyName property screenState
                            success <- successInner
                            screenState)
                            propertyName
                            screen
                            world
                    (success, world)
            else (false, world)

        static member internal setScreenProperty propertyName (property : obj * Type) screen world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "Name" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "Persistent" -> World.setScreenPersistent (property |> fst :?> bool) screen world
            | "CreationTimeStampNp" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "Imperative" -> failwith ^ "Cannot change screen " + propertyName + "."
            | "ScriptOpt" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "Script" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "ScriptFrameNp" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "OnRegister" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "OnUnregister" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "OnUpdate" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "OnPostUpdate" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "OnActualize" -> failwith ^ "Cannot change screen " + propertyName + " dynamically."
            | "EntityTreeNp" -> failwith ^ "Cannot change screen entity tree."
            | "TransitionStateNp" -> World.setScreenTransitionStateNp (property |> fst :?> TransitionState) screen world
            | "TransitionTicksNp" -> World.setScreenTransitionTicksNp (property |> fst :?> int64) screen world
            | "Incoming" -> World.setScreenIncoming (property |> fst :?> Transition) screen world
            | "Outgoing" -> World.setScreenOutgoing (property |> fst :?> Transition) screen world
            | _ -> World.updateScreenState (ScreenState.setProperty propertyName property) propertyName screen world

        static member private screenOnRegisterChanged evt world =
            let screen = evt.Subscriber : Screen
            let world = World.registerScreen screen world
            let world = World.unregisterScreen screen world
            (Cascade, world)

        static member private screenScriptOptChanged evt world =
            let screen = evt.Subscriber : Screen
            match World.getScreenScriptOpt screen world with
            | Some script ->
                match World.assetTagToScriptOpt script world with
                | (Some script, world) -> (Cascade, World.setScreenScript script screen world)
                | (None, world) -> (Cascade, world)
            | None -> (Cascade, world)

        static member internal registerScreen screen world =
            let world = World.monitor World.screenOnRegisterChanged (ltoa<ParticipantChangeData<Screen, World>> [!!"Screen"; !!"Change"; !!(Property? OnRegister); !!"Event"] ->- screen) screen world
            let world = World.monitor World.screenScriptOptChanged (ltoa<ParticipantChangeData<Screen, World>> [!!"Screen"; !!"Change"; !!(Property? ScriptOpt); !!"Event"] ->- screen) screen world
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = World.getScreenDispatcherNp screen world
                    let world = dispatcher.Register (screen, world)
                    let eventTrace = EventTrace.record "World" "registerScreen" EventTrace.empty
                    let world = World.publish () (ltoa<unit> [!!"Screen"; !!"Register"; !!"Event"] ->- screen) eventTrace screen world
                    eval (World.getScreenOnUnregister screen world) (World.getScreenScriptFrameNp screen world) screen world |> snd)
                    screen
                    world
            World.choose world

        static member internal unregisterScreen screen world =
            let world =
                World.withEventContext (fun world ->
                    let world = eval (World.getScreenOnRegister screen world) (World.getScreenScriptFrameNp screen world) screen world |> snd
                    let dispatcher = World.getScreenDispatcherNp screen world
                    let eventTrace = EventTrace.record "World" "unregisterScreen" EventTrace.empty
                    let world = World.publish () (ltoa<unit> [!!"Screen"; !!"Unregistering"; !!"Event"] ->- screen) eventTrace screen world
                    dispatcher.Unregister (screen, world))
                    screen
                    world
            World.choose world

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not ^ World.containsScreen screen world
            if isNew || mayReplace then
                let world = World.addScreenState screenState screen world
                if isNew then World.registerScreen screen world else world
            else failwith ^ "Adding a screen that the world already contains at address '" + scstring screen.ScreenAddress + "'."

        static member internal removeScreen3 removeLayers screen world =
            if World.containsScreen screen world then
                let world = World.unregisterScreen screen world
                let world = removeLayers screen world
                World.removeScreenState screen world
            else world

        static member internal writeScreen4 writeLayers screen screenDescriptor world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.DispatcherNp
            let screenDescriptor = { screenDescriptor with ScreenDispatcher = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            writeLayers screen screenDescriptor world

        static member internal readScreen4 readLayers screenDescriptor nameOpt world =
            
            // create the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcher
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find ScreenDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the screen state and populate its properties
            let screenState = ScreenState.make None None dispatcher
            let screenState = Reflection.attachProperties ScreenState.copy screenState.DispatcherNp screenState
            let screenState = Reflection.readPropertiesToTarget ScreenState.copy screenDescriptor.ScreenProperties screenState

            // apply the name if one is provided
            let screenState =
                match nameOpt with
                | Some name -> { screenState with Name = name }
                | None -> screenState

            // add the screen's state to the world
            let screen = screenState.Name |> ntoa |> Screen.proxy
            let screenState =
                if World.containsScreen screen world
                then { screenState with EntityTreeNp = World.getScreenEntityTreeNp screen world }
                else screenState
            let world = World.addScreen true screenState screen world
            
            // read the screen's layers
            let world = readLayers screenDescriptor screen world |> snd
            (screen, world)

        /// View all of the properties of a screen.
        static member viewScreenProperties screen world =
            let state = World.getScreenState screen world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member private layerStateKeyEquality
            (layerAddress : Layer Address, layerStates : UMap<Layer Address, LayerState>)
            (layerAddress2 : Layer Address, layerStates2 : UMap<Layer Address, LayerState>) =
            refEq layerAddress layerAddress2 && refEq layerStates layerStates2

        static member private layerGetFreshKeyAndValue layer world =
            let layerStateOpt = UMap.tryFind layer.LayerAddress world.LayerStates
            ((layer.LayerAddress, world.LayerStates), layerStateOpt)

        static member private layerStateFinder layer world =
            KeyedCache.getValue
                World.layerStateKeyEquality
                (fun () -> World.layerGetFreshKeyAndValue layer world)
                (layer.LayerAddress, world.LayerStates)
                (World.getLayerCachedOpt world)

        static member private layerStateAdder layerState layer world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        match UMap.tryFind layerName layerDirectory with
                        | Some (layerAddress, entityDirectory) ->
                            let layerDirectory = UMap.add layerName (layerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                        | None ->
                            let entityDirectory = UMap.makeEmpty None
                            let layerDirectory = UMap.add layerName (layer.LayerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add layer '" + scstring layer.LayerAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateRemover layer world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        let layerDirectory = UMap.remove layerName layerDirectory
                        UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove layer '" + scstring layer.LayerAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
            let layerStates = UMap.remove layer.LayerAddress world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateSetter layerState layer world =
#if DEBUG
            if not ^ UMap.containsKey layer.LayerAddress world.LayerStates then
                failwith ^ "Cannot set the state of a non-existent layer '" + scstring layer.LayerAddress + "'"
            if not ^ World.qualifyEventContext (atooa layer.LayerAddress) world then
                failwith ^ "Cannot set the state of a layer in an unqualifed event context."
#endif
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with LayerStates = layerStates }

        static member private addLayerState layerState layer world =
            World.layerStateAdder layerState layer world

        static member private removeLayerState layer world =
            World.layerStateRemover layer world

        static member private publishLayerChange (propertyName : string) (layer : Layer) oldWorld world =
            let changeEventAddress = ltoa [!!"Layer"; !!"Change"; !!propertyName; !!"Event"] ->>- layer.LayerAddress
            let eventTrace = EventTrace.record "World" "publishLayerChange" EventTrace.empty
            World.publish6 { Participant = layer; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace layer false world

        static member private getLayerStateOpt layer world =
            World.layerStateFinder layer world

        static member private getLayerState layer world =
            match World.getLayerStateOpt layer world with
            | Some layerState -> layerState
            | None -> failwith ^ "Could not find layer with address '" + scstring layer.LayerAddress + "'."

        static member private setLayerState layerState layer world =
            World.layerStateSetter layerState layer world

        static member private updateLayerStateWithoutEvent updater layer world =
            let layerState = World.getLayerState layer world
            let layerState = updater layerState
            World.setLayerState layerState layer world

        static member private updateLayerState updater propertyName layer world =
            let oldWorld = world
            let world = World.updateLayerStateWithoutEvent updater layer world
            World.publishLayerChange propertyName layer oldWorld world

        /// Check that the world contains a layer.
        static member containsLayer layer world =
            Option.isSome ^ World.getLayerStateOpt layer world

        static member internal getLayerId layer world = (World.getLayerState layer world).Id
        static member internal getLayerName layer world = (World.getLayerState layer world).Name
        static member internal getLayerDispatcherNp layer world = (World.getLayerState layer world).DispatcherNp
        static member internal getLayerSpecialization layer world = (World.getLayerState layer world).Specialization
        static member internal getLayerPersistent layer world = (World.getLayerState layer world).Persistent
        static member internal setLayerPersistent value layer world = World.updateLayerState (fun layerState -> { layerState with Persistent = value }) Property? Persistent layer world
        static member internal getLayerCreationTimeStampNp layer world = (World.getLayerState layer world).CreationTimeStampNp
        static member internal getLayerImperative layer world = Xtension.getImperative (World.getLayerState layer world).Xtension
        static member internal getLayerScriptOpt layer world = (World.getLayerState layer world).ScriptOpt
        static member internal setLayerScriptOpt value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptOpt = value }) Property? ScriptOpt layer world
        static member internal getLayerScript layer world = (World.getLayerState layer world).Script
        static member internal setLayerScript value layer world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateLayerState (fun layerState -> { layerState with Script = value }) Property? Script layer world
            let world = World.setLayerScriptFrameNp scriptFrame layer world
            evalManyWithLogging value scriptFrame layer world |> snd
        static member internal getLayerScriptFrameNp layer world = (World.getLayerState layer world).ScriptFrameNp
        static member internal setLayerScriptFrameNp value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptFrameNp = value }) Property? ScriptFrameNp layer world
        static member internal getLayerOnRegister layer world = (World.getLayerState layer world).OnRegister
        static member internal setLayerOnRegister value layer world = World.updateLayerState (fun layerState -> { layerState with OnRegister = value }) Property? OnRegister layer world
        static member internal getLayerOnUnregister layer world = (World.getLayerState layer world).OnUnregister
        static member internal setLayerOnUnregister value layer world = World.updateLayerState (fun layerState -> { layerState with OnUnregister = value }) Property? OnUnregister layer world
        static member internal getLayerOnUpdate layer world = (World.getLayerState layer world).OnUpdate
        static member internal setLayerOnUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnUpdate = value }) Property? OnUpdate layer world
        static member internal getLayerOnPostUpdate layer world = (World.getLayerState layer world).OnPostUpdate
        static member internal setLayerOnPostUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnPostUpdate = value }) Property? OnPostUpdate layer world
        static member internal getLayerOnActualize layer world = (World.getLayerState layer world).OnActualize
        static member internal setLayerOnActualize value layer world = World.updateLayerState (fun layerState -> { layerState with OnActualize = value }) Property? OnActualize layer world
        static member internal getLayerDepth layer world = (World.getLayerState layer world).Depth
        static member internal setLayerDepth value layer world = World.updateLayerState (fun layerState -> { layerState with Depth = value }) Property? Depth layer world
        static member internal getLayerVisible layer world = (World.getLayerState layer world).Visible
        static member internal setLayerVisible value layer world = World.updateLayerState (fun layerState -> { layerState with Visible = value }) Property? Visible layer world

        static member internal tryGetLayerProperty propertyName layer world =
            if World.containsLayer layer world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> Some (World.getLayerId layer world :> obj, typeof<Guid>)
                | "Name" -> Some (World.getLayerName layer world :> obj, typeof<Name>)
                | "DispatcherNp" -> Some (World.getLayerDispatcherNp layer world :> obj, typeof<LayerDispatcher>)
                | "Specialization" -> Some (World.getLayerSpecialization layer world :> obj, typeof<string>)
                | "Persistent" -> Some (World.getLayerPersistent layer world :> obj, typeof<bool>)
                | "CreationTimeStampNp" -> Some (World.getLayerCreationTimeStampNp layer world :> obj, typeof<int64>)
                | "Imperative" -> Some (World.getLayerImperative layer world :> obj, typeof<bool>)
                | "ScriptOpt" -> Some (World.getLayerScriptOpt layer world :> obj, typeof<AssetTag option>)
                | "Script" -> Some (World.getLayerScript layer world :> obj, typeof<Scripting.Expr list>)
                | "ScriptFrameNp" -> Some (World.getLayerScript layer world :> obj, typeof<Scripting.ProceduralFrame list>)
                | "OnRegister" -> Some (World.getLayerOnRegister layer world :> obj, typeof<Scripting.Expr>)
                | "OnUnregister" -> Some (World.getLayerOnUnregister layer world :> obj, typeof<Scripting.Expr>)
                | "OnUpdate" -> Some (World.getLayerOnUpdate layer world :> obj, typeof<Scripting.Expr>)
                | "OnPostUpdate" -> Some (World.getLayerOnPostUpdate layer world :> obj, typeof<Scripting.Expr>)
                | "OnActualize" -> Some (World.getLayerOnActualize layer world :> obj, typeof<Scripting.Expr>)
                | "Depth" -> Some (World.getLayerDepth layer world :> obj, typeof<single>)
                | "Visible" -> Some (World.getLayerVisible layer world :> obj, typeof<single>)
                | _ -> LayerState.tryGetProperty propertyName (World.getLayerState layer world)
            else None

        static member internal getLayerProperty propertyName layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (World.getLayerId layer world :> obj, typeof<Guid>)
            | "Name" -> (World.getLayerName layer world :> obj, typeof<Name>)
            | "DispatcherNp" -> (World.getLayerDispatcherNp layer world :> obj, typeof<LayerDispatcher>)
            | "Specialization" -> (World.getLayerSpecialization layer world :> obj, typeof<string>)
            | "Persistent" -> (World.getLayerPersistent layer world :> obj, typeof<bool>)
            | "CreationTimeStampNp" -> (World.getLayerCreationTimeStampNp layer world :> obj, typeof<int64>)
            | "Imperative" -> (World.getLayerImperative layer world :> obj, typeof<bool>)
            | "ScriptOpt" -> (World.getLayerScriptOpt layer world :> obj, typeof<AssetTag option>)
            | "Script" -> (World.getLayerScript layer world :> obj, typeof<Scripting.Expr list>)
            | "ScriptFrameNp" -> (World.getLayerScript layer world :> obj, typeof<Scripting.ProceduralFrame list>)
            | "OnRegister" -> (World.getLayerOnRegister layer world :> obj, typeof<Scripting.Expr>)
            | "OnUnregister" -> (World.getLayerOnUnregister layer world :> obj, typeof<Scripting.Expr>)
            | "OnUpdate" -> (World.getLayerOnUpdate layer world :> obj, typeof<Scripting.Expr>)
            | "OnPostUpdate" -> (World.getLayerOnPostUpdate layer world :> obj, typeof<Scripting.Expr>)
            | "OnActualize" -> (World.getLayerOnActualize layer world :> obj, typeof<Scripting.Expr>)
            | "Depth" -> (World.getLayerDepth layer world :> obj, typeof<single>)
            | "Visible" -> (World.getLayerVisible layer world :> obj, typeof<single>)
            | _ -> LayerState.getProperty propertyName (World.getLayerState layer world)

        static member internal trySetLayerProperty propertyName (property : obj * Type) layer world =
            if World.containsLayer layer world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> (false, world)
                | "Name" -> (false, world)
                | "DispatcherNp" -> (false, world)
                | "Specialization" -> (false, world)
                | "Persistent" -> (true, World.setLayerPersistent (property |> fst :?> bool) layer world)
                | "CreationTimeStampNp" -> (false, world)
                | "Imperative" -> (false, world)
                | "ScriptOpt" -> (false, world)
                | "Script" -> (false, world)
                | "ScriptFrameNp" -> (false, world)
                | "OnRegister" -> (false, world)
                | "OnUnregister" -> (false, world)
                | "OnUpdate" -> (false, world)
                | "OnPostUpdate" -> (false, world)
                | "OnActualize" -> (false, world)
                | _ ->
                    // HACK: needed to mutate a flag to get the success state out of an updateLayerState callback...
                    let mutable success = false
                    let world =
                        World.updateLayerState (fun layerState ->
                            let (successInner, layerState) = LayerState.trySetProperty propertyName property layerState
                            success <- successInner
                            layerState)
                            propertyName
                            layer
                            world
                    (success, world)
            else (false, world)

        static member internal setLayerProperty propertyName (property : obj * Type) layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Name" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Persistent" -> World.setLayerPersistent (property |> fst :?> bool) layer world
            | "CreationTimeStampNp" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Imperative" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "ScriptOpt" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "Script" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "ScriptFrameNp" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnRegister" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnUnregister" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnUpdate" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnPostUpdate" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnActualize" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | _ -> World.updateLayerState (LayerState.setProperty propertyName property) propertyName layer world

        static member private layerOnRegisterChanged evt world =
            let layer = evt.Subscriber : Layer
            let world = World.registerLayer layer world
            let world = World.unregisterLayer layer world
            (Cascade, world)

        static member private layerScriptOptChanged evt world =
            let layer = evt.Subscriber : Layer
            match World.getLayerScriptOpt layer world with
            | Some script ->
                match World.assetTagToScriptOpt script world with
                | (Some script, world) -> (Cascade, World.setLayerScript script layer world)
                | (None, world) -> (Cascade, world)
            | None -> (Cascade, world)

        static member internal registerLayer layer world =
            let world = World.monitor World.layerOnRegisterChanged (ltoa<ParticipantChangeData<Layer, World>> [!!"Layer"; !!"Change"; !!(Property? OnRegister); !!"Event"] ->- layer) layer world
            let world = World.monitor World.layerScriptOptChanged (ltoa<ParticipantChangeData<Layer, World>> [!!"Layer"; !!"Change"; !!(Property? ScriptOpt); !!"Event"] ->- layer) layer world
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = World.getLayerDispatcherNp layer world
                    let world = dispatcher.Register (layer, world)
                    let eventTrace = EventTrace.record "World" "registerLayer" EventTrace.empty
                    let world = World.publish () (ltoa<unit> [!!"Layer"; !!"Register"; !!"Event"] ->- layer) eventTrace layer world
                    eval (World.getLayerOnUnregister layer world) (World.getLayerScriptFrameNp layer world) layer world |> snd)
                    layer
                    world
            World.choose world

        static member internal unregisterLayer layer world =
            let world =
                World.withEventContext (fun world ->
                    let world = eval (World.getLayerOnRegister layer world) (World.getLayerScriptFrameNp layer world) layer world |> snd
                    let dispatcher = World.getLayerDispatcherNp layer world
                    let eventTrace = EventTrace.record "World" "unregisterLayer" EventTrace.empty
                    let world = World.publish () (ltoa<unit> [!!"Layer"; !!"Unregistering"; !!"Event"] ->- layer) eventTrace layer world
                    dispatcher.Unregister (layer, world))
                    layer
                    world
            World.choose world

        static member private addLayer mayReplace layerState layer world =
            let isNew = not ^ World.containsLayer layer world
            if isNew || mayReplace then
                let world = World.addLayerState layerState layer world
                if isNew then World.registerLayer layer world else world
            else failwith ^ "Adding a layer that the world already contains at address '" + scstring layer.LayerAddress + "'."

        static member internal removeLayer3 removeEntities layer world =
            if World.containsLayer layer world then
                let world = World.unregisterLayer layer world
                let world = removeEntities layer world
                World.removeLayerState layer world
            else world

        /// Create a layer and add it to the world.
        static member createLayer5 dispatcherName specializationOpt nameOpt screen world =
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ^ "Could not find a LayerDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
            let layerState = LayerState.make specializationOpt nameOpt dispatcher
            let layerState = Reflection.attachProperties LayerState.copy dispatcher layerState
            let layer = screen.ScreenAddress -<<- ntoa<Layer> layerState.Name |> Layer.proxy
            let world = World.addLayer false layerState layer world
            (layer, world)

        /// Create a layer and add it to the world.
        static member createLayer<'d when 'd :> LayerDispatcher> specializationOpt nameOpt screen world =
            World.createLayer5 typeof<'d>.Name specializationOpt nameOpt screen world

        static member internal writeLayer4 writeEntities layer layerDescriptor world =
            let layerState = World.getLayerState layer world
            let layerDispatcherName = getTypeName layerState.DispatcherNp
            let layerDescriptor = { layerDescriptor with LayerDispatcher = layerDispatcherName }
            let getLayerProperties = Reflection.writePropertiesFromTarget tautology3 layerDescriptor.LayerProperties layerState
            let layerDescriptor = { layerDescriptor with LayerProperties = getLayerProperties }
            writeEntities layer layerDescriptor world

        static member internal readLayer5 readEntities layerDescriptor nameOpt screen world =

            // create the dispatcher
            let dispatcherName = layerDescriptor.LayerDispatcher
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find LayerDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<LayerDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the layer state and populate its properties
            let layerState = LayerState.make None None dispatcher
            let layerState = Reflection.attachProperties LayerState.copy layerState.DispatcherNp layerState
            let layerState = Reflection.readPropertiesToTarget LayerState.copy layerDescriptor.LayerProperties layerState

            // apply the name if one is provided
            let layerState =
                match nameOpt with
                | Some name -> { layerState with Name = name }
                | None -> layerState

            // add the layer's state to the world
            let layer = screen.ScreenAddress -<<- ntoa<Layer> layerState.Name |> Layer.proxy
            let world = World.addLayer true layerState layer world

            // read the layer's entities
            let world = readEntities layerDescriptor layer world |> snd
            (layer, world)

        /// View all of the properties of a layer.
        static member viewLayerProperties layer world =
            let state = World.getLayerState layer world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member private entityStateKeyEquality
            (entityAddress : Entity Address, entityStates : UMap<Entity Address, EntityState>)
            (entityAddress2 : Entity Address, entityStates2 : UMap<Entity Address, EntityState>) =
            refEq entityAddress entityAddress2 && refEq entityStates entityStates2

        static member private entityGetFreshKeyAndValue entity world =
            let entityStateOpt = UMap.tryFind entity.EntityAddress world.EntityStates
            ((entity.EntityAddress, world.EntityStates), entityStateOpt)

        static member private entityStateFinder entity world =
            let entityStateOpt = entity.EntityStateOpt
            if isNull (entityStateOpt :> obj) then
                let entityStateOpt =
                    KeyedCache.getValue
                        World.entityStateKeyEquality
                        (fun () -> World.entityGetFreshKeyAndValue entity world)
                        (entity.EntityAddress, world.EntityStates)
                        (World.getEntityCachedOpt world)
                match entityStateOpt with
                | Some entityState ->
                    if  entityState.CachableNp &&
                        Xtension.getImperative entityState.Xtension then
                        entity.EntityStateOpt <-
                            match entityStateOpt with
                            | None -> Unchecked.defaultof<EntityState>
                            | Some entityState -> entityState
                    entityState
                | None -> Unchecked.defaultof<EntityState>
            else entityStateOpt

        static member private entityStateAdder entityState entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; layerName; entityName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        match UMap.tryFind layerName layerDirectory with
                        | Some (layerAddress, entityDirectory) ->
                            let entityDirectory = UMap.add entityName entity.EntityAddress entityDirectory
                            let layerDirectory = UMap.add layerName (layerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent layer."
                    | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = UMap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateRemover entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; layerName; entityName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        match UMap.tryFind layerName layerDirectory with
                        | Some (layerAddress, entityDirectory) ->
                            let entityDirectory = UMap.remove entityName entityDirectory
                            let layerDirectory = UMap.add layerName (layerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent layer."
                    | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = UMap.remove entity.EntityAddress world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateSetter entityState entity world =
#if DEBUG
            if not ^ UMap.containsKey entity.EntityAddress world.EntityStates then
                failwith ^ "Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'"
            if not ^ World.qualifyEventContext (atooa entity.EntityAddress) world then
                failwith ^ "Cannot set the state of an entity in an unqualifed event context."
#endif
            let entityStates = UMap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private addEntityState entityState entity world =
            World.entityStateAdder entityState entity world

        static member private removeEntityState entity world =
            World.entityStateRemover entity world

        static member private publishEntityChange propertyName entity oldWorld world =
            let changeEventAddress = ltoa [!!"Entity"; !!"Change"; !!propertyName; !!"Event"] ->>- entity.EntityAddress
            let eventTrace = EventTrace.record "World" "publishEntityChange" EventTrace.empty
            World.publish6 { Participant = entity; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace entity false world

        static member private getEntityStateOpt entity world =
            let entityStateOpt = World.entityStateFinder entity world
            if isNull (entityStateOpt :> obj) then None
            else Some entityStateOpt

        static member private getEntityState entity world =
#if DEBUG
            match World.getEntityStateOpt entity world with
            | Some entityState -> entityState
            | None -> failwith ^ "Could not find entity with address '" + scstring entity.EntityAddress + "'."
#else
            World.entityStateFinder entity world
#endif

        static member internal getEntityXtensionProperties entity world =
            let entityState = World.getEntityState entity world
            entityState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private setEntityState entityState entity world =
            World.entityStateSetter entityState entity world

        static member private updateEntityStateInternal updater mutability entityState entity world =
            let entityState = updater entityState
            if mutability && Xtension.getImperative entityState.Xtension then (entityState, world)
            else (entityState, World.setEntityState entityState entity world)

        static member private updateEntityStateWithoutEvent updater mutability entity world =
            let entityState = World.getEntityState entity world
            let (_, world) = World.updateEntityStateInternal updater mutability entityState entity world
            world

        static member private updateEntityState updater mutability (propertyName : string) entity world =
            let oldWorld = world
            let entityState = World.getEntityState entity world
            let (entityState, world) = World.updateEntityStateInternal updater mutability entityState entity world
            if entityState.PublishChanges || propertyName.EndsWith "Pa" then World.publishEntityChange propertyName entity oldWorld world
            else world

        static member private updateEntityStatePlus updater mutability (propertyName : string) entity world =
            let oldWorld = world
            let entityState = World.getEntityState entity world
            let (entityState, world) = World.updateEntityStateInternal updater mutability entityState entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            if entityState.PublishChanges || propertyName.EndsWith "Pa" then World.publishEntityChange propertyName entity oldWorld world
            else world

        static member private publishEntityChanges entity oldWorld world =
            let entityState = World.getEntityState entity world
            let properties = World.getProperties entityState
            if entityState.PublishChanges
            then List.fold (fun world (propertyName, _) -> World.publishEntityChange propertyName entity oldWorld world) world properties
            else world

        /// Check that the world contains an entity.
        static member containsEntity entity world =
            Option.isSome ^ World.getEntityStateOpt entity world

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

        // NOTE: Wouldn't macros be nice?
        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntityName entity world = (World.getEntityState entity world).Name
        static member internal getEntityDispatcherNp entity world = (World.getEntityState entity world).DispatcherNp
        static member internal getEntitySpecialization entity world = (World.getEntityState entity world).Specialization
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Persistent <- value; entityState else { entityState with Persistent = value }) false Property? Persistent entity world
        static member internal getEntityCreationTimeStampNp entity world = (World.getEntityState entity world).CreationTimeStampNp
        static member internal getEntityImperative entity world = Xtension.getImperative (World.getEntityState entity world).Xtension
        static member internal getEntityCachableNp entity world = (World.getEntityState entity world).CachableNp
        static member internal getEntityOverlayNameOpt entity world = (World.getEntityState entity world).OverlayNameOpt
        static member internal setEntityOverlayNameOpt value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.OverlayNameOpt <- value; entityState else { entityState with OverlayNameOpt = value }) false Property? OverlayNameOpt entity world
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal setEntityPosition value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Position <- value; entityState else { entityState with EntityState.Position = value }) true Property? Position entity world
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal setEntitySize value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Size <- value; entityState else { entityState with EntityState.Size = value }) true Property? Size entity world
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal setEntityRotation value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Rotation <- value; entityState else { entityState with EntityState.Rotation = value }) true Property? Rotation entity world
        static member internal getEntityDepth entity world = (World.getEntityState entity world).Depth
        static member internal setEntityDepth value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Depth <- value; entityState else { entityState with EntityState.Depth = value }) true Property? Depth entity world
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Overflow
        static member internal setEntityOverflow value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Overflow <- value; entityState else { entityState with EntityState.Overflow = value }) true Property? Overflow entity world
        static member internal getEntityViewType entity world = (World.getEntityState entity world).ViewType
        static member internal setEntityViewType value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.ViewType <- value; entityState else { entityState with EntityState.ViewType = value }) true Property? ViewType entity world
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal setEntityVisible value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Visible <- value; entityState else { entityState with EntityState.Visible = value }) true Property? Visible entity world
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal setEntityEnabled value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Enabled <- value; entityState else { entityState with EntityState.Enabled = value }) true Property? Enabled entity world
        static member internal getEntityOmnipresent entity world = (World.getEntityState entity world).Omnipresent
        static member internal setEntityOmnipresent value entity world = World.updateEntityStatePlus (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.Omnipresent <- value; entityState else { entityState with EntityState.Omnipresent = value }) true Property? Omnipresent entity world
        static member internal getEntityPublishChanges entity world = (World.getEntityState entity world).PublishChanges
        static member internal setEntityPublishChanges value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.PublishChanges <- value; entityState else { entityState with PublishChanges = value }) false Property? PublishChanges entity world
        static member internal getEntityPublishUpdatesNp entity world = (World.getEntityState entity world).PublishUpdatesNp
        static member internal setEntityPublishUpdatesNp value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.PublishUpdatesNp <- value; entityState else { entityState with PublishUpdatesNp = value }) false Property? PublishUpdatesNp entity world
        static member internal getEntityPublishPostUpdatesNp entity world = (World.getEntityState entity world).PublishPostUpdatesNp
        static member internal setEntityPublishPostUpdatesNp value entity world = World.updateEntityState (fun entityState -> if Xtension.getImperative entityState.Xtension then entityState.PublishPostUpdatesNp <- value; entityState else { entityState with PublishPostUpdatesNp = value }) false Property? PublishPostUpdatesNp entity world
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityFacetsNp entity world = (World.getEntityState entity world).FacetsNp

        static member internal getEntityTransform entity world =
            EntityState.getTransform (World.getEntityState entity world)
        
        static member internal setEntityTransform value entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent (EntityState.setTransform value) true entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            if World.getEntityPublishChanges entity world then
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

        static member private tryRemoveFacet facetName entityState entityOpt world =
            match List.tryFind (fun facet -> getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match entityOpt with
                    | Some entity ->
                        let world = World.withEventContext (fun world -> facet.Unregister (entity, world)) entity world
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.copy propertyNames entityState
                let entityState =
                    let facetNames = Set.remove facetName entityState.FacetNames
                    let facets = List.remove ((=) facet) entityState.FacetsNp
                    if Xtension.getImperative entityState.Xtension then
                        entityState.FacetNames <- facetNames
                        entityState.FacetsNp <- facets
                        entityState
                    else { entityState with FacetNames = facetNames; FacetsNp = facets }
                match entityOpt with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) entityOpt world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let entityState =
                        let facetNames = Set.add facetName entityState.FacetNames
                        let facets = facet :: entityState.FacetsNp
                        if Xtension.getImperative entityState.Xtension then
                            entityState.FacetNames <- facetNames
                            entityState.FacetsNp <- facets
                            entityState
                        else { entityState with FacetNames = facetNames; FacetsNp = facets }
                    let entityState = Reflection.attachProperties EntityState.copy facet entityState
                    match entityOpt with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityState entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.withEventContext (fun world -> facet.Register (entity, world)) entity world
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState entityOpt world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState entityOpt world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member internal trySetFacetNames facetNames entityState entityOpt world =
            let intrinsicFacetNames = World.getIntrinsicFacetNames entityState
            let extrinsicFacetNames = Set.fold (flip Set.remove) facetNames intrinsicFacetNames
            let facetNamesToRemove = Set.difference entityState.FacetNames extrinsicFacetNames
            let facetNamesToAdd = Set.difference extrinsicFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState entityOpt world =
            let facetNamesToRemove = Set.difference oldFacetNames entityState.FacetNames
            let facetNamesToAdd = Set.difference entityState.FacetNames oldFacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.copy entityDispatchers facets entityState.DispatcherNp entityState

        static member internal applyEntityOverlay oldOverlayer overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OverlayNameOpt with
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

        static member internal tryGetEntityProperty propertyName entity world =
            if World.containsEntity entity world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> Some (World.getEntityId entity world :> obj, typeof<Guid>)
                | "Name" -> Some (World.getEntityName entity world :> obj, typeof<Name>)
                | "DispatcherNp" -> Some (World.getEntityDispatcherNp entity world :> obj, typeof<EntityDispatcher>)
                | "Persistent" -> Some (World.getEntityPersistent entity world :> obj, typeof<bool>)
                | "Specialization" -> Some (World.getEntitySpecialization entity world :> obj, typeof<string>)
                | "CreationTimeStampNp" -> Some (World.getEntityCreationTimeStampNp entity world :> obj, typeof<int64>)
                | "Imperative" -> Some (World.getEntityImperative entity world :> obj, typeof<bool>)
                | "CachableNp" -> Some (World.getEntityCachableNp entity world :> obj, typeof<bool>)
                | "OverlayNameOpt" -> Some (World.getEntityOverlayNameOpt entity world :> obj, typeof<string option>)
                | "Position" -> Some (World.getEntityPosition entity world :> obj, typeof<Vector2>)
                | "Size" -> Some (World.getEntitySize entity world :> obj, typeof<Vector2>)
                | "Rotation" -> Some (World.getEntityRotation entity world :> obj, typeof<single>)
                | "Depth" -> Some (World.getEntityDepth entity world :> obj, typeof<single>)
                | "Overflow" -> Some (World.getEntityOverflow entity world :> obj, typeof<Vector2>)
                | "ViewType" -> Some (World.getEntityViewType entity world :> obj, typeof<ViewType>)
                | "Visible" -> Some (World.getEntityVisible entity world :> obj, typeof<bool>)
                | "Enabled" -> Some (World.getEntityEnabled entity world :> obj, typeof<bool>)
                | "Omnipresent" -> Some (World.getEntityOmnipresent entity world :> obj, typeof<bool>)
                | "PublishChanges" -> Some (World.getEntityPublishChanges entity world :> obj, typeof<bool>)
                | "PublishUpdatesNp" -> Some (World.getEntityPublishUpdatesNp entity world :> obj, typeof<bool>)
                | "PublishPostUpdatesNp" -> Some (World.getEntityPublishPostUpdatesNp entity world :> obj, typeof<bool>)
                | "FacetNames" -> Some (World.getEntityFacetNames entity world :> obj, typeof<string Set>)
                | "FacetsNp" -> Some (World.getEntityFacetsNp entity world :> obj, typeof<Facet list>)
                | _ -> EntityState.tryGetProperty propertyName (World.getEntityState entity world)
            else None

        static member internal getEntityProperty propertyName entity world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (World.getEntityId entity world :> obj, typeof<Guid>)
            | "Name" -> (World.getEntityName entity world :> obj, typeof<Name>)
            | "DispatcherNp" -> (World.getEntityDispatcherNp entity world :> obj, typeof<EntityDispatcher>)
            | "Persistent" -> (World.getEntityPersistent entity world :> obj, typeof<bool>)
            | "Specialization" -> (World.getEntitySpecialization entity world :> obj, typeof<string>)
            | "CreationTimeStampNp" -> (World.getEntityCreationTimeStampNp entity world :> obj, typeof<int64>)
            | "Imperative" -> (World.getEntityImperative entity world :> obj, typeof<bool>)
            | "CachableNp" -> (World.getEntityCachableNp entity world :> obj, typeof<bool>)
            | "OverlayNameOpt" -> (World.getEntityOverlayNameOpt entity world :> obj, typeof<string option>)
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

        static member internal trySetEntityProperty propertyName (property : obj * Type) entity world =
            if World.containsEntity entity world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> (false, world)
                | "Name" -> (false, world)
                | "DispatcherNp" -> (false, world)
                | "Specialization" -> (false, world)
                | "Persistent" -> (true, World.setEntityPersistent (property |> fst :?> bool) entity world)
                | "CreationTimeStampNp" -> (false, world)
                | "Imperative" -> (false, world)
                | "CachableNp" -> (false, world)
                | "Position" -> (true, World.setEntityPosition (property |> fst :?> Vector2) entity world)
                | "Size" -> (true, World.setEntitySize (property |> fst :?> Vector2) entity world)
                | "Rotation" -> (true, World.setEntityRotation (property |> fst :?> single) entity world)
                | "Depth" -> (true, World.setEntityDepth (property |> fst :?> single) entity world)
                | "Overflow" -> (true, World.setEntityOverflow (property |> fst :?> Vector2) entity world)
                | "ViewType" -> (true, World.setEntityViewType (property |> fst :?> ViewType) entity world)
                | "Visible" -> (true, World.setEntityVisible (property |> fst :?> bool) entity world)
                | "Enabled" -> (true, World.setEntityEnabled (property |> fst :?> bool) entity world)
                | "Omnipresent" -> (true, World.setEntityOmnipresent (property |> fst :?> bool) entity world)
                | "PublishChanges" -> (true, World.setEntityPublishChanges (property |> fst :?> bool) entity world)
                | "PublishUpdatesNp" -> (false, world)
                | "PublishPostUpdatesNp" -> (false, world)
                | "FacetNames" -> (false, world)
                | "FacetsNp" -> (false, world)
                | _ ->
                    // HACK: needed to mutate a flag to get the success state out of an updateEntityState callback...
                    let mutable success = false
                    let world =
                        World.updateEntityState (fun entityState ->
                            let (successInner, entityState) = EntityState.trySetProperty propertyName property entityState
                            success <- successInner
                            entityState)
                            true
                            propertyName
                            entity
                            world
                    (success, world)
            else (false, world)

        static member internal setEntityProperty propertyName (property : obj * Type) entity world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "Name" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "Persistent" -> World.setEntityPersistent (property |> fst :?> bool) entity world
            | "CreationTimeStampNp" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "Imperative" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "CachableNp" -> failwith ^ "Cannot change entity " + propertyName + "."
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
            | "PublishUpdatesNp" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "PublishPostUpdatesNp" -> failwith ^ "Cannot change entity " + propertyName + "."
            | "FacetNames" -> failwith ^ "Cannot change entity " + propertyName + " dynamically."
            | "FacetsNp" -> failwith ^ "Cannot change entity " + propertyName + ". dynamically"
            | _ -> World.updateEntityState (EntityState.setProperty propertyName property) true propertyName entity world

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
            { SortDepth = entityState.Depth; SortPositionY = entityState.Position.Y; SortTarget = entity }

        static member private updateEntityPublishEventFlag setFlag entity eventAddress world =
            let publishUpdates =
                match UMap.tryFind eventAddress ^ World.getSubscriptions world with
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
                            SpatialTree.addElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let world = World.setScreenEntityTreeNpNoEvent entityTree screen world

                // register entity if needed
                if isNew then
                    World.withEventContext (fun world ->
                        let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                        let facets = World.getEntityFacetsNp entity world
                        let world = dispatcher.Register (entity, world)
                        let world = List.fold (fun world (facet : Facet) -> facet.Register (entity, world)) world facets
                        let world = World.updateEntityPublishFlags entity world
                        let eventTrace = EventTrace.record "World" "addEntity" EventTrace.empty
                        World.publish () (ltoa<unit> [!!"Entity"; !!"Register"; !!"Event"] ->- entity) eventTrace entity world)
                        entity
                        world
                else world

            // handle failure
            else failwith ^ "Adding an entity that the world already contains at address '" + scstring entity.EntityAddress + "'."

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =
            World.removeEntity entity world

        /// Create an entity and add it to the world.
        static member createEntity5 dispatcherName specializationOpt nameOpt layer world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ^ "Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"

            // try to compute the routed overlay name
            let classification = Classification.make dispatcherName ^ Option.getOrDefault Constants.Engine.VanillaSpecialization specializationOpt
            let overlayNameOpt = OverlayRouter.findOverlayNameOpt classification overlayRouter

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make specializationOpt nameOpt overlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let entityState =
                match overlayNameOpt with
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
                match entityState.OverlayNameOpt with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something
                    if dispatcherName <> overlayName then
                        let facetNames = World.getFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // add entity's state to world
            let entity = layer.LayerAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity false entityState entity world
            (entity, world)

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> specializationOpt nameOpt layer world =
            World.createEntity5 typeof<'d>.Name specializationOpt nameOpt layer world

        static member private removeEntity entity world =
            
            // ensure entity exists in the world
            if World.containsEntity entity world then
                
                // publish event and unregister entity
                let world =
                    World.withEventContext (fun world ->
                        let eventTrace = EventTrace.record "World" "removeEntity" EventTrace.empty
                        let world = World.publish () (ltoa<unit> [!!"Entity"; !!"Unregistering"; !!"Event"] ->- entity) eventTrace entity world
                        let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                        let facets = World.getEntityFacetsNp entity world
                        let world = dispatcher.Unregister (entity, world)
                        List.fold (fun world (facet : Facet) -> facet.Unregister (entity, world)) world facets)
                        entity
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
                            SpatialTree.removeElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // remove cached entity event addresses
                EventWorld.cleanEventAddressCache entity.EntityAddress

                // remove the entity from the world
                World.removeEntityState entity world

            // pass
            else world

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor nameOpt layer world =

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
                        | None -> failwith ^ "Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
                    (dispatcherName, dispatcher)

            // try to compute the routed overlay name
            let classification = Classification.makeVanilla dispatcherName
            let overlayNameOpt = OverlayRouter.findOverlayNameOpt classification overlayRouter

            // make the bare entity state with name as id
            let entityState = EntityState.make None None overlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let entityState = Reflection.tryReadOverlayNameOptToTarget EntityState.copy entityDescriptor.EntityProperties entityState
            let entityState =
                match (overlayNameOpt, entityState.OverlayNameOpt) with
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
                match entityState.OverlayNameOpt with
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
                match nameOpt with
                | Some name -> { entityState with Name = name }
                | None -> entityState

            // add entity state to the world
            let entity = layer.LayerAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity true entityState entity world
            (entity, world)

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.DispatcherNp
            let entityDescriptor = { entityDescriptor with EntityDispatcher = entityDispatcherName }
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OverlayNameOpt" && propertyType = typeof<string option> then
                    let overlayRouter = World.getOverlayRouter world
                    let classification = Classification.make entityDispatcherName entityState.Specialization
                    let defaultOverlayNameOpt = OverlayRouter.findOverlayNameOpt classification overlayRouter
                    defaultOverlayNameOpt <> (propertyValue :?> string option)
                else
                    let overlayer = World.getOverlayer world
                    let facetNames = World.getFacetNamesReflectively entityState
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entityState overlayer
            let getEntityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            { entityDescriptor with EntityProperties = getEntityProperties }

        /// Reassign an entity's identity and / or layer. Note that since this destroys the reassigned entity
        /// immediately, you should not call this inside an event handler that involves the reassigned entity itself.
        static member reassignEntityImmediate entity nameOpt layer world =
            let entityState = World.getEntityState entity world
            let world = World.removeEntity entity world
            let (id, name) = Reflection.deriveIdAndName nameOpt
            let entityState = { entityState with Id = id; Name = name }
            let transmutedEntity = layer.LayerAddress -<<- ntoa<Entity> name |> Entity.proxy
            let world = World.addEntity false entityState transmutedEntity world
            (transmutedEntity, world)

        /// Reassign an entity's identity and / or layer.
        static member reassignEntity entity nameOpt layer world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.reassignEntityImmediate entity nameOpt layer world |> snd }}
            World.addTasklet tasklet world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOverlayNameOpt overlayNameOpt entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOverlayNameOpt = oldEntityState.OverlayNameOpt
            let entityState =
                if Xtension.getImperative oldEntityState.Xtension
                then oldEntityState.OverlayNameOpt <- overlayNameOpt; oldEntityState
                else { oldEntityState with OverlayNameOpt = overlayNameOpt }
            match (oldOverlayNameOpt, overlayNameOpt) with
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
                let world =
                    if World.getEntityPublishChanges entity world
                    then World.publishEntityChanges entity oldWorld world
                    else world
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
                let world =
                    if World.getEntityPublishChanges entity world
                    then World.publishEntityChanges entity oldWorld world
                    else world
                Right world
            | Left error -> Left error

        /// View all of the properties of an entity.
        static member viewEntityProperties entity world =
            let state = World.getEntityState entity world
            let properties = World.getProperties state
            Array.ofList properties

    type World with

        static member containsSimulant (simulant : Simulant) (world : World) =
            (world :> EventWorld<Game, World>).ContainsParticipant simulant

        static member tryProxySimulant address =
            match Address.getNames address with
            | [] -> Some (Game.proxy Address.empty :> Simulant)
            | [_] -> Some (Screen.proxy (Address.changeType<Simulant, Screen> address) :> Simulant)
            | [_; _] -> Some (Layer.proxy (Address.changeType<Simulant, Layer> address) :> Simulant)
            | [_; _; _] -> Some (Entity.proxy (Address.changeType<Simulant, Entity> address) :> Simulant)
            | _ -> None

        static member tryGetSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.tryGetGameProperty name world
            | :? Screen as screen -> World.tryGetScreenProperty name screen world
            | :? Layer as layer -> World.tryGetLayerProperty name layer world
            | :? Entity as entity -> World.tryGetEntityProperty name entity world
            | _ -> None

        static member getSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.getGameProperty name world
            | :? Screen as screen -> World.getScreenProperty name screen world
            | :? Layer as layer -> World.getLayerProperty name layer world
            | :? Entity as entity -> World.getEntityProperty name entity world
            | _ -> failwithumf ()

        static member trySetSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name property entity world
            | _ -> (false, world)

        static member setSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name property entity world
            | _ -> failwithumf ()

    type World with

        static member internal updateEntityInEntityTreeImpl entity oldWorld world =
            // OPTIMIZATION: attempts to avoid constructing a screen address on each call to decrease address hashing
            // OPTIMIZATION: assumes a valid entity address with List.head on its names
            let screen =
                match (World.getGameState world).SelectedScreenOpt with
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
                        SpatialTree.updateElement
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
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap layer world =
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
                let entity = layer.LayerAddress -<<- ntoa<Entity> name |> Entity.proxy
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

    type World with

        static member getScriptEnv world =
            world.ScriptEnv

        static member getScriptEnvBy by world =
            by world.ScriptEnv

        static member private setScriptEnv env world =
            World.choose { world with ScriptEnv = env }

        static member updateScriptEnv updater world =
            let env = World.getScriptEnv world
            let env = updater env
            World.setScriptEnv env world

        static member tryUpdateScriptEnv tryUpdater world =
            let env = World.getScriptEnv world
            match tryUpdater env with
            | Some env -> Some ^ World.setScriptEnv env world
            | None -> None

        /// Get the context of the script system.
        static member getScriptContext (world : World) =
            world.ScriptContext

        /// Get the context of the script system.
        static member setScriptContext context (world : World) =
            { world with ScriptContext = context }

        /// Evaluate a script expression.
        static member eval expr localFrame simulant world =
            eval expr localFrame simulant world

        /// Evaluate a script expression, with logging on violation result.
        static member evalWithLogging expr localFrame simulant world =
            evalWithLogging expr localFrame simulant world

        /// Evaluate a series of script expressions.
        static member evalMany exprs localFrame simulant world =
            evalMany exprs localFrame simulant world

        /// Evaluate a series of script expressions, with logging on violation results.
        static member evalManyWithLogging exprs localFrame simulant world =
            evalManyWithLogging exprs localFrame simulant world

    type World with

        /// Make the world.
        static member internal make eventSystem dispatchers subsystems scriptEnv ambientState gameSpecializationOpt activeGameDispatcher =
            let gameState = GameState.make gameSpecializationOpt activeGameDispatcher
            let screenStates = UMap.makeEmpty None
            let layerStates = UMap.makeEmpty None
            let entityStates = UMap.makeEmpty None
            let world =
                { EventSystem = eventSystem
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  ScriptEnv = scriptEnv
                  ScriptContext = Game.proxy Address.empty
                  ScreenCachedOpt = KeyedCache.make (Address.empty<Screen>, screenStates) None
                  LayerCachedOpt = KeyedCache.make (Address.empty<Layer>, layerStates) None
                  EntityCachedOpt = KeyedCache.make (Address.empty<Entity>, entityStates) None
                  ScreenDirectory = UMap.makeEmpty None
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = screenStates
                  LayerStates = layerStates
                  EntityStates = entityStates }
            World.choose world