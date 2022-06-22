// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.Linq
open System.Reflection
open FSharpx.Collections
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    /// The game. Always exists.
    let Game = Game ()
    
    [<RequireQualifiedAccess>]
    module Default =

        /// The default screen - may or may not exist.
        let Screen = Screen "Screen"

        /// The default group - may or may not exist.
        let Group = Screen / "Group"

        /// The default entity - may or may not exist.
        let Entity = Group / "Entity"

[<AutoOpen>]
module WorldModuleOperators =

    /// Derive a screen from a name.
    let ntos (screenName : string) =
        Screen screenName

    /// Attempt to resolve a relationship from a simulant.
    let tryResolve<'t when 't :> Simulant> (simulant : Simulant) (relation : 't Relation) : 't option =
        let simulant2 = Relation.resolve<Simulant, 't> simulant.SimulantAddress relation
        if simulant2.Names.Length >= 3 && typeof<'t> = typeof<Entity> then Some (Entity (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 2 && typeof<'t> = typeof<Group> then Some (Group (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 1 && typeof<'t> = typeof<Screen> then Some (Screen (simulant2.Names.[0]) :> Simulant :?> 't)
        elif simulant2.Names.Length = 0 && typeof<'t> = typeof<Game> then Some (Simulants.Game :> Simulant :?> 't)
        else None

    /// Relate the second simulant to the first.
    let relate<'t when 't :> Simulant> (simulant : Simulant) (simulant2 : 't) : 't Relation =
        Relation.relate<Simulant, 't> simulant.SimulantAddress (atoa simulant2.SimulantAddress)

[<AutoOpen; ModuleBinding>]
module WorldModule =

    /// Declarative lens comparable.
    /// TODO: P1: remove the fake IEquatable and IComparable implementations after upgrading Prime.
    type [<CustomEquality; CustomComparison>] internal LensComparable<'k when 'k : equality> =
        { LensHash : int
          LensKey : 'k
          LensItem : Lens<obj, World> }
        override this.Equals _ = failwithnie ()
        override this.GetHashCode () = failwithnie ()
        interface IEquatable<'k LensComparable> with member this.Equals _ = failwithnie ()
        interface IComparable<'k LensComparable> with member this.CompareTo _ = failwithnie ()

    /// Declarative lens comparer.
    type internal LensComparer<'k when 'k : equality> () =
        interface IEqualityComparer<'k LensComparable> with
            member this.Equals (left, right) = left.LensHash = right.LensHash && left.LensKey.Equals right.LensKey
            member this.GetHashCode value = value.LensHash

    /// F# reach-around for evaluating a script expression.
    let mutable internal eval : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalMany : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating a script expression.
    let mutable internal evalWithLogging : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalManyWithLogging : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is selected.
    let mutable internal isSelected : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is ignoring bindings.
    let mutable internal ignorePropertyBindings : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for getting a screen's Ecs.
    let mutable internal getScreenEcs : Screen -> World -> Ecs.Ecs =
        Unchecked.defaultof<_>

    /// F# reach-around for sorting subscriptions by elevation.
    let mutable internal sortSubscriptionsByElevation : (Guid * SubscriptionEntry) seq -> obj -> (Guid * SubscriptionEntry) seq =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal evictScreenElements : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal admitScreenElements : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal registerScreenPhysics : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal unregisterScreenPhysics : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for adding script unsubscriptions to simulants.
    let mutable internal addSimulantScriptUnsubscription : Unsubscription -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unsubscribing script subscriptions of simulants.
    let mutable internal unsubscribeSimulantScripts : Simulant -> World -> World =
        Unchecked.defaultof<_>
        
    /// F# reach-around for binding properties.
    /// HACK: bind5 allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fake lenses do not function.
    let mutable internal bind5 : Simulant -> World Lens -> World Lens -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal register : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal unregister : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal expandContent : (SplashDescriptor -> Screen -> Screen -> World -> World) -> SimulantContent -> ContentOrigin -> Simulant -> Simulant -> World -> Simulant option * World =
        Unchecked.defaultof<_>

    let mutable internal destroyImmediate : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal destroy : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal trySignalFacet : obj -> string -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal trySignal : obj -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    // OPTIMIZATION: slightly reduce Elmish simulant synchronization allocation.
    let private emptyPreviousSimulants =
        USet.makeEmpty (LensComparer ()) Imperative

    // OPTIMIZATION: slightly reduce Elmish simulant synchronization allocation.
    let private emptyRemovedSimulants =
        HashSet ()

    type World with // Construction

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

        /// Assert that the current world is the chosen world (used for debugging).
        static member assertChosen (world : World) =
#if DEBUG
            if world :> obj <> Debug.World.Chosen then
                Console.WriteLine "Fault"
#endif
            ignore world

        /// Make the world.
        static member internal make plugin eventDelegate dispatchers subsystems scriptingEnv ambientState quadtree octree activeGameDispatcher =
            let config = AmbientState.getConfig ambientState
            let elmishBindingsMap = UMap.makeEmpty HashIdentity.Structural config
            let entityStates = UMap.makeEmpty HashIdentity.Structural config
            let groupStates = UMap.makeEmpty HashIdentity.Structural config
            let screenStates = UMap.makeEmpty HashIdentity.Structural config
            let gameState = GameState.make activeGameDispatcher
            let simulants = UMap.singleton HashIdentity.Structural config (Simulants.Game :> Simulant) None
            let worldExtension = { DestructionListRev = []; Dispatchers = dispatchers; Plugin = plugin; ScriptingEnv = scriptingEnv; ScriptingContext = Game () }
            let world =
                { EventSystemDelegate = eventDelegate
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Unchecked.defaultof<Entity>, entityStates)) Unchecked.defaultof<EntityState>
                  EntityStates = entityStates
                  GroupStates = groupStates
                  ScreenStates = screenStates
                  GameState = gameState
                  EntityMounts = UMap.makeEmpty HashIdentity.Structural config
                  Quadtree = MutantCache.make id quadtree
                  Octree = MutantCache.make id octree
                  SelectedEcsOpt = None
                  ElmishBindingsMap = elmishBindingsMap
                  AmbientState = ambientState
                  Subsystems = subsystems
                  Simulants = simulants
                  WorldExtension = worldExtension }
            let world = { world with GameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState world }
            World.choose world

    type World with // Caching

        /// Get the optional cached entity.
        static member internal getEntityCachedOpt world =
            world.EntityCachedOpt

        /// Get the simulants.
        static member internal getSimulants world =
            world.Simulants

    type World with // Dispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.WorldExtension.Dispatchers.GameDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.WorldExtension.Dispatchers.ScreenDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.WorldExtension.Dispatchers.GroupDispatchers

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.WorldExtension.Dispatchers.EntityDispatchers

        /// Get the facets of the world.
        static member getFacets world =
            world.WorldExtension.Dispatchers.Facets

    type World with // AmbientState

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }

        /// Get whether the engine is running imperatively.
        [<FunctionBinding>]
        static member getImperative world =
            World.getAmbientStateBy AmbientState.getImperative world

        /// Get whether the engine is running stand-alone.
        [<FunctionBinding>]
        static member getStandAlone world =
            World.getAmbientStateBy AmbientState.getStandAlone world

        /// Get collection config value.
        [<FunctionBinding>]
        static member getCollectionConfig world =
            World.getAmbientStateBy AmbientState.getConfig world

        /// Get the the liveness state of the engine.
        [<FunctionBinding>]
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        static member internal updateTime world =
            World.updateAmbientState AmbientState.updateTime world
    
        /// Get the update rate.
        [<FunctionBinding>]
        static member getUpdateRate world =
            World.getAmbientStateBy AmbientState.getUpdateRate world

        /// Set the update rate, starting at the end of the current frame.
        [<FunctionBinding>]
        static member setUpdateRate updateRate world =
            World.frame (World.updateAmbientState (AmbientState.setUpdateRateImmediate updateRate)) Simulants.Game world

        /// Check that the update rate is non-zero.
        [<FunctionBinding>]
        static member isAdvancing world =
            World.getAmbientStateBy AmbientState.isAdvancing world

        /// Check that the update rate is zero.
        [<FunctionBinding>]
        static member isHalted world =
            World.getAmbientStateBy AmbientState.isHalted world

        /// Get the world's update time.
        [<FunctionBinding>]
        static member getUpdateTime world =
            World.getAmbientStateBy AmbientState.getUpdateTime world

        /// Get the world's clock time.
        /// No script function binding due to lack of a DateTimeOffset script conversion.
        static member getClockTime world =
            World.getAmbientStateBy AmbientState.getClockTime world

        /// Get the world's clock delta time in normalized floating point units.
        [<FunctionBinding>]
        static member getClockDelta world =
            World.getAmbientStateBy AmbientState.getClockDelta world

        /// Place the engine into a state such that the app will exit at the end of the current frame.
        [<FunctionBinding>]
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getMetadata world =
            AmbientState.getMetadata (World.getAmbientState world)

        static member internal setMetadata assetMetadataMap world =
            World.updateAmbientState (AmbientState.setMetadata assetMetadataMap) world

        /// Try to get the texture metadata of the given asset.
        [<FunctionBinding>]
        static member tryGetTextureSize assetTag world =
            Metadata.tryGetTextureSize assetTag (World.getMetadata world)

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        [<FunctionBinding>]
        static member getTextureSize assetTag world =
            Metadata.getTextureSize assetTag (World.getMetadata world)

        /// Try to get the texture size metadata of the given asset.
        [<FunctionBinding>]
        static member tryGetTextureSizeF assetTag world =
            Metadata.tryGetTextureSizeF assetTag (World.getMetadata world)

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        [<FunctionBinding>]
        static member getTextureSizeF assetTag world =
            Metadata.getTextureSizeF assetTag (World.getMetadata world)

        /// Try to get the tile map metadata of the given asset.
        static member tryGetTileMapMetadata assetTag world =
            Metadata.tryGetTileMapMetadata assetTag (World.getMetadata world)

        /// Forcibly get the tile map metadata of the given asset (throwing on failure).
        static member getTileMapMetadata assetTag world =
            Metadata.getTileMapMetadata assetTag (World.getMetadata world)

        /// Try to get the static model metadata of the given asset.
        static member tryGetStaticModel assetTag world =
            Metadata.tryGetStaticModel assetTag (World.getMetadata world)

        /// Forcibly get the static model metadata of the given asset (throwing on failure).
        static member getStaticModel assetTag world =
            Metadata.getStaticModel assetTag (World.getMetadata world)

        /// Try to get the bounds metadata of the given asset.
        static member tryGetBounds assetTag world =
            Metadata.tryGetBounds assetTag (World.getMetadata world)

        /// Forcibly get the bounds metadata of the given asset (throwing on failure).
        static member getBounds assetTag world =
            Metadata.getBounds assetTag (World.getMetadata world)

        /// Get a copy of the metadata map.
        static member getMetadataMap world =
            Metadata.getMetadataMap (World.getMetadata world)

        /// Get a map of all discovered assets.
        static member getAssetMap world =
            Metadata.getAssetMap (World.getMetadata world)

        static member internal getKeyValueStoreBy by world =
            World.getAmbientStateBy (AmbientState.getKeyValueStoreBy by) world

        static member internal getKeyValueStore world =
            World.getAmbientStateBy AmbientState.getKeyValueStore world

        static member internal setKeyValueStore symbolStore world =
            World.updateAmbientState (AmbientState.setKeyValueStore symbolStore) world

        static member internal updateKeyValueStore updater world =
            World.updateAmbientState (AmbientState.updateKeyValueStore updater) world

        /// Attempt to look up a value from the world's key value store without allocating.
        static member tryGetKeyedValueFast<'a> (key, world, value : 'a outref) =
            let ambientState = World.getAmbientState world
            let kvs = AmbientState.getKeyValueStore ambientState
            let mutable valueObj = Unchecked.defaultof<obj>
            if kvs.TryGetValue (key, &valueObj) then
                value <- valueObj :?> 'a
                true
            else false

        /// Attempt to look up a value from the world's key value store.
        static member tryGetKeyedValue<'a> key world =
            match World.getKeyValueStoreBy (UMap.tryFind key) world with
            | Some value -> Some (value :?> 'a)
            | None -> None

        /// Look up a value from the world's key value store, throwing an exception if it is not found.
        static member getKeyedValue<'a> key world =
            World.getKeyValueStoreBy (UMap.find key) world :?> 'a

        /// Add a value to the world's key value store.
        static member addKeyedValue<'a> key (value : 'a) world =
            World.updateKeyValueStore (UMap.add key (value :> obj)) world

        /// Remove a value from the world's key value store.
        static member removeKeyedValue key world =
            World.updateKeyValueStore (UMap.remove key) world

        /// Transform a value in the world's key value store if it exists.
        static member updateKeyedValue<'a> (updater : 'a -> 'a) key world =
            World.addKeyedValue key (updater (World.getKeyedValue<'a> key world)) world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal removeTasklets simulant world =
            World.updateAmbientState (AmbientState.removeTasklets simulant) world

        static member internal clearTasklets world =
            World.updateAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientState (AmbientState.restoreTasklets tasklets) world

        static member internal getTaskletsProcessing world =
            World.getAmbientStateBy AmbientState.getTaskletsProcessing world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet simulant tasklet world =
            World.updateAmbientState (AmbientState.addTasklet simulant tasklet) world

        /// Schedule an operation to be executed by the engine at the given time.
        static member schedule fn time (simulant : Simulant) world =
            let tasklet = { ScheduledTime = time; ScheduledOp = fn }
            World.addTasklet simulant tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame.
        static member frame fn (simulant : Simulant) world =
            let taskletsProcessing = World.getTaskletsProcessing world
            World.schedule fn (World.getUpdateTime world + if taskletsProcessing then 1L else 0L) simulant world

        /// Schedule an operation to be executed by the engine with the given delay.
        static member delay fn delay simulant world =
            let tasklet = { ScheduledTime = World.getUpdateTime world + delay; ScheduledOp = fn }
            World.addTasklet simulant tasklet world

        /// Attempt to get the window flags.
        [<FunctionBinding>]
        static member tryGetWindowFlags world =
            World.getAmbientStateBy AmbientState.tryGetWindowFlags world

        /// Attempt to check that the window is minimized.
        [<FunctionBinding>]
        static member tryGetWindowMinimized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMinimized world

        /// Attempt to check that the window is maximized.
        [<FunctionBinding>]
        static member tryGetWindowMaximized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMaximized world
            
        /// Attempt to check that the window is in a full screen state.
        [<FunctionBinding>]
        static member tryGetWindowFullScreen world =
            World.getAmbientStateBy AmbientState.tryGetWindowFullScreen world

        /// Attempt to set the window's full screen state.
        [<FunctionBinding>]
        static member trySetWindowFullScreen fullScreen world =
            World.updateAmbientState (AmbientState.trySetWindowFullScreen fullScreen) world

        /// Attempt to get the window size.
        [<FunctionBinding>]
        static member tryGetWindowSize world =
            World.getAmbientStateBy (AmbientState.tryGetWindowSize) world

        /// Check whether the world should sleep rather than run.
        [<FunctionBinding>]
        static member shouldSleep world =
            World.getAmbientStateBy AmbientState.shouldSleep world

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage implicitDelimiters packageName world =
            World.getSymbolStoreBy (SymbolStore.tryLoadSymbolPackage implicitDelimiters packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.getSymbolStoreBy (SymbolStore.unloadSymbolPackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol assetTag metadata world =
            let symbolStore = World.getSymbolStore world
            SymbolStore.tryFindSymbol assetTag metadata symbolStore

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols implicitDelimiters assetTags world =
            let symbolStore = World.getSymbolStore world
            SymbolStore.tryFindSymbols implicitDelimiters assetTags symbolStore

        /// Reload all the symbols in the symbol store.
        [<FunctionBinding>]
        static member reloadSymbols world =
            World.getSymbolStoreBy SymbolStore.reloadSymbols world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get overlays.
        static member getOverlays world =
            World.getOverlayerBy Overlayer.getOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getOverlayRouterBy by world =
            let overlayRouter = World.getOverlayRouter world
            by overlayRouter

        static member internal setOverlayRouter router world =
            World.updateAmbientState (AmbientState.setOverlayRouter router) world

        static member internal tryFindRoutedOverlayNameOpt dispatcherName state =
            World.getOverlayRouterBy (OverlayRouter.tryFindOverlayNameOpt dispatcherName) state

    type World with // Quadtree

        static member internal getQuadtree world =
            world.Quadtree

        static member internal setQuadtree quadtree world =
            if World.getImperative world then
                world.Quadtree <- quadtree
                world
            else World.choose { world with Quadtree = quadtree }

        static member internal updateQuadtree updater world =
            World.setQuadtree (updater (World.getQuadtree world))

    type World with // Octree

        static member internal getOctree world =
            world.Octree

        static member internal setOctree octree world =
            if World.getImperative world then
                world.Octree <- octree
                world
            else World.choose { world with Octree = octree }

        static member internal updateOctree updater world =
            World.setOctree (updater (World.getOctree world))

    type World with // SelectedEcsOpt

        /// Attempt to get the currently selected ECS.
        static member getSelectedEcsOpt world =
            world.SelectedEcsOpt

        /// Get the currently selected ECS or throw an exception.
        static member getSelectedEcs world =
            match  world.SelectedEcsOpt with
            | Some selectedEcsOpt -> selectedEcsOpt
            | None -> failwith "Cannot get Ecs when no screen is selected."

        static member internal setSelectedEcsOpt selectedEcsOpt world =
            if World.getImperative world then
                world.SelectedEcsOpt <- selectedEcsOpt
                world
            else World.choose { world with SelectedEcsOpt = selectedEcsOpt }

    type World with // Subsystems

        static member internal getSubsystems world =
            world.Subsystems

        static member internal setSubsystems subsystems world =
            World.choose { world with Subsystems = subsystems }

        static member internal updateSubsystems updater world =
            World.setSubsystems (updater world.Subsystems) world

        static member internal cleanUpSubsystems world =
            World.updateSubsystems (fun subsystems -> subsystems.RendererProcess.Terminate (); subsystems) world

    type World with // EventSystem

        /// Get event subscriptions.
        static member internal getSubscriptions world =
            EventSystem.getSubscriptions<World> world

        /// Get event unsubscriptions.
        static member internal getUnsubscriptions world =
            EventSystem.getUnsubscriptions<World> world

        /// Get how events are being traced.
        static member getEventTracerOpt (world : World) =
            EventSystem.getEventTracerOpt world

        /// Set how events are being traced.
        static member setEventTracerOpt tracerOpt (world : World) =
            World.choose (EventSystem.setEventTracerOpt tracerOpt world)

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventSystem.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            World.choose (EventSystem.setEventFilter filter world)

        /// Publish an event with no subscription sorting.
        /// OPTIMIZATION: unrolled publishPlus here for speed.
        static member publishPlus<'a, 'p when 'p :> Simulant>
            (eventData : 'a)
            (eventAddress : 'a Address)
            eventTrace
            (publisher : 'p)
            hierarchical
            selectedOnly
            (world : World) =

            // OPTIMIZATION: generalize only once
            let eventAddressObj = Address.generalize eventAddress

#if DEBUG
            // log event based on event filter
            EventSystemDelegate.logEvent<World> eventAddressObj eventTrace world.EventSystemDelegate
#endif

            // get subscriptions the fastest way possible
            // OPTIMIZATION: subscriptions nullable to elide allocation via Seq.empty.
            let subscriptionsOpt =
                if hierarchical then
                    EventSystemDelegate.getSubscriptionsSorted
                        sortSubscriptionsByElevation eventAddressObj world.EventSystemDelegate world
                else
                    let subscriptions = EventSystemDelegate.getSubscriptions world.EventSystemDelegate
                    match UMap.tryFind eventAddressObj subscriptions with
                    | Some subscriptions -> OMap.toSeq subscriptions
                    | None -> null

            // publish to each subscription
            // OPTIMIZATION: inlined foldWhile here in order to compact the call stack.
            // OPTIMIZATION: fused PublishEventHook for speed.
            if notNull subscriptionsOpt then
                let mutable (going, enr) = (true, subscriptionsOpt.GetEnumerator ())
                let mutable (handling, world) = (Cascade, world)
                while going && enr.MoveNext () do
                    let (_, subscriptionEntry) = enr.Current
                    if  (match handling with Cascade -> true | Resolve -> false) &&
                        (match World.getLiveness world with Live -> true | Dead -> false) then
                        let subscriber = subscriptionEntry.Subscriber
                        if not selectedOnly || isSelected subscriber world then
                            let result =
                                let namesLength = subscriber.SimulantAddress.Names.Length
                                if namesLength >= 3
                                then EventSystem.publishEvent<'a, 'p, Entity, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                else
                                    match namesLength with
                                    | 0 ->
                                        match subscriber with
                                        | :? Game -> EventSystem.publishEvent<'a, 'p, Game, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                        | :? GlobalSimulantGeneralized -> EventSystem.publishEvent<'a, 'p, Simulant, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                        | _ -> failwithumf ()
                                    | 1 -> EventSystem.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                    | 2 -> EventSystem.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                    | _ -> failwithumf ()
                            handling <- fst result
                            world <- snd result
                            world |> World.choose |> ignore
                        else () // nothing to do
                    else going <- false
                world
            else world

        /// Publish an event with no subscription sorting.
        static member publish<'a, 'p when 'p :> Simulant>
            eventData eventAddress eventTrace publisher world =
            World.publishPlus<'a, 'p> eventData eventAddress eventTrace publisher false false world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionId world =
            World.choose (EventSystem.unsubscribe<World> subscriptionId world)

        /// Subscribe to an event using the given subscriptionId, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            subscriptionId callback eventAddress subscriber world =
            mapSnd World.choose (EventSystem.subscribePlus<'a, 's, World> subscriptionId callback eventAddress subscriber world)

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            World.choose (EventSystem.subscribe<'a, 's, World> callback eventAddress subscriber world)

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback..
        static member monitorPlus<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            mapSnd World.choose (EventSystem.monitorPlus<'a, 's, World> callback eventAddress subscriber world)

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            World.choose (EventSystem.monitor<'a, 's, World> callback eventAddress subscriber world)

    type World with // ElmishBindingsMap

        static member internal makeLensesCurrent (keys : IComparable List) lensGeneralized world =
            let config = World.getCollectionConfig world
            let mutable current = USet.makeEmpty (LensComparer ()) config
            for key in keys do
                // OPTIMIZATION: ensure item is extracted during validation only.
                // This creates a strong dependency on the map being used in a perfectly predictable way (one validate, one getWithoutValidation).
                // Any violation of this implicit invariant will result in a NullReferenceException.
                let mutable itemCached = Unchecked.defaultof<obj> // ELMISH_CACHE
                let validateOpt =
                    match lensGeneralized.ValidateOpt with
                    | Some validate -> Some (fun world ->
                        if validate world then
                            let mapGeneralized = Lens.getWithoutValidation lensGeneralized world
                            let struct (found, itemOpt) = mapGeneralized.TryGetValue key
                            if found then itemCached <- itemOpt
                            found
                        else false)
                    | None -> Some (fun world ->
                        let mapGeneralized = Lens.getWithoutValidation lensGeneralized world
                        let struct (found, itemOpt) = mapGeneralized.TryGetValue key
                        if found then itemCached <- itemOpt
                        found)
                let getWithoutValidation =
                    fun _ ->
                        let result = itemCached
                        itemCached <- Unchecked.defaultof<obj> // avoid GC promotion by throwing away map ASAP
                        result
                let lensItem =
                    { Name = lensGeneralized.Name
                      ParentOpt = Some (lensGeneralized :> World Lens)
                      ValidateOpt = validateOpt
                      GetWithoutValidation = getWithoutValidation
                      SetOpt = None
                      This = lensGeneralized.This }
                let item = { LensHash = key.GetHashCode (); LensKey = key; LensItem = lensItem }
                current <- USet.add item current
            current

        static member internal removeSynchronizedSimulants contentKey (removed : _ HashSet) world =
            Seq.fold (fun world lensComparable ->
                let key = lensComparable.LensKey
                match World.tryGetKeyedValueFast (contentKey, world) with
                | (true, simulantMap : Map<'a, Simulant>) ->
                    match simulantMap.TryGetValue key with
                    | (true, simulant) ->
                        let simulantMap = Map.remove key simulantMap
                        let world =
                            if Map.isEmpty simulantMap
                            then World.removeKeyedValue contentKey world
                            else World.addKeyedValue contentKey simulantMap world
                        destroy simulant world
                    | (false, _) -> world
                | (false, _) -> world)
                world removed

        static member internal addSynchronizedSimulants
            (contentMapper : IComparable -> Lens<obj, World> -> World -> SimulantContent)
            (contentKey : Guid)
            (mapGeneralized : MapGeneralized)
            (added : _ HashSet)
            (origin : ContentOrigin)
            (owner : Simulant)
            (parent : Simulant)
            world =
            Seq.fold (fun world lensComparable ->
                let key = lensComparable.LensKey
                let lens = lensComparable.LensItem
                if mapGeneralized.ContainsKey key then
                    let content = contentMapper key lens world
                    let (simulantOpt, world) = expandContent Unchecked.defaultof<_> content origin owner parent world
                    match World.tryGetKeyedValueFast (contentKey, world) with
                    | (false, _) ->
                        match simulantOpt with
                        | Some simulant -> World.addKeyedValue contentKey (Map.singleton key simulant) world
                        | None -> world
                    | (true, simulantMap) ->
                        match simulantOpt with
                        | Some simulant -> World.addKeyedValue contentKey (Map.add key simulant simulantMap) world
                        | None -> world
                else world)
                world added

        static member internal synchronizeSimulants contentMapper contentKey mapGeneralized current origin owner parent world =
            let previous =
                match World.tryGetKeyedValueFast<IComparable LensComparable USet> (contentKey, world) with // ELMISH_CACHE
                | (false, _) -> emptyPreviousSimulants
                | (true, previous) -> previous
            let added = USet.differenceFast current previous
            let removed =
                if added.Count = 0 && USet.length current = USet.length previous
                then emptyRemovedSimulants // infer no removals
                else USet.differenceFast previous current
            let changed = added.Count <> 0 || removed.Count <> 0
            if changed then
                // HACK: we need to use content key for an additional purpose, so we add 2 (key with add 1 may already be used in invoking function).
                // TODO: make sure our removal of this key's entry is comprehensive or has some way of not creating too many dead entries.
                let contentKeyPlus2 = Gen.idDeterministic 2 contentKey // ELMISH_CACHE
                let world = World.removeSynchronizedSimulants contentKeyPlus2 removed world
                let world = World.addSynchronizedSimulants contentMapper contentKeyPlus2 mapGeneralized added origin owner parent world
                let world = World.addKeyedValue contentKey current world
                world
            else world

        static member private publishPropertyBinding binding world =
            if binding.PBRight.Validate world then
                let value = binding.PBRight.GetWithoutValidation world
                let changed =
                    match binding.PBPrevious with
                    | ValueSome previous ->
                        if value =/= previous
                        then binding.PBPrevious <- ValueSome value; true
                        else false
                    | ValueNone -> binding.PBPrevious <- ValueSome value; true
                let allowPropertyBinding =
#if DEBUG
                    // OPTIMIZATION: only compute in DEBUG mode.
                    not (ignorePropertyBindings binding.PBLeft.This world)
#else
                    true
#endif
                if changed && allowPropertyBinding
                then binding.PBLeft.TrySet value world
                else world
            else world

        static member private publishPropertyBindingGroup bindings world =
            let parentChanged =
                match bindings.PBSParentPrevious with
                | ValueSome parentPrevious ->
                    let parent = bindings.PBSParent
                    if parent.Validate world then
                        let parentValue = parent.GetWithoutValidation world
                        if parentValue =/= parentPrevious
                        then bindings.PBSParentPrevious <- ValueSome parentValue; true
                        else false
                    else true
                | ValueNone ->
                    let parent = bindings.PBSParent
                    if parent.Validate world then
                        let parentValue = parent.GetWithoutValidation world
                        bindings.PBSParentPrevious <- ValueSome parentValue
                        true
                    else true
            if parentChanged
            then OMap.foldv (flip World.publishPropertyBinding) world bindings.PBSPropertyBindings
            else world

        static member private publishContentBinding binding world =
            // HACK: we need to use content key for an additional purpose, so we add 1.
            // TODO: make sure our removal of this key's entry is comprehensive or has some way of not creating too many dead entries.
            let contentKeyPlus1 = Gen.idDeterministic 1 binding.CBContentKey // ELMISH_CACHE
            if Lens.validate binding.CBSource world then
                let mapGeneralized = Lens.getWithoutValidation binding.CBSource world
                let currentKeys = mapGeneralized.Keys
                let previousKeys =
                    match World.tryGetKeyedValueFast<IComparable List> (contentKeyPlus1, world) with
                    | (false, _) -> List () // TODO: use cached module binding of empty List.
                    | (true, previous) -> previous
                if not (currentKeys.SequenceEqual previousKeys) then
                    let world = World.addKeyedValue contentKeyPlus1 currentKeys world
                    let current = World.makeLensesCurrent currentKeys binding.CBSource world
                    World.synchronizeSimulants binding.CBMapper binding.CBContentKey mapGeneralized current binding.CBOrigin binding.CBOwner binding.CBParent world
                else world
            else
                let config = World.getCollectionConfig world
                let lensesCurrent = USet.makeEmpty (LensComparer ()) config
                let world = World.synchronizeSimulants binding.CBMapper binding.CBContentKey (MapGeneralized.make Map.empty) lensesCurrent binding.CBOrigin binding.CBOwner binding.CBParent world
                World.removeKeyedValue contentKeyPlus1 world

        static member internal publishChangeBinding propertyName simulant world =
            let propertyAddress = PropertyAddress.make propertyName simulant
            match world.ElmishBindingsMap.TryGetValue propertyAddress with
            | (true, elmishBindings) ->
                OMap.foldv (fun world elmishBinding ->
                    match elmishBinding with
                    | PropertyBinding binding -> World.publishPropertyBinding binding world
                    | PropertyBindingGroup bindings -> World.publishPropertyBindingGroup bindings world
                    | ContentBinding binding -> World.publishContentBinding binding world)
                    world elmishBindings.EBSBindings
            | (false, _) -> world

    type World with // Scripting

        static member internal getGlobalFrame (world : World) =
            ScriptingSystem.getGlobalFrame world

        static member internal getLocalFrame (world : World) =
            ScriptingSystem.getLocalFrame world

        static member internal setLocalFrame localFrame (world : World) =
            ScriptingSystem.setLocalFrame localFrame world

        /// Get the context of the script system.
        static member internal getScriptContext (world : World) =
            world.WorldExtension.ScriptingContext

        /// Set the context of the script system.
        static member internal setScriptContext context (world : World) =
            World.choose { world with WorldExtension = { world.WorldExtension with ScriptingContext = context }}

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

        /// Attempt to evaluate a script.
        static member tryEvalScript scriptFilePath world =
            ScriptingSystem.tryEvalScript World.choose scriptFilePath world

    type World with // Destruction

        static member internal addSimulantToDestruction simulant world =
            { world with
                WorldExtension =
                    { world.WorldExtension with
                        DestructionListRev = simulant :: world.WorldExtension.DestructionListRev }}

        static member internal tryRemoveSimulantFromDestruction simulant world =
            { world with
                WorldExtension =
                    { world.WorldExtension with
                        DestructionListRev = List.remove ((=) simulant) world.WorldExtension.DestructionListRev }}

    type World with // Plugin

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle world =
            world.WorldExtension.Plugin.TryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle

        static member internal preFrame world =
            world.WorldExtension.Plugin.PreFrame world

        static member internal perFrame world =
            world.WorldExtension.Plugin.PerFrame world

        static member internal postFrame world =
            world.WorldExtension.Plugin.PostFrame world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getMemberProperties (state : SimulantState) =
            state |>
            getType |>
            (fun ty -> ty.GetProperties ()) |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.PropertyType, property.GetValue state)) |>
            Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getXtensionProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            List.ofSeq |>
            List.sortBy fst |>
            List.map (fun (name, property) -> (name, property.PropertyType, property.PropertyValue))

        /// Provides a full view of all the properties of some SimulantState.
        static member internal getProperties state =
            List.append
                (World.getMemberProperties state)
                (World.getXtensionProperties state)

        /// Present properties for viewing.
        static member internal viewProperties state =
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c |> Array.sortBy fst

    type World with // Handlers

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)

        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)