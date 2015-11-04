// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open FSharpx.Collections
open OpenTK
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Transition =

    /// Make a screen transition.
    let make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          OptDissolveImage = None }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GameState =

    /// Make a game state value.
    let make dispatcher =
        { Id = Core.makeId ()
          OptSelectedScreen = None
          PublishChanges = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = Xtension.safe }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ScreenState =

    /// Make a screen state value.
    let make optSpecialization optName dispatcher =
        let id = Core.makeId ()
        let screenState =
            { Id = id
              Name = match optName with Some name -> name | None -> Name.make ^ acstring id
              OptSpecialization = optSpecialization 
              TransitionStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              DispatcherNp = dispatcher
              EntityTreeNp = Unchecked.defaultof<Entity QuadTree MutantCache>
              Xtension = Xtension.safe }
        let quadTree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
        { screenState with EntityTreeNp = MutantCache.make Operators.id quadTree }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GroupState =

    /// Make a group state value.
    let make optSpecialization optName dispatcher =
        let id = Core.makeId ()
        { GroupState.Id = id
          Name = match optName with Some name -> name | None -> Name.make ^ acstring id
          OptSpecialization = optSpecialization
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = Xtension.safe }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EntityState =

    /// Make an entity state value.
    let make optSpecialization optName optOverlayName dispatcher =
        let id = Core.makeId ()
        { Id = id
          Name = match optName with Some name -> name | None -> Name.make ^ acstring id
          OptSpecialization = optSpecialization
          Position = Vector2.Zero
          Size = Constants.Engine.DefaultEntitySize
          Rotation = 0.0f
          Depth = 0.0f
          Overflow = Vector2.Zero
          Visible = true
          ViewType = Relative
          Omnipresent = false
          PublishUpdates = true
          PublishChanges = false
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          FacetNames = Set.empty
          FacetsNp = []
          OptOverlayName = optOverlayName
          Xtension = Xtension.safe }

[<AutoOpen>]
module SimulationOperators =

    /// Convert a name string to a screen's proxy.
    let (!>) screenNameStr = Screen.proxy ^ ntoa ^ Name.make screenNameStr

    /// Convert a name to a screen's proxy.
    let ntos screenName = Screen.proxy ^ ntoa screenName

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    let gtoe (group : Group) entityName = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    let stog (screen : Screen) groupName = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName

    /// Convert an entity's proxy to a group's by removing the entity's name from the end.
    let etog (entity : Entity) = !< entity

    /// Convert a group's proxy to a screen's by removing the group's name from the end.
    let gtos group = Screen.proxy ^ Address.take<Group, Screen> 1 group.GroupAddress

[<RequireQualifiedAccess>]
module Simulants =

    let Game = { GameAddress = Address.empty }
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    let DefaultGroup = DefaultScreen => Constants.Engine.DefaultGroupName
    let DefaultEntity = DefaultGroup => Constants.Engine.DefaultEntityName

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Subsystems =

    let getSubsystem<'s when 's :> Subsystem> name subsystems =
        Map.find name subsystems.SubsystemMap :?> 's

    let getSubsystemBy<'s, 't when 's :> Subsystem> by name subsystems : 't =
        let subsystem = getSubsystem<'s> name subsystems
        by subsystem

    let setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name subsystems =
        { SubsystemMap = Map.add name (subsystem :> Subsystem) subsystems.SubsystemMap }

    let updateSubsystem<'s, 'w when 's :> Subsystem> (updater : 's -> 'w -> 's) name subsystems world =
        let subsystem = getSubsystem<'s> name subsystems
        let subsystem = updater subsystem world
        setSubsystem subsystem name subsystems

    let updateSubsystems (updater : Subsystem -> 'w -> Subsystem) subsystems world =
        Map.fold
            (fun subsystems name subsystem -> let subsystem = updater subsystem world in setSubsystem subsystem name subsystems)
            subsystems
            subsystems.SubsystemMap

    let clearSubsystemsMessages subsystems world =
        updateSubsystems (fun is _ -> is.ClearMessages ()) subsystems world

    let make subsystems =
        { SubsystemMap = subsystems }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Callbacks =

    /// Clear all held tasklets.
    let clearTasklets callbacks =
        { callbacks with Tasklets = Queue.empty }

    /// Restore previously-held tasklets.
    let restoreTasklets (tasklets : Tasklet Queue) callbacks =
        { callbacks with Tasklets = Queue.ofSeq ^ Seq.append (callbacks.Tasklets :> Tasklet seq) (tasklets :> Tasklet seq) }

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet callbacks =
        { callbacks with Tasklets = Queue.conj tasklet callbacks.Tasklets }

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets callbacks =
        { callbacks with Tasklets = Queue.ofSeq ^ Seq.append (tasklets :> Tasklet seq) (callbacks.Tasklets :> Tasklet seq) }

    /// Add callback state.
    let addCallbackState key state callbacks =
        { callbacks with CallbackStates = Vmap.add key (state :> obj) callbacks.CallbackStates }

    /// Remove callback state.
    let removeCallbackState key callbacks =
        { callbacks with CallbackStates = Vmap.remove key callbacks.CallbackStates }

    /// Get callback state.
    let getCallbackState<'a> key callbacks =
        let state = Vmap.find key callbacks.CallbackStates
        state :?> 'a

    /// Make a callbacks value.
    let make () =
        { Subscriptions = Vmap.makeEmpty (KeyEq Address.equals) Constants.Engine.SubscriptionMapDepth
          Unsubscriptions = Vmap.makeEmpty (KeyEq (=)) Constants.Engine.SubscriptionMapDepth
          Tasklets = Queue.empty
          CallbackStates = Vmap.makeEmpty (KeyEq (=)) Constants.Engine.CallbackStateMapDepth }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Components =

    /// Get the facets.
    let getFacets components =
        components.Facets

    /// Get the entity dispatchers.
    let getEntityDispatchers components =
        components.EntityDispatchers

    /// Get the group dispatchers.
    let getGroupDispatchers components =
        components.GroupDispatchers

    /// Get the screen dispatchers.
    let getScreenDispatchers components =
        components.ScreenDispatchers

    /// Get the game dispatchers.
    let getGameDispatchers components =
        components.GameDispatchers

    /// Make a subsystems value.
    let make facets entityDispatchers groupDispatchers screenDispatchers gameDispatchers =
        { Facets = facets
          EntityDispatchers = entityDispatchers
          GroupDispatchers = groupDispatchers
          ScreenDispatchers = screenDispatchers
          GameDispatchers = gameDispatchers }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module WorldState =

    /// Get the tick rate.
    let getTickRate state =
        state.TickRate

    /// Get the tick rate as a floating-point value.
    let getTickRateF state =
        single ^ getTickRate state

    /// Set the tick rate without waiting for the end of the current update. Only use
    /// this if you need it and understand the engine internals well enough to know the
    /// consequences.
    let setTickRateImmediately tickRate state =
        { state with TickRate = tickRate }

    /// Get the tick time.
    let getTickTime state =
        state.TickTime

    /// Query that ticking is enabled.
    let isTicking state =
        getTickRate state <> 0L

    /// Update the tick time by the tick rate.
    let updateTickTime state =
        { state with TickTime = getTickTime state + getTickRate state }

    /// Get the world's update count.
    let getUpdateCount state =
        state.UpdateCount

    /// Increment the update count.
    let incrementUpdateCount state =
        { state with UpdateCount = inc ^ getUpdateCount state }

    /// Get the the liveness state of the engine.
    let getLiveness state =
        state.Liveness

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit state =
        { state with Liveness = Exiting }

    /// Get a value from the camera.
    let getCameraBy by state =
        by state.Camera

    /// Get the camera used to view the world.
    let getCamera state =
        getCameraBy id state

    /// Set the camera used to view the world.
    let setCamera camera state =
        { state with Camera = camera }

    /// Update the camera used to view the world.
    let updateCamera updater state =
        let camera = updater ^ getCamera state
        setCamera camera state

    /// Get the current destination screen if a screen transition is currently underway.
    let getOptScreenTransitionDestination state =
        state.OptScreenTransitionDestination

    /// Set the current destination screen if a screen transition is currently underway.
    let internal setOptScreenTransitionDestination destination state =
        { state with OptScreenTransitionDestination = destination }

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        world.State.AssetMetadataMap

    /// Set the asset metadata map.
    let setAssetMetadataMap assetMetadataMap state =
        { state with AssetMetadataMap = assetMetadataMap }

    /// Set the overlayer.
    let setOverlayer overlayer state =
        { state with Overlayer = overlayer }

    /// Get the user-defined state, casted to 'u.
    let getUserState state : 'u =
        state.UserState :?> 'u

    /// Set the user-defined state of the world.
    let setUserState (userState : 'u) state =
        { state with UserState = userState }

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) state =
        let userState = getUserState state
        let userState = updater userState
        setUserState userState state

    /// Make a world state value.
    let make tickRate assetMetadataMap overlayRouter overlayer camera userState =
        { TickRate = tickRate
          TickTime = 0L
          UpdateCount = 0L
          Liveness = Running
          OptScreenTransitionDestination = None
          AssetMetadataMap = assetMetadataMap
          AssetGraphFilePath = String.Empty
          OverlayRouter = overlayRouter
          OverlayFilePath = String.Empty
          Overlayer = overlayer
          Camera = camera
          OptEntityCache = Unchecked.defaultof<KeyedCache<Entity Address * World, EntityState option>>
          RefClipboard = ref None
          UserState = userState }