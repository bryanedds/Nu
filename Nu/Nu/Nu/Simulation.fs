// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.IO
open System.Reflection
open System.Xml
open FSharpx
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module InterativityModule =

    /// Describes the game engine's current level of 'interactivity'.
    type Interactivity =
        | GuiOnly
        | GuiAndPhysics
        | GuiAndPhysicsAndGamePlay

[<RequireQualifiedAccess>]
module Interactivity =

    /// Query that the game engine is in game-playing mode.
    let isGamePlaying interactivity =
        match interactivity with
        | GuiOnly -> false
        | GuiAndPhysics -> false
        | GuiAndPhysicsAndGamePlay -> true

    /// Query that the physics system is running.
    let isPhysicsRunning interactivity =
        match interactivity with
        | GuiOnly -> false
        | GuiAndPhysics -> true
        | GuiAndPhysicsAndGamePlay -> true

[<AutoOpen>]
module TransitionTypeModule =

    /// The type of a screen transition. Incoming means a new screen is being shown, and Outgoing
    /// means an existing screen being hidden.
    type TransitionType =
        | Incoming
        | Outgoing

[<AutoOpen>]
module ScreenStateModule =

    /// The state of a screen's transition.
    type TransitionState =
        | IncomingState
        | OutgoingState
        | IdlingState

    /// Describes the behavior of the screen dissolving algorithm.
    type [<StructuralEquality; NoComparison>] DissolveData =
        { IncomingTime : int64
          OutgoingTime : int64
          DissolveImage : AssetTag }

    /// Describes the behavior of the screen splash algorithm.
    type [<StructuralEquality; NoComparison>] SplashData =
        { DissolveData : DissolveData
          IdlingTime : int64
          SplashImage : AssetTag }

[<AutoOpen>]
module TileMapModule =

    /// The data needed to describe a Tiled tile map.
    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : Vector2i
          TileSize : Vector2i
          TileSizeF : Vector2
          TileMapSize : Vector2i
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : Vector2i }

    /// The data needed to describe a Tiled tile.
    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : Vector2i
          OptTileSetTile : TmxTilesetTile option
          TilePosition : Vector2i }

[<AutoOpen>]
module SimulationModule =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    /// Describes one of a screen's transition processes.
    type [<CLIMutable; StructuralEquality; NoComparison>] TransitionDescriptor =
        { TransitionType : TransitionType
          TransitionLifetime : int64
          OptDissolveImage : AssetTag option }

        /// Make a screen transition descriptor.
        static member make transitionType =
            { TransitionType = transitionType
              TransitionLifetime = 0L
              OptDissolveImage = None }

    /// The data for a mouse move event.
    type [<StructuralEquality; NoComparison>] MouseMoveData =
        { Position : Vector2 }

    /// The data for a mouse button event.
    type [<StructuralEquality; NoComparison>] MouseButtonData =
        { Position : Vector2
          Button : MouseButton
          Down : bool }

    /// The data for a keyboard key event.
    type [<StructuralEquality; NoComparison>] KeyboardKeyData =
        { ScanCode : int
          Repeated : bool
          Down : bool }

    /// The data for a collision event.
    type [<StructuralEquality; NoComparison>] CollisionData =
        { Normal : Vector2
          Speed : single
          Collidee : Entity }

    /// The data for a world state change event.
    and [<StructuralEquality; NoComparison>] WorldStateChangeData =
        { OldWorldState : WorldState }

    /// The data for a simulant change event.
    and [<StructuralEquality; NoComparison>] SimulantChangeData<'s when 's :> Simulant> =
        { Simulant : 's
          OldWorld : World }

    /// An event used by the game engine's purely-functional event system.
    and [<ReferenceEquality>] Event<'a, 's when 's :> Simulant> =
        { Subscriber : 's
          Publisher : Simulant // TODO: consider making this a list so that Observer can provide all useful addresses
          EventAddress : 'a Address
          Data : 'a }

    /// Describes whether an event has been resolved or should cascade to down-stream handlers.
    and EventHandling =
        | Resolve
        | Cascade

    /// Describes an event subscription.
    and Subscription<'a, 's when 's :> Simulant> = Event<'a, 's> -> World -> EventHandling * World

    /// Describes an event subscription that can be boxed / unboxed.
    and BoxableSubscription = obj -> World -> EventHandling * World

    /// An entry in the subscription map.
    and SubscriptionEntry = Guid * Simulant * obj

    /// A map of event subscriptions.
    and SubscriptionEntries = Map<obj Address, SubscriptionEntry rQueue>

    /// Abstracts over a subscription sorting procedure.
    and SubscriptionSorter = SubscriptionEntry rQueue -> World -> SubscriptionEntry rQueue

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, obj Address * Simulant>

    /// A task to be completed at the given time, with time being accounted for by the world
    /// state's TickTime value.
    and [<ReferenceEquality>] Task =
        { ScheduledTime : int64
          Operation : World -> World }

    /// The default dispatcher for games.
    and GameDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true]

        /// Register a game when adding it to the world. Note that there is not corresponding
        /// Unregister method due to the inability to remove a game from the world.
        abstract Register : Game -> World -> World
        default dispatcher.Register _ world = world

    /// The default dispatcher for screens.
    and ScreenDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a screen when adding it to the world.
        abstract Register : Screen -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister a screen when removing it from the world.
        abstract Unregister : Screen -> World -> World
        default dispatcher.Unregister _ world = world

    /// The default dispatcher for groups.
    and GroupDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a group when adding it to a screen.
        abstract Register : Group -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister a group when removing it from a screen.
        abstract Unregister : Group -> World -> World
        default dispatcher.Unregister _ world = world

    /// The default dispatcher for entities.
    and EntityDispatcher () =

        static member FieldDefinitions =
            [define? Position Vector2.Zero
             define? Depth 0.0f
             define? Size DefaultEntitySize
             define? Rotation 0.0f
             define? Visible true
             define? ViewType Relative
             define? PublishChanges true
             define? Persistent true]

        /// Register an entity when adding it to a group.
        abstract Register : Entity -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister an entity when removing it from a group.
        abstract Unregister : Entity -> World -> World
        default dispatcher.Unregister _ world = world

        /// Propagate an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity -> World -> World
        default dispatcher.PropagatePhysics _ world = world

        /// Get the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : Entity -> World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors _ _ = []

        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        abstract GetQuickSize : Entity -> World -> Vector2
        default dispatcher.GetQuickSize _ _ = Vector2.One

        /// Get the priority with which an entity is picked in the editor.
        abstract GetPickingPriority : Entity -> World -> single
        default dispatcher.GetPickingPriority entity world = entity.GetDepth world

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        /// Register a facet when adding it to an entity.
        abstract Register : Entity -> World -> World
        default facet.Register entity world = facet.RegisterPhysics entity world

        /// Unregister a facet when removing it from an entity.
        abstract Unregister : Entity -> World -> World
        default facet.Unregister entity world = facet.UnregisterPhysics entity world

        /// Participate in the registration of an entity's physics with the physics subsystem.
        abstract RegisterPhysics : Entity -> World -> World
        default facet.RegisterPhysics _ world = world

        /// Participate in the unregistration of an entity's physics from the physics subsystem.
        abstract UnregisterPhysics : Entity -> World -> World
        default facet.UnregisterPhysics _ world = world

        /// Participate in the propagation an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity -> World -> World
        default facet.PropagatePhysics _ world = world

        /// Participate in getting the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : Entity -> World -> RenderDescriptor list
        default facet.GetRenderDescriptors _ _ = []

        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity -> World -> Vector2
        default facet.GetQuickSize _ _ = DefaultEntitySize

    /// A marker interface for the simulation state types (GameState, ScreenState, GroupState,
    /// and EntityState).
    and SimulantState = interface end

    /// Hosts the ongoing state of a game. Should rarely be accessed directly by end-users.
    and [<CLIMutable; StructuralEquality; NoComparison>] GameState =
        { Id : Guid
          OptSelectedScreen : Screen option
          PublishChanges : bool
          CreationTimeNp : DateTime
          DispatcherNp : GameDispatcher
          Xtension : Xtension }

        interface SimulantState

        /// Make a game state value.
        static member make dispatcher =
            { Id = Core.makeId ()
              OptSelectedScreen = None
              PublishChanges = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Hosts the ongoing state of a screen. Should rarely be accessed directly by end-users.
    and [<CLIMutable; StructuralEquality; NoComparison>] ScreenState =
        { Id : Guid
          Name : string
          TransitionStateNp : TransitionState
          TransitionTicksNp : int64
          Incoming : TransitionDescriptor
          Outgoing : TransitionDescriptor
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : ScreenDispatcher
          Xtension : Xtension }

        interface SimulantState

        /// Make a screen state value.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              TransitionStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = TransitionDescriptor.make Incoming
              Outgoing = TransitionDescriptor.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Hosts the ongoing state of a group. Should rarely be accessed directly by end-users.
    and [<CLIMutable; StructuralEquality; NoComparison>] GroupState =
        { Id : Guid
          Name : string
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : GroupDispatcher
          Xtension : Xtension }

        interface SimulantState

        /// Make a group state value.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Hosts the ongoing state of an entity. Should rarely be accessed directly by end-users.
    and [<CLIMutable; StructuralEquality; NoComparison>] EntityState =
        { Id : Guid
          Name : string
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Visible : bool
          ViewType : ViewType
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime // just needed for ordering writes to reduce diff volumes
          DispatcherNp : EntityDispatcher
          FacetNames : string list
          FacetsNp : Facet list
          OptOverlayName : string option
          Xtension : Xtension }

        interface SimulantState

        /// Get the names of all facets used by an entity via reflection.
        /// TODO: see if this should be used as often as it is, and if it is needed in only one or
        /// two cases, just inline it.
        static member getFacetNamesReflectively entityState =
            List.map Reflection.getTypeName entityState.FacetsNp

        /// Query that a facet is compatible with those already being used by an entity.
        /// Note a facet is incompatible with any other facet if it contains any fields that has
        /// the same name but a different type.
        static member isFacetCompatible entityDispatcherMap facet (entityState : EntityState) =
            let facetType = facet.GetType ()
            let facetFieldDefinitions = Reflection.getFieldDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Map.tryFind definition.FieldName entityState.Xtension.XFields with
                        | Some field -> field.GetType () <> definition.FieldType
                        | None -> false)
                    facetFieldDefinitions
            else false

        /// Make an entity state value.
        static member make dispatcher optOverlayName optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              Position = Vector2.Zero
              Depth = 0.0f
              Size = DefaultEntitySize
              Rotation = 0.0f
              Visible = true
              ViewType = Relative
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              FacetNames = []
              FacetsNp = []
              OptOverlayName = optOverlayName
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}
            
    /// A marker interface for the simulation types (Game, Screen, Group, and Entity).
    /// The only methods that have a place in here are those used internally by Nu's event system.
    and Simulant =
        interface
            /// Get the entity's publishing priority.
            abstract GetPublishingPriority : (Entity -> World -> single) -> World -> single
            /// Get the simulant's address.
            abstract SimulantAddress : SimulantState Address
        end

    /// The game type that hosts the various screens used to navigate through a game.
    and [<StructuralEquality; NoComparison>] Game =
        { GameAddress : GameState Address }
        interface Simulant with
            member this.GetPublishingPriority _ _ = GamePublishingPriority
            member this.SimulantAddress = Address.changeType<GameState, SimulantState> this.GameAddress
        end
        static member proxy address = { GameAddress = address }

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<StructuralEquality; NoComparison>] Screen =
        { ScreenAddress : ScreenState Address }
        interface Simulant with
            member this.GetPublishingPriority _ _ = ScreenPublishingPriority
            member this.SimulantAddress = Address.changeType<ScreenState, SimulantState> this.ScreenAddress
        end
        static member proxy address = { ScreenAddress = address }

    /// Forms a logical group of entities.
    and [<StructuralEquality; NoComparison>] Group =
        { GroupAddress : GroupState Address }
        interface Simulant with
            member this.GetPublishingPriority _ _ = GroupPublishingPriority
            member this.SimulantAddress = Address.changeType<GroupState, SimulantState> this.GroupAddress
        end
        static member proxy address = { GroupAddress = address }

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    and [<StructuralEquality; NoComparison>] Entity =
        { EntityAddress : EntityState Address }
        interface Simulant with
            member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
            member this.SimulantAddress = Address.changeType<EntityState, SimulantState> this.EntityAddress
        end
        static member proxy address = { EntityAddress = address }

    /// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
    /// specific values.
    and NuPlugin () =

        /// Make user-defined subsystems such that Nu can utilitze them at run-time.
        abstract MakeSubsystems : unit -> (string * Subsystem) list
        default this.MakeSubsystems () = []
        
        /// Optionally make a user-defined game dispatchers such that Nu can utililize it at run-time.
        abstract MakeOptGameDispatcher : unit -> GameDispatcher option
        default this.MakeOptGameDispatcher () = None
        
        /// Make user-defined screen dispatchers such that Nu can utililize them at run-time.
        abstract MakeScreenDispatchers : unit -> ScreenDispatcher list
        default this.MakeScreenDispatchers () = []
        
        /// Make user-defined group dispatchers such that Nu can utililize them at run-time.
        abstract MakeGroupDispatchers : unit -> GroupDispatcher list
        default this.MakeGroupDispatchers () = []
        
        /// Make user-defined entity dispatchers such that Nu can utililize them at run-time.
        abstract MakeEntityDispatchers : unit -> EntityDispatcher list
        default this.MakeEntityDispatchers () = []
        
        /// Make user-defined assets such that Nu can utililize them at run-time.
        abstract MakeFacets : unit -> Facet list
        default this.MakeFacets () = []
        
        /// Make the overlay routes that will allow Nu to use different overlays for the specified
        /// types. For example, a returned router of (typeof<ButtonDispatcher>.Name, Some "CustomButtonOverlay")
        /// will cause all buttons to use the overlay with the name "CustomButtonOverlay" rather
        /// than the default "ButtonDispatcher" overlay.
        abstract MakeOverlayRoutes : unit -> (string * string option) list
        default this.MakeOverlayRoutes () = []

    /// Represents an untyped message to a subsystem.
    and SubsystemMessage = obj

    /// Represents an untype result of a subsystem.
    and SubsystemResult = obj

    /// The type of subsystem. Dictates where subsystem's processing happens in the game loop.
    and SubsystemType =
        | UpdateType
        | RenderType
        | AudioType

    /// Represents a subsystem by which additional engine-level subsystems such as AI, optimized
    /// special FX, and the like can be added.
    and Subsystem =
        interface
            /// The type of subsystem. Dictates where its processing happens in the game loop.
            abstract SubsystemType : SubsystemType
            /// The ordering by which the subsystem will be processed relative to other subsystems of the same type.
            abstract SubsystemOrder : single
            /// Clear the messages queued by subsystem.
            abstract ClearMessages : unit -> Subsystem
            /// Enqueue a message for the subsystem.
            abstract EnqueueMessage : SubsystemMessage -> Subsystem
            /// Processed the queued messages with the subsystem.
            abstract ProcessMessages : World -> SubsystemResult * Subsystem
            /// Apply the result of the message processing to the world.
            abstract ApplyResult : SubsystemResult -> World -> World
            /// Clean up any resources used by the subsystem.
            abstract CleanUp : World -> Subsystem * World
        end

    /// The world's subsystems.
    and Subsystems = Map<string, Subsystem>

    /// The world's components.
    and [<ReferenceEquality>] Components =
        { EntityDispatchers : Map<string, EntityDispatcher>
          GroupDispatchers : Map<string, GroupDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          GameDispatchers : Map<string, GameDispatcher>
          Facets : Map<string, Facet> }

    /// The world's simple callback facilities.
    and [<ReferenceEquality>] Callbacks =
        { Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          CallbackStates : Map<Guid, obj> }

    /// The world's state.
    /// TODO: Provide an indirect accessor.
    and [<ReferenceEquality>] WorldState =
        { TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          OptScreenTransitionDestination : Screen option
          Camera : Camera // TODO: move out of World state and give it it's own indirect accessor
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFilePath : string
          Overlayer : Overlayer
          OverlayRouter : OverlayRouter
          OverlayFilePath : string
          UserState : obj } // TODO: consider also moving out of world state and providing an indirect accessor

    and Game with
        
        member this.GetId world = (World.getGameState world).Id
        member this.GetCreationTimeNp world = (World.getGameState world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getGameState world).DispatcherNp
        member this.GetOptSelectedScreen world = (World.getGameState world).OptSelectedScreen
        member this.SetOptSelectedScreen value world = World.updateGameState (fun (gameState : GameState) -> { gameState with OptSelectedScreen = value }) world
        member this.GetPublishChanges world = (World.getGameState world).PublishChanges
        member this.SetPublishChanges value world = World.updateGameState (fun gameState -> { gameState with PublishChanges = value }) world
        member this.GetXtension world = (World.getGameState world).Xtension
        member this.SetXtension value world = World.updateGameState (fun gameState -> { gameState with Xtension = value }) world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// Query that a game dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and Screen with

        member this.GetId world = (World.getScreenState this world).Id
        member this.GetName world = (World.getScreenState this world).Name
        member this.GetCreationTimeNp world = (World.getScreenState this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getScreenState this world).DispatcherNp
        member this.GetTransitionStateNp world = (World.getScreenState this world).TransitionStateNp
        member this.SetTransitionStateNp value world = World.updateScreenState (fun screenState -> { screenState with TransitionStateNp = value }) this world
        member this.GetTransitionTicksNp world = (World.getScreenState this world).TransitionTicksNp
        member this.SetTransitionTicksNp value world = World.updateScreenState (fun screenState -> { screenState with TransitionTicksNp = value }) this world
        member this.GetIncoming world = (World.getScreenState this world).Incoming
        member this.SetIncoming value world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) this world
        member this.GetOutgoing world = (World.getScreenState this world).Outgoing
        member this.SetOutgoing value world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) this world
        member this.GetPublishChanges world = (World.getScreenState this world).PublishChanges
        member this.SetPublishChanges value world = World.updateScreenState (fun (screenState : ScreenState) -> { screenState with PublishChanges = value }) this world
        member this.GetPersistent world = (World.getScreenState this world).Persistent
        member this.SetPersistent value world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) this world
        member this.GetXtension world = (World.getScreenState this world).Xtension
        member this.SetXtension value world = World.updateScreenState (fun screenState -> { screenState with Xtension = value }) this world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// Query that a screen is in an idling state (not transitioning in nor out).
        member this.IsIdling world =
            this.GetTransitionStateNp world = IdlingState

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and Group with

        member this.GetId world = (World.getGroupState this world).Id
        member this.GetName world = (World.getGroupState this world).Name
        member this.GetCreationTimeNp world = (World.getGroupState this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getGroupState this world).DispatcherNp
        member this.GetPublishChanges world = (World.getGroupState this world).PublishChanges
        member this.SetPublishChanges value world = World.updateGroupState (fun (groupState : GroupState) -> { groupState with PublishChanges = value }) this world
        member this.GetPersistent world = (World.getGroupState this world).Persistent
        member this.SetPersistent value world = World.updateGroupState (fun groupState -> { groupState with Persistent = value }) this world
        member this.GetXtension world = (World.getGroupState this world).Xtension
        member this.SetXtension xtension world = World.updateGroupState (fun groupState -> { groupState with Xtension = xtension}) this world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and Entity with
        
        member this.GetId world = (World.getEntityState this world).Id
        member this.GetName world = (World.getEntityState this world).Name
        member this.GetCreationTimeNp world = (World.getEntityState this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getEntityState this world).DispatcherNp
        member this.GetFacetNames world = (World.getEntityState this world).FacetNames
        member this.GetFacetsNp world = (World.getEntityState this world).FacetsNp
        member this.GetPosition world = (World.getEntityState this world).Position
        member this.SetPosition value world = World.updateEntityState (fun (entityState : EntityState) -> { entityState with Position = value }) this world
        member this.GetDepth world = (World.getEntityState this world).Depth
        member this.SetDepth value world = World.updateEntityState (fun entityState -> { entityState with Depth = value }) this world
        member this.GetSize world = (World.getEntityState this world).Size
        member this.SetSize value world = World.updateEntityState (fun entityState -> { entityState with Size = value }) this world
        member this.GetRotation world = (World.getEntityState this world).Rotation
        member this.SetRotation value world = World.updateEntityState (fun entityState -> { entityState with Rotation = value }) this world
        member this.GetVisible world = (World.getEntityState this world).Visible
        member this.SetVisible value world = World.updateEntityState (fun entityState -> { entityState with Visible = value }) this world
        member this.GetViewType world = (World.getEntityState this world).ViewType
        member this.SetViewType value world = World.updateEntityState (fun entityState -> { entityState with ViewType = value }) this world
        member this.GetPublishChanges world = (World.getEntityState this world).PublishChanges
        member this.SetPublishChanges value world = World.updateEntityState (fun entityState -> { entityState with PublishChanges = value }) this world
        member this.GetPersistent world = (World.getEntityState this world).Persistent
        member this.SetPersistent value world = World.updateEntityState (fun entityState -> { entityState with Persistent = value }) this world
        member this.GetOptOverlayName world = (World.getEntityState this world).OptOverlayName
        member this.SetOptOverlayName value world = World.updateEntityState (fun entityState -> { entityState with OptOverlayName = value }) this world
        member this.GetXtension world = (World.getEntityState this world).Xtension
        member this.SetXtension xtension world = World.updateEntityState (fun entityState -> { entityState with Xtension = xtension}) this world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// TODO: document!
        member this.GetTransform world : Transform =
            { Position = this.GetPosition world
              Depth = this.GetDepth world
              Size = this.GetSize world
              Rotation = this.GetRotation world }

        /// TODO: document!
        member this.SetTransform positionSnap rotationSnap transform world =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            world |>
                this.SetPosition transform.Position |>
                this.SetDepth transform.Depth |>
                this.SetSize transform.Size |>
                this.SetRotation transform.Rotation

        /// TODO: document!
        member this.SetPositionSnapped snap position world =
            let snapped = Math.snap2F snap position
            this.SetPosition snapped world

        /// Query that an entity dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    ///
    /// TODO: attempt to implement with Fsharpx.PersistentHashMap with hash cached in Address type.
    and [<ReferenceEquality>] World =
        { SimulantStates : GameState * Map<string, ScreenState * Map<string, GroupState * Map<string, EntityState>>>
          Subsystems : Subsystems
          Components : Components
          Callbacks : Callbacks
          State : WorldState }

        (* Simulant Address Converters *)
        
        static member internal atoua address = Address.changeType<'a, SimulantState> address
        static member internal atosa address = Address.changeType<'a, ScreenState> address
        static member internal atoga address = Address.changeType<'a, GroupState> address
        static member internal atoea address = Address.changeType<'a, EntityState> address
        static member internal gatoea groupAddress entityName = Address.changeType<GroupState, EntityState> groupAddress ->- ntoa entityName
        static member internal satoga screenAddress groupName = Address.changeType<ScreenState, GroupState> screenAddress ->- ntoa groupName
        static member internal satoea screenAddress groupName entityName = World.gatoea (World.satoga screenAddress groupName) entityName
        static member internal eatoga entityAddress = Address.take<EntityState, GroupState> 2 entityAddress
        static member internal gatosa groupAddress = Address.take<GroupState, ScreenState> 1 groupAddress
        static member internal eatosa entityAddress = Address.take<EntityState, ScreenState> 1 entityAddress

        (* World Constants *)
        
        static member internal Game = Game.proxy Address.empty
        static member internal DefaultScreen = Screen.proxy <| ntoa DefaultScreenName
        static member internal DefaultGroup = Group.proxy <| World.satoga World.DefaultScreen.ScreenAddress DefaultGroupName
        static member internal DefaultEntity = Entity.proxy <| World.gatoea World.DefaultGroup.GroupAddress DefaultEntityName
        static member internal AnyEventAddress = ntoa<obj> "*"
        static member internal TickEventAddress = ntoa<unit> "Tick"
        static member internal SelectEventAddress = ntoa<unit> "Select"
        static member internal DeselectEventAddress = ntoa<unit> "Deselect"
        static member internal DownEventAddress = ntoa<unit> "Down"
        static member internal UpEventAddress = ntoa<unit> "Up"
        static member internal ClickEventAddress = ntoa<unit> "Click"
        static member internal OnEventAddress = ntoa<unit> "On"
        static member internal OffEventAddress = ntoa<unit> "Off"
        static member internal TouchEventAddress = ntoa<Vector2> "Touch"
        static member internal UntouchEventAddress = ntoa<Vector2> "Untouch"
        static member internal MouseEventAddress = ntoa<obj> "Mouse"
        static member internal MouseMoveEventAddress = World.MouseEventAddress -<- ntoa<MouseMoveData> "Move"
        static member internal MouseDragEventAddress = World.MouseEventAddress -<- ntoa<MouseMoveData> "Drag"
        static member internal MouseLeftEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Left"
        static member internal MouseCenterEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Center"
        static member internal MouseRightEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Right"
        static member internal MouseX1EventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "X1"
        static member internal MouseX2EventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "X2"
        static member internal MouseLeftDownEventAddress = World.MouseLeftEventAddress -|- ntoa "Down"
        static member internal MouseLeftUpEventAddress = World.MouseLeftEventAddress -|- ntoa "Up"
        static member internal MouseLeftChangeEventAddress = World.MouseLeftEventAddress -|- ntoa "Change"
        static member internal MouseCenterDownEventAddress = World.MouseCenterEventAddress -|- ntoa "Down"
        static member internal MouseCenterUpEventAddress = World.MouseCenterEventAddress -|- ntoa "Up"
        static member internal MouseCenterChangeEventAddress = World.MouseCenterEventAddress -|- ntoa "Change"
        static member internal MouseRightDownEventAddress = World.MouseRightEventAddress -|- ntoa "Down"
        static member internal MouseRightUpEventAddress = World.MouseRightEventAddress -|- ntoa "Up"
        static member internal MouseRightChangeEventAddress = World.MouseRightEventAddress -|- ntoa "Change"
        static member internal MouseX1DownEventAddress = World.MouseX1EventAddress -|- ntoa "Down"
        static member internal MouseX1UpEventAddress = World.MouseX1EventAddress -|- ntoa "Up"
        static member internal MouseX1ChangeEventAddress = World.MouseX1EventAddress -|- ntoa "Change"
        static member internal MouseX2DownEventAddress = World.MouseX2EventAddress -|- ntoa "Down"
        static member internal MouseX2UpEventAddress = World.MouseX2EventAddress -|- ntoa "Up"
        static member internal MouseX2ChangeEventAddress = World.MouseX2EventAddress -|- ntoa "Change"
        static member internal KeyboardKeyEventAddress = ntoa<obj> "KeyboardKey"
        static member internal KeyboardKeyDownEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Down"
        static member internal KeyboardKeyUpEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Up"
        static member internal KeyboardKeyChangeEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Change"
        static member internal CollisionEventAddress = ntoa<CollisionData> "Collision"
        static member internal IncomingEventAddress = ntoa<unit> "Incoming"
        static member internal IncomingStartEventAddress = World.IncomingEventAddress -|- ntoa "Start"
        static member internal IncomingFinishEventAddress = World.IncomingEventAddress -|- ntoa "Finish"
        static member internal OutgoingEventAddress = ntoa<unit> "Outgoing"
        static member internal OutgoingStartEventAddress = World.OutgoingEventAddress -|- ntoa "Start"
        static member internal OutgoingFinishEventAddress = World.OutgoingEventAddress -|- ntoa "Finish"
        static member internal WorldStateEventAddress = ntoa<obj> "WorldState"
        static member internal WorldStateChangeEventAddress = World.WorldStateEventAddress -<- ntoa<WorldStateChangeData> "Change"
        static member internal GameEventAddress = ntoa<obj> "Game"
        static member internal GameChangeEventAddress = World.GameEventAddress -<- ntoa<Game SimulantChangeData> "Change"
        static member internal ScreenEventAddress = ntoa<obj> "Screen"
        static member internal ScreenAddEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Add"
        static member internal ScreenRemovingEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Removing"
        static member internal ScreenChangeEventAddress = World.ScreenEventAddress -<- ntoa<Screen SimulantChangeData> "Change"
        static member internal GroupEventAddress = ntoa<obj> "Group"
        static member internal GroupAddEventAddress = World.GroupEventAddress -<- ntoa<unit> "Add"
        static member internal GroupRemovingEventAddress = World.GroupEventAddress -<- ntoa<unit> "Removing"
        static member internal GroupChangeEventAddress = World.GroupEventAddress -<- ntoa<Group SimulantChangeData> "Change"
        static member internal EntityEventAddress = ntoa<obj> "Entity"
        static member internal EntityAddEventAddress = World.EntityEventAddress -<- ntoa<unit> "Add"
        static member internal EntityRemovingEventAddress = World.EntityEventAddress -<- ntoa<unit> "Removing"
        static member internal EntityChangeEventAddress = World.EntityEventAddress -<- ntoa<Entity SimulantChangeData> "Change"
        static member internal DefaultDissolveImage = { PackageName = DefaultPackageName; AssetName = "Image8" }

        (* Publishing *)

        static member private sortFstDesc (priority : single, _) (priority2 : single, _) =
            // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
            if priority > priority2 then -1
            elif priority < priority2 then 1
            else 0

        static member private getAnyEventAddresses eventAddress =
            // OPTIMIZATION: uses memoization.
            if not <| Address.isEmpty eventAddress then
                let anyEventAddressesKey = Address.allButLast eventAddress
                match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
                | (true, anyEventAddresses) -> anyEventAddresses
                | (false, _) ->
                    let eventAddressList = eventAddress.Names
                    let anyEventAddressList = World.AnyEventAddress.Names
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressList - 1 do
                            let subNameList = List.take i eventAddressList @ anyEventAddressList
                            yield Address.make subNameList]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world : (single * SubscriptionEntry) list =
            List.foldBack
                (fun (key, simulant : Simulant, subscription) subscriptions ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, simulant, subscription))
                    subscription :: subscriptions)
                subscriptions
                []

        static member private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            publishSorter subList world
    
        static member private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
            let boxableSubscription = fun (event : obj) world ->
                try subscription (event :?> Event<'a, 's>) world
                with
                | :? InvalidCastException ->
                    // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                    // up an event type parameter for some form of World.publish or subscribe.
                    reraise ()
                | _ -> reraise ()
            box boxableSubscription

        static member private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
            (subscriber : Simulant) (publisher : 'p) (eventAddress : 'a Address) (eventData : 'a) subscription world =
            let event =
                { Subscriber = subscriber :?> 's
                  Publisher = publisher :> Simulant
                  EventAddress = eventAddress
                  Data = eventData }
            let callableSubscription = unbox<BoxableSubscription> subscription
            let result = callableSubscription event world
            Some result

        /// Make a key used to track an unsubscription with a subscription.
        static member makeSubscriptionKey () = Guid.NewGuid ()

        /// Make a callback key used to track callback states.
        static member makeCallbackKey () = Guid.NewGuid ()

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
            let subscriptions = World.getSortableSubscriptions by subscriptions world
            let subscriptions = List.sortWith World.sortFstDesc subscriptions
            List.map snd subscriptions

        /// Sort subscriptions by their editor picking priority.
        static member sortSubscriptionsByPickingPriority subscriptions world =
            World.sortSubscriptionsBy
                (fun (entity : Entity) world ->
                    let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
                    dispatcher.GetPickingPriority entity world)
                subscriptions
                world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
            subscriptions

        /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
        static member publish<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            let objEventAddress = atooa eventAddress
            let subscriptions = World.getSubscriptionsSorted publishSorter objEventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriber : Simulant, subscription) ->
                        if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                            (match world.State.Liveness with Running -> true | Exiting -> false) then
                            match subscriber.SimulantAddress.Names with
                            | [] -> World.publishEvent<'a, 'p, Game> subscriber publisher eventAddress eventData subscription world
                            | [_] -> World.publishEvent<'a, 'p, Screen> subscriber publisher eventAddress eventData subscription world
                            | [_; _] -> World.publishEvent<'a, 'p, Group> subscriber publisher eventAddress eventData subscription world
                            | [_; _; _] -> World.publishEvent<'a, 'p, Entity> subscriber publisher eventAddress eventData subscription world
                            | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                        else None)
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            World.publish World.sortSubscriptionsByHierarchy eventData eventAddress publisher world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty eventAddress then
                let objEventAddress = atooa eventAddress
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, subscriber :> Simulant, World.boxSubscription subscription)
                    match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (objEventAddress, subscriber :> Simulant) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe4<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribe (World.makeSubscriptionKey ()) subscription eventAddress subscriber world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
            | Some (eventAddress, subscriber) ->
                match Map.tryFind eventAddress world.Callbacks.Subscriptions with
                | Some subscriptionList ->
                    let subscriptionList =
                        List.remove
                            (fun (subscriptionKey', subscriber', _) ->
                                subscriptionKey' = subscriptionKey &&
                                subscriber' = subscriber)
                            subscriptionList
                    let subscriptions = 
                        match subscriptionList with
                        | [] -> Map.remove eventAddress world.Callbacks.Subscriptions
                        | _ -> Map.add eventAddress subscriptionList world.Callbacks.Subscriptions
                    let unsubscriptions = Map.remove subscriptionKey world.Callbacks.Unsubscriptions
                    let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                    { world with Callbacks = callbacks }
                | None -> world
            | None -> world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty subscriber.SimulantAddress then
                let monitorKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe<'a, 's> monitorKey subscription eventAddress subscriber world
                let subscription' = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe monitorKey world
                    (Cascade, world)
                let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriber.SimulantAddress
                World.subscribe<unit, 's> removalKey subscription' removingEventAddress subscriber world
            else failwith "Cannot monitor events with an anonymous subscriber."

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)

        (* Subsystems *)

        static member internal getSubsystem<'s when 's :> Subsystem> name world =
            Map.find name world.Subsystems :?> 's

        static member internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
            let subsystem = World.getSubsystem<'s> name world
            by subsystem

        static member internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
            let subsystems = Map.add name (subsystem :> Subsystem) world.Subsystems
            { world with Subsystems = subsystems }

        static member internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
            let subsystem = World.getSubsystem<'s> name world
            let subsystem = updater subsystem world
            World.setSubsystem subsystem name world

        static member internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
            Map.fold
                (fun world name subsystem -> let subsystem = updater subsystem world in World.setSubsystem subsystem name world)
                world
                world.Subsystems

        static member internal clearSubsystemsMessages world =
            World.updateSubsystems (fun is _ -> is.ClearMessages ()) world

        /// Add a physics message to the world.
        static member addPhysicsMessage (message : PhysicsMessage) world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

        /// Add a rendering message to the world.
        static member addRenderMessage (message : RenderMessage) world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

        (* Callbacks *)

        static member internal clearTasks world =
            let callbacks = { world.Callbacks with Tasks = [] }
            { world with Callbacks = callbacks }

        static member internal restoreTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
            { world with Callbacks = callbacks }

        /// Add a task to be executed by the engine at the specified task tick.
        static member addTask task world =
            let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add multiple task to be executed by the engine at the specified task tick.
        static member addTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add callback state to the world.
        static member addCallbackState key state world =
            let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Remove callback state from the world.
        static member removeCallbackState key world =
            let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Get callback state from the world.
        static member getCallbackState<'a> key world =
            let state = Map.find key world.Callbacks.CallbackStates
            state :?> 'a

        (* WorldState *)

        /// Get the state of the world.
        static member getState world =
            world.State

        /// Set the state of the world.
        static member setState state world =
            let oldState = world.State
            let world = { world with State = state }
            World.publish4 { OldWorldState = oldState } World.WorldStateChangeEventAddress World.Game world

        /// Update the state of the world and the world.
        static member updateStateAndW updater world =
            let (state, world) = updater world.State world
            World.setState state world

        /// Update the state of the world.
        static member updateStateW updater world =
            World.updateStateAndW (fun state world -> (updater state world, world)) world

        /// Update the state of the world.
        static member updateState updater world =
            World.updateStateW (fun state _ -> updater state) world

        /// Update the world by its state.
        static member updateByState updater world : World =
            updater world.State world

        /// Lens the state of the world.
        static member lensState =
            { Get = World.getState
              Set = World.setState }

        /// Get the world's tick time.
        static member getTickTime world =
            world.State.TickTime

        static member internal incrementTickTime world =
            let state = { world.State with TickTime = world.State.TickTime + 1L }
            World.setState state world

        /// Get the the liveness state of the world.
        static member getLiveness world =
            world.State.Liveness

        /// Place the engine into a state such that the app will exit at the end of the current tick.
        static member exit world =
            let state = { world.State with Liveness = Exiting }
            World.setState state world

        /// Query that the engine is in game-playing mode.
        static member isGamePlaying world =
            Interactivity.isGamePlaying world.State.Interactivity

        /// Query that the physics system is running.
        static member isPhysicsRunning world =
            Interactivity.isPhysicsRunning world.State.Interactivity

        /// Get the interactivity state of the world.
        static member getInteractivity world =
            world.State.Interactivity

        /// Set the level of the world's interactivity.
        static member setInteractivity interactivity world =
            let state = { world.State with Interactivity = interactivity }
            World.setState state world

        /// Update the the level of the world's interactivity.
        static member updateInteractivity updater world =
            let interactivity = updater <| World.getInteractivity world
            World.setInteractivity interactivity world

        /// Get the camera used to view the world.
        static member getCamera world =
            world.State.Camera

        /// Set the camera used to view the world.
        static member setCamera camera world =
            let state = { world.State with Camera = camera }
            World.setState state world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            let camera = updater <| World.getCamera world
            World.setCamera camera world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            world.State.OptScreenTransitionDestination

        static member internal setOptScreenTransitionDestination destination world =
            let state = { world.State with OptScreenTransitionDestination = destination }
            World.setState state world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            world.State.AssetMetadataMap

        static member internal setAssetMetadataMap assetMetadataMap world =
            let state = { world.State with AssetMetadataMap = assetMetadataMap }
            World.setState state world

        static member internal setOverlayer overlayer world =
            let state = { world.State with Overlayer = overlayer }
            World.setState state world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            world.State.UserState :?> 'u

        /// Set the user state of the world.
        static member setUserState (userState : 'u) world =
            let state = { world.State with UserState = userState }
            World.setState state world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            let state = World.getUserState world
            let state = updater state
            World.setUserState state world

        (* Facet / Entity internals *)

        static member private tryGetFacet facetName world =
            match Map.tryFind facetName world.Components.Facets with
            | Some facet -> Right <| facet
            | None -> Left <| "Invalid facet name '" + facetName + "'."

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToAdd = Set.difference newFacetNames oldFacetNames
            List.ofSeq facetNamesToAdd

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToRemove = Set.difference oldFacetNames newFacetNames
            List.ofSeq facetNamesToRemove

        static member private getEntityFieldDefinitionNamesToDetach entityState facetToRemove =

            // get the field definition name counts of the current, complete entity
            let fieldDefinitions = Reflection.getReflectiveFieldDefinitionMap entityState
            let fieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts fieldDefinitions

            // get the field definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetFieldDefinitions = Map.singleton facetType.Name <| Reflection.getFieldDefinitions facetType
            let facetFieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts facetFieldDefinitions

            // compute the difference of the counts
            let finalFieldDefinitionNameCounts =
                Map.map
                    (fun fieldName fieldCount ->
                        match Map.tryFind fieldName facetFieldDefinitionNameCounts with
                        | Some facetFieldCount -> fieldCount - facetFieldCount
                        | None -> fieldCount)
                    fieldDefinitionNameCounts

            // build a set of all field names where the final counts are negative
            Map.fold
                (fun fieldNamesToDetach fieldName fieldCount ->
                    if fieldCount = 0
                    then Set.add fieldName fieldNamesToDetach
                    else fieldNamesToDetach)
                Set.empty
                finalFieldDefinitionNameCounts

        static member private tryRemoveFacet syncing facetName entityState optEntity world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match optEntity with
                    | Some entity ->
                        let world = facet.Unregister entity world
                        (World.getEntityState entity world, world)
                    | None -> (entityState, world)
                let entityState = { entityState with Id = entityState.Id } // hacky copy
                let fieldNames = World.getEntityFieldDefinitionNamesToDetach entityState facet
                Reflection.detachFieldsViaNames fieldNames entityState
                let entityState =
                    if syncing then entityState
                    else { entityState with FacetNames = List.remove ((=) (Reflection.getTypeName facet)) entityState.FacetNames }
                let entityState = { entityState with FacetsNp = List.remove ((=) facet) entityState.FacetsNp }
                let world =
                    match optEntity with
                    | Some entity -> World.setEntityState entityState entity world
                    | None -> world
                Right (entityState, world)
            | None -> Left <| "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet syncing facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                if EntityState.isFacetCompatible world.Components.EntityDispatchers facet entityState then
                    let entityState = { entityState with FacetsNp = facet :: entityState.FacetsNp }
                    Reflection.attachFields facet entityState
                    let entityState =
                        if syncing then entityState
                        else { entityState with FacetNames = Reflection.getTypeName facet :: entityState.FacetNames }
                    match optEntity with
                    | Some entity ->
                        let world = facet.Register entity world
                        let entityState = World.getEntityState entity world
                        Right (entityState, world)
                    | None -> Right (entityState, world)
                else Left <| "Facet '" + Reflection.getTypeName facet + "' is incompatible with entity '" + entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets syncing facetNamesToRemove entityState optEntity world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet syncing facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets syncing facetNamesToAdd entityState optEntity world =
            List.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet syncing facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member private trySetFacetNames oldFacetNames newFacetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames newFacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames newFacetNames
            match World.tryRemoveFacets false facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets false facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal trySynchronizeFacets oldFacetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entityState.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entityState.FacetNames
            match World.tryRemoveFacets true facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets true facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member private attachIntrinsicFacetsViaNames (entityState : EntityState) world =
            let components = world.Components
            let entityState = { entityState with Id = entityState.Id } // hacky copy
            Reflection.attachIntrinsicFacets components.EntityDispatchers components.Facets entityState.DispatcherNp entityState
            entityState

        (* Entity *)

        static member private optEntityStateFinder entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) -> Map.tryFind entityName entityStateMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateAdder (entityState : EntityState) entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.add entityName entityState entityStateMap
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateRemover entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.remove entityName entityStateMap
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private getEntityStateMap group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) -> entityStateMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member internal getEntityState (entity : Entity) world =
            Option.get ^ World.getOptEntityState entity world

        static member internal setEntityStateWithoutEvent entityState entity world =
            World.entityStateAdder entityState entity world

        static member internal setOptEntityStateWithoutEvent optEntityState entity world =
            match optEntityState with 
            | Some entityState -> World.entityStateAdder entityState entity world
            | None -> World.entityStateRemover entity world

        static member internal setEntityState entityState (entity : Entity) world =
            let oldWorld = world
            let world = World.entityStateAdder entityState entity world
            if entityState.PublishChanges then
                World.publish4
                    { Simulant = entity; OldWorld = oldWorld }
                    (World.EntityChangeEventAddress ->>- entity.EntityAddress)
                    entity
                    world
            else world

        static member internal updateEntityState updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

        static member private addEntityState entityState entity world =
            if not <| World.containsEntity entity world then
                let world = World.setEntityStateWithoutEvent entityState entity world
                let world = World.registerEntity entity world
                World.publish4 () (World.EntityAddEventAddress ->>- entity.EntityAddress) entity world
            else failwith <| "Adding an entity that the world already contains at address '" + acstring entity.EntityAddress + "'."

        static member private registerEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let world = dispatcher.Register entity world
            List.fold (fun world (facet : Facet) -> facet.Register entity world) world facets

        static member private unregisterEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let world = dispatcher.Unregister entity world
            List.fold (fun world (facet : Facet) -> facet.Unregister entity world) world facets

        /// Query that the world contains an entity.
        static member containsEntity entity world =
            Option.isSome <| World.optEntityStateFinder entity world

        /// Get all the entities contained by a group.
        static member getEntities group world =
            let entityStateMap = World.getEntityStateMap group world
            Seq.map (fun (kvp : KeyValuePair<string, _>) -> Entity.proxy <| World.gatoea group.GroupAddress kvp.Key) entityStateMap

        // Get all the entities in the world.
        static member getEntities1 world =
            World.getGroups1 world |>
            Seq.map (fun group -> World.getEntities group world) |>
            Seq.concat

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight
        /// publishing depends on the entity's existence. Use with caution.
        static member destroyEntityImmediate entity world =
            let world = World.publish4 () (World.EntityRemovingEventAddress ->>- entity.EntityAddress) entity world
            if World.containsEntity entity world then
                let world = World.unregisterEntity entity world
                World.setOptEntityStateWithoutEvent None entity world
            else world

        /// Destroy an entity in the world on the next tick. Use this rather than
        /// destroyEntityImmediate unless you need the latter's specific behavior.
        static member destroyEntity entity world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.destroyEntityImmediate entity world }
            World.addTask task world

        /// Destroy multiple entities in the world immediately. Can be dangerous if existing
        /// in-flight publishing depends on any of the entities' existences. Use with caution.
        static member destroyEntitiesImmediate entities world =
            List.foldBack
                (fun entity world -> World.destroyEntityImmediate entity world)
                (List.ofSeq entities)
                world

        /// Destroy multiple entities in the world. Use this rather than destroyEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member destroyEntities entities world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.destroyEntitiesImmediate entities world }
            World.addTask task world

        /// Create an entity and add it to the world.
        static member createEntity dispatcherName optName group world =
            
            // find the entity's dispatcher
            let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
            
            // compute the default opt overlay name
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = Map.find intrinsicOverlayName world.State.OverlayRouter

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make dispatcher defaultOptOverlayName optName

            // attach the entity state's intrinsic facets and their fields
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let entityState =
                match defaultOptOverlayName with
                | Some defaultOverlayName ->
                    let overlayer = world.State.Overlayer
                    Overlayer.applyOverlayToFacetNames intrinsicOverlayName defaultOverlayName entityState overlayer overlayer
                        
                    // synchronize the entity's facets (and attach their fields)
                    match World.trySynchronizeFacets [] entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher fields
            Reflection.attachFields dispatcher entityState

            // apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->

                    // OPTIMIZATION: apply overlay only when it will change something (EG - when it's not the intrinsic overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = EntityState.getFacetNamesReflectively entityState
                        Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entityState world.State.Overlayer
                        entityState
                    else entityState
                | None -> entityState

            // add entity's state to world
            let entity = Entity.proxy <| World.gatoea group.GroupAddress entityState.Name
            let world = World.addEntityState entityState entity world
            (entity, world)

        /// Write an entity to an xml writer.
        static member writeEntity (writer : XmlWriter) (entity : Entity) world =
            let entityState = World.getEntityState entity world
            let dispatcherTypeName = Reflection.getTypeName entityState.DispatcherNp
            writer.WriteAttributeString (DispatcherNameAttributeName, dispatcherTypeName)
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let defaultOptOverlayName = Map.find dispatcherTypeName world.State.OverlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let facetNames = EntityState.getFacetNamesReflectively entityState
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entityState world.State.Overlayer
            Reflection.writePropertiesFromTarget shouldWriteProperty writer entityState

        /// Write multiple entities to an xml writer.
        static member writeEntities (writer : XmlWriter) entities world =
            let entitiesSorted = Seq.sortBy (fun (entity : Entity) -> entity.GetCreationTimeNp world) entities
            let entitiesPersistent = Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) entitiesSorted
            for entity in entitiesPersistent do
                writer.WriteStartElement typeof<Entity>.Name
                World.writeEntity writer entity world
                writer.WriteEndElement ()

        /// Read an entity from an xml node.
        static member readEntity entityNode defaultDispatcherName optName group world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName entityNode
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName world.Components.EntityDispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
                    (dispatcherName, dispatcher)

            // compute the default overlay names
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = Map.find intrinsicOverlayName world.State.OverlayRouter

            // make the bare entity state with name as id
            let entityState = EntityState.make dispatcher defaultOptOverlayName None

            // attach the entity state's intrinsic facets and their fields
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            Reflection.tryReadOptOverlayNameToTarget entityNode entityState
            match (defaultOptOverlayName, entityState.OptOverlayName) with
            | (Some defaultOverlayName, Some overlayName) ->
                let overlayer = world.State.Overlayer
                Overlayer.applyOverlayToFacetNames defaultOverlayName overlayName entityState overlayer overlayer
            | (_, _) -> ()

            // read the entity state's facet names
            Reflection.readFacetNamesToTarget entityNode entityState
            
            // synchronize the entity state's facets (and attach their fields)
            let entityState =
                match World.trySynchronizeFacets [] entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> debug error; entityState

            // attach the entity state's dispatcher fields
            Reflection.attachFields dispatcher entityState

            // attempt to apply the entity state's overlay
            match entityState.OptOverlayName with
            | Some overlayName ->

                // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                if intrinsicOverlayName <> overlayName then
                    let facetNames = EntityState.getFacetNamesReflectively entityState
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entityState world.State.Overlayer
                else ()
            | None -> ()

            // read the entity state's properties
            Reflection.readPropertiesToTarget entityNode entityState

            // apply the name if one is provided
            let entityState = match optName with Some name -> { entityState with Name = name } | None -> entityState

            // add entity state to the world
            let entity = Entity.proxy <| World.gatoea group.GroupAddress entityState.Name
            let world = World.destroyEntityImmediate entity world
            let world = World.addEntityState entityState entity world
            (entity, world)

        /// Read multiple entities from an xml node.
        static member readEntities (groupNode : XmlNode) defaultDispatcherName group world =
            match groupNode.SelectSingleNode EntitiesNodeName with
            | null -> ([], world)
            | entitiesNode ->
                let (entitiesRev, world) =
                    Seq.fold
                        (fun (entitiesRev, world) entityNode ->
                            let (entity, world) = World.readEntity entityNode defaultDispatcherName None group world
                            (entity :: entitiesRev, world))
                        ([], world)
                        (enumerable <| entitiesNode.SelectNodes EntityNodeName)
                (List.rev entitiesRev, world)

        /// Propagate an entity's physics properties from the physics subsystem.
        static member propagatePhysics (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world
            let facets = entity.GetFacetsNp world
            let world = dispatcher.PropagatePhysics entity world
            List.fold
                (fun world (facet : Facet) -> facet.PropagatePhysics entity world)
                world
                facets
        
        /// Get the render descriptors needed to render an entity.
        static member getRenderDescriptors (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let renderDescriptors = dispatcher.GetRenderDescriptors entity world
            List.foldBack
                (fun (facet : Facet) renderDescriptors ->
                    let descriptors = facet.GetRenderDescriptors entity world
                    descriptors @ renderDescriptors)
                facets
                renderDescriptors
        
        /// Get the quick size of an entity (the appropriate user-defined size for an entity).
        static member getQuickSize (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let quickSize = dispatcher.GetQuickSize entity world
            List.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize entity world
                    Vector2 (
                        Math.Max (quickSize.X, maxSize.X),
                        Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                facets

        /// Get the priority with which an entity is picked in the editor.
        static member getPickingPriority (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            dispatcher.GetPickingPriority entity world

        /// TODO: document!
        static member pickingSort entities world =
            let entities = List.ofSeq entities
            let prioritiesAndEntities = List.map (fun (entity : Entity) -> (World.getPickingPriority entity world, entity)) entities
            let prioritiesAndEntities = List.sortWith World.sortFstDesc prioritiesAndEntities
            List.map snd prioritiesAndEntities

        /// TODO: document!
        static member tryPick position entities world =
            let entitiesSorted = World.pickingSort entities world
            List.tryFind
                (fun (entity : Entity) ->
                    let positionWorld = Camera.mouseToWorld (entity.GetViewType world) position world.State.Camera
                    let transform = entity.GetTransform world
                    let picked = Math.isPointInBounds3 positionWorld transform.Position transform.Size
                    picked)
                entitiesSorted

        (* Group *)

        static member private optGroupStateFinder group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, _) -> Some groupState
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateAdder (groupState : GroupState) group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) ->
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None ->
                        let groupStateMap = Map.add groupName (groupState, Map.empty) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                | None -> failwith <| "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateRemover group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) ->
                        if Map.isEmpty entityStateMap then
                            let groupStateMap = Map.remove groupName groupStateMap
                            let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                            { world with SimulantStates = (gameState, screenStateMap) }
                        else failwith <| "Cannot remove group " + acstring group.GroupAddress + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getGroupStateMap screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) -> groupStateMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getOptGroupState group world =
            World.optGroupStateFinder group world

        static member internal getGroupState group world : GroupState =
            Option.get ^ World.getOptGroupState group world

        static member internal setGroupStateWithoutEvent groupState group world =
            World.groupStateAdder groupState group world

        static member internal setOptGroupStateWithoutEvent optGroupState group world =
            match optGroupState with 
            | Some groupState -> World.groupStateAdder groupState group world
            | None -> World.groupStateRemover group world

        static member internal setGroupState groupState group world =
            let oldWorld = world
            let world = World.groupStateAdder groupState group world
            if groupState.PublishChanges then
                World.publish4
                    { Simulant = group; OldWorld = oldWorld }
                    (World.GroupChangeEventAddress ->>- group.GroupAddress)
                    group
                    world
            else world

        static member internal updateGroupState updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

        static member private addGroupState groupState group world =
            if not <| World.containsGroup group world then
                let world = World.setGroupStateWithoutEvent groupState group world
                let world = World.registerGroup group world
                World.publish4 () (World.GroupAddEventAddress ->>- group.GroupAddress) group world
            else failwith <| "Adding a group that the world already contains at address '" + acstring group.GroupAddress + "'."

        static member private registerGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world : GroupDispatcher
            dispatcher.Register group world

        static member private unregisterGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world : GroupDispatcher
            dispatcher.Unregister group world

        /// Query that the world contains a group.
        static member containsGroup group world =
            Option.isSome <| World.optGroupStateFinder group world

        /// Get all the groups in a screen.
        static member getGroups screen world =
            let groupStateMap = World.getGroupStateMap screen world
            Seq.map
                (fun (kvp : KeyValuePair<string, _>) -> Group.proxy <| World.satoga screen.ScreenAddress kvp.Key)
                groupStateMap

        // Get all the groups in the world.
        static member getGroups1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getGroups screen world) |>
            Seq.concat

        /// Destroy a group in the world immediately. Can be dangerous if existing in-flight
        /// publishing depends on the group's existence. Use with caution.
        static member destroyGroupImmediate group world =
            let world = World.publish4 () (World.GroupRemovingEventAddress ->>- group.GroupAddress) group world
            if World.containsGroup group world then
                let world = World.unregisterGroup group world
                let entities = World.getEntities group world
                let world = World.destroyEntitiesImmediate entities world
                World.setOptGroupStateWithoutEvent None group world
            else world

        /// Destroy a group in the world on the next tick. Use this rather than
        /// destroyGroupImmediate unless you need the latter's specific behavior.
        static member destroyGroup group world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.destroyGroupImmediate group world }
            World.addTask task world
            
        /// Destroy multiple groups in the world immediately. Can be dangerous if existing
        /// in-flight publishing depends on any of the groups' existences. Use with caution.
        static member destroyGroupsImmediate groups world =
            List.foldBack
                (fun group world -> World.destroyGroupImmediate group world)
                (List.ofSeq groups)
                world

        /// Destroy multiple groups from the world. Use this rather than destroyEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member destroyGroups groups world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.destroyGroupsImmediate groups world }
            World.addTask task world

        /// Create a group and add it to the world.
        static member createGroup dispatcherName optName screen world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let groupState = GroupState.make dispatcher optName
            Reflection.attachFields dispatcher groupState
            let group = Group.proxy <| World.satoga screen.ScreenAddress groupState.Name
            let world = World.addGroupState groupState group world
            (group, world)

        /// Write a group to an xml writer.
        static member writeGroup (writer : XmlWriter) group world =
            let groupState = World.getGroupState group world
            let entities = World.getEntities group world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName groupState.DispatcherNp)
            Reflection.writePropertiesFromTarget tautology3 writer groupState
            writer.WriteStartElement EntitiesNodeName
            World.writeEntities writer entities world
            writer.WriteEndElement ()

        /// Write a group to an xml file.
        static member writeGroupToFile (filePath : string) group world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GroupNodeName
            World.writeGroup writer group world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.WriteEndDocument ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple groups to an xml writer.
        static member writeGroups (writer : XmlWriter) groups world =
            let groupsSorted = Seq.sortBy (fun (group : Group) -> group.GetCreationTimeNp world) groups
            let groupsPersistent = Seq.filter (fun (group : Group) -> group.GetPersistent world) groupsSorted
            for group in groupsPersistent do
                writer.WriteStartElement GroupNodeName
                World.writeGroup writer group world
                writer.WriteEndElement ()

        /// Read a group from an xml node.
        static member readGroup groupNode defaultDispatcherName defaultEntityDispatcherName optName screen world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName groupNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GroupDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName world.Components.GroupDispatchers
            
            // make the bare group state with name as id
            let groupState = GroupState.make dispatcher None

            // attach the group state's instrinsic fields from its dispatcher if any
            Reflection.attachFields groupState.DispatcherNp groupState

            // read the group state's properties
            Reflection.readPropertiesToTarget groupNode groupState

            // apply the name if one is provided
            let groupState = match optName with Some name -> { groupState with Name = name } | None -> groupState

            // add the group's state to the world
            let group = Group.proxy <| World.satoga screen.ScreenAddress groupState.Name
            let world = World.destroyGroupImmediate group world
            let world = World.addGroupState groupState group world

            // read the group's entities
            let world = snd <| World.readEntities (groupNode : XmlNode) defaultEntityDispatcherName group world
            (group, world)

        /// Read a group from an xml file.
        static member readGroupFromFile (filePath : string) optName screen world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name optName screen world

        /// Read multiple groups from an xml node.
        static member readGroups (screenNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName screen world =
            match screenNode.SelectSingleNode GroupsNodeName with
            | null -> ([], world)
            | groupsNode ->
                let (groupsRev, world) =
                    Seq.fold
                        (fun (groupsRev, world) groupNode ->
                            let (group, world) = World.readGroup groupNode defaultDispatcherName defaultEntityDispatcherName None screen world
                            (group :: groupsRev, world))
                        ([], world)
                        (enumerable <| groupsNode.SelectNodes GroupNodeName)
                (List.rev groupsRev, world)

        (* Screen *)

        static member private optScreenStateFinder screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, _) -> Some screenState
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateAdder (screenState : ScreenState) screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None ->
                    let screenStateMap = Map.add screenName (screenState, Map.empty) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateRemover screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    if Map.isEmpty groupStateMap then
                        let screenStateMap = Map.remove screenName screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    else failwith <| "Cannot remove screen " + acstring screen.ScreenAddress + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getScreenStateMap world =
            snd world.SimulantStates

        static member internal getOptScreenState screen world =
            World.optScreenStateFinder screen world

        static member internal getScreenState screen world : ScreenState =
            Option.get ^ World.getOptScreenState screen world

        static member internal setScreenStateWithoutEvent screenState screen world =
            World.screenStateAdder screenState screen world

        static member internal setOptScreenStateWithoutEvent optScreenState screen world =
            match optScreenState with
            | Some screenState -> World.screenStateAdder screenState screen world
            | None -> World.screenStateRemover screen world

        static member internal setScreenState screenState screen world =
            let oldWorld = world
            let world = World.screenStateAdder screenState screen world
            if screenState.PublishChanges then
                World.publish4
                    { Simulant = screen; OldWorld = oldWorld }
                    (World.ScreenChangeEventAddress ->>- screen.ScreenAddress)
                    screen
                    world
            else world

        static member internal updateScreenState updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        static member private addScreenState screenState screen world =
            if not <| World.containsScreen screen world then
                let world = World.setScreenStateWithoutEvent screenState screen world
                let world = World.registerScreen screen world
                World.publish4 () (World.ScreenAddEventAddress ->>- screen.ScreenAddress) screen world
            else failwith <| "Adding a screen that the world already contains at address '" + acstring screen.ScreenAddress + "'."

        static member private registerScreen (screen : Screen) world =
            let dispatcher = screen.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Register screen world

        static member private unregisterScreen (screen : Screen) world =
            let dispatcher = screen.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Unregister screen world

        /// Query that the world contains a screen.
        static member containsScreen screen world =
            Option.isSome <| World.optScreenStateFinder screen world

        /// Get all the world's screens.
        static member getScreens world =
            World.getScreenStateMap world |>
                Map.fold (fun screensRev screenName _ -> (Screen.proxy <| ntoa screenName) :: screensRev) [] |>
                List.rev

        /// Destroy a screen in the world immediately. Can be dangerous if existing in-flight
        /// publishing depends on the screen's existence. Use with caution.
        static member destroyScreenImmediate screen world =
            let world = World.publish4 () (World.ScreenRemovingEventAddress ->>- screen.ScreenAddress) screen world
            if World.containsScreen screen world then
                let world = World.unregisterScreen screen world
                let groups = World.getGroups screen world
                let world = World.destroyGroupsImmediate groups world
                World.setOptScreenStateWithoutEvent None screen world
            else world

        /// Destroy a screen in the world on the next tick. Use this rather than
        /// destroyScreenImmediate unless you need the latter's specific behavior.
        static member destroyScreen screen world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.destroyScreenImmediate screen world }
            World.addTask task world

        /// Create a screen and add it to the world.
        static member createScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screenState = ScreenState.make dispatcher optName
            Reflection.attachFields dispatcher screenState
            let screen = Screen.proxy <| ntoa screenState.Name
            let world = World.addScreenState screenState screen world
            (screen, world)
        
        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen dissolveData dispatcherName optName world =
            let optDissolveImage = Some dissolveData.DissolveImage
            let (screen, world) = World.createScreen dispatcherName optName world
            let world = screen.SetIncoming { TransitionDescriptor.make Incoming with TransitionLifetime = dissolveData.IncomingTime; OptDissolveImage = optDissolveImage } world
            let world = screen.SetOutgoing { TransitionDescriptor.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; OptDissolveImage = optDissolveImage } world
            (screen, world)

        /// Write a screen to an xml writer.
        static member writeScreen (writer : XmlWriter) screen world =
            let screenState = World.getScreenState screen world
            let groups = World.getGroups screen world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName screenState.DispatcherNp)
            Reflection.writePropertiesFromTarget tautology3 writer screenState
            writer.WriteStartElement GroupsNodeName
            World.writeGroups writer groups world
            writer.WriteEndElement ()

        /// Write a screen to an xml file.
        static member writeScreenToFile (filePath : string) screen world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement ScreenNodeName
            World.writeScreen writer screen world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple screens to an xml writer.
        static member writeScreens (writer : XmlWriter) screens world =
            let screensSorted = Seq.sortBy (fun (screen : Screen) -> screen.GetCreationTimeNp world) screens
            let screensPersistent = Seq.filter (fun (screen : Screen) -> screen.GetPersistent world) screensSorted
            for screen in screensPersistent do
                writer.WriteStartElement ScreenNodeName
                World.writeScreen writer screen world
                writer.WriteEndElement ()

        /// Read a screen from an xml node.
        static member readScreen (screenNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName optName world =
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName screenNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.ScreenDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName world.Components.ScreenDispatchers
            let screenState = ScreenState.make dispatcher None
            Reflection.attachFields screenState.DispatcherNp screenState
            Reflection.readPropertiesToTarget screenNode screenState
            let screenState = match optName with Some name -> { screenState with Name = name } | None -> screenState
            let screen = Screen.proxy <| ntoa screenState.Name
            let world = World.destroyScreenImmediate screen world
            let world = World.addScreenState screenState screen world
            let world = snd <| World.readGroups (screenNode : XmlNode) defaultGroupDispatcherName defaultEntityDispatcherName screen world
            (screen, world)

        /// Read a screen from an xml file.
        static member readScreenFromFile (filePath : string) optName world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let screenNode = rootNode.[ScreenNodeName]
            World.readScreen screenNode typeof<ScreenDispatcher>.Name typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name optName world

        /// Read multiple screens from an xml node.
        static member readScreens (gameNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            match gameNode.SelectSingleNode ScreensNodeName with
            | null -> ([], world)
            | screensNode ->
                let (screensRev, world) =
                    Seq.fold
                        (fun (screens, world) screenNode ->
                            let (screen, world) = World.readScreen screenNode defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName None world
                            (screen :: screens, world))
                        ([], world)
                        (enumerable <| screensNode.SelectNodes ScreenNodeName)
                (List.rev screensRev, world)

        (* Game *)

        static member internal getGameStateMap world =
            let gameState = World.getGameState world
            let screenStateMap = World.getScreenStateMap world
            (gameState, screenStateMap)

        static member internal getGameState world : GameState =
            fst world.SimulantStates

        static member internal setGameState gameState world =
            let oldWorld = world
            let screenStateMap = World.getScreenStateMap world
            let world = { world with SimulantStates = (gameState, screenStateMap) }
            if gameState.PublishChanges then
                World.publish4
                    { OldWorld = oldWorld; Simulant = World.Game }
                    (World.GameChangeEventAddress ->>- World.Game.GameAddress)
                    World.Game
                    world
            else world

        static member internal updateGameState updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

        static member internal makeGameState dispatcher =
            let gameState = GameState.make dispatcher
            Reflection.attachFields dispatcher gameState
            gameState

        static member internal registerGame (world : World) : World =
            let dispatcher = World.Game.GetDispatcherNp world : GameDispatcher
            dispatcher.Register World.Game world

        (* World *)

        /// Try to get the currently selected screen.
        static member getOptSelectedScreen world =
            World.Game.GetOptSelectedScreen world

        /// Set the currently selected screen or None. Be careful using this function directly as
        //// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member setOptSelectedScreen optScreen world =
            World.Game.SetOptSelectedScreen optScreen world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        static member getSelectedScreen world =
            Option.get <| World.getOptSelectedScreen world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreen screen world =
            World.setOptSelectedScreen (Some screen) world

        /// Query that a simulant is the either currently selected screen or contained by it.
        static member isSimulantSelected<'s when 's :> Simulant> (simulant : 's) world =
            let optScreen = World.getOptSelectedScreen world
            let optScreenNames = Option.map (fun (screen : Screen) -> screen.ScreenAddress.Names) optScreen
            match (simulant.SimulantAddress.Names, optScreenNames) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

        /// Write a game to an xml writer.
        static member writeGame (writer : XmlWriter) world =
            let gameState = World.getGameState world
            let screens = World.getScreens world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName gameState.DispatcherNp)
            Reflection.writePropertiesFromTarget tautology3 writer gameState
            writer.WriteStartElement ScreensNodeName
            World.writeScreens writer screens world
            writer.WriteEndElement ()

        /// Write a game to an xml file.
        static member writeGameToFile (filePath : string) world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GameNodeName
            World.writeGame writer world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from an xml node.
        static member readGame
            gameNode defaultDispatcherName defaultScreenDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName gameNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GameDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName world.Components.GameDispatchers
            let gameState = World.makeGameState dispatcher
            Reflection.readPropertiesToTarget gameNode gameState
            let world = World.setGameState gameState world
            let world =
                snd <| World.readScreens
                    gameNode
                    defaultScreenDispatcherName
                    defaultGroupDispatcherName
                    defaultEntityDispatcherName
                    world
            world

        /// Read a game from an xml file.
        static member readGameFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let gameNode = rootNode.[GameNodeName]
            World.readGame
                gameNode
                typeof<GameDispatcher>.Name
                typeof<ScreenDispatcher>.Name
                typeof<GroupDispatcher>.Name
                typeof<EntityDispatcher>.Name
                world

        (* Simulant *)

        /// Query that the world contains a simulant.
        static member containsSimulant<'a when 'a :> Simulant> (simulant : 'a) world =
            match simulant :> Simulant with
            | :? Game -> true
            | :? Screen as screen -> World.containsScreen screen world
            | :? Group as group -> World.containsGroup group world
            | :? Entity as entity -> World.containsEntity entity world
            | _ -> failwithumf ()