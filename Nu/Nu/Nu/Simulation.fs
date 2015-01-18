// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

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

    /// Query that the engine is in game-playing mode.
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

    /// The state of a screen in regards to its transition.
    type ScreenState =
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
          Collidee : EntityRep }

    /// The data for a world state change event.
    and [<StructuralEquality; NoComparison>] WorldStateChangeData =
        { OldWorldState : WorldState }

    /// The data for a simulant change event.
    and [<StructuralEquality; NoComparison>] SimulantChangeData<'s when 's :> SimulantRep> =
        { SimulantRep : 's
          OldWorld : World }

    /// An event used by Nu's purely functional event system.
    and [<ReferenceEquality>] Event<'a, 's when 's :> SimulantRep> =
        { SubscriberRep : 's
          PublisherRep : SimulantRep // TODO: consider making this a list so that Observer can provide all useful addresses
          EventAddress : 'a Address
          Data : 'a }

    /// Describes whether an event has been resolved or should cascade.
    and EventHandling =
        | Resolve
        | Cascade

    /// Describes a game event subscription.
    and Subscription<'a, 's when 's :> SimulantRep> = Event<'a, 's> -> World -> EventHandling * World

    /// Describes a game event subscription that can be boxed / unboxed.
    and BoxableSubscription = obj -> World -> EventHandling * World

    /// An entry into the world's subscription map.
    and SubscriptionEntry = Guid * SimulantRep * obj

    /// A map of event subscriptions.
    and SubscriptionEntries = Map<obj Address, SubscriptionEntry rQueue>

    /// Abstracts over a subscription sorting procedure.
    and SubscriptionSorter = SubscriptionEntry rQueue -> World -> SubscriptionEntry rQueue

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, obj Address * SimulantRep>

    /// A task to be completed at the given time, with time being represented by the world's tick
    /// field.
    and [<ReferenceEquality>] Task =
        { ScheduledTime : int64
          Operation : World -> World }

    /// The default dispatcher for games.
    and GameDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true]

        /// Register a game when adding it to the world. Note that there is not corresponding
        /// Unregister method due to the inability to remove a game from the world.
        abstract Register : GameRep -> World -> World
        default dispatcher.Register _ world = world

    /// The default dispatcher for screens.
    and ScreenDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a screen when adding it to the world.
        abstract Register : ScreenRep -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister a screen when removing it from the world.
        abstract Unregister : ScreenRep -> World -> World
        default dispatcher.Unregister _ world = world

    /// The default dispatcher for groups.
    and GroupDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a group when adding it to a screen.
        abstract Register : GroupRep -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister a group when removing it from a screen.
        abstract Unregister : GroupRep -> World -> World
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
        abstract Register : EntityRep -> World -> World
        default dispatcher.Register _ world = world

        /// Unregister an entity when removing it from a group.
        abstract Unregister : EntityRep -> World -> World
        default dispatcher.Unregister _ world = world

        /// Propagate an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : EntityRep -> World -> World
        default dispatcher.PropagatePhysics _ world = world

        /// Get the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : EntityRep -> World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors _ _ = []

        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        abstract GetQuickSize : EntityRep -> World -> Vector2
        default dispatcher.GetQuickSize _ _ = Vector2.One

        /// Get the priority with which an entity is picked in the editor.
        abstract GetPickingPriority : EntityRep -> World -> single
        default dispatcher.GetPickingPriority entityRep world = entityRep.GetDepth world

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        /// Register a facet when adding it to an entity.
        abstract Register : EntityRep -> World -> World
        default facet.Register entityRep world = facet.RegisterPhysics entityRep world

        /// Unregister a facet when removing it from an entity.
        abstract Unregister : EntityRep -> World -> World
        default facet.Unregister entityRep world = facet.UnregisterPhysics entityRep world

        /// Participate in the registration of an entity's physics with the physics subsystem.
        abstract RegisterPhysics : EntityRep -> World -> World
        default facet.RegisterPhysics _ world = world

        /// Participate in the unregistration of an entity's physics from the physics subsystem.
        abstract UnregisterPhysics : EntityRep -> World -> World
        default facet.UnregisterPhysics _ world = world

        /// Participate in the propagation an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : EntityRep -> World -> World
        default facet.PropagatePhysics _ world = world

        /// Participate in getting the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : EntityRep -> World -> RenderDescriptor list
        default facet.GetRenderDescriptors _ _ = []

        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : EntityRep -> World -> Vector2
        default facet.GetQuickSize _ _ = DefaultEntitySize

    /// A marker interface for simulation types (Game, Screen, Group, Entity).
    and Simulant = interface end

    /// The game type that hosts the various screens used to navigate through a game.
    and [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          OptSelectedScreenRep : ScreenRep option
          PublishChanges : bool
          CreationTimeNp : DateTime
          DispatcherNp : GameDispatcher
          Xtension : Xtension }

        interface Simulant

        /// Make a game.
        static member make dispatcher =
            { Id = Core.makeId ()
              OptSelectedScreenRep = None
              PublishChanges = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          Name : string
          ScreenStateNp : ScreenState
          TransitionTicksNp : int64
          Incoming : TransitionDescriptor
          Outgoing : TransitionDescriptor
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : ScreenDispatcher
          Xtension : Xtension }

        interface Simulant

        /// Make a screen.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              ScreenStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = TransitionDescriptor.make Incoming
              Outgoing = TransitionDescriptor.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Forms logical groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          Name : string
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : GroupDispatcher
          Xtension : Xtension }

        interface Simulant

        /// Make a group.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Group.Id = id
              Name = match optName with None -> acstring id | Some name -> name
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    and [<CLIMutable; StructuralEquality; NoComparison>] Entity =
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

        interface Simulant

        /// Get the names of all facets used by an entity via reflection.
        /// TODO: see if this should be used as often as it is, and if it is needed in only one or
        /// two cases, just inline it.
        static member getFacetNamesReflectively entity =
            List.map Reflection.getTypeName entity.FacetsNp

        /// Query that a facet is compatible with those already being used by an entity.
        /// Note a facet is incompatible with any other facet if it contains any fields that has
        /// the same name but a different type.
        static member isFacetCompatible entityDispatcherMap facet (entity : Entity) =
            let facetType = facet.GetType ()
            let facetFieldDefinitions = Reflection.getFieldDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entity then
                List.notExists
                    (fun definition ->
                        match Map.tryFind definition.FieldName entity.Xtension.XFields with
                        | Some field -> field.GetType () <> definition.FieldType
                        | None -> false)
                    facetFieldDefinitions
            else false

        /// Make an entity.
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
            
    /// A marker interface for simulation types (Game, Screen, Group, Entity).
    /// The only methods that have a place in here are those used internally by Nu's event system.
    and SimulantRep =
        interface
            /// Get the entity's publishing priority.
            abstract GetPublishingPriority : (EntityRep -> World -> single) -> World -> single
            abstract SimulantAddress : Simulant Address
        end

    /// The game type that hosts the various screens used to navigate through a game.
    and [<StructuralEquality; NoComparison>] GameRep =
        { GameAddress : Game Address }
        
        interface SimulantRep with
            member this.GetPublishingPriority _ _ = GamePublishingPriority
            member this.SimulantAddress = Address.changeType<Game, Simulant> this.GameAddress
        end

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<StructuralEquality; NoComparison>] ScreenRep =
        { ScreenAddress : Screen Address }
        
        interface SimulantRep with
            member this.GetPublishingPriority _ _ = ScreenPublishingPriority
            member this.SimulantAddress = Address.changeType<Screen, Simulant> this.ScreenAddress
        end

    /// Forms a logical group of entities.
    and [<StructuralEquality; NoComparison>] GroupRep =
        { GroupAddress : Group Address }
        
        interface SimulantRep with
            member this.GetPublishingPriority _ _ = GroupPublishingPriority
            member this.SimulantAddress = Address.changeType<Group, Simulant> this.GroupAddress
        end

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    and [<StructuralEquality; NoComparison>] EntityRep =
        { EntityAddress : Entity Address }

        interface SimulantRep with
            member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
            member this.SimulantAddress = Address.changeType<Entity, Simulant> this.EntityAddress
        end

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

    /// The world's higher order facilities.
    and [<ReferenceEquality>] Callbacks =
        { Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          CallbackStates : Map<Guid, obj> }

    /// The world's state.
    and [<ReferenceEquality>] WorldState =
        { TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          OptScreenTransitionDestinationRep : ScreenRep option
          Camera : Camera
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFilePath : string
          Overlayer : Overlayer
          OverlayRouter : OverlayRouter
          OverlayFilePath : string
          UserState : obj }

    and GameRep with
        
        member this.GetId world = (World.getGame world).Id
        member this.GetCreationTimeNp world = (World.getGame world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getGame world).DispatcherNp
        member this.GetOptSelectedScreenRep world = (World.getGame world).OptSelectedScreenRep
        member this.SetOptSelectedScreenRep value world = World.updateGame (fun (game : Game) -> { game with OptSelectedScreenRep = value}) world
        member this.GetPublishChanges world = (World.getGame world).PublishChanges
        member this.SetPublishChanges value world = World.updateGame (fun game -> { game with PublishChanges = value}) world
        member this.GetXtension world = (World.getGame world).Xtension
        member this.SetXtension value world = World.updateGame (fun game -> { game with Xtension = value}) world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        /// Query that a game dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and ScreenRep with

        member this.GetId world = (World.getScreen this world).Id
        member this.GetName world = (World.getScreen this world).Name
        member this.GetCreationTimeNp world = (World.getScreen this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getScreen this world).DispatcherNp
        member this.GetScreenStateNp world = (World.getScreen this world).ScreenStateNp
        member this.SetScreenStateNp value world = World.updateScreen (fun screen -> { screen with ScreenStateNp = value}) this world
        member this.GetTransitionTicksNp world = (World.getScreen this world).TransitionTicksNp
        member this.SetTransitionTicksNp value world = World.updateScreen (fun screen -> { screen with TransitionTicksNp = value}) this world
        member this.GetIncoming world = (World.getScreen this world).Incoming
        member this.SetIncoming value world = World.updateScreen (fun screen -> { screen with Incoming = value}) this world
        member this.GetOutgoing world = (World.getScreen this world).Outgoing
        member this.SetOutgoing value world = World.updateScreen (fun screen -> { screen with Outgoing = value}) this world
        member this.GetPublishChanges world = (World.getScreen this world).PublishChanges
        member this.SetPublishChanges value world = World.updateScreen (fun (screen : Screen) -> { screen with PublishChanges = value}) this world
        member this.GetPersistent world = (World.getScreen this world).Persistent
        member this.SetPersistent value world = World.updateScreen (fun screen -> { screen with Persistent = value}) this world
        member this.GetXtension world = (World.getScreen this world).Xtension
        member this.SetXtension value world = World.updateScreen (fun screen -> { screen with Xtension = value}) this world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world
        member this.GetIdling world = this.GetScreenStateNp world = IdlingState

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and GroupRep with

        member this.GetId world = (World.getGroup this world).Id
        member this.GetName world = (World.getGroup this world).Name
        member this.GetCreationTimeNp world = (World.getGroup this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getGroup this world).DispatcherNp
        member this.GetXtension world = (World.getGroup this world).Xtension
        member this.GetPublishChanges world = (World.getGroup this world).PublishChanges
        member this.SetPublishChanges value world = World.updateGroup (fun (group : Group) -> { group with PublishChanges = value}) this world
        member this.GetPersistent world = (World.getGroup this world).Persistent
        member this.SetPersistent value world = World.updateGroup (fun group -> { group with Persistent = value}) this world

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    and EntityRep with
        
        member this.GetId world = (World.getEntity this world).Id
        member this.GetName world = (World.getEntity this world).Name
        member this.GetCreationTimeNp world = (World.getEntity this world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getEntity this world).DispatcherNp
        member this.GetFacetNames world = (World.getEntity this world).FacetNames
        member this.GetFacetsNp world = (World.getEntity this world).FacetsNp
        member this.GetPosition world = (World.getEntity this world).Position
        member this.SetPosition value world = World.updateEntity (fun (entity : Entity) -> { entity with Position = value}) this world
        member this.GetDepth world = (World.getEntity this world).Depth
        member this.SetDepth value world = World.updateEntity (fun entity -> { entity with Depth = value}) this world
        member this.GetSize world = (World.getEntity this world).Size
        member this.SetSize value world = World.updateEntity (fun entity -> { entity with Size = value}) this world
        member this.GetRotation world = (World.getEntity this world).Rotation
        member this.SetRotation value world = World.updateEntity (fun entity -> { entity with Rotation = value}) this world
        member this.GetVisible world = (World.getEntity this world).Visible
        member this.SetVisible value world = World.updateEntity (fun entity -> { entity with Visible = value}) this world
        member this.GetViewType world = (World.getEntity this world).ViewType
        member this.SetViewType value world = World.updateEntity (fun entity -> { entity with ViewType = value}) this world
        member this.GetPublishChanges world = (World.getEntity this world).PublishChanges
        member this.SetPublishChanges value world = World.updateEntity (fun entity -> { entity with PublishChanges = value}) this world
        member this.GetPersistent world = (World.getEntity this world).Persistent
        member this.SetPersistent value world = World.updateEntity (fun entity -> { entity with Persistent = value}) this world
        member this.GetOptOverlayName world = (World.getEntity this world).OptOverlayName
        member this.SetOptOverlayName value world = World.updateEntity (fun entity -> { entity with OptOverlayName = value}) this world
        member this.GetXtension world = (World.getEntity this world).Xtension
        member this.SetXtension xtension world = World.updateEntity (fun entity -> { entity with Xtension = xtension}) this world
        member this.UpdateXtension updater world = this.SetXtension (updater <| this.GetXtension world) world

        member this.SetPositionSnapped snap position world =
            let snapped = Math.snap2F snap position
            this.SetPosition snapped world

        member this.GetTransform world : Transform =
            { Position = this.GetPosition world
              Depth = this.GetDepth world
              Size = this.GetSize world
              Rotation = this.GetRotation world }

        member this.SetTransform positionSnap rotationSnap transform world =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            world |>
                this.SetPosition transform.Position |>
                this.SetDepth transform.Depth |>
                this.SetSize transform.Size |>
                this.SetRotation transform.Rotation

        /// Query that an entity dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    ///
    /// TODO: attempt to implement with Fsharpx.PersistentHashMap with hash cached in Address type.
    and [<ReferenceEquality>] World =
        { Simulants : Game * Map<string, Screen * Map<string, Group * Map<string, Entity>>>
          Subsystems : Subsystems
          Components : Components
          Callbacks : Callbacks
          State : WorldState }

        static member internal atoua address = Address.changeType<'a, Simulant> address
        static member internal atosa address = Address.changeType<'a, Screen> address
        static member internal atoga address = Address.changeType<'a, Group> address
        static member internal atoea address = Address.changeType<'a, Entity> address
        static member internal gatoea groupAddress entityName = Address.changeType<Group, Entity> groupAddress ->- ntoa entityName
        static member internal satoga screenAddress groupName = Address.changeType<Screen, Group> screenAddress ->- ntoa groupName
        static member internal satoea screenAddress groupName entityName = World.gatoea (World.satoga screenAddress groupName) entityName
        static member internal eatoga entityAddress = Address.take<Entity, Group> 2 entityAddress
        static member internal gatosa groupAddress = Address.take<Group, Screen> 1 groupAddress
        static member internal eatosa entityAddress = Address.take<Entity, Screen> 1 entityAddress

        static member internal GameAddress = Address<Game>.empty
        static member internal GameRep = { GameAddress = World.GameAddress }
        static member internal DefaultScreenAddress = ntoa<Screen> DefaultGroupName
        static member internal DefaultGroupAddress = World.satoga World.DefaultScreenAddress DefaultGroupName
        static member internal DefaultEntityAddress = World.gatoea World.DefaultGroupAddress DefaultEntityName
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
        static member internal GameChangeEventAddress = World.GameEventAddress -<- ntoa<GameRep SimulantChangeData> "Change"
        static member internal ScreenEventAddress = ntoa<obj> "Screen"
        static member internal ScreenAddEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Add"
        static member internal ScreenRemovingEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Removing"
        static member internal ScreenChangeEventAddress = World.ScreenEventAddress -<- ntoa<ScreenRep SimulantChangeData> "Change"
        static member internal GroupEventAddress = ntoa<obj> "Group"
        static member internal GroupAddEventAddress = World.GroupEventAddress -<- ntoa<unit> "Add"
        static member internal GroupRemovingEventAddress = World.GroupEventAddress -<- ntoa<unit> "Removing"
        static member internal GroupChangeEventAddress = World.GroupEventAddress -<- ntoa<GroupRep SimulantChangeData> "Change"
        static member internal EntityEventAddress = ntoa<obj> "Entity"
        static member internal EntityAddEventAddress = World.EntityEventAddress -<- ntoa<unit> "Add"
        static member internal EntityRemovingEventAddress = World.EntityEventAddress -<- ntoa<unit> "Removing"
        static member internal EntityChangeEventAddress = World.EntityEventAddress -<- ntoa<EntityRep SimulantChangeData> "Change"
        static member internal DefaultDissolveImage = { PackageName = DefaultPackageName; AssetName = "Image8" }

        (* Publishing *)

        // OPTIMIZATION: priority annotated as single to decrease GC pressure.
        static member private sortFstDesc (priority : single, _) (priority2 : single, _) =
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

        static member private getSortableSubscriptions
            getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world :
            (single * SubscriptionEntry) list =
            List.foldBack
                (fun (key, simulantRep : SimulantRep, subscription) subscriptions ->
                    let priority = simulantRep.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, simulantRep, subscription))
                    subscription :: subscriptions)
                subscriptions
                []

        static member private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            let subListRev = List.rev subList
            publishSorter subListRev world
    
        static member private boxSubscription<'a, 's when 's :> SimulantRep> (subscription : Subscription<'a, 's>) =
            let boxableSubscription = fun (event : obj) world ->
                try subscription (event :?> Event<'a, 's>) world
                with
                | :? InvalidCastException ->
                    // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                    // up an event type parameter for some form of World.publish or subscribe.
                    reraise ()
                | _ -> reraise ()
            box boxableSubscription

        static member private publishEvent<'a, 'p, 's when 'p :> SimulantRep and 's :> SimulantRep>
            (subscriberRep : SimulantRep) (publisherRep : 'p) (eventAddress : 'a Address) (eventData : 'a) subscription world =
            let event =
                { SubscriberRep = subscriberRep :?> 's
                  PublisherRep = publisherRep :> SimulantRep
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
                (fun (entityRep : EntityRep) world ->
                    let dispatcher = entityRep.GetDispatcherNp world : EntityDispatcher
                    dispatcher.GetPickingPriority entityRep world)
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
        static member publish<'a, 'p when 'p :> SimulantRep> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisherRep : 'p) world =
            let objEventAddress = atooa eventAddress
            let subscriptions = World.getSubscriptionsSorted publishSorter objEventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriberRep : SimulantRep, subscription) ->
                        if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                            (match world.State.Liveness with Running -> true | Exiting -> false) then
                            match subscriberRep.SimulantAddress.Names with
                            | [] -> World.publishEvent<'a, 'p, GameRep> subscriberRep publisherRep eventAddress eventData subscription world
                            | [_] -> World.publishEvent<'a, 'p, ScreenRep> subscriberRep publisherRep eventAddress eventData subscription world
                            | [_; _] -> World.publishEvent<'a, 'p, GroupRep> subscriberRep publisherRep eventAddress eventData subscription world
                            | [_; _; _] -> World.publishEvent<'a, 'p, EntityRep> subscriberRep publisherRep eventAddress eventData subscription world
                            | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                        else None)
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4<'a, 'p when 'p :> SimulantRep>
            (eventData : 'a) (eventAddress : 'a Address) (publisherRep : 'p) world =
            World.publish World.sortSubscriptionsByHierarchy eventData eventAddress publisherRep world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> SimulantRep>
            subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberRep : 's) world =
            if not <| Address.isEmpty eventAddress then
                let objEventAddress = atooa eventAddress
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, subscriberRep :> SimulantRep, World.boxSubscription subscription)
                    match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (objEventAddress, subscriberRep :> SimulantRep) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe4<'a, 's when 's :> SimulantRep>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberRep : 's) world =
            World.subscribe (World.makeSubscriptionKey ()) subscription eventAddress subscriberRep world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
            | Some (eventAddress, subscriberRep) ->
                match Map.tryFind eventAddress world.Callbacks.Subscriptions with
                | Some subscriptionList ->
                    let subscriptionList =
                        List.remove
                            (fun (subscriptionKey', subscriberRep', _) ->
                                subscriptionKey' = subscriptionKey &&
                                subscriberRep' = subscriberRep)
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
        static member monitor<'a, 's when 's :> SimulantRep>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberRep : 's) world =
            if not <| Address.isEmpty subscriberRep.SimulantAddress then
                let monitorKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe<'a, 's> monitorKey subscription eventAddress subscriberRep world
                let subscription' = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe monitorKey world
                    (Cascade, world)
                let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriberRep.SimulantAddress
                World.subscribe<unit, 's> removalKey subscription' removingEventAddress subscriberRep world
            else failwith "Cannot monitor events with an anonymous subscriber."

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> SimulantRep> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> SimulantRep> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> SimulantRep> (_ : Event<'a, 's>) (world : World) =
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

        /// Clear the physics messages.
        static member internal clearPhysicsMessages world =
            World.updateSubsystem (fun is _ -> is.ClearMessages ()) IntegratorSubsystemName world

        /// Clear the rendering messages.
        static member internal clearRenderMessages world =
            World.updateSubsystem (fun rs _ -> rs.ClearMessages ()) RendererSubsystemName world

        /// Clear the audio messages.
        static member internal clearAudioMessages world =
            World.updateSubsystem (fun aps _ -> aps.ClearMessages ()) AudioPlayerSubsystemName world

        /// Add a physics message to the world.
        static member addPhysicsMessage message world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

        /// Add a rendering message to the world.
        static member addRenderMessage message world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

        (* Callbacks *)

        /// Add a task to be executed by the engine at the specified task tick.
        static member addTask task world =
            let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add multiple task to be executed by the engine at the specified task tick.
        static member addTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Restore tasks to be executed by the engine at the specified task tick.
        static member internal restoreTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
            { world with Callbacks = callbacks }

        /// Clear all tasks.
        static member internal clearTasks world =
            let callbacks = { world.Callbacks with Tasks = [] }
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
        static member getState world = world.State

        /// Set the state of the world.
        static member setState state world =
            let oldState = world.State
            let world = { world with State = state }
            World.publish4 { OldWorldState = oldState } World.WorldStateChangeEventAddress World.GameRep world

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

        /// Get the tick time.
        static member getTickTime world =
            world.State.TickTime

        /// Increment the tick time.
        static member internal incrementTickTime world =
            let state = { world.State with TickTime = world.State.TickTime + 1L }
            World.setState state world

        /// Get the the liveness state of the world.
        static member getLiveness world =
            world.State.Liveness

        /// Place the world into a state such that the app will exit at the end of the current frame.
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

        /// Get the OptScreenTransitionDestinationRep field of the world.
        static member getOptScreenTransitionDestinationRep world =
            world.State.OptScreenTransitionDestinationRep

        /// Set the OptScreenTransitionDestinationRep field of the world.
        static member internal setOptScreenTransitionDestinationRep destinationRep world =
            let state = { world.State with OptScreenTransitionDestinationRep = destinationRep }
            World.setState state world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            world.State.AssetMetadataMap

        /// Set the asset metadata map.
        static member internal setAssetMetadataMap assetMetadataMap world =
            let state = { world.State with AssetMetadataMap = assetMetadataMap }
            World.setState state world

        /// Set the Overlayer field of the world.
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

        static member private getEntityFieldDefinitionNamesToDetach entity facetToRemove =

            // get the field definition name counts of the current, complete entity
            let fieldDefinitions = Reflection.getReflectiveFieldDefinitionMap entity
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

        static member private tryRemoveFacet syncing facetName entity optAddress world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entity.FacetsNp with
            | Some facet ->
                let (entity, world) =
                    match optAddress with
                    | Some address ->
                        let entityRep = { EntityAddress = address }
                        let world = facet.Unregister entityRep world
                        (World.getEntity entityRep world, world)
                    | None -> (entity, world)
                let entity = { entity with Id = entity.Id } // hacky copy
                let fieldNames = World.getEntityFieldDefinitionNamesToDetach entity facet
                Reflection.detachFieldsViaNames fieldNames entity
                let entity =
                    if syncing then entity
                    else { entity with FacetNames = List.remove ((=) (Reflection.getTypeName facet)) entity.FacetNames }
                let entity = { entity with FacetsNp = List.remove ((=) facet) entity.FacetsNp }
                let world =
                    match optAddress with
                    | Some address ->
                        let entityRep = { EntityAddress = address }
                        World.setEntity entity entityRep world
                    | None -> world
                Right (entity, world)
            | None -> Left <| "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet syncing facetName (entity : Entity) optAddress world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                if Entity.isFacetCompatible world.Components.EntityDispatchers facet entity then
                    let entity = { entity with FacetsNp = facet :: entity.FacetsNp }
                    Reflection.attachFields facet entity
                    let entity =
                        if syncing then entity
                        else { entity with FacetNames = Reflection.getTypeName facet :: entity.FacetNames }
                    match optAddress with
                    | Some address ->
                        let entityRep = { EntityAddress = address }
                        let world = facet.Register entityRep world
                        let entity = World.getEntity entityRep world
                        Right (entity, world)
                    | None -> Right (entity, world)
                else Left <| "Facet '" + Reflection.getTypeName facet + "' is incompatible with entity '" + entity.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets syncing facetNamesToRemove entity optAddress world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryRemoveFacet syncing facetName entity optAddress world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToRemove

        static member private tryAddFacets syncing facetNamesToAdd entity optAddress world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryAddFacet syncing facetName entity optAddress world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToAdd

        static member private trySetFacetNames oldFacetNames newFacetNames entity optAddress world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames newFacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames newFacetNames
            match World.tryRemoveFacets false facetNamesToRemove entity optAddress world with
            | Right (entity, world) -> World.tryAddFacets false facetNamesToAdd entity optAddress world
            | Left _ as left -> left

        static member private trySynchronizeFacets oldFacetNames entity optAddress world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entity.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entity.FacetNames
            match World.tryRemoveFacets true facetNamesToRemove entity optAddress world with
            | Right (entity, world) -> World.tryAddFacets true facetNamesToAdd entity optAddress world
            | Left _ as left -> left

        static member private attachIntrinsicFacetsViaNames (entity : Entity) world =
            let components = world.Components
            let entity = { entity with Id = entity.Id } // hacky copy
            Reflection.attachIntrinsicFacets components.EntityDispatchers components.Facets entity.DispatcherNp entity
            entity

        (* Entity *)

        static member private optEntityFinder (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityAdder (entity : Entity) (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.add entityName entity entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityRemover (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.remove entityName entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private getEntityMapInGroup groupRep world =
            match groupRep.GroupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring groupRep.GroupAddress + "'."

        static member internal getOptEntity entityRep world =
            World.optEntityFinder entityRep.EntityAddress world

        static member internal getEntity (entityRep : EntityRep) world =
            Option.get ^ World.getOptEntity entityRep world

        static member internal setEntityWithoutEvent entity entityRep world =
            World.entityAdder entity entityRep.EntityAddress world

        static member internal setOptEntityWithoutEvent optEntity entityRep world =
            match optEntity with 
            | Some entity -> World.entityAdder entity entityRep.EntityAddress world
            | None -> World.entityRemover entityRep.EntityAddress world

        static member internal setEntity entity (entityRep : EntityRep) world =
            let oldWorld = world
            let world = World.entityAdder entity entityRep.EntityAddress world
            if entity.PublishChanges then
                World.publish4
                    { SimulantRep = entityRep; OldWorld = oldWorld }
                    (World.EntityChangeEventAddress ->>- entityRep.EntityAddress)
                    entityRep
                    world
            else world

        static member internal updateEntity updater entityRep world =
            let entity = World.getEntity entityRep world
            let entity = updater entity
            World.setEntity entity entityRep world

        static member private registerEntity (entityRep : EntityRep) world =
            let dispatcher = entityRep.GetDispatcherNp world : EntityDispatcher
            let facets = entityRep.GetFacetsNp world
            let world = dispatcher.Register entityRep world
            List.fold
                (fun world (facet : Facet) -> facet.Register entityRep world)
                world
                facets
        
        static member private unregisterEntity (entityRep : EntityRep) world =
            let facets = entityRep.GetFacetsNp world
            List.fold
                (fun world (facet : Facet) -> facet.Unregister entityRep world)
                world
                facets

        static member private addEntity entity entityRep world =
            if not <| World.containsEntity entityRep world then
                let world = World.setEntityWithoutEvent entity entityRep world
                let world = World.registerEntity entityRep world
                World.publish4 () (World.EntityAddEventAddress ->>- entityRep.EntityAddress) entityRep world
            else failwith <| "Adding an entity that the world already contains at address '" + acstring entityRep.EntityAddress + "'."

        static member private addEntities entities (groupRep : GroupRep) world =
            let groupAddress = groupRep.GroupAddress
            Map.fold
                (fun world entityName entity ->
                    let entityAddress = World.gatoea groupAddress entityName
                    let entityRep = { EntityAddress = entityAddress }
                    World.addEntity entity entityRep world)
                world
                entities

        /// Query that the world contains an entity at the given address.
        static member containsEntity entityRep world =
            Option.isSome <| World.optEntityFinder entityRep.EntityAddress world

        /// Get all the entity addresses in the group at the given address.
        static member getEntityRepsInGroup groupRep world =
            let entityMap = World.getEntityMapInGroup groupRep world
            let groupAddress = groupRep.GroupAddress
            Seq.map (fun (kvp : KeyValuePair<string, _>) -> { EntityAddress = World.gatoea groupAddress kvp.Key }) entityMap

        /// Remove an entity from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the entity's existence. Use with caution.
        static member removeEntityImmediate entityRep world =
            let world = World.publish4 () (World.EntityRemovingEventAddress ->>- entityRep.EntityAddress) entityRep world
            if World.containsEntity entityRep world then
                let world = World.unregisterEntity entityRep world
                World.setOptEntityWithoutEvent None entityRep world
            else world

        /// Remove an entity from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeEntity entityRep world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.removeEntityImmediate entityRep world }
            World.addTask task world

        /// Remove multiple entities from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the entities' existences. Use with caution.
        static member removeEntitiesImmediate entityReps world =
            List.foldBack
                (fun entityRep world -> World.removeEntityImmediate entityRep world)
                (List.ofSeq entityReps)
                world

        /// Remove multiple entities from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeEntities entityReps world =
            World.removeEntitiesImmediate entityReps world

        /// Create an entity and add it to the world.
        static member createEntity dispatcherName optName groupRep world =
            
            // find the entity's dispatcher
            let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
            
            // compute the default opt overlay name
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = Map.find intrinsicOverlayName world.State.OverlayRouter

            // make the bare entity with name as id
            let entity = Entity.make dispatcher defaultOptOverlayName optName

            // attach the entity's intrinsic facets and their fields
            let entity = World.attachIntrinsicFacetsViaNames entity world

            // apply the entity's overlay to its facet names
            let entity =
                match defaultOptOverlayName with
                | Some defaultOverlayName ->
                    let overlayer = world.State.Overlayer
                    Overlayer.applyOverlayToFacetNames intrinsicOverlayName defaultOverlayName entity overlayer overlayer
                        
                    // synchronize the entity's facets (and attach their fields)
                    match World.trySynchronizeFacets [] entity None world with
                    | Right (entity, _) -> entity
                    | Left error -> debug error; entity
                | None -> entity

            // attach the entity's dispatcher fields
            Reflection.attachFields dispatcher entity

            // apply the entity's overlay
            let entity =
                match entity.OptOverlayName with
                | Some overlayName ->

                    // OPTIMIZATION: apply overlay only when it will change something (EG - when it's not the intrinsic overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = Entity.getFacetNamesReflectively entity
                        Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.State.Overlayer
                        entity
                    else entity
                | None -> entity

            // add entity to world
            let entityRep = { EntityAddress = World.gatoea groupRep.GroupAddress entity.Name }
            let world = World.addEntity entity entityRep world
            (entityRep, world)

        /// Write an entity to an xml writer.
        static member writeEntity (writer : XmlWriter) (entityRep : EntityRep) world =
            let entity = World.getEntity entityRep world
            let dispatcher = entity.DispatcherNp
            let dispatcherTypeName = Reflection.getTypeName dispatcher
            writer.WriteAttributeString (DispatcherNameAttributeName, dispatcherTypeName)
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let defaultOptOverlayName = Map.find dispatcherTypeName world.State.OverlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let facetNames = Entity.getFacetNamesReflectively entity
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entity world.State.Overlayer
            Reflection.writePropertiesFromTarget shouldWriteProperty writer entity

        /// Write multiple entities to an xml writer.
        static member writeEntities (writer : XmlWriter) entityReps world =
            let entityRepsSorted = Seq.sortBy (fun (entityRep : EntityRep) -> entityRep.GetCreationTimeNp world) entityReps
            let entityRepsPersistent = Seq.filter (fun (entityRep : EntityRep) -> entityRep.GetPersistent world) entityRepsSorted
            for entityRep in entityRepsPersistent do
                writer.WriteStartElement typeof<Entity>.Name
                World.writeEntity writer entityRep world
                writer.WriteEndElement ()

        /// Read an entity from an xml node.
        static member readEntity (entityNode : XmlNode) defaultDispatcherName groupRep world =

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

            // make the bare entity with name as id
            let entity = Entity.make dispatcher defaultOptOverlayName None

            // attach the entity's intrinsic facets and their fields
            let entity = World.attachIntrinsicFacetsViaNames entity world

            // read the entity's overlay and apply it to its facet names if applicable
            Reflection.tryReadOptOverlayNameToTarget entityNode entity
            match (defaultOptOverlayName, entity.OptOverlayName) with
            | (Some defaultOverlayName, Some overlayName) ->
                let overlayer = world.State.Overlayer
                Overlayer.applyOverlayToFacetNames defaultOverlayName overlayName entity overlayer overlayer
            | (_, _) -> ()

            // read the entity's facet names
            Reflection.readFacetNamesToTarget entityNode entity
            
            // synchronize the entity's facets (and attach their fields)
            let entity =
                match World.trySynchronizeFacets [] entity None world with
                | Right (entity, _) -> entity
                | Left error -> debug error; entity

            // attach the entity's dispatcher fields
            Reflection.attachFields dispatcher entity

            // attempt to apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName ->

                // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                if intrinsicOverlayName <> overlayName then
                    let facetNames = Entity.getFacetNamesReflectively entity
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.State.Overlayer
                else ()
            | None -> ()

            // read the entity's properties
            Reflection.readPropertiesToTarget entityNode entity

            // add entity to the world
            let entityRep = { EntityAddress = World.gatoea groupRep.GroupAddress entity.Name }
            let world = World.addEntity entity entityRep world
            (entityRep, world)

        /// Read multiple entities from an xml node.
        static member readEntities (groupNode : XmlNode) defaultDispatcherName groupRep world =
            match groupNode.SelectSingleNode EntitiesNodeName with
            | null -> ([], world)
            | entitiesNode ->
                Seq.foldBack
                    (fun entityNode (entityReps, world) ->
                        let (entityRep, world) = World.readEntity entityNode defaultDispatcherName groupRep world
                        (entityRep :: entityReps, world))
                    (enumerable <| entitiesNode.SelectNodes EntityNodeName)
                    ([], world)

        /// Propagate an entity's physics properties from the physics subsystem.
        static member propagatePhysics (entityRep : EntityRep) world =
            let dispatcher = entityRep.GetDispatcherNp world
            let facets = entityRep.GetFacetsNp world
            let world = dispatcher.PropagatePhysics entityRep world
            List.fold
                (fun world (facet : Facet) -> facet.PropagatePhysics entityRep world)
                world
                facets
        
        /// Get the render descriptors needed to render an entity.
        static member getRenderDescriptors (entityRep : EntityRep) world =
            let dispatcher = entityRep.GetDispatcherNp world : EntityDispatcher
            let facets = entityRep.GetFacetsNp world
            let renderDescriptors = dispatcher.GetRenderDescriptors entityRep world
            List.foldBack
                (fun (facet : Facet) renderDescriptors ->
                    let descriptors = facet.GetRenderDescriptors entityRep world
                    descriptors @ renderDescriptors)
                facets
                renderDescriptors
        
        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        static member getQuickSize (entityRep : EntityRep) world =
            let dispatcher = entityRep.GetDispatcherNp world : EntityDispatcher
            let facets = entityRep.GetFacetsNp world
            let quickSize = dispatcher.GetQuickSize entityRep world
            List.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize entityRep world
                    Vector2 (
                        Math.Max (quickSize.X, maxSize.X),
                        Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                facets

        /// Get the priority with which an entity is picked in the editor.
        static member getPickingPriority (entityRep : EntityRep) world =
            let dispatcher = entityRep.GetDispatcherNp world : EntityDispatcher
            dispatcher.GetPickingPriority entityRep world

        /// TODO: document!
        static member pickingSort entityReps world =
            let prioritiesAndEntityReps = List.map (fun (entityRep : EntityRep) -> (World.getPickingPriority entityRep world, entityRep)) entityReps
            let prioritiesAndEntityReps = List.sortWith World.sortFstDesc prioritiesAndEntityReps
            List.map snd prioritiesAndEntityReps

        /// TODO: document!
        static member tryPick position entityReps world =
            let entityRepsSorted = World.pickingSort entityReps world
            List.tryFind
                (fun (entityRep : EntityRep) ->
                    let positionWorld = Camera.mouseToWorld (entityRep.GetViewType world) position world.State.Camera
                    let transform = entityRep.GetTransform world
                    let picked = Math.isPointInBounds3 positionWorld transform.Position transform.Size
                    picked)
                entityRepsSorted

        (* Group *)

        static member private optGroupFinder (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, _) -> Some group
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupAdder (group : Group) (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None ->
                        let groupMap = Map.add groupName (group, Map.empty) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                | None -> failwith <| "Cannot add group '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupRemover (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        if Map.isEmpty entityMap then
                            let groupMap = Map.remove groupName groupMap
                            let screenMap = Map.add screenName (screen, groupMap) screenMap
                            { world with Simulants = (game, screenMap) }
                        else failwith <| "Cannot remove group " + acstring address + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member internal getGroupMapInScreen screenRep world =
            match screenRep.ScreenAddress.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenRep.ScreenAddress + "'."

        static member internal getOptGroup groupRep world =
            World.optGroupFinder groupRep.GroupAddress world

        static member internal getGroup groupRep world : Group =
            Option.get ^ World.getOptGroup groupRep world

        static member internal setGroupWithoutEvent group groupRep world =
            World.groupAdder group groupRep.GroupAddress world

        static member internal setOptGroupWithoutEvent optGroup groupRep world =
            match optGroup with 
            | Some group -> World.groupAdder group groupRep.GroupAddress world
            | None -> World.groupRemover groupRep.GroupAddress world

        static member internal setGroup group groupRep world =
            let oldWorld = world
            let world = World.groupAdder group groupRep.GroupAddress world
            if group.PublishChanges then
                World.publish4
                    { SimulantRep = groupRep; OldWorld = oldWorld }
                    (World.GroupChangeEventAddress ->>- groupRep.GroupAddress)
                    groupRep
                    world
            else world

        static member internal updateGroup updater groupRep world =
            let group = World.getGroup groupRep world
            let group = updater group
            World.setGroup group groupRep world

        static member private registerGroup (groupRep : GroupRep) world =
            let dispatcher = groupRep.GetDispatcherNp world : GroupDispatcher
            dispatcher.Register groupRep world

        static member private unregisterGroup (groupRep : GroupRep) world =
            let dispatcher = groupRep.GetDispatcherNp world : GroupDispatcher
            dispatcher.Unregister groupRep world

        static member private addGroup groupHierarchy groupRep world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup groupRep world then
                let world = World.setGroupWithoutEvent group groupRep world
                let world = World.addEntities entities groupRep world
                let world = World.registerGroup groupRep world
                World.publish4 () (World.GroupAddEventAddress ->>- groupRep.GroupAddress) groupRep world
            else failwith <| "Adding a group that the world already contains at address '" + acstring groupRep.GroupAddress + "'."

        static member private addGroups groupHierarchies screenRep world =
            Map.fold
                (fun world groupName groupHierarchy ->
                    let screenAddress = screenRep.ScreenAddress
                    let groupAddress = World.satoga screenAddress groupName
                    World.addGroup groupHierarchy { GroupAddress = groupAddress } world)
                world
                groupHierarchies

        /// Query that the world contains a group at the given address.
        static member containsGroup groupRep world =
            Option.isSome <| World.optGroupFinder groupRep.GroupAddress world

        /// Get all the group addresses in the screen at the given address.
        static member getGroupRepsInScreen screenRep world =
            let groupMap = World.getGroupMapInScreen screenRep world
            let screenAddress = screenRep.ScreenAddress
            Seq.map (fun (kvp : KeyValuePair<string, _>) -> { GroupAddress = World.satoga screenAddress kvp.Key }) groupMap

        /// Remove a group from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the group's existence. Use with caution.
        static member removeGroupImmediate groupRep world =
            let world = World.publish4 () (World.GroupRemovingEventAddress ->>- groupRep.GroupAddress) groupRep world
            if World.containsGroup groupRep world then
                let world = World.unregisterGroup groupRep world
                let entityReps = World.getEntityRepsInGroup groupRep world
                let world = World.removeEntitiesImmediate entityReps world
                World.setOptGroupWithoutEvent None groupRep world
            else world

        /// Remove a group from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeGroup groupRep world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.removeGroupImmediate groupRep world }
            World.addTask task world
            
        /// Remove multiple groups from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the groups' existences. Use with caution.
        static member removeGroupsImmediate groupReps world =
            List.foldBack
                (fun groupRep world -> World.removeGroupImmediate groupRep world)
                (List.ofSeq groupReps)
                world

        /// Remove multiple groups from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeGroups groupReps world =
            World.removeGroupsImmediate groupReps world

        /// Create a group and add it to the world.
        static member createGroup dispatcherName optName screenRep world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let group = Group.make dispatcher optName
            Reflection.attachFields dispatcher group
            let groupRep = { GroupAddress = World.satoga screenRep.ScreenAddress group.Name }
            let world = World.addGroup (group, Map.empty) groupRep world
            (groupRep, world)

        /// Write a group hierarchy to an xml writer.
        static member writeGroup (writer : XmlWriter) groupRep world =
            let group = World.getGroup groupRep world
            let entityReps = World.getEntityRepsInGroup groupRep world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName group.DispatcherNp)
            Reflection.writePropertiesFromTarget tautology3 writer group
            writer.WriteStartElement EntitiesNodeName
            World.writeEntities writer entityReps world
            writer.WriteEndElement ()

        /// Write a group hierarchy to an xml file.
        static member writeGroupToFile (filePath : string) groupRep world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GroupNodeName
            World.writeGroup writer groupRep world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.WriteEndDocument ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple group hierarchies to an xml writer.
        static member writeGroups (writer : XmlWriter) groupReps world =
            let groupRepsSorted = Seq.sortBy (fun (groupRep : GroupRep) -> groupRep.GetCreationTimeNp world) groupReps
            let groupRepsPersistent = Seq.filter (fun (groupRep : GroupRep) -> groupRep.GetPersistent world) groupRepsSorted
            for groupRep in groupRepsPersistent do
                writer.WriteStartElement GroupNodeName
                World.writeGroup writer groupRep world
                writer.WriteEndElement ()

        /// Read a group hierarchy from an xml node.
        static member readGroup (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName screenRep world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName groupNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GroupDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName world.Components.GroupDispatchers
            
            // make the bare group with name as id
            let group = Group.make dispatcher None
            
            // attach the group's instrinsic fields from its dispatcher if any
            Reflection.attachFields group.DispatcherNp group

            // read the groups's properties
            Reflection.readPropertiesToTarget groupNode group
            
            // read the group's entities
            let groupRep = { GroupAddress = World.satoga screenRep.ScreenAddress group.Name }
            let world = World.addGroup (group, Map.empty) groupRep world
            let world = snd <| World.readEntities (groupNode : XmlNode) defaultEntityDispatcherName groupRep world

            // return the group, entities, and world
            (groupRep, world)

        /// Read a group hierarchy from an xml file.
        static member readGroupFromFile (filePath : string) screenRep world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name screenRep world

        /// Read multiple group hierarchies from an xml node.
        static member readGroups (screenNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName screenRep world =
            match screenNode.SelectSingleNode GroupsNodeName with
            | null -> ([], world)
            | groupsNode ->
                Seq.foldBack
                    (fun groupNode (groupReps, world) ->
                        let groupRep = World.readGroup groupNode defaultDispatcherName defaultEntityDispatcherName screenRep world
                        (groupRep :: groupReps, world))
                    (enumerable <| groupsNode.SelectNodes GroupNodeName)
                    ([], world)

        (* Screen *)

        static member private optScreenFinder (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, _) -> Some screen
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenAdder (screen : Screen) (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    let screenMap = Map.add screenName (screen, groupMap) screenMap
                    { world with Simulants = (game, screenMap) }
                | None ->
                    let screenMap = Map.add screenName (screen, Map.empty) screenMap
                    { world with Simulants = (game, screenMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenRemover (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    if Map.isEmpty groupMap then
                        let screenMap = Map.remove screenName screenMap
                        { world with Simulants = (game, screenMap) }
                    else failwith <| "Cannot remove screen " + acstring address + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member internal getScreenMap world =
            snd world.Simulants

        static member internal getOptScreen screenRep world =
            World.optScreenFinder screenRep.ScreenAddress world

        static member internal getScreen screenRep world : Screen =
            Option.get ^ World.getOptScreen screenRep world

        static member internal setScreenWithoutEvent screen screenRep world =
            World.screenAdder screen screenRep.ScreenAddress world

        static member internal setOptScreenWithoutEvent optScreen screenRep world =
            match optScreen with 
            | Some screen -> World.screenAdder screen screenRep.ScreenAddress world
            | None -> World.screenRemover screenRep.ScreenAddress world

        static member internal setScreen screen screenRep world =
            let oldWorld = world
            let world = World.screenAdder screen screenRep.ScreenAddress world
            if screen.PublishChanges then
                World.publish4
                    { SimulantRep = screenRep; OldWorld = oldWorld }
                    (World.ScreenChangeEventAddress ->>- screenRep.ScreenAddress)
                    screenRep
                    world
            else world

        static member internal updateScreen updater screenRep world =
            let screen = World.getScreen screenRep world
            let screen = updater screen
            World.setScreen screen screenRep world

        static member private registerScreen (screenRep : ScreenRep) world =
            let dispatcher = screenRep.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Register screenRep world

        static member private unregisterScreen (screenRep : ScreenRep) world =
            let dispatcher = screenRep.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Unregister screenRep world

        static member private addScreen screenHierarchy screenRep world =
            let (screen, groupHierarchies) = screenHierarchy
            if not <| World.containsScreen screenRep world then
                let world = World.setScreenWithoutEvent screen screenRep world
                let world = World.addGroups groupHierarchies screenRep world
                let world = World.registerScreen screenRep world
                World.publish4 () (World.ScreenAddEventAddress ->>- screenRep.ScreenAddress) screenRep world
            else failwith <| "Adding a screen that the world already contains at address '" + acstring screenRep.ScreenAddress + "'."

        /// Query that the world contains a group at the given address.
        static member containsScreen screenRep world =
            Option.isSome <| World.optScreenFinder screenRep.ScreenAddress world

        /// Get the addresses of all the world's screens.
        static member getScreenReps world =
            let screenMap = World.getScreenMap world
            Map.foldBack (fun screenName _ screenReps -> { ScreenAddress = ntoa<Screen> screenName } :: screenReps) screenMap []

        /// Remove a screen from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the screen's existence. Use with caution.
        static member removeScreenImmediate screenRep world =
            let world = World.publish4 () (World.ScreenRemovingEventAddress ->>- screenRep.ScreenAddress) screenRep world
            if World.containsScreen screenRep world then
                let world = World.unregisterScreen screenRep world
                let groupReps = World.getGroupRepsInScreen screenRep world
                let world = World.removeGroupsImmediate groupReps world
                World.setOptScreenWithoutEvent None screenRep world
            else world

        /// Remove a screen from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeScreen screenRep world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> World.removeScreenImmediate screenRep world }
            World.addTask task world

        /// Create a screen and add it to the world.
        static member createScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher optName
            Reflection.attachFields dispatcher screen
            let screenRep = { ScreenAddress = ntoa<Screen> screen.Name }
            let world = World.addScreen (screen, Map.empty) screenRep world
            (screenRep, world)
        
        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen dissolveData dispatcherName optName world =
            let optDissolveImage = Some dissolveData.DissolveImage
            let (screenRep, world) = World.createScreen dispatcherName optName world
            let world = screenRep.SetIncoming { TransitionDescriptor.make Incoming with TransitionLifetime = dissolveData.IncomingTime; OptDissolveImage = optDissolveImage } world
            let world = screenRep.SetOutgoing { TransitionDescriptor.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; OptDissolveImage = optDissolveImage } world
            (screenRep, world)

        /// Write a screen hierarchy to an xml writer.
        static member writeScreen (writer : XmlWriter) screenRep world =
            let screen = World.getScreen screenRep world
            let groupReps = World.getGroupRepsInScreen screenRep world
            writer.WriteAttributeString (DispatcherNameAttributeName, (screen.DispatcherNp.GetType ()).Name)
            Reflection.writePropertiesFromTarget tautology3 writer screen
            writer.WriteStartElement GroupsNodeName
            World.writeGroups writer groupReps world
            writer.WriteEndElement ()

        /// Write a screen hierarchy to an xml file.
        static member writeScreenToFile (filePath : string) screenRep world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement ScreenNodeName
            World.writeScreen writer screenRep world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple screen hierarchies to an xml writer.
        static member writeScreens (writer : XmlWriter) screenReps world =
            let screenRepsSorted = Seq.sortBy (fun (screenRep : ScreenRep) -> screenRep.GetCreationTimeNp world) screenReps
            let screenRepsPersistent = Seq.filter (fun (screenRep : ScreenRep) -> screenRep.GetPersistent world) screenRepsSorted
            for screenRep in screenRepsPersistent do
                writer.WriteStartElement ScreenNodeName
                World.writeScreen writer screenRep world
                writer.WriteEndElement ()

        /// Read a screen hierarchy from an xml node.
        static member readScreen (screenNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName screenNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.ScreenDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher None
            Reflection.attachFields screen.DispatcherNp screen
            Reflection.readPropertiesToTarget screenNode screen
            let screenRep = { ScreenAddress = ntoa<Screen> screen.Name }
            let world = World.addScreen (screen, Map.empty) screenRep world
            let world = snd <| World.readGroups (screenNode : XmlNode) defaultGroupDispatcherName defaultEntityDispatcherName screenRep world
            (screenRep, world)

        /// Read a screen hierarchy from an xml file.
        static member readScreenFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let screenNode = rootNode.[ScreenNodeName]
            World.readScreen screenNode typeof<ScreenDispatcher>.Name typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

        /// Read multiple screen hierarchies from an xml node.
        static member readScreens (gameNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            match gameNode.SelectSingleNode ScreensNodeName with
            | null -> ([], world)
            | screensNode ->
                Seq.foldBack
                    (fun screenNode (screenReps, world) ->
                        let (screenRep, world) = World.readScreen screenNode defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world
                        (screenRep :: screenReps, world))
                    (enumerable <| screensNode.SelectNodes ScreenNodeName)
                    ([], world)

        (* Game *)

        static member internal getGameMap world =
            let game = World.getGame world
            let screenMap = World.getScreenMap world
            (game, screenMap)

        static member internal getGame world : Game =
            fst world.Simulants

        static member internal setGame game world =
            let oldWorld = world
            let screenMap = World.getScreenMap world
            let world = { world with Simulants = (game, screenMap) }
            if game.PublishChanges then
                World.publish4
                    { OldWorld = oldWorld; SimulantRep = World.GameRep }
                    (World.GameChangeEventAddress ->>- World.GameAddress)
                    World.GameRep
                    world
            else world

        static member internal updateGame updater world =
            let game = World.getGame world
            let game = updater game
            World.setGame game world

        static member internal makeGame dispatcher =
            let game = Game.make dispatcher
            Reflection.attachFields dispatcher game
            game

        static member internal registerGame (gameRep : GameRep) (world : World) : World =
            let dispatcher = gameRep.GetDispatcherNp world : GameDispatcher
            dispatcher.Register gameRep world

        (* World *)

        /// Try to get the address of the currently selected screen.
        static member getOptSelectedScreenRep world =
            World.GameRep.GetOptSelectedScreenRep world

        /// Set the address of the currently selected screen to Some Address or None. Be careful
        /// using this function directly as you may be wanting to use the higher-level
        /// World.transitionScreen function.
        static member setOptSelectedScreenRep optScreenRep world =
            World.GameRep.SetOptSelectedScreenRep optScreenRep world

        /// Get the address of the currently selected screen (failing with an exception if there
        /// isn't one).
        static member getSelectedScreenRep world =
            Option.get <| World.getOptSelectedScreenRep world
        
        /// Set the address of the currently selected screen. Be careful using this function
        /// directly as you may be wanting to use the higher-level World.transitionScreen function.
        static member setSelectedScreenRep screenRep world =
            World.setOptSelectedScreenRep (Some screenRep) world

        /// Query that a simulant at the given address is the currently selected screen, is
        /// contained by the currently selected screen or its groups.
        static member isSimulantSelected<'s when 's :> SimulantRep> (simulantRep : 's) world =
            let optScreenRep = World.getOptSelectedScreenRep world
            let optScreenNames = Option.map (fun (screenRep : ScreenRep) -> screenRep.ScreenAddress.Names) optScreenRep
            match (simulantRep.SimulantAddress.Names, optScreenNames) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

        /// Write a game hierarchy to an xml writer.
        static member writeGame (writer : XmlWriter) world =
            let game = World.getGame world
            let screenReps = World.getScreenReps world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName game.DispatcherNp)
            Reflection.writePropertiesFromTarget tautology3 writer game
            writer.WriteStartElement ScreensNodeName
            World.writeScreens writer screenReps world
            writer.WriteEndElement ()

        /// Write a game hierarchy to an xml file.
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

        /// Read a game hierarchy from an xml node.
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
            let game = World.makeGame dispatcher
            Reflection.readPropertiesToTarget gameNode game
            let world = World.setGame game world
            let world =
                snd <| World.readScreens
                    gameNode
                    defaultScreenDispatcherName
                    defaultGroupDispatcherName
                    defaultEntityDispatcherName
                    world
            world

        /// Read a game hierarchy from an xml file.
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

        /// Query that the world contains a simulant at the given address.
        static member containsSimulant<'a when 'a :> SimulantRep> (simulantRep : 'a) world =
            match simulantRep :> SimulantRep with
            | :? GameRep -> true
            | :? ScreenRep as screenRep -> World.containsScreen screenRep world
            | :? GroupRep as groupRep -> World.containsGroup groupRep world
            | :? EntityRep as entityRep -> World.containsEntity entityRep world
            | _ -> failwithumf ()