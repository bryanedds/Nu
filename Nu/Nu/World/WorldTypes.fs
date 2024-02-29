// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Numerics
open System.Reflection
open Prime

[<RequireQualifiedAccess>]
module internal WorldTypes =

    // Debugging variables.
    let mutable internal Chosen = obj ()

    // Empty content variables.
    // OPTIMIZATION: allows us to avoid allocating content objects for entities that don't use them.
    let mutable internal EmptyGameContent = Unchecked.defaultof<obj>
    let mutable internal EmptyScreenContent = Unchecked.defaultof<obj>
    let mutable internal EmptyGroupContent = Unchecked.defaultof<obj>
    let mutable internal EmptyEntityContent = Unchecked.defaultof<obj>

    // Debugging F# reach-arounds.
    let mutable internal viewGame = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewScreen = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewGroup = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewEntity = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())

    // EventGraph F# reach-arounds.
    let mutable internal getSelectedScreenIdling : obj -> bool = Unchecked.defaultof<_>
    let mutable internal getSelectedScreenTransitioning : obj -> bool = Unchecked.defaultof<_>
    let mutable internal handleSubscribeAndUnsubscribeEvent : bool -> obj Address -> Simulant -> obj -> obj = Unchecked.defaultof<_>

    // Entity F# reach-arounds.
    let mutable internal getEntityIs2d : obj -> obj -> bool = Unchecked.defaultof<_>

/// The type of a subscription callback.
type Callback<'a, 's when 's :> Simulant> = Event<'a, 's> -> World -> Handling * World

/// Represents an unsubscription operation for an event.
and Unsubscription = World -> World

/// Details replacement for editing behavior for a simulant property, allowing the user to indicate that a property was
/// replaced.
and [<ReferenceEquality>] ReplaceProperty =
    { Snapshot : World -> World
      FocusProperty : World -> World
      IndicateReplaced : World -> World
      PropertyDescriptor : PropertyDescriptor }

/// Details additional editing behavior for an simulant's properties.
and AppendProperties =
    { Snapshot : World -> World
      UnfocusProperty : World -> World }

/// Details the additional editing behavior for a simulant in a viewport.
and [<ReferenceEquality>] OverlayViewport =
    { Snapshot : World -> World
      ViewportView : Matrix4x4
      ViewportProjection : Matrix4x4
      ViewportBounds : Box2 }

/// Specifies an aspect of simulant editing to perform.
and [<ReferenceEquality>] EditOperation =
    | ReplaceProperty of ReplaceProperty
    | AppendProperties of AppendProperties
    | OverlayViewport of OverlayViewport

/// The data for a change in a simulant.
and ChangeData =
    { Name : string
      Previous : obj
      Value : obj }

/// A generalized simulant lens.
and Lens =
    interface
        /// The name of the property accessed by the lens.
        abstract Name : string
        /// The simulant whose property is accessed by the lens.
        abstract This : Simulant
        /// Get the value of the property accessed by the lens.
        abstract Get : World -> obj
        /// Get an optional setter function that updates the property accessed by the lens.
        abstract SetOpt : (obj -> World -> World) voption
        /// Attempt to set the lensed property to the given value.
        abstract TrySet : obj -> World -> World
        /// The change event associated with the lensed property.
        abstract ChangeEvent : ChangeData Address
        /// The type of the lensed property.
        abstract Type : Type
        end

/// Provides access to the property of a simulant.
/// Initially inspired by Haskell lenses, but highly specialized for simulant properties.
and [<ReferenceEquality>] Lens<'a, 's when 's :> Simulant> =
    { Name : string
      This : 's
      Get : World -> 'a
      SetOpt : ('a -> World -> World) voption }

    interface Lens with
        member this.Name = this.Name
        member this.This = this.This :> Simulant
        member this.Get world = this.Get world :> obj
        member this.SetOpt = ValueOption.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.TrySet value world = match this.SetOpt with ValueSome set -> set (value :?> 'a) world | ValueNone -> world
        member this.ChangeEvent = this.ChangeEvent
        member this.Type = typeof<'a>

    /// Get the lensed value mapped by the `by` function.
    member this.GetBy by world =
        by (this.Get world)

    /// Get the lensed value mapped by the `by` function that includes the world value in its input.
    member this.GetByWorld by world =
        by (this.Get world) world

    /// Attempt to set the property in the world to the given value.
    member this.TrySet value world =
        match this.SetOpt with
        | ValueSome setter -> (true, setter value world)
        | ValueNone -> (false, world)

    /// Set the lensed property to the given value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    member this.Set value world =
        match this.TrySet value world with
        | (true, world) -> world
        | (false, _) -> failwith ("Lens for '" + this.Name + "' is readonly.")

    /// Attempt to update the lensed property's value using the given updater function that also receives the world as input.
    member this.TryUpdateWorld (updater : 'a -> World -> 'a) world =
        let value = this.Get world
        let value' = updater value world
        this.TrySet value' world

    /// Attempt to update the lensed property's value using the given updater function, optionally updating the world value in the process.
    member this.TryUpdateEffect (updater : 'a -> World -> ('a * World)) (world : World) =
        let value = this.Get world
        let (value', world) = updater value world
        this.TrySet value' world

    /// Attempt to update the lensed property's value using the given updater function.
    member this.TryUpdate (updater : 'a -> 'a) world =
        this.TryUpdateWorld (fun value _ -> updater value) world

    /// Update the lensed property's value using the given updater function that also receives the world as input.
    /// Returns the updated world or throws an exception if the lens is readonly.
    member this.UpdateWorld updater world =
        match this.TryUpdateWorld updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    /// Update the lensed property's value using the given updater function, optionally updating the world value in the process.
    /// Returns the updated world or throws an exception if the lens is readonly.
    member this.UpdateEffect updater world =
        match this.TryUpdateEffect updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    /// Update the lensed property's value using the given updater function.
    /// Returns the updated world or throws an exception if the lens is readonly.
    member this.Update updater world =
        match this.TryUpdate updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    /// The change event associated with the lensed property.
    member this.ChangeEvent : ChangeData Address =
        let names = [|Constants.Lens.ChangeName; this.Name; Constants.Lens.EventName|]
        match box this.This with
        | null ->
            // HACK: this case is a hack to allow Nu to resolve events contextually.
            let hashCode = Constants.Lens.ChangeNameHash ^^^ hash this.Name ^^^ Constants.Lens.EventNameHash
            let changeEventAddress = { Names = names; HashCode = hashCode; Anonymous = true }
            changeEventAddress 
        | _ -> rtoa names --> this.This.SimulantAddress

    /// The type of the lensed property.
    member inline this.Type = typeof<'a>

    /// Adds the specified value to the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( += ) (lens : Lens<_, _>, value) =  lens.Update (flip (+) value)

    /// Subtracts the specified value from the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( -= ) (lens : Lens<_, _>, value) =  lens.Update (flip (-) value)

    /// Multiplies the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( *= ) (lens : Lens<_, _>, value) =  lens.Update (flip (*) value)

    /// Divides the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( /= ) (lens : Lens<_, _>, value) =  lens.Update (flip (/) value)

    /// Computes the modulus of the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( %= ) (lens : Lens<_, _>, value) =  lens.Update (flip (%) value)

    /// Negates the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( ~+ ) (lens : Lens<_, _>) =  lens.Update (~+)

    /// Negates the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( ~- ) (lens : Lens<_, _>) =  lens.Update (~-)

    /// Increments the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( !+ ) (lens : Lens<_, _>) =  lens.Update inc

    /// Decrements the lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline ( !- ) (lens : Lens<_, _>) =  lens.Update dec

    /// Set a lensed property's value.
    /// Returns the updated world or throws an exception if the lens is readonly.
    static member inline (<--) (lens : Lens<_, _>, value) = lens.Set value

    /// Get a lensed property's value.
    static member inline (!.) (lens : Lens<_, _>) = fun world -> lens.Get world

/// A model-message-command-content (MMCC) signal tag type.
and Signal = interface end

/// A model-message-command-content (MMCC) message tag type.
and Message = inherit Signal

/// A model-message-command-content (MMCC) command tag type.
and Command = inherit Signal

/// Specifies the desired screen, if any, or whether to ignore screen desire functionality altogether.
and DesiredScreen =
    | Desire of Screen
    | DesireNone
    | DesireIgnore

/// The data required to execute slide screen presentation.
and Slide =
    { IdlingTime : GameTime
      Destination : Screen }

/// Describes the behavior of a screen.
and ScreenBehavior =
    | Vanilla
    | Dissolve of DissolveDescriptor * SongDescriptor option
    | Slide of DissolveDescriptor * SlideDescriptor * SongDescriptor option * Screen
    | OmniScreen

/// The data for a change in the world's ambient state.
and AmbientChangeData = 
    { OldWorldWithOldState : World }

/// Describes the information needed to sort simulants.
/// OPTIMIZATION: carries related simulant to avoid GC pressure.
/// NOTE: SortPriority can't be structified because it is currently cast to IComparable.
and [<CustomEquality; CustomComparison>] SortPriority =
    { SortElevation : single
      SortHorizon : single
      SortTarget : Simulant }

    static member equals left right =
        left.SortElevation = right.SortElevation &&
        left.SortTarget = right.SortTarget

    static member compare left right =
        if left.SortElevation < right.SortElevation then 1
        elif left.SortElevation > right.SortElevation then -1
        elif left.SortHorizon < right.SortHorizon then -1
        elif left.SortHorizon > right.SortHorizon then 1
        else 0

    override this.GetHashCode () =
        this.SortElevation.GetHashCode ()

    override this.Equals that =
        match that with
        | :? SortPriority as that -> SortPriority.equals this that
        | _ -> failwithumf ()

    interface SortPriority IComparable with
        member this.CompareTo that =
            SortPriority.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? SortPriority as that -> (this :> SortPriority IComparable).CompareTo that
            | _ -> failwithumf ()

/// Generalized interface tag for late-bound objects.
and LateBindings = interface end

/// Generalized interface tag for dispatchers.
and Dispatcher = inherit LateBindings

/// Generalized interface tag for simulant dispatchers.
and SimulantDispatcher () = interface Dispatcher

/// The default dispatcher for games.
and GameDispatcher () =
    inherit SimulantDispatcher ()

    /// Register a game when adding it to the world.
    abstract Register : Game * World -> World
    default this.Register (_, world) = world

    /// Unregister a game when finished with the world.
    abstract Unregister : Game * World -> World
    default this.Unregister (_, world) = world

    /// Pre-update a game.
    abstract PreUpdate : Game * World -> World
    default this.PreUpdate (_, world) = world

    /// Update a game.
    abstract Update : Game * World -> World
    default this.Update (_, world) = world

    /// Post-update a game.
    abstract PostUpdate : Game * World -> World
    default this.PostUpdate (_, world) = world

    /// Render a game.
    abstract Render : RenderPass * Game * World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a game.
    abstract Signal : obj * Game * World -> World
    default this.Signal (_, _, world) = world

    /// Attempt to get the initial model value if the dispatcher defines one.
    abstract TryGetInitialModel<'a> : World -> 'a option
    default this.TryGetInitialModel _ = None

    /// Attempt to synchronize the content of a game.
    abstract TrySynchronize : bool * Game * World -> World
    default this.TrySynchronize (_, _, world) = world

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : EditOperation * Game * World -> World
    default this.Edit (_, _, world) = world

    /// Attempt to truncate a game model.
    abstract TryTruncateModel<'a> : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a game model.
    abstract TryUntruncateModel<'a> : 'a * Game * World -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for screens.
and ScreenDispatcher () =
    inherit SimulantDispatcher ()

    /// Register a screen when adding it to the world.
    abstract Register : Screen * World -> World
    default this.Register (_, world) = world

    /// Unregister a screen when removing it from the world.
    abstract Unregister : Screen * World -> World
    default this.Unregister (_, world) = world

    /// Pre-update a screen.
    abstract PreUpdate : Screen * World -> World
    default this.PreUpdate (_, world) = world

    /// Update a screen.
    abstract Update : Screen * World -> World
    default this.Update (_, world) = world

    /// Post-update a screen.
    abstract PostUpdate : Screen * World -> World
    default this.PostUpdate (_, world) = world

    /// Render a screen.
    abstract Render : RenderPass * Screen * World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a screen.
    abstract Signal : obj * Screen * World -> World
    default this.Signal (_, _, world) = world

    /// Attempt to get the initial model value if the dispatcher defines one.
    abstract TryGetInitialModel<'a> : World -> 'a option
    default this.TryGetInitialModel _ = None

    /// Attempt to synchronize the content of a screen.
    abstract TrySynchronize : bool * Screen * World -> World
    default this.TrySynchronize (_, _, world) = world

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : EditOperation * Screen * World -> World
    default this.Edit (_, _, world) = world

    /// Attempt to truncate a screen model.
    abstract TryTruncateModel<'a> : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a screen model.
    abstract TryUntruncateModel<'a> : 'a * Screen* World  -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for groups.
and GroupDispatcher () =
    inherit SimulantDispatcher ()

    /// Register a group when adding it to a screen.
    abstract Register : Group * World -> World
    default this.Register (_, world) = world

    /// Unregister a group when removing it from a screen.
    abstract Unregister : Group * World -> World
    default this.Unregister (_, world) = world

    /// Pre-update a group.
    abstract PreUpdate : Group * World -> World
    default this.PreUpdate (_, world) = world

    /// Update a group.
    abstract Update : Group * World -> World
    default this.Update (_, world) = world

    /// Post-update a group.
    abstract PostUpdate : Group * World -> World
    default this.PostUpdate (_, world) = world

    /// Render a group.
    abstract Render : RenderPass * Group * World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a group.
    abstract Signal : obj * Group * World -> World
    default this.Signal (_, _, world) = world

    /// Attempt to get the initial model value if the dispatcher defines one.
    abstract TryGetInitialModel<'a> : World -> 'a option
    default this.TryGetInitialModel _ = None

    /// Attempt to synchronize the content of a group.
    abstract TrySynchronize : bool * Group * World -> World
    default this.TrySynchronize (_, _, world) = world

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : EditOperation * Group * World -> World
    default this.Edit (_, _, world) = world

    /// Attempt to truncate a group model.
    abstract TryTruncateModel<'a> : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a group model.
    abstract TryUntruncateModel<'a> : 'a * Group* World  -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for entities.
and EntityDispatcher (is2d, perimeterCentered, physical) =
    inherit SimulantDispatcher ()

    static member Properties =
        [Define? Position Vector3.Zero
         Define? PositionLocal Vector3.Zero
         Define? Rotation Quaternion.Identity
         Define? RotationLocal Quaternion.Identity
         Define? Scale Vector3.One
         Define? ScaleLocal Vector3.One
         Define? Offset Vector3.Zero
         Define? Angles Vector3.Zero
         Define? AnglesLocal Vector3.Zero
         Define? Degrees Vector3.Zero
         Define? DegreesLocal Vector3.Zero
         Define? Size Constants.Engine.Entity3dSizeDefault
         Define? Elevation 0.0f
         Define? ElevationLocal 0.0f
         Define? Overflow 1.0f
         Define? Presence Exterior
         Define? Absolute false
         Define? Model { DesignerType = typeof<unit>; DesignerValue = () }
         Define? MountOpt Option<Entity Relation>.None
         Define? PropagationSourceOpt Option<Entity>.None
         Define? PublishChangeEvents false
         Define? Enabled true
         Define? EnabledLocal true
         Define? Visible true
         Define? VisibleLocal true
         Define? Pickable true
         Define? PerimeterCentered true
         Define? Static false
         Define? LightProbe false
         Define? Light false
         Define? AlwaysUpdate false
         Define? AlwaysRender false
         Define? PublishPreUpdates false
         Define? PublishUpdates false
         Define? PublishPostUpdates false
         Define? Persistent true
         Define? PropagatedDescriptorOpt Option<EntityDescriptor>.None]

    /// Register an entity when adding it to a group.
    abstract Register : Entity * World -> World
    default this.Register (_, world) = world

    /// Unregister an entity when removing it from a group.
    abstract Unregister : Entity * World -> World
    default this.Unregister (_, world) = world

#if !DISABLE_ENTITY_PRE_UPDATE
    /// Pre-update an entity.
    abstract PreUpdate : Entity * World -> World
    default this.PreUpdate (_, world) = world
#endif

    /// Update an entity.
    abstract Update : Entity * World -> World
    default this.Update (_, world) = world

#if !DISABLE_ENTITY_POST_UPDATE
    /// Post-update an entity.
    abstract PostUpdate : Entity * World -> World
    default this.PostUpdate (_, world) = world
#endif

    /// Render an entity.
    abstract Render : RenderPass * Entity * World -> unit
    default this.Render (_, _, _) = ()

    /// Apply physics changes from a physics engine to an entity.
    abstract ApplyPhysics : Vector3 * Quaternion * Vector3 * Vector3 * Entity * World -> World
    default this.ApplyPhysics (_, _, _, _, _, world) = world

    /// Send a signal to an entity.
    abstract Signal : obj * Entity * World -> World
    default this.Signal (_, _, world) = world

    /// Attempt to get the initial model value if the dispatcher defines one.
    abstract TryGetInitialModel<'a> : World -> 'a option
    default this.TryGetInitialModel _ = None

    /// Attempt to synchronize content of an entity.
    abstract TrySynchronize : bool * Entity * World -> World
    default this.TrySynchronize (_, _, world) = world

    /// Get the default size of an entity.
    abstract GetAttributesInferred : Entity * World -> AttributesInferred
    default this.GetAttributesInferred (_, _) =
        if this.Is2d
        then AttributesInferred.make Constants.Engine.Entity2dSizeDefault v3Zero
        else AttributesInferred.make Constants.Engine.Entity3dSizeDefault v3Zero

    /// Attempt to pick an entity with a ray.
    abstract RayCast : Ray3 * Entity * World -> single array
    default this.RayCast (_, _, _) = [||]

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : EditOperation * Entity * World -> World
    default this.Edit (_, _, world) = world

    /// Attempt to truncate an entity model.
    abstract TryTruncateModel<'a> : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate an entity model.
    abstract TryUntruncateModel<'a> : 'a * Entity* World  -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

    /// Whether the dispatcher participates directly in a physics system (not counting its facets).
    member this.Physical = physical

    /// Whether the dispatcher uses a centered perimeter by default.
    member this.PerimeterCentered = perimeterCentered

    /// Whether the dispatcher has a 2-dimensional transform interpretation.
    member this.Is2d = is2d

    /// Whether the dispatcher has a 3-dimensional transform interpretation.
    member this.Is3d = not is2d

/// Dynamically augments an entity's behavior in a composable way.
and Facet (physical) =

    /// Register a facet when adding it to an entity.
    abstract Register : Entity * World -> World
    default this.Register (_, world) = world

    /// Unregister a facet when removing it from an entity.
    abstract Unregister : Entity * World -> World
    default this.Unregister (_, world) = world

    /// Participate in the registration of an entity's physics with the physics subsystem.
    abstract RegisterPhysics : Entity * World -> World
    default this.RegisterPhysics (_, world) = world

    /// Participate in the unregistration of an entity's physics from the physics subsystem.
    abstract UnregisterPhysics : Entity * World -> World
    default this.UnregisterPhysics (_, world) = world

#if !DISABLE_ENTITY_PRE_UPDATE
    /// Pre-update a facet.
    abstract PreUpdate : Entity * World -> World
    default this.PreUpdate (_, world) = world
#endif

    /// Update a facet.
    abstract Update : Entity * World -> World
    default this.Update (_, world) = world

#if !DISABLE_ENTITY_POST_UPDATE
    /// Post-update a facet.
    abstract PostUpdate : Entity * World -> World
    default this.PostUpdate (_, world) = world
#endif

    /// Render a facet.
    abstract Render : RenderPass * Entity * World -> unit
    default this.Render (_, _, _) = ()

    /// Participate in attempting to pick an entity with a ray.
    abstract RayCast : Ray3 * Entity * World -> single array
    default this.RayCast (_, _, _) = [||]

    /// Participate in getting the default size of an entity.
    abstract GetAttributesInferred : Entity * World -> AttributesInferred
    default this.GetAttributesInferred (entity, world) =
        if WorldTypes.getEntityIs2d entity world
        then AttributesInferred.make Constants.Engine.Entity2dSizeDefault v3Zero
        else AttributesInferred.make Constants.Engine.Entity3dSizeDefault v3Zero

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : EditOperation * Entity * World -> World
    default this.Edit (_, _, world) = world

    /// Whether a facet participates in a physics system.
    member this.Physical = physical

    interface LateBindings

/// Describes a property to the MMCC content system.
and [<ReferenceEquality>] PropertyContent =
    { PropertyInitializer : bool
      PropertyLens : Lens
      PropertyValue : obj }
    static member inline make initializer lens value =
        { PropertyInitializer = initializer
          PropertyLens = lens
          PropertyValue = value }

/// Describes an initializer to the MMCC content system.
and [<ReferenceEquality>] InitializerContent =
    | PropertyContent of PropertyContent
    | EventSignalContent of obj Address * obj
    | EventHandlerContent of PartialEquatable<obj Address, Event -> obj>

/// Describes a simulant to the MMCC content system.
and SimulantContent =
    abstract DispatcherNameOpt : string option
    abstract SimulantNameOpt : string option
    abstract SimulantCachedOpt : Simulant with get, set
    abstract EventSignalContentsOpt : OrderedDictionary<obj Address * obj, Guid>
    abstract EventHandlerContentsOpt : OrderedDictionary<int * obj Address, Guid * (Event -> obj)>
    abstract PropertyContentsOpt : List<PropertyContent>
    abstract GetChildContentsOpt<'v when 'v :> SimulantContent> : unit -> OrderedDictionary<string, 'v>

/// Describes a game to the MMCC content system.
and [<ReferenceEquality>] GameContent =
    { InitialScreenNameOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, Guid> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, Guid * (Event -> obj)> // OPTIMIZATION: lazily created.
      mutable PropertyContentsOpt : List<PropertyContent> // OPTIMIZATION: lazily created.
      ScreenContents : OrderedDictionary<string, ScreenContent> }
    interface SimulantContent with
        member this.DispatcherNameOpt = None
        member this.SimulantNameOpt = None
        member this.SimulantCachedOpt with get () = this.SimulantCachedOpt and set value = this.SimulantCachedOpt <- value
        member this.EventSignalContentsOpt = this.EventSignalContentsOpt
        member this.EventHandlerContentsOpt = this.EventHandlerContentsOpt
        member this.PropertyContentsOpt = this.PropertyContentsOpt
        member this.GetChildContentsOpt<'v when 'v :> SimulantContent> () = this.ScreenContents :> obj :?> OrderedDictionary<string, 'v>
    static member empty =
        { InitialScreenNameOpt = None
          SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = null
          EventHandlerContentsOpt = null
          PropertyContentsOpt = null
          ScreenContents = OrderedDictionary StringComparer.Ordinal }

/// Describes a screen to the MMCC content system.
and [<ReferenceEquality>] ScreenContent =
    { ScreenDispatcherName : string
      ScreenName : string
      ScreenBehavior : ScreenBehavior
      GroupFilePathOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, Guid> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, Guid * (Event -> obj)> // OPTIMIZATION: lazily created.
      mutable PropertyContentsOpt : List<PropertyContent> // OPTIMIZATION: lazily created.
      GroupContents : OrderedDictionary<string, GroupContent> }
    interface SimulantContent with
        member this.DispatcherNameOpt = Some this.ScreenDispatcherName
        member this.SimulantNameOpt = Some this.ScreenName
        member this.SimulantCachedOpt with get () = this.SimulantCachedOpt and set value = this.SimulantCachedOpt <- value
        member this.EventSignalContentsOpt = this.EventSignalContentsOpt
        member this.EventHandlerContentsOpt = this.EventHandlerContentsOpt
        member this.PropertyContentsOpt = this.PropertyContentsOpt
        member this.GetChildContentsOpt<'v when 'v :> SimulantContent> () = this.GroupContents :> obj :?> OrderedDictionary<string, 'v>
    static member empty =
        { ScreenDispatcherName = nameof ScreenDispatcher
          ScreenName = nameof Screen
          ScreenBehavior = Vanilla
          GroupFilePathOpt = None
          SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = null
          EventHandlerContentsOpt = null
          PropertyContentsOpt = null
          GroupContents = OrderedDictionary StringComparer.Ordinal }

/// Describes a group to the MMCC content system.
and [<ReferenceEquality>] GroupContent =
    { GroupDispatcherName : string
      GroupName : string
      GroupFilePathOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, Guid> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, Guid * (Event -> obj)> // OPTIMIZATION: lazily created.
      mutable PropertyContentsOpt : List<PropertyContent> // OPTIMIZATION: lazily created.
      mutable EntityContentsOpt : OrderedDictionary<string, EntityContent> } // OPTIMIZATION: lazily created.
    interface SimulantContent with
        member this.DispatcherNameOpt = Some this.GroupDispatcherName
        member this.SimulantNameOpt = Some this.GroupName
        member this.SimulantCachedOpt with get () = this.SimulantCachedOpt and set value = this.SimulantCachedOpt <- value
        member this.EventSignalContentsOpt = this.EventSignalContentsOpt
        member this.EventHandlerContentsOpt = this.EventHandlerContentsOpt
        member this.PropertyContentsOpt = this.PropertyContentsOpt
        member this.GetChildContentsOpt<'v when 'v :> SimulantContent> () = this.EntityContentsOpt :> obj :?> OrderedDictionary<string, 'v>
    static member empty =
        { GroupDispatcherName = nameof GroupDispatcher
          GroupName = nameof Group
          GroupFilePathOpt = None
          SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = null
          EventHandlerContentsOpt = null
          PropertyContentsOpt = null
          EntityContentsOpt = null }

/// Describes an entity to the MMCC content system.
and [<ReferenceEquality>] EntityContent =
    { EntityDispatcherName : string
      EntityName : string
      mutable EntityCachedOpt : Entity // OPTIMIZATION: allows us to more often hit the EntityStateOpt cache. May be null.
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, Guid> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, Guid * (Event -> obj)> // OPTIMIZATION: lazily created.
      mutable PropertyContentsOpt : List<PropertyContent> // OPTIMIZATION: lazily created.
      mutable EntityContentsOpt : OrderedDictionary<string, EntityContent> } // OPTIMIZATION: lazily created.
    interface SimulantContent with
        member this.DispatcherNameOpt = Some this.EntityDispatcherName
        member this.SimulantNameOpt = Some this.EntityName
        member this.SimulantCachedOpt with get () = this.EntityCachedOpt :> Simulant and set value = this.EntityCachedOpt <- value :?> Entity
        member this.EventSignalContentsOpt = this.EventSignalContentsOpt
        member this.EventHandlerContentsOpt = this.EventHandlerContentsOpt
        member this.PropertyContentsOpt = this.PropertyContentsOpt
        member this.GetChildContentsOpt<'v when 'v :> SimulantContent> () = this.EntityContentsOpt :> obj :?> OrderedDictionary<string, 'v>
    static member empty =
        { EntityDispatcherName = nameof EntityDispatcher
          EntityName = nameof Entity
          EntityCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = null
          EventHandlerContentsOpt = null
          PropertyContentsOpt = null
          EntityContentsOpt = null }

/// Generalized interface for simulant state.
and SimulantState =
    interface
        abstract GetXtension : unit -> Xtension
        end

/// Hosts the ongoing state of a game.
and [<ReferenceEquality; CLIMutable>] GameState =
    { Dispatcher : GameDispatcher
      Xtension : Xtension
      Model : DesignerProperty
      Content : GameContent
      OmniScreenOpt : Screen option
      SelectedScreenOpt : Screen option
      DesiredScreen : DesiredScreen
      ScreenTransitionDestinationOpt : Screen option
      Eye2dCenter : Vector2
      Eye2dSize : Vector2
      Eye3dCenter : Vector3
      Eye3dRotation : Quaternion
      Eye3dFrustumInterior : Frustum // OPTIMIZATION: cached value
      Eye3dFrustumExterior : Frustum // OPTIMIZATION: cached value
      Eye3dFrustumImposter : Frustum // OPTIMIZATION: cached value
      Order : int64
      Id : Guid }

    /// Make a game state value.
    static member make (dispatcher : GameDispatcher) =
        let eye3dCenter = Constants.Engine.Eye3dCenterDefault
        let eye3dRotation = quatIdentity
        let viewport = Constants.Render.Viewport
        { Dispatcher = dispatcher
          Xtension = Xtension.makeFunctional ()
          Model = { DesignerType = typeof<unit>; DesignerValue = () }
          Content = WorldTypes.EmptyGameContent :?> GameContent
          OmniScreenOpt = None
          SelectedScreenOpt = None
          DesiredScreen = DesireIgnore
          ScreenTransitionDestinationOpt = None
          Eye2dCenter = v2Zero
          Eye2dSize = v2 (single Constants.Render.VirtualResolutionX) (single Constants.Render.VirtualResolutionY)
          Eye3dCenter = eye3dCenter
          Eye3dRotation = eye3dRotation
          Eye3dFrustumInterior = viewport.Frustum (Constants.Render.NearPlaneDistanceInterior, Constants.Render.FarPlaneDistanceInterior, eye3dCenter, eye3dRotation)
          Eye3dFrustumExterior = viewport.Frustum (Constants.Render.NearPlaneDistanceExterior, Constants.Render.FarPlaneDistanceExterior, eye3dCenter, eye3dRotation)
          Eye3dFrustumImposter = viewport.Frustum (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, eye3dCenter, eye3dRotation)
          Order = Core.getTimeStampUnique ()
          Id = Gen.id }

    /// Try to get an xtension property and its type information.
    static member tryGetProperty (propertyName, gameState, propertyRef : Property outref) =
        Xtension.tryGetProperty (propertyName, gameState.Xtension, &propertyRef)

    /// Get an xtension property and its type information.
    static member getProperty propertyName gameState =
        Xtension.getProperty propertyName gameState.Xtension

    /// Try to set an xtension property with explicit type information.
    static member trySetProperty propertyName property gameState =
        match Xtension.trySetProperty propertyName property gameState.Xtension with
        | struct (true, xtension) -> struct (true, { gameState with Xtension = xtension })
        | struct (false, _) -> struct (false, gameState)

    /// Set an xtension property with explicit type information.
    static member setProperty propertyName property gameState =
        { gameState with GameState.Xtension = Xtension.setProperty propertyName property gameState.Xtension }

    /// Attach an xtension property.
    static member attachProperty name property gameState =
        { gameState with GameState.Xtension = Xtension.attachProperty name property gameState.Xtension }

    /// Detach an xtension property.
    static member detachProperty name gameState =
        let xtension = Xtension.detachProperty name gameState.Xtension
        { gameState with GameState.Xtension = xtension }

    /// Copy a game such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with GameState.Dispatcher = this.Dispatcher }

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of a screen.
and [<ReferenceEquality; CLIMutable>] ScreenState =
    { Dispatcher : ScreenDispatcher
      Xtension : Xtension
      Model : DesignerProperty
      Content : ScreenContent
      TransitionState : TransitionState
      Incoming : Transition
      Outgoing : Transition
      SlideOpt : Slide option
      Protected : bool
      Persistent : bool
      Order : int64
      Id : uint64
      Name : string }

    /// Make a screen state value.
    static member make time nameOpt (dispatcher : ScreenDispatcher) =
        let (id, name) = Gen.id64AndNameIf nameOpt
        { Dispatcher = dispatcher
          Xtension = Xtension.makeFunctional ()
          Model = { DesignerType = typeof<unit>; DesignerValue = () }
          Content = WorldTypes.EmptyScreenContent :?> ScreenContent
          TransitionState = IdlingState time
          Incoming = Transition.make Incoming
          Outgoing = Transition.make Outgoing
          SlideOpt = None
          Protected = false
          Persistent = true
          Order = Core.getTimeStampUnique ()
          Id = id
          Name = name }

    /// Try to get an xtension property and its type information.
    static member tryGetProperty (propertyName, screenState, propertyRef : Property outref) =
        Xtension.tryGetProperty (propertyName, screenState.Xtension, &propertyRef)

    /// Get an xtension property and its type information.
    static member getProperty propertyName screenState =
        Xtension.getProperty propertyName screenState.Xtension

    /// Try to set an xtension property with explicit type information.
    static member trySetProperty propertyName property screenState =
        match Xtension.trySetProperty propertyName property screenState.Xtension with
        | struct (true, xtension) -> struct (true, { screenState with Xtension = xtension })
        | struct (false, _) -> struct (false, screenState)

    /// Set an xtension property with explicit type information.
    static member setProperty propertyName property screenState =
        { screenState with ScreenState.Xtension = Xtension.setProperty propertyName property screenState.Xtension }

    /// Attach an xtension property.
    static member attachProperty name property screenState =
        { screenState with ScreenState.Xtension = Xtension.attachProperty name property screenState.Xtension }

    /// Detach an xtension property.
    static member detachProperty name screenState =
        let xtension = Xtension.detachProperty name screenState.Xtension
        { screenState with ScreenState.Xtension = xtension }

    /// Copy a screen such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with ScreenState.Dispatcher = this.Dispatcher }

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of a group.
and [<ReferenceEquality; CLIMutable>] GroupState =
    { Dispatcher : GroupDispatcher
      Xtension : Xtension
      Model : DesignerProperty
      Content : GroupContent
      Visible : bool
      Protected : bool
      Persistent : bool
      Order : int64
      Id : uint64
      Name : string }

    /// Make a group state value.
    static member make nameOpt (dispatcher : GroupDispatcher) =
        let (id, name) = Gen.id64AndNameIf nameOpt
        { Dispatcher = dispatcher
          Xtension = Xtension.makeFunctional ()
          Model = { DesignerType = typeof<unit>; DesignerValue = () }
          Content = WorldTypes.EmptyGroupContent :?> GroupContent
          Visible = true
          Protected = false
          Persistent = true
          Order = Core.getTimeStampUnique ()
          Id = id
          Name = name }

    /// Try to get an xtension property and its type information.
    static member tryGetProperty (propertyName, groupState, propertyRef : Property outref) =
        Xtension.tryGetProperty (propertyName, groupState.Xtension, &propertyRef)

    /// Get an xtension property and its type information.
    static member getProperty propertyName groupState =
        Xtension.getProperty propertyName groupState.Xtension

    /// Try to set an xtension property with explicit type information.
    static member trySetProperty propertyName property groupState =
        match Xtension.trySetProperty propertyName property groupState.Xtension with
        | struct (true, xtension) -> struct (true, { groupState with Xtension = xtension })
        | struct (false, _) -> struct (false, groupState)

    /// Set an xtension property with explicit type information.
    static member setProperty propertyName property groupState =
        { groupState with GroupState.Xtension = Xtension.setProperty propertyName property groupState.Xtension }

    /// Attach an xtension property.
    static member attachProperty name property groupState =
        { groupState with GroupState.Xtension = Xtension.attachProperty name property groupState.Xtension }

    /// Detach an xtension property.
    static member detachProperty name groupState =
        let xtension = Xtension.detachProperty name groupState.Xtension
        { groupState with GroupState.Xtension = xtension }

    /// Copy a group such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with GroupState.Dispatcher = this.Dispatcher }

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of an entity.
/// OPTIMIZATION: ScriptFrameOpt is instantiated only when needed.
and [<ReferenceEquality; CLIMutable>] EntityState =
    { mutable Transform : Transform
      mutable Dispatcher : EntityDispatcher
      mutable Facets : Facet array
      mutable Xtension : Xtension
      mutable Model : DesignerProperty
      mutable Content : EntityContent
      mutable PositionLocal : Vector3
      mutable RotationLocal : Quaternion
      mutable ScaleLocal : Vector3
      mutable AnglesLocal : Vector3
      mutable ElevationLocal : single
      mutable MountOpt : Entity Relation option
      mutable PropagationSourceOpt : Entity option
      mutable OverlayNameOpt : string option
      mutable FacetNames : string Set
      mutable PropagatedDescriptorOpt : EntityDescriptor option
      mutable Order : int64
      Id : uint64
      Surnames : string array }

    member this.PerimeterCenter with get () = this.Transform.PerimeterCenter and set value = this.Transform.PerimeterCenter <- value
    member this.PerimeterBottom with get () = this.Transform.PerimeterBottom and set value = this.Transform.PerimeterBottom <- value
    member this.PerimeterBottomLeft with get () = this.Transform.PerimeterBottomLeft and set value = this.Transform.PerimeterBottomLeft <- value
    member this.PerimeterMin with get () = this.Transform.PerimeterMin and set value = this.Transform.PerimeterMin <- value
    member this.PerimeterMax with get () = this.Transform.PerimeterMax and set value = this.Transform.PerimeterMax <- value
    member this.PerimeterCenterLocal with get () = this.PositionLocal + (this.Transform.PerimeterCenter - this.Transform.Position)
    member this.PerimeterBottomLocal with get () = this.PositionLocal + (this.Transform.PerimeterBottom - this.Transform.Position)
    member this.PerimeterBottomLeftLocal with get () = this.PositionLocal + (this.Transform.PerimeterBottomLeft - this.Transform.Position)
    member this.PerimeterMinLocal with get () = this.PositionLocal + (this.Transform.PerimeterMin - this.Transform.Position)
    member this.PerimeterMaxLocal with get () = this.PositionLocal + (this.Transform.PerimeterMax - this.Transform.Position)
    member this.Position with get () = this.Transform.Position and set value = this.Transform.Position <- value
    member this.Rotation with get () = this.Transform.Rotation and set value = this.Transform.Rotation <- value
    member this.Scale with get () = this.Transform.Scale and set value = this.Transform.Scale <- value
    member this.Offset with get () = this.Transform.Offset and set value = this.Transform.Offset <- value
    member this.Angles with get () = this.Transform.Angles and set value = this.Transform.Angles <- value
    member this.Degrees with get () = this.Transform.Degrees and set value = this.Transform.Degrees <- value
    member this.DegreesLocal with get () = Math.RadiansToDegrees3d this.AnglesLocal and set value = this.AnglesLocal <- Math.DegreesToRadians3d value
    member this.Size with get () = this.Transform.Size and set value = this.Transform.Size <- value
    member this.RotationMatrix with get () = this.Transform.RotationMatrix
    member this.Elevation with get () = this.Transform.Elevation and set value = this.Transform.Elevation <- value
    member this.Overflow with get () = this.Transform.Overflow and set value = this.Transform.Overflow <- value
    member this.AffineMatrix with get () = this.Transform.AffineMatrix
    member this.PerimeterUnscaled with get () = this.Transform.PerimeterUnscaled and set value = this.Transform.PerimeterUnscaled <- value
    member this.Perimeter with get () = this.Transform.Perimeter and set value = this.Transform.Perimeter <- value
    member this.Bounds with get () = if this.Is2d then this.Transform.Bounds2d else this.Transform.Bounds3d
    member this.Presence with get () = this.Transform.Presence and set value = this.Transform.Presence <- value
    member internal this.Active with get () = this.Transform.Active and set value = this.Transform.Active <- value
    member internal this.Dirty with get () = this.Transform.Dirty and set value = this.Transform.Dirty <- value
    member internal this.Invalidated with get () = this.Transform.Invalidated and set value = this.Transform.Invalidated <- value
    member this.Absolute with get () = this.Transform.Absolute and set value = this.Transform.Absolute <- value
    member this.Imperative with get () = this.Transform.Imperative and set value = this.Transform.Imperative <- value
    member this.PublishChangeEvents with get () = this.Transform.PublishChangeEvents and set value = this.Transform.PublishChangeEvents <- value
    member this.Enabled with get () = this.Transform.Enabled and set value = this.Transform.Enabled <- value
    member this.EnabledLocal with get () = this.Transform.EnabledLocal and set value = this.Transform.EnabledLocal <- value
    member this.Visible with get () = this.Transform.Visible and set value = this.Transform.Visible <- value
    member this.VisibleLocal with get () = this.Transform.VisibleLocal and set value = this.Transform.VisibleLocal <- value
    member this.Pickable with get () = this.Transform.Pickable and internal set value = this.Transform.Pickable <- value
    member this.AlwaysUpdate with get () = this.Transform.AlwaysUpdate and set value = this.Transform.AlwaysUpdate <- value
    member this.AlwaysRender with get () = this.Transform.AlwaysRender and set value = this.Transform.AlwaysRender <- value
    member this.PublishPreUpdates with get () = this.Transform.PublishPreUpdates and set value = this.Transform.PublishPreUpdates <- value
    member this.PublishUpdates with get () = this.Transform.PublishUpdates and set value = this.Transform.PublishUpdates <- value
    member this.PublishPostUpdates with get () = this.Transform.PublishPostUpdates and set value = this.Transform.PublishPostUpdates <- value
    member this.Protected with get () = this.Transform.Protected and internal set value = this.Transform.Protected <- value
    member this.Persistent with get () = this.Transform.Persistent and set value = this.Transform.Persistent <- value
    member this.Mounted with get () = this.Transform.Mounted and set value = this.Transform.Mounted <- value
    member this.Is2d with get () = this.Dispatcher.Is2d
    member this.Is3d with get () = this.Dispatcher.Is3d
    member this.Physical with get () = this.Dispatcher.Physical || Array.exists (fun (facet : Facet) -> facet.Physical) this.Facets // TODO: consider using a cache flag to keep from recomputing this.
    member this.PerimeterCentered with get () = this.Transform.PerimeterCentered and set value = this.Transform.PerimeterCentered <- value
    member this.Static with get () = this.Transform.Static and set value = this.Transform.Static <- value
    member this.LightProbe with get () = this.Transform.LightProbe and set value = this.Transform.LightProbe <- value
    member this.Light with get () = this.Transform.Light and set value = this.Transform.Light <- value
    member this.Optimized with get () = this.Transform.Optimized

    /// Make an entity state value.
    static member make imperative surnamesOpt overlayNameOpt (dispatcher : EntityDispatcher) =
        let mutable transform = Transform.makeDefault dispatcher.PerimeterCentered
        transform.Imperative <- imperative
        let (id, surnames) = Gen.id64AndSurnamesIf surnamesOpt
        { Transform = transform
          Dispatcher = dispatcher
          Facets = [||]
          Xtension = Xtension.makeEmpty imperative
          Model = { DesignerType = typeof<unit>; DesignerValue = () }
          Content = WorldTypes.EmptyEntityContent :?> EntityContent
          PositionLocal = Vector3.Zero
          RotationLocal = Quaternion.Identity
          ScaleLocal = Vector3.One
          AnglesLocal = Vector3.Zero
          ElevationLocal = 0.0f
          MountOpt = None
          PropagationSourceOpt = None
          OverlayNameOpt = overlayNameOpt
          FacetNames = Set.empty
          PropagatedDescriptorOpt = None
          Order = Core.getTimeStampUnique ()
          Id = id
          Surnames = surnames }

    /// Copy an entity state.
    /// This is used when we want to retain an old version of an entity state in face of mutation.
    static member inline copy (entityState : EntityState) =
        { entityState with EntityState.Dispatcher = entityState.Dispatcher }

    /// Copy an entity state, invalidating the incoming reference.
    /// This is used when we want to retain an old version of an entity state in face of mutation.
    static member inline diverge (entityState : EntityState) =
        let entityState' = EntityState.copy entityState
        entityState.Transform.InvalidateFast () // OPTIMIZATION: invalidate fast.
        entityState'

    /// Check that there exists an xtenstion proprty that is a runtime property.
    static member inline containsRuntimeProperties entityState =
        Xtension.containsRuntimeProperties entityState.Xtension

    /// Try to get an xtension property and its type information.
    static member tryGetProperty (propertyName, entityState, propertyRef : Property outref) =
        Xtension.tryGetProperty (propertyName, entityState.Xtension, &propertyRef)

    /// Get an xtension property and its type information.
    static member getProperty propertyName entityState =
        Xtension.getProperty propertyName entityState.Xtension

    /// Try to set an xtension property with explicit type information.
    static member trySetProperty propertyName property (entityState : EntityState) =
        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
        match Xtension.trySetProperty propertyName property entityState.Xtension with
        | struct (true, xtension) ->
            entityState.Xtension <- xtension // redundant if xtension is imperative
            struct (true, entityState)
        | struct (false, _) -> struct (false, entityState)

    /// Set an xtension property with explicit type information.
    static member setProperty propertyName property (entityState : EntityState) =
        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
        let xtension = Xtension.setProperty propertyName property entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    /// Attach an xtension property.
    static member attachProperty name property (entityState : EntityState) =
        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
        let xtension = Xtension.attachProperty name property entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    /// Detach an xtension property.
    static member detachProperty name (entityState : EntityState) =
        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
        let xtension = Xtension.detachProperty name entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Converts Game types, interning its strings for look-up speed.
and GameConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then (source :?> Game).ToString ()
        elif destType = typeof<Symbol> then (AddressConverter typeof<Game Address>).ConvertTo ((source :?> Game).GameAddress, destType)
        elif destType = pointType then source
        else failconv "Invalid GameConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr -> Game (stoa addressStr)
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) -> Game (stoa addressStr)
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Atom or Text for conversion to Game." (Some addressSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid GameConverter conversion from source." None

/// The game type that hosts the various screens used to navigate through a game.
and [<TypeConverter (typeof<GameConverter>)>] Game (gameAddress : Game Address) =

#if DEBUG
    // check that address is of correct length for a game
    do if gameAddress.Length <> 1 || gameAddress.Names.[0] <> Constants.Engine.GameName then
        failwith "Game address must be length of 1 with name = 'Game'."
#endif

    /// A convenience reference to get the universal game handle.
    static let handle = Game (ntoa Constants.Engine.GameName)

    // cache the simulant address to avoid allocation
    let simulantAddress = atoa<Game, Simulant> gameAddress

    /// The address of the game.
    member this.GameAddress = gameAddress

    /// Get the names of a game.
    member inline this.Names = Address.getNames this.GameAddress

    /// Get the name of a game.
    member inline this.Name = Address.getName this.GameAddress

    /// Get the latest value of a game's properties.
    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member private this.View = WorldTypes.viewGame WorldTypes.Chosen

    /// A convenience accessor to get the universal game handle.
    static member Handle = handle

    /// Derive a screen from the game.
    static member (/) (game : Game, screenName) = let _ = game in Screen (rtoa [|Constants.Engine.GameName; screenName|])

    /// Concatenate an address with a game's address, taking the type of first address.
    static member (-->) (address : 'a Address, game : Game) =
        // HACK: anonymizes address when entity is null due to internal engine trickery.
        if isNull (game :> obj) then Address.anonymize address else acatf address game.GameAddress

    override this.ToString () =
        scstring this.GameAddress

    override this.Equals that =
        match that with
        | :? Game as that -> this.GameAddress = that.GameAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.GameAddress

    interface Simulant with
        member this.SimulantAddress = simulantAddress
        end

    interface Game IComparable with
        member this.CompareTo that =
            Address.compare this.GameAddress that.GameAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Game as that -> (this :> Game IComparable).CompareTo that
            | _ -> failwith "Invalid Game comparison (comparee not of type Game)."

/// Converts Screen types, interning its strings for look-up speed.
and ScreenConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then (source :?> Screen).ToString ()
        elif destType = typeof<Symbol> then (AddressConverter typeof<Screen Address>).ConvertTo ((source :?> Screen).ScreenAddress, destType)
        elif destType = pointType then source
        else failconv "Invalid ScreenConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr -> Screen (stoa addressStr)
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) -> Screen (stoa addressStr)
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Atom or Text for conversion to Screen." (Some addressSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid ScreenConverter conversion from source." None

/// The screen type that allows transitioning to and from other screens, and also hosts the
/// currently interactive groups of entities.
and [<TypeConverter (typeof<ScreenConverter>)>] Screen (screenAddress) =

#if DEBUG
    // check that address is of correct length for a screen
    do if screenAddress.Length <> 2 || screenAddress.Names.[0] <> Constants.Engine.GameName then
        failwith "Screen address must be length of 2 with Game name = 'Game'."
#endif

    // cache the simulant address to avoid allocation
    let simulantAddress = atoa<Screen, Simulant> screenAddress

    /// Create a group reference from an address string.
    new (screenAddressStr : string) = Screen (stoa screenAddressStr)

    /// Create a screen reference from a list of names.
    new (screenNames : string array) = Screen (rtoa screenNames)

    /// Create a screen reference from a list of names.
    new (screenNames : string list) = Screen (ltoa screenNames)

    /// Create a screen reference from a name string.
    new (gameName : string, screenName : string) = Screen (rtoa [|gameName; screenName|])

    /// The address of the screen.
    member this.ScreenAddress = screenAddress

    /// Get the names of a screen.
    member inline this.Names = Address.getNames this.ScreenAddress

    /// Get the name of a screen.
    member inline this.Name = Address.getName this.ScreenAddress

    /// Get the latest value of a screen's properties.
    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member private this.View = WorldTypes.viewScreen (this :> obj) WorldTypes.Chosen

    /// Derive a group from its screen.
    static member (/) (screen : Screen, groupName) = Group (atoa<Screen, Group> screen.ScreenAddress --> ntoa groupName)

    /// Concatenate an address with a screen's address, taking the type of first address.
    static member (-->) (address : 'a Address, screen : Screen) =
        // HACK: anonymizes address when screen is null due to internal engine trickery.
        if isNull (screen :> obj) then Address.anonymize address else acatf address screen.ScreenAddress

    override this.ToString () =
        scstring this.ScreenAddress

    override this.Equals that =
        match that with
        | :? Screen as that -> this.ScreenAddress = that.ScreenAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.ScreenAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Screen as that -> (this :> Screen IComparable).CompareTo that
            | _ -> failwith "Invalid Screen comparison (comparee not of type Screen)."

    interface Screen IComparable with
        member this.CompareTo that =
            Address.compare this.ScreenAddress that.ScreenAddress

    interface Simulant with
        member this.SimulantAddress = simulantAddress
        end

/// Converts Group types, interning its strings for look-up speed.
and GroupConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then (source :?> Group).ToString ()
        elif destType = typeof<Symbol> then (AddressConverter typeof<Group Address>).ConvertTo ((source :?> Group).GroupAddress, destType)
        elif destType = pointType then source
        else failconv "Invalid GroupConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr -> Group (stoa addressStr)
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) -> Group (stoa addressStr)
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Atom or Text for conversion to Group." (Some addressSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid GroupConverter conversion from source." None

/// Forms a logical group of entities.
and [<TypeConverter (typeof<GroupConverter>)>] Group (groupAddress) =

#if DEBUG
    // check that address is of correct length for a group
    do if groupAddress.Length <> 3 || groupAddress.Names.[0] <> Constants.Engine.GameName then
        failwith "Group address must be length of 3 with Game name = 'Game'."
#endif

    // cache the simulant address to avoid allocation
    let simulantAddress = atoa<Group, Simulant> groupAddress

    /// Create a group reference from an address string.
    new (groupAddressStr : string) = Group (stoa groupAddressStr)

    /// Create a group reference from a list of names.
    new (groupNames : string array) = Group (rtoa groupNames)

    /// Create a group reference from a list of names.
    new (groupNames : string list) = Group (ltoa groupNames)

    /// Create a group reference from a the required names.
    new (gameName : string, screenName : string, groupName : string) = Group [gameName; screenName; groupName]

    /// The address of the group.
    member this.GroupAddress = groupAddress

    /// The containing screen of the group.
    member this.Screen = let names = this.GroupAddress.Names in Screen (names.[0], names[1])

    /// Get the names of a group.
    member inline this.Names = Address.getNames this.GroupAddress

    /// Get the name of a group.
    member inline this.Name = Address.getName this.GroupAddress

    /// Get the latest value of a group's properties.
    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member private this.View = WorldTypes.viewGroup (this :> obj) WorldTypes.Chosen

    /// Derive an entity from its group.
    static member (/) (group : Group, entityName) = Entity (atoa<Group, Entity> group.GroupAddress --> ntoa entityName)

    /// Concatenate an address with a group's address, taking the type of first address.
    static member (-->) (address : 'a Address, group : Group) =
        // HACK: anonymizes address when group is null due to internal engine trickery.
        if isNull (group :> obj) then Address.anonymize address else acatf address group.GroupAddress

    override this.ToString () =
        scstring this.GroupAddress

    override this.Equals that =
        match that with
        | :? Group as that -> this.GroupAddress = that.GroupAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.GroupAddress

    interface Simulant with
        member this.SimulantAddress = simulantAddress
        end

    interface Group IComparable with
        member this.CompareTo that =
            Address.compare this.GroupAddress that.GroupAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Group as that -> (this :> Group IComparable).CompareTo that
            | _ -> failwith "Invalid Group comparison (comparee not of type Group)."

/// Converts Entity types, interning its strings for look-up speed.
and EntityConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then (source :?> Entity).ToString ()
        elif destType = typeof<Symbol> then (AddressConverter typeof<Entity Address>).ConvertTo ((source :?> Entity).EntityAddress, destType)
        elif destType = pointType then source
        else failconv "Invalid EntityConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr -> Entity (stoa addressStr)
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) -> Entity (stoa addressStr)
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Atom or Text for conversion to Entity." (Some addressSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid EntityConverter conversion from source." None

/// The type around which the whole game engine is based! Used in combination with dispatchers to implement things
/// like buttons, characters, blocks, and things of that sort.
and [<TypeConverter (typeof<EntityConverter>)>] Entity (entityAddress) =

#if DEBUG
    // check that address is of correct length for an entity
    do if entityAddress.Length < 4 || entityAddress.Names.[0] <> Constants.Engine.GameName then
        failwith "Entity address must be length >= 4 with Game name = 'Game'."
#endif

    /// The entity's cached state.
    let mutable entityStateOpt = Unchecked.defaultof<EntityState>

    // cache the simulant address to avoid allocation
    let simulantAddress = atoa<Entity, Simulant> entityAddress

    /// Create an entity reference from an address string.
    new (entityAddressStr : string) = Entity (stoa entityAddressStr)

    /// Create an entity reference from an array of names.
    new (surnames : string array) = Entity (rtoa surnames)

    /// Create an entity reference from a list of names.
    new (surnames : string list) = Entity (ltoa surnames)

    /// Create an entity reference from a the required names.
    new (gameName : string, screenName : string, groupName : string, entityName : string) = Entity [gameName; screenName; groupName; entityName]

    /// The address of the entity.
    member this.EntityAddress = entityAddress

    /// The containing screen of the entity.
    member this.Screen = let names = this.EntityAddress.Names in Screen (names.[0], names.[1])

    /// The containing group of the entity.
    member this.Group = let names = this.EntityAddress.Names in Group (names.[0], names.[1], names.[2])

    /// The containing parent of the entity.
    member this.Parent =
        let names = this.EntityAddress.Names
        let namesLength = Array.length names
        if namesLength < 5
        then Group (Array.take 3 names) :> Simulant
        else Entity (Array.take (dec namesLength) names) :> Simulant

    /// Get the names of an entity.
    member this.Names = Address.getNames this.EntityAddress

    /// Get the surnames of an entity (the names of an entity not including group or screen).
    member this.Surnames = Address.getNames this.EntityAddress |> Array.skip 3

    /// Get the last name of an entity.
    member this.Name = Address.getNames this.EntityAddress |> Array.last

    /// The cached entity state for imperative entities.
    member internal this.EntityStateOpt
        with get () = entityStateOpt
        and set value = entityStateOpt <- value

    /// Get the latest value of an entity's properties.
    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member private this.View = WorldTypes.viewEntity (this :> obj) WorldTypes.Chosen

    /// Derive an entity from its parent entity.
    static member (/) (parentEntity : Entity, entityName) = Entity (parentEntity.EntityAddress --> ntoa entityName)

    /// Concatenate an address with an entity, taking the type of first address.
    static member (-->) (address : 'a Address, entity : Entity) =
        // HACK: anonymizes address when entity is null due to internal engine trickery.
        if isNull (entity :> obj) then Address.anonymize address else acatf address entity.EntityAddress

    override this.ToString () =
        scstring this.EntityAddress

    override this.Equals that =
        match that with
        | :? Entity as that -> this.EntityAddress = that.EntityAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.EntityAddress

    interface Simulant with
        member this.SimulantAddress = simulantAddress
        end

    interface Entity IComparable with
        member this.CompareTo that =
            Address.compare this.EntityAddress that.EntityAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Entity as that -> (this :> Entity IComparable).CompareTo that
            | _ -> failwith "Invalid Entity comparison (comparee not of type Entity)."

/// Describes a generalized simulant value independent of the engine.
/// Not used for serialization.
and SimulantDescriptor =
    { SimulantSurnamesOpt : string array option
      SimulantDispatcherName : string
      SimulantProperties : (string * Property) list
      SimulantChildren : SimulantDescriptor list }

/// Describes an entity value independent of the engine.
/// Used to directly serialize an entity.
and EntityDescriptor =
    { EntityDispatcherName : string
      EntityProperties : Map<string, Symbol>
      EntityDescriptors : EntityDescriptor list }

    /// Derive a name from the descriptor.
    static member getNameOpt descriptor =
        descriptor.EntityProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// Set a name for the descriptor.
    static member setNameOpt nameOpt descriptor =
        match nameOpt with
        | Some name -> { descriptor with EntityProperties = Map.add Constants.Engine.NamePropertyName (valueToSymbol name) descriptor.EntityProperties }
        | None -> { descriptor with EntityProperties = Map.remove Constants.Engine.NamePropertyName descriptor.EntityProperties }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcherName = String.Empty
          EntityProperties = Map.empty
          EntityDescriptors  = [] }

/// Describes a group value independent of the engine.
/// Used to directly serialize a group.
and GroupDescriptor =
    { GroupDispatcherName : string
      GroupProperties : Map<string, Symbol>
      EntityDescriptors : EntityDescriptor list }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.GroupProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// The empty group descriptor.
    static member empty =
        { GroupDispatcherName = String.Empty
          GroupProperties = Map.empty
          EntityDescriptors = [] }

/// Describes a screen value independent of the engine.
/// Used to directly serialize a screen.
and ScreenDescriptor =
    { ScreenDispatcherName : string
      ScreenProperties : Map<string, Symbol>
      GroupDescriptors : GroupDescriptor list }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcherName = String.Empty
          ScreenProperties = Map.empty
          GroupDescriptors = [] }

/// Describes a game value independent of the engine.
/// Used to directly serialize a game.
and GameDescriptor =
    { GameDispatcherName : string
      GameProperties : Map<string, Symbol>
      ScreenDescriptors : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          ScreenDescriptors = [] }

/// The world's dispatchers (including facets).
/// NOTE: it would be nice to make this structure internal, but doing so would non-trivially increase the number of
/// parameters of World.make, which is already rather long.
and [<ReferenceEquality>] Dispatchers =
    { Facets : Map<string, Facet>
      EntityDispatchers : Map<string, EntityDispatcher>
      GroupDispatchers : Map<string, GroupDispatcher>
      ScreenDispatchers : Map<string, ScreenDispatcher>
      GameDispatchers : Map<string, GameDispatcher> }

/// The subsystems contained by the engine.
and [<ReferenceEquality>] internal Subsystems =
    { ImGui : ImGui
      PhysicsEngine2d : PhysicsEngine
      PhysicsEngine3d : PhysicsEngine
      RendererProcess : RendererProcess
      AudioPlayer : AudioPlayer }

/// Keeps the World from occupying more than two cache lines.
and [<ReferenceEquality>] internal WorldExtension =
    { DestructionListRev : Simulant list
      Dispatchers : Dispatchers
      Plugin : NuPlugin
      PropagationTargets : UMap<Entity, Entity USet> }

/// The world, in a functional programming sense. Hosts the simulation state, the dependencies needed to implement a
/// game, messages to by consumed by the various engine subsystems, and general configuration data.
and [<ReferenceEquality>] World =
    internal
        { // cache line 1 (assuming 16 byte header)
          mutable ChooseCount : int // NOTE: this allows us to check the integrity of the world's imperative subsystems.
          EventGraph : EventGraph
          EntityStates : SUMap<Entity, EntityState>
          GroupStates : UMap<Group, GroupState>
          ScreenStates : UMap<Screen, ScreenState>
          GameState : GameState
          // cache line 2
          EntityMounts : UMap<Entity, Entity USet>
          Quadtree : Entity Quadtree
          mutable OctreeOpt : Entity Octree option // OPTIMIZATION: allow for None for games that don't use 3D.
          AmbientState : World AmbientState
          Subsystems : Subsystems
          Simulants : UMap<Simulant, Simulant USet option> // OPTIMIZATION: using None instead of empty USet to descrease number of USet instances.
          JobSystem : JobSystem
          WorldExtension : WorldExtension }

    /// Check that the world is executing with imperative semantics where applicable.
    member this.Imperative =
        this.AmbientState.Imperative

    /// Check that the world is executing with functional semantics.
    member this.Functional =
        not this.AmbientState.Imperative

    /// Check that the world is accompanied (such as by an editor program that controls it).
    member this.Accompanied =
        this.AmbientState.Accompanied

    /// Check that the world is unaccompanied (such as being absent of an editor program that controls it).
    member this.Unaccompanied =
        not this.AmbientState.Accompanied

    /// Check that the world is advancing (not halted).
    member this.Advancing =
        this.AmbientState.Advancing

    /// Check that the world is halted (not advancing).
    member this.Halted =
        not this.AmbientState.Advancing

    /// Get the number of updates that have transpired.
    member this.UpdateTime =
        AmbientState.getUpdateTime this.AmbientState

    /// Get the tick delta as a number of environment ticks.
    member this.TickDelta =
        AmbientState.getTickDelta this.AmbientState

    /// Get the tick time as a number of environment ticks.
    member this.TickTime =
        AmbientState.getTickTime this.AmbientState

    /// Get the amount of clock time that has transpired between this and the previous frame.
    member this.ClockDelta =
        AmbientState.getClockDelta this.AmbientState

    /// Get the clock time as of when the current frame began.
    member this.ClockTime =
        AmbientState.getClockTime this.AmbientState

    /// Get the polymorphic engine time delta.
    member this.GameDelta =
        AmbientState.getGameDelta this.AmbientState

    /// Get the polymorphic engine time.
    member this.GameTime =
        AmbientState.getGameTime this.AmbientState

    /// Get the amount of date time that has transpired between this and the previous frame.
    member this.DateDelta =
        AmbientState.getDateDelta this.AmbientState

    /// Get the date time as of when the current frame began.
    member this.DateTime =
        AmbientState.getDateTime this.AmbientState

#if DEBUG
    member internal this.Choose () =
        match WorldTypes.Chosen with
        | :? World as this -> 
            if this.ChooseCount <> this.ChooseCount then
                Log.debug "World utilization order error. Likely a world reference has been accidentally dropped or World.switch wasn't used where required."
        | _ -> ()
        this.ChooseCount <- inc this.ChooseCount // mutation is fine here since calling Choose implies we're doing so on a new reference in functional mode
        WorldTypes.Chosen <- this
        this
#else
    member inline internal this.Choose () =
        WorldTypes.Chosen <- this
        this
#endif

    override this.ToString () =
        ""

/// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
/// specific values and configurations.
and [<AbstractClass>] NuPlugin () =

    /// Whether or not code reloading is permitted by current plugin
    abstract AllowCodeReload : bool
    default this.AllowCodeReload = true

    /// Provides a list of modes for setting game state via the editor.
    abstract EditModes : Map<string, World -> World>
    default this.EditModes = Map.empty

    /// Invoke a user-defined callback.
    abstract Invoke : string -> obj list -> World -> World
    default this.Invoke _ _ world = world

    /// Make a list of keyed values to hook into the engine.
    abstract MakeKeyedValues : World -> ((Guid * obj) list) * World
    default this.MakeKeyedValues world = ([], world)

    /// Attempt to make an emitter of the given name.
    abstract TryMakeEmitter : GameTime -> GameTime -> GameTime -> single -> int -> string -> Particles.Emitter option
    default this.TryMakeEmitter time lifeTimeOpt particleLifeTimeOpt particleRate particleMax emitterName =
        match emitterName with
        | "BasicStaticSpriteEmitter" -> Particles.BasicStaticSpriteEmitter.makeDefault time lifeTimeOpt particleLifeTimeOpt particleRate particleMax :> Particles.Emitter |> Some
        | "BasicStaticBillboardEmitter" -> Particles.BasicStaticBillboardEmitter.makeDefault time lifeTimeOpt particleLifeTimeOpt particleRate particleMax :> Particles.Emitter |> Some
        | _ -> None

    /// Attempt to convert a sequence of entities to the given scenery entity, destroying all those that were
    /// successfully converted.
    abstract TryConvertEntitiesToScenery : Entity seq -> Entity -> World -> World
    default this.TryConvertEntitiesToScenery _ _ world = world // fail to convert any by default.

    /// Attempt to convert a given scenery entity to a sequence of entities, creating all those that were
    /// successfully converted.
    abstract TryConvertSceneryToEntities : Entity -> World -> (Entity seq * World)
    default this.TryConvertSceneryToEntities _ world = (Seq.empty, world) // fail to convert any by default.

    /// A call-back at the beginning of each frame.
    abstract PreProcess : World -> World
    default this.PreProcess world = world

    /// A call-back during each frame.
    abstract PerProcess : World -> World
    default this.PerProcess world = world

    /// A call-back at the end of each frame.
    abstract PostProcess : World -> World
    default this.PostProcess world = world

    /// A call-back for imgui processing.
    abstract ImGuiProcess : World -> World
    default this.ImGuiProcess world = world

    /// A call-back for imgui post-processing.
    abstract ImGuiPostProcess : World -> World
    default this.ImGuiPostProcess world = world

    /// Birth facets / dispatchers of type 'a from plugin.
    member internal this.Birth<'a> assemblies =
        Array.map (fun (assembly : Assembly) ->
            let types =
                assembly.GetTypes () |>
                Array.filter (fun ty -> ty.IsSubclassOf typeof<'a>) |>
                Array.filter (fun ty -> not ty.IsAbstract) |>
                Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0))
            Array.map (fun (ty : Type) -> (ty.Name, Activator.CreateInstance ty :?> 'a)) types)
            assemblies |>
        Array.concat

    interface LateBindings

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =

    let name<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.Name

    let get<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) world =
        lens.Get world

    let getBy<'a, 'b, 's when 's :> Simulant> by (lens : Lens<'a, 's>) world : 'b =
        lens.GetBy by world

    let getByWorld<'a, 'b, 's when 's :> Simulant> by (lens : Lens<'a, 's>) world : 'b =
        lens.GetByWorld by world

    let setOpt<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        match lens.SetOpt with
        | ValueSome set -> set a world
        | ValueNone -> world

    let trySet<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        lens.TrySet a world

    let set<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        lens.Set a world

    let tryUpdateEffect<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.TryUpdateEffect updater world

    let tryUpdateWorld<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.TryUpdateWorld updater world

    let tryUpdate<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.TryUpdate updater world

    let updateEffect<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.UpdateEffect updater world

    let updateWorld<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.UpdateWorld updater world

    let update<'a, 's when 's :> Simulant> updater (lens : Lens<'a, 's>) world =
        lens.Update updater world

    let changeEvent<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.ChangeEvent

    let ty<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.Type

    let make<'a, 's when 's :> Simulant> (name : string) (this : 's) (get : World -> 'a) set : Lens<'a, 's> =
        { Name = name; This = this; Get = get; SetOpt = ValueSome set }

    let makeReadOnly<'a, 's when 's :> Simulant> (name : string) (this : 's) (get : World -> 'a) : Lens<'a, 's> =
        { Name = name; This = this; Get = get; SetOpt = ValueNone }

[<AutoOpen>]
module LensOperators =

    /// Make a writable lens.
    let lens<'a, 's when 's :> Simulant> name (this : 's) (get : World -> 'a) set =
        Lens.make name this get set

    /// Make a read-only lens.
    let lensReadOnly<'a, 's when 's :> Simulant> name (this : 's) (get : World -> 'a) =
        Lens.makeReadOnly name this get

    /// Define a property along with its initial value.
    let define (lens : Lens<'a, 's>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    /// Define a property along with its initial value, also initializing its global attributes as non-persistent.
    let nonPersistent (lens : Lens<'a, 's>) (value : 'a) =
        Reflection.initPropertyNonPersistent true lens.Name
        define lens value

    /// Define a variable property.
    let variable (lens : Lens<'a, 's>) (var : World -> 'a) =
        Reflection.initPropertyNonPersistent true lens.Name
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> var (world :?> World) :> obj))

    /// Define a computed property.
    let computed (lens : Lens<'a, 's>) (get : 't -> World -> 'a) (setOpt : ('a -> 't -> World -> World) option) =
        Reflection.initPropertyNonPersistent true lens.Name
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> World) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> World) :> obj)
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =

    let rec
        processSignal<'model, 'message, 'command, 's when 'message :> Message and 'command :> Command and 's :> Simulant>
        (processMessage : 'model * 'message * 's * World -> Signal list * 'model)
        (processCommand : 'model * 'command * 's * World -> Signal list * World)
        (modelLens : Lens<'model, 's>)
        (signal : Signal)
        (simulant : 's)
        (world : World) :
        World =
        match signal :> obj with
        | :? 'message as message ->
            let model = Lens.get modelLens world
            let (signals, model) = processMessage (model, message, simulant, world)
            let world = Lens.set model modelLens world
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world
        | :? 'command as command ->
            let model = Lens.get modelLens world
            let (signals, world) = processCommand (model, command, simulant, world)
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world
        | _ -> failwithumf ()

    and processSignals processMessage processCommand modelLens signals simulant world =
        List.fold
            (fun world signal -> processSignal processMessage processCommand modelLens signal simulant world)
            world signals

[<AutoOpen>]
module SignalOperators =

    /// Signal constructor.
    /// Wonky name because F# reserve `sig` as a keyword.
    let inline signal<'s when 's :> Signal> (signal : 's) = signal :> Signal

    /// Singleton signal-value pair constructor.
    let inline withSignal (signal : Signal) value = ([signal], value)

    /// Signals-value pair constructor.
    let inline withSignals (signals : Signal list) value = (signals, value)

    /// Signaless signals-value pair constructor.
    let inline just value = (([] : Signal list), value)