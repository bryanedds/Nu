// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Numerics
open System.Reflection
open DotRecast.Core
open DotRecast.Detour
open JoltPhysicsSharp
open Prime

[<RequireQualifiedAccess>]
module internal WorldTypes =

    // Debugging variables.
    let mutable internal WorldForDebug = obj ()

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
    let mutable internal handleSubscribeAndUnsubscribeEvent : bool -> obj Address -> Simulant -> obj -> unit = Unchecked.defaultof<_>

    // Simulant F# reach-arounds.
    let mutable internal getEntityIs2d : obj -> obj -> bool = Unchecked.defaultof<_>

/// The type of a subscription callback.
type Callback<'a, 's when 's :> Simulant> = Event<'a, 's> -> World -> Handling

/// Represents an unsubscription operation for an event.
and Unsubscription = World -> unit

/// The data for a change in a simulant.
and ChangeData =
    { Name : string
      Previous : obj
      Value : obj }

/// Provides access to the property of a simulant via an interface.
/// Initially inspired by Haskell lenses, but specialized for simulant properties.
and Lens =
    interface
        
        /// The name of the property accessed by the lens.
        abstract Name : string
        
        /// The simulant whose property is accessed by the lens.
        abstract This : Simulant
        
        /// Get the value of the property accessed by the lens.
        abstract Get : world : World -> obj
        
        /// Get an optional setter function that updates the property accessed by the lens.
        abstract SetOpt : (obj -> World -> unit) voption
        
        /// Attempt to set the lensed property to the given value.
        abstract TrySet : value : obj -> world : World -> bool
        
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
      SetOpt : ('a -> World -> unit) voption }

    /// Get the lensed value mapped by the `by` function that includes the world value in its input.
    member lens.GetByPlus by world =
        by (lens.Get world) world

    /// Get the lensed value mapped by the `by` function.
    member lens.GetBy by world =
        by (lens.Get world)

    /// Attempt to set the property in the world to the given value.
    member lens.TrySet value world =
        match lens.SetOpt with
        | ValueSome setter ->
            setter value world
            true
        | ValueNone -> false

    /// Set the lensed property to the given value.
    /// Throws an exception if the lens is readonly.
    member lens.Set value world =
        match lens.SetOpt with
        | ValueSome setter -> setter value world
        | ValueNone -> failwith ("Lens for '" + lens.Name + "' is readonly.")

    /// Attempt to transform the lensed property's value using the given mapper function that also receives the world as input.
    member lens.TryMapPlus (mapper : 'a -> World -> 'a) world =
        match lens.SetOpt with
        | ValueSome setter ->
            setter (mapper (lens.Get world) world) world
            true
        | ValueNone -> false

    /// Attempt to transform the lensed property's value using the given mapper function.
    member lens.TryMap (mapper : 'a -> 'a) world =
        match lens.SetOpt with
        | ValueSome setter ->
            setter (mapper (lens.Get world)) world
            true
        | ValueNone -> false

    /// Update the lensed property's value using the given mapper function that also receives the world as input.
    /// Throws an exception if the lens is readonly.
    member lens.MapPlus mapper world =
        match lens.SetOpt with
        | ValueSome setter -> setter (mapper (lens.Get world) world) world
        | ValueNone -> failwithumf ()

    /// Update the lensed property's value using the given mapper function.
    /// Throws an exception if the lens is readonly.
    member lens.Map mapper world =
        match lens.SetOpt with
        | ValueSome setter -> setter (mapper (lens.Get world)) world
        | ValueNone -> failwithumf ()

    /// The change event associated with the lensed property.
    member lens.ChangeEvent : ChangeData Address =
        let names = [|Constants.Lens.ChangeName; lens.Name; Constants.Lens.EventName|]
        match box lens.This with
        | null ->
            // HACK: this case is a hack to allow Nu to resolve events contextually.
            let hashCode = Constants.Lens.ChangeNameHash ^^^ hash lens.Name ^^^ Constants.Lens.EventNameHash
            let changeEventAddress = { Names = names; HashCode = hashCode; Anonymous = true }
            changeEventAddress
        | _ -> rtoa (Array.append names lens.This.SimulantAddress.Names)

    /// The type of the lensed property.
    member inline lens.Type = typeof<'a>

    /// Adds the specified value to the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( += ) (lens : Lens<_, _>, value) = lens.Map (flip (+) value)

    /// Subtracts the specified value from the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( -= ) (lens : Lens<_, _>, value) = lens.Map (flip (-) value)

    /// Multiplies the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( *= ) (lens : Lens<_, _>, value) = lens.Map (flip (*) value)

    /// Divides the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( /= ) (lens : Lens<_, _>, value) = lens.Map (flip (/) value)

    /// Computes the modulus of the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( %= ) (lens : Lens<_, _>, value) = lens.Map (flip (%) value)

    /// Negates the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( ~+ ) (lens : Lens<_, _>) = lens.Map (~+)

    /// Negates the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( ~- ) (lens : Lens<_, _>) = lens.Map (~-)

    /// Increments the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( !+ ) (lens : Lens<_, _>) = lens.Map inc

    /// Decrements the lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( !- ) (lens : Lens<_, _>) = lens.Map dec

    /// Set a lensed property's value.
    /// Throws an exception if the lens is readonly.
    static member inline ( <-- ) (lens : Lens<_, _>, value) = lens.Set value

    /// Get a lensed property's value.
    static member inline ( !. ) (lens : Lens<_, _>) = fun world -> lens.Get world

    interface Lens with
        member lens.Name = lens.Name
        member lens.This = lens.This :> Simulant
        member lens.Get world = lens.Get world :> obj
        member lens.SetOpt = ValueOption.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) lens.SetOpt
        member lens.TrySet value world = match lens.SetOpt with ValueSome set -> set (value :?> 'a) world; true | ValueNone -> false
        member lens.ChangeEvent = lens.ChangeEvent
        member lens.Type = typeof<'a>

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

/// Specified the requested song, if any, or whether to ignore song request functionality altogether.
and RequestedSong =
    | Request of SongDescriptor
    | RequestFadeOut of GameTime
    | RequestNone
    | RequestIgnore

/// Specifies the desired screen, if any, or whether to ignore screen desire functionality altogether.
and DesiredScreen =
    | Desire of Screen
    | DesireNone
    | DesireIgnore

/// Describes the behavior of a screen.
and ScreenBehavior =
    | Vanilla
    | Dissolve of DissolveDescriptor * SongDescriptor option
    | Slide of DissolveDescriptor * SlideDescriptor * SongDescriptor option * Screen

/// The data required to execute slide screen presentation.
and Slide =
    { IdlingTime : GameTime
      Destination : Screen }

/// Identifies a navigation entry.
and NavId =
    { NavEntity : Entity
      NavIndex : int }

/// Represents 3d navigation capabilies for a screen.
/// NOTE: this type is intended only for internal engine use.
and [<ReferenceEquality; NoComparison>] Nav3d =
    { Nav3dContext : RcContext
      Nav3dBodies : Map<NavId, Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape>
      Nav3dBodiesOldOpt : Map<NavId, Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape> option
      Nav3dConfig : Nav3dConfig
      Nav3dConfigOldOpt : Nav3dConfig option
      Nav3dMeshOpt : (string option * NavBuilderResultData * DtNavMesh * DtNavMeshQuery) option }

    // Make an empty 3d navigation service.
    static member makeEmpty () =
        { Nav3dContext = RcContext ()
          Nav3dBodies = Map.empty
          Nav3dBodiesOldOpt = None
          Nav3dConfig = Nav3dConfig.defaultConfig
          Nav3dConfigOldOpt = None
          Nav3dMeshOpt = None }

/// Context for editing behavior.
and EditContext =
    { Snapshot : SnapshotType -> World -> unit
      FocusProperty : unit -> unit
      UnfocusProperty : unit -> unit
      SearchAssetViewer : unit -> unit
      DragDropPayloadOpt : string option
      SnapDrag : single
      SelectedScreen : Screen
      SelectedGroup : Group
      SelectedEntityOpt : Entity option
      ToSymbolMemo : IDictionary<struct (Type * obj), Symbol>
      OfSymbolMemo : IDictionary<struct (Type * Symbol), obj> }

/// Details replacement for editing behavior for a simulant property, allowing the user to indicate that a property was
/// replaced.
and [<ReferenceEquality>] ReplaceProperty =
    { IndicateReplaced : unit -> unit
      PropertyDescriptor : PropertyDescriptor
      EditContext : EditContext }

/// Details additional editing behavior for a simulant's properties.
and AppendProperties =
    { EditContext : EditContext }

/// Details additional editing behavior for hierarchy context menu.
and HierarchyContext =
    { EditContext : EditContext }

/// Details additional editing behavior for viewport context menu.
and ViewportContext =
    { RightClickPosition : Vector2
      EditContext : EditContext }

/// Details the additional editing behavior for a simulant in a viewport.
and [<ReferenceEquality>] ViewportOverlay =
    { ViewportView : Matrix4x4
      ViewportProjection : Matrix4x4
      ViewportBounds : Box2
      EditContext : EditContext }

/// Specifies an aspect of simulant editing to perform.
and [<ReferenceEquality>] EditOperation =
    | ReplaceProperty of ReplaceProperty
    | AppendProperties of AppendProperties
    | HierarchyContext of HierarchyContext
    | ViewportContext of ViewportContext
    | ViewportOverlay of ViewportOverlay

/// Describes the type of snapshot taken for operation tracking.
and SnapshotType =
    | WipePropagationTargets
    | TranslateEntity
    | RotateEntity
    | ScaleEntity
    | AutoBoundsEntity
    | MoveEntityToOrigin
    | PropagateEntity
    | ReorderEntities
    | SetEntityFrozen of bool
    | SetEntityFamilyStatic of bool
    | ChangeEntityDispatcher
    | RenameEntity
    | CreateEntity
    | DeleteEntity
    | CutEntity
    | PasteEntity
    | LoadEntity
    | DuplicateEntity
    | CreateGroup
    | RenameGroup
    | OpenGroup
    | CloseGroup
    | ChangeProperty of int64 option * string
    | Evaluate of string
    | RestorePoint
    | NormalizeAttenuation
    | RencenterInProbeBounds
    | ResetProbeBounds
    | VolumeEdit of string
    | FreezeEntities
    | ThawEntities
    | Permafreeze
    | ReregisterPhysics
    | SynchronizeNav
    | SetEditMode of int
    | ReloadCode
    | Advance
    | Halt
    | UserDefinedSnapshot of Image AssetTag * string // a user-defined type of snapshot

    member this.Label =
        match this with
        | WipePropagationTargets -> (scstringMemo this).Spaced
        | TranslateEntity -> (scstringMemo this).Spaced
        | RotateEntity -> (scstringMemo this).Spaced
        | ScaleEntity -> (scstringMemo this).Spaced
        | AutoBoundsEntity -> (scstringMemo this).Spaced
        | MoveEntityToOrigin -> (scstringMemo this).Spaced
        | PropagateEntity -> (scstringMemo this).Spaced
        | ReorderEntities -> (scstringMemo this).Spaced
        | SetEntityFrozen frozen -> if frozen then "Freeze Entity" else "Thaw Entity"
        | SetEntityFamilyStatic static_ -> if static_ then "Staticize Entity Family" else "Dynamize Entity Family"
        | ChangeEntityDispatcher -> (scstringMemo this).Spaced
        | RenameEntity -> (scstringMemo this).Spaced
        | CreateEntity -> (scstringMemo this).Spaced
        | DeleteEntity -> (scstringMemo this).Spaced
        | CutEntity -> (scstringMemo this).Spaced
        | PasteEntity -> (scstringMemo this).Spaced
        | LoadEntity -> (scstringMemo this).Spaced
        | DuplicateEntity -> (scstringMemo this).Spaced
        | RenameGroup -> (scstringMemo this).Spaced
        | CreateGroup -> (scstringMemo this).Spaced
        | OpenGroup -> (scstringMemo this).Spaced
        | CloseGroup -> (scstringMemo this).Spaced
        | ChangeProperty (_, propertyName) -> "Change Property " + propertyName
        | Evaluate _ -> "Evaluate F# Expression"
        | RestorePoint -> (scstringMemo this).Spaced
        | NormalizeAttenuation -> (scstringMemo this).Spaced
        | RencenterInProbeBounds -> (scstringMemo this).Spaced
        | ResetProbeBounds -> (scstringMemo this).Spaced
        | VolumeEdit volumeEditType -> "Volume Edit " + volumeEditType
        | FreezeEntities -> (scstringMemo this).Spaced
        | ThawEntities -> (scstringMemo this).Spaced
        | Permafreeze -> (scstringMemo this).Spaced
        | ReregisterPhysics -> (scstringMemo this).Spaced
        | SynchronizeNav -> (scstringMemo this).Spaced
        | SetEditMode i -> (scstringMemo this).Spaced + " (" + string (inc i) + " of 2)"
        | ReloadCode -> (scstringMemo this).Spaced
        | Advance -> (scstringMemo this).Spaced
        | Halt -> (scstringMemo this).Spaced
        | UserDefinedSnapshot (_, label) -> label

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
    abstract Register : game : Game * world : World -> unit
    default this.Register (_, _) = ()

    /// Unregister a game when finished with the world.
    abstract Unregister : game : Game * world : World -> unit
    default this.Unregister (_, _) = ()

    /// Attempt to ImSim process a game.
    abstract TryProcess : zeroDelta : bool * game : Game * world : World -> unit
    default this.TryProcess (_, _, _) = ()

    /// Pre-update a game.
    abstract PreUpdate : game : Game * world : World -> unit
    default this.PreUpdate (_, _) = ()

    /// Update a game.
    abstract Update : game : Game * world : World -> unit
    default this.Update (_, _) = ()

    /// Post-update a game.
    abstract PostUpdate : game : Game * world : World -> unit
    default this.PostUpdate (_, _) = ()

    /// Render a game.
    abstract Render : renderPass : RenderPass * game : Game * world : World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a game.
    abstract Signal : signalObj : obj * game : Game * world : World -> unit
    default this.Signal (_, _, _) = ()

    /// Attempt to get the fallback model value if the dispatcher defines one.
    abstract TryGetFallbackModel<'a> : modelSymbol : Symbol * game : Game * world : World -> 'a option
    default this.TryGetFallbackModel (_, _, _) = None

    /// Attempt to synchronize the content of a game.
    abstract TrySynchronize : initializing : bool * reinitializing : bool * game : Game * world : World -> unit
    default this.TrySynchronize (_, _, _, _) = ()

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : op : EditOperation * game : Game * world : World -> unit
    default this.Edit (_, _, _) = ()

    /// Attempt to truncate a game model.
    abstract TryTruncateModel<'a> : model : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a game model.
    abstract TryUntruncateModel<'a> : model : 'a * game : Game * world : World -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for screens.
and ScreenDispatcher () =
    inherit SimulantDispatcher ()

    /// Register a screen when adding it to the world.
    abstract Register : screen : Screen * world : World -> unit
    default this.Register (_, _) = ()

    /// Unregister a screen when removing it from the world.
    abstract Unregister : screen : Screen * world : World -> unit
    default this.Unregister (_, _) = ()

    /// Attempt to ImSim process a screen.
    abstract TryProcess : zeroDelta : bool * screen : Screen * world : World -> unit
    default this.TryProcess (_, _, _) = ()

    /// Pre-update a screen.
    abstract PreUpdate : screen : Screen * world : World -> unit
    default this.PreUpdate (_, _) = ()

    /// Update a screen.
    abstract Update : screen : Screen * world : World -> unit
    default this.Update (_, _) = ()

    /// Post-update a screen.
    abstract PostUpdate : screen : Screen * world : World -> unit
    default this.PostUpdate (_, _) = ()

    /// Render a screen.
    abstract Render : renderPass : RenderPass * screen : Screen * world : World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a screen.
    abstract Signal : signalObj : obj * screen : Screen * world : World -> unit
    default this.Signal (_, _, _) = ()

    /// Attempt to get the fallback model value if the dispatcher defines one.
    abstract TryGetFallbackModel<'a> : modelSymbol : Symbol * screen : Screen * world : World -> 'a option
    default this.TryGetFallbackModel (_, _, _) = None

    /// Attempt to synchronize the content of a screen.
    abstract TrySynchronize : initializing : bool * reinitializing : bool * screen : Screen * world : World -> unit
    default this.TrySynchronize (_, _, _, _) = ()

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : op : EditOperation * screen : Screen * world : World -> unit
    default this.Edit (_, _, _) = ()

    /// Attempt to truncate a screen model.
    abstract TryTruncateModel<'a> : model : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a screen model.
    abstract TryUntruncateModel<'a> : model : 'a * screen : Screen * world : World -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for groups.
and GroupDispatcher () =
    inherit SimulantDispatcher ()

    /// Register a group when adding it to a screen.
    abstract Register : group : Group * world : World -> unit
    default this.Register (_, _) = ()

    /// Unregister a group when removing it from a screen.
    abstract Unregister : group : Group * world : World -> unit
    default this.Unregister (_, _) = ()

    /// Attempt to ImSim process a group.
    abstract TryProcess : zeroDelta : bool * group : Group * world : World -> unit
    default this.TryProcess (_, _, _) = ()

    /// Pre-update a group.
    abstract PreUpdate : group : Group * world : World -> unit
    default this.PreUpdate (_, _) = ()

    /// Update a group.
    abstract Update : group : Group * world : World -> unit
    default this.Update (_, _) = ()

    /// Post-update a group.
    abstract PostUpdate : group : Group * world : World -> unit
    default this.PostUpdate (_, _) = ()

    /// Render a group.
    abstract Render : renderPass : RenderPass * group : Group * world : World -> unit
    default this.Render (_, _, _) = ()

    /// Send a signal to a group.
    abstract Signal : signalObj : obj * group : Group * world : World -> unit
    default this.Signal (_, _, _) = ()

    /// Attempt to get the fallback model value if the dispatcher defines one.
    abstract TryGetFallbackModel<'a> : modelSymbol : Symbol * group : Group * world : World -> 'a option
    default this.TryGetFallbackModel (_, _, _) = None

    /// Attempt to synchronize the content of a group.
    abstract TrySynchronize : initializing : bool * reinitializing : bool * group : Group * world : World -> unit
    default this.TrySynchronize (_, _, _, _) = ()

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : op : EditOperation * group : Group * world : World -> unit
    default this.Edit (_, _, _) = ()

    /// Attempt to truncate a group model.
    abstract TryTruncateModel<'a> : model : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate a group model.
    abstract TryUntruncateModel<'a> : model : 'a * group : Group * world : World -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

/// The default dispatcher for entities.
and EntityDispatcher (is2d, physical, lightProbe, light) =
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
         Define? Size Vector3.One
         Define? Elevation 0.0f
         Define? ElevationLocal 0.0f
         Define? Overflow 1.0f
         Define? Presence Exterior
         Define? Absolute false
         Define? Model { DesignerType = typeof<unit>; DesignerValue = () }
         Define? PropagationSourceOpt Option<Entity>.None
         Define? PublishChangeEvents false
         Define? Enabled true
         Define? EnabledLocal true
         Define? Visible true
         Define? VisibleLocal true
         Define? CastShadow true
         Define? Pickable true
         Define? Static false
         Define? AlwaysUpdate false
         Define? AlwaysRender false
         Define? PublishPreUpdates false
         Define? PublishUpdates false
         Define? PublishPostUpdates false
         Define? Persistent true
         Define? PropagatedDescriptorOpt Option<EntityDescriptor>.None]

    /// The presence override, if any.
    abstract PresenceOverride : Presence voption
    default this.PresenceOverride = ValueNone

    /// Register an entity when adding it to a group.
    abstract Register : entity : Entity * world : World -> unit
    default this.Register (_, _) = ()

    /// Unregister an entity when removing it from a group.
    abstract Unregister : entity : Entity * world : World -> unit
    default this.Unregister (_, _) = ()

    /// Participate in the registration of an entity's physics with the physics subsystems.
    abstract RegisterPhysics : entity : Entity * world : World -> unit
    default this.RegisterPhysics (_, _) = ()

    /// Participate in the unregistration of an entity's physics from the physics subsystems.
    abstract UnregisterPhysics : entity : Entity * world : World -> unit
    default this.UnregisterPhysics (_, _) = ()

    /// Attempt to ImSim process an entity.
    abstract TryProcess : zeroDelta : bool * entity : Entity * world : World -> unit
    default this.TryProcess (_, _, _) = ()

    /// Update an entity.
    abstract Update : entity : Entity * world : World -> unit
    default this.Update (_, _) = ()

    /// Render an entity.
    abstract Render : renderPass : RenderPass * entity : Entity * world : World -> unit
    default this.Render (_, _, _) = ()

    /// Apply physics changes from a physics engine to an entity.
    abstract Physics : center : Vector3 * rotation : Quaternion * linearVelocity : Vector3 * angularVelocity : Vector3 * entity : Entity * world : World -> unit
    default this.Physics (_, _, _, _, _, _) = ()

    /// Send a signal to an entity.
    abstract Signal : signalObj : obj * entity : Entity * world : World -> unit
    default this.Signal (_, _, _) = ()

    /// Attempt to get the fallback model value if the dispatcher defines one.
    abstract TryGetFallbackModel<'a> : modelSymbol : Symbol * entity : Entity * world : World -> 'a option
    default this.TryGetFallbackModel (_, _, _) = None

    /// Attempt to synchronize that content of an entity.
    abstract TrySynchronize : initializing : bool * reinitializing : bool * entity : Entity * world : World -> unit
    default this.TrySynchronize (_, _, _, _) = ()

    /// Get the default size of an entity.
    abstract GetAttributesInferred : entity : Entity * world : World -> AttributesInferred
    default this.GetAttributesInferred (_, _) =
        if this.Is2d
        then AttributesInferred.important Constants.Engine.Entity2dSizeDefault v3Zero
        else AttributesInferred.important Constants.Engine.Entity3dSizeDefault v3Zero

    /// Attempt to pick an entity with a ray.
    abstract RayCast : ray : Ray3 * entity : Entity * world : World -> Intersection array
    default this.RayCast (_, _, _) = [||]

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : op : EditOperation * entity : Entity * world : World -> unit
    default this.Edit (_, _, _) = ()

    /// Attempt to truncate an entity model.
    abstract TryTruncateModel<'a> : model : 'a -> 'a option
    default this.TryTruncateModel _ = None

    /// Attempt to untruncate an entity model.
    abstract TryUntruncateModel<'a> : model : 'a * Entity : Entity * world : World -> 'a option
    default this.TryUntruncateModel (_, _, _) = None

    /// Whether the dispatcher has a 2-dimensional transform interpretation.
    member this.Is2d = is2d

    /// Whether the dispatcher has a 3-dimensional transform interpretation.
    member this.Is3d = not is2d

    /// Whether the dispatcher participates directly in a physics system (not counting its facets).
    member this.Physical = physical

    /// Whether the dispatcher participates directly in light mapping (not counting its facets).
    member this.LightProbe = lightProbe

    /// Whether the dispatcher participates directly in lighting (not counting its facets).
    member this.Light = light

/// Dynamically augments an entity's behavior in a composable way.
and Facet (physical, lightProbe, light) =

    /// The presence override, if any.-
    abstract PresenceOverride : Presence voption
    default this.PresenceOverride = ValueNone

    /// Register a facet when adding it to an entity.
    abstract Register : entity : Entity * world : World -> unit
    default this.Register (_, _) = ()

    /// Unregister a facet when removing it from an entity.
    abstract Unregister : entity : Entity * world : World -> unit
    default this.Unregister (_, _) = ()

    /// Participate in the registration of an entity's physics with the physics subsystems.
    abstract RegisterPhysics : entity : Entity * world : World -> unit
    default this.RegisterPhysics (_, _) = ()

    /// Participate in the unregistration of an entity's physics from the physics subsystems.
    abstract UnregisterPhysics : entity : Entity * world : World -> unit
    default this.UnregisterPhysics (_, _) = ()

    /// Update a facet.
    abstract Update : entity : Entity * world : World -> unit
    default this.Update (_, _) = ()

    /// Render a facet.
    abstract Render : renderPass : RenderPass * entity : Entity * world : World -> unit
    default this.Render (_, _, _) = ()

    /// Participate in attempting to pick an entity with a ray.
    abstract RayCast : ray : Ray3 * entity : Entity * world : World -> Intersection array
    default this.RayCast (_, _, _) = [||]

    /// Participate in getting the default size of an entity.
    abstract GetAttributesInferred : entity : Entity * world : World -> AttributesInferred
    default this.GetAttributesInferred (_, _) = AttributesInferred.unimportant

    /// Participate in defining additional editing behavior for an entity via the ImGui API.
    abstract Edit : op : EditOperation * entity : Entity * world : World -> unit
    default this.Edit (_, _, _) = ()

    /// Whether a facet participates in a physics system.
    member this.Physical = physical

    /// Whether a facet participates in light mapping.
    member this.LightProbe = lightProbe

    /// Whether a facet participates in lighting.
    member this.Light = light

    interface LateBindings

/// A model-message-command-content (MMCC) signal tag type.
and Signal = interface end

/// A model-message-command-content (MMCC) message tag type.
and Message = inherit Signal

/// A model-message-command-content (MMCC) command tag type.
and Command = inherit Signal

/// Describes the type of property to the model-message-command-content (MMCC) content system.
and [<Struct>] PropertyType =
    | InitializingProperty
    | ReinitializingProperty
    | DynamicProperty

/// Describes property content to the model-message-command-content (MMCC) content system.
and [<ReferenceEquality>] PropertyContent =
    { PropertyType : PropertyType
      PropertyLens : Lens
      PropertyValue : obj }
    static member inline make ty lens value =
        { PropertyType = ty
          PropertyLens = lens
          PropertyValue = value }

/// Describes definition content to the model-message-command-content (MMCC) content system.
and [<ReferenceEquality>] DefinitionContent<'s when 's :> Simulant> =
    | PropertyContent of PropertyContent
    | EventSignalContent of obj Address * obj
    | EventHandlerContent of PartialEquatable<obj Address, Event -> obj>

/// Describes a simulant to the model-message-command-content (MMCC) content system.
and SimulantContent =
    abstract DispatcherNameOpt : string option
    abstract SimulantNameOpt : string option
    abstract SimulantCachedOpt : Simulant with get, set
    abstract EventSignalContentsOpt : OrderedDictionary<obj Address * obj, uint64>
    abstract EventHandlerContentsOpt : OrderedDictionary<int * obj Address, uint64 * (Event -> obj)>
    abstract PropertyContentsOpt : List<PropertyContent>
    abstract GetChildContentsOpt<'v when 'v :> SimulantContent> : unit -> OrderedDictionary<string, 'v>

/// Describes a game to the model-message-command-content (MMCC) content system.
and [<ReferenceEquality>] GameContent =
    { InitialScreenNameOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, uint64> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, uint64 * (Event -> obj)> // OPTIMIZATION: lazily created.
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

/// Describes a screen to the model-message-command-content (MMCC) content system.
and [<ReferenceEquality>] ScreenContent =
    { ScreenDispatcherName : string
      ScreenName : string
      ScreenBehavior : ScreenBehavior
      GroupFilePathOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, uint64> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, uint64 * (Event -> obj)> // OPTIMIZATION: lazily created.
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

/// Describes a group to the model-message-command-content (MMCC) content system.
and [<ReferenceEquality>] GroupContent =
    { GroupDispatcherName : string
      GroupName : string
      GroupFilePathOpt : string option
      mutable SimulantCachedOpt : Simulant
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, uint64> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, uint64 * (Event -> obj)> // OPTIMIZATION: lazily created.
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
      EntityFilePathOpt : string option
      mutable EntityCachedOpt : Entity // OPTIMIZATION: allows us to more often hit the EntityStateOpt cache. May be null.
      mutable EventSignalContentsOpt : OrderedDictionary<obj Address * obj, uint64> // OPTIMIZATION: lazily created.
      mutable EventHandlerContentsOpt : OrderedDictionary<int * obj Address, uint64 * (Event -> obj)> // OPTIMIZATION: lazily created.
      mutable PropertyContentsOpt : List<PropertyContent> // OPTIMIZATION: lazily created.
      mutable EntityContentsOpt : OrderedDictionary<string, EntityContent> } // OPTIMIZATION: lazily created.
    member this.MountOptOpt =
        match this.PropertyContentsOpt with
        | null -> ValueNone
        | propertyContents ->
            let mutable result = ValueNone
            for content in propertyContents do // manual for loop to ensure we get the last mount property when there's multiple
                if content.PropertyLens.Name = Constants.Engine.MountOptPropertyName then
                    result <- content.PropertyValue :?> Entity Address option |> ValueSome
            result
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
          EntityFilePathOpt = None
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
      mutable Xtension : Xtension // mutable to allow inserting new properties on code reload
      mutable Model : DesignerProperty // mutable to allow inserting fallback model on code reload
      Content : GameContent
      SelectedScreenOpt : Screen option
      DesiredScreen : DesiredScreen
      ScreenTransitionDestinationOpt : Screen option
      Eye2dCenter : Vector2
      Eye2dSize : Vector2
      Eye3dCenter : Vector3
      Eye3dRotation : Quaternion
      Eye3dFieldOfView : single
      Eye3dFrustumInterior : Frustum // OPTIMIZATION: cached value.
      Eye3dFrustumExterior : Frustum // OPTIMIZATION: cached value.
      Eye3dFrustumImposter : Frustum // OPTIMIZATION: cached value.
      Order : int64
      Id : uint64 }

    /// Copy a game state such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with GameState.Dispatcher = this.Dispatcher }

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

    /// Make a game state value.
    static member make (dispatcher : GameDispatcher) =
        let eye3dCenter = Constants.Engine.Eye3dCenterDefault
        let eye3dRotation = quatIdentity
        let eye3dFieldOfView = Constants.Engine.Eye3dFieldOfViewDefault
        let viewportInterior = Viewport.makeInterior ()
        let viewportExterior = Viewport.makeExterior ()
        let viewportImposter = Viewport.makeImposter ()
        { Dispatcher = dispatcher
          Xtension = Xtension.makeFunctional ()
          Model = { DesignerType = typeof<unit>; DesignerValue = () }
          Content = WorldTypes.EmptyGameContent :?> GameContent
          SelectedScreenOpt = None
          DesiredScreen = DesireIgnore
          ScreenTransitionDestinationOpt = None
          Eye2dCenter = v2Zero
          Eye2dSize = Constants.Render.DisplayVirtualResolution.V2
          Eye3dCenter = eye3dCenter
          Eye3dRotation = eye3dRotation
          Eye3dFieldOfView = eye3dFieldOfView
          Eye3dFrustumInterior = Viewport.getFrustum eye3dCenter eye3dRotation eye3dFieldOfView viewportInterior
          Eye3dFrustumExterior = Viewport.getFrustum eye3dCenter eye3dRotation eye3dFieldOfView viewportExterior
          Eye3dFrustumImposter = Viewport.getFrustum eye3dCenter eye3dRotation eye3dFieldOfView viewportImposter
          Order = Core.getTimeStampUnique ()
          Id = Gen.id64 }

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of a screen.
and [<ReferenceEquality; CLIMutable>] ScreenState =
    { Dispatcher : ScreenDispatcher
      mutable Xtension : Xtension // mutable to allow inserting new properties on code reload
      mutable Model : DesignerProperty // mutable to allow inserting fallback model on code reload
      Content : ScreenContent
      TransitionState : TransitionState
      Incoming : Transition
      Outgoing : Transition
      RequestedSong : RequestedSong
      SlideOpt : Slide option
      Nav3d : Nav3d
      Protected : bool
      Persistent : bool
      Order : int64
      Id : uint64
      Name : string }

    /// Copy a screen state such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with ScreenState.Dispatcher = this.Dispatcher }

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
          RequestedSong = RequestIgnore
          SlideOpt = None
          Nav3d = Nav3d.makeEmpty ()
          Protected = false
          Persistent = true
          Order = Core.getTimeStampUnique ()
          Id = id
          Name = name }

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of a group.
and [<ReferenceEquality; CLIMutable>] GroupState =
    { Dispatcher : GroupDispatcher
      mutable Xtension : Xtension // mutable to allow inserting new properties on code reload
      mutable Model : DesignerProperty // mutable to allow inserting fallback model on code reload
      Content : GroupContent
      Visible : bool
      Protected : bool
      Persistent : bool
      Order : int64
      Id : uint64
      Name : string }

    /// Copy a group state such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
    static member copy this =
        { this with GroupState.Dispatcher = this.Dispatcher }

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

    interface SimulantState with
        member this.GetXtension () = this.Xtension

/// Hosts the ongoing state of an entity.
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
      mutable MountOpt : Entity Address option
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
    member this.PerimeterCenterLocal = this.PositionLocal + (this.Transform.PerimeterCenter - this.Transform.Position)
    member this.PerimeterBottomLocal = this.PositionLocal + (this.Transform.PerimeterBottom - this.Transform.Position)
    member this.PerimeterBottomLeftLocal = this.PositionLocal + (this.Transform.PerimeterBottomLeft - this.Transform.Position)
    member this.PerimeterMinLocal = this.PositionLocal + (this.Transform.PerimeterMin - this.Transform.Position)
    member this.PerimeterMaxLocal = this.PositionLocal + (this.Transform.PerimeterMax - this.Transform.Position)
    member this.Position with get () = this.Transform.Position and set value = this.Transform.Position <- value
    member this.Rotation with get () = this.Transform.Rotation and set value = this.Transform.Rotation <- value
    member this.Scale with get () = this.Transform.Scale and set value = this.Transform.Scale <- value
    member this.Offset with get () = this.Transform.Offset and set value = this.Transform.Offset <- value
    member this.Angles with get () = this.Transform.Angles and set value = this.Transform.Angles <- value
    member this.Degrees with get () = this.Transform.Degrees and set value = this.Transform.Degrees <- value
    member this.DegreesLocal with get () = Math.RadiansToDegrees3d this.AnglesLocal and set value = this.AnglesLocal <- Math.DegreesToRadians3d value
    member this.Size with get () = this.Transform.Size and set value = this.Transform.Size <- value
    member this.RotationMatrix = this.Transform.RotationMatrix
    member this.Elevation with get () = this.Transform.Elevation and set value = this.Transform.Elevation <- value
    member this.Overflow with get () = this.Transform.Overflow and set value = this.Transform.Overflow <- value
    member this.AffineMatrix = this.Transform.AffineMatrix
    member this.PerimeterUnscaled with get () = this.Transform.PerimeterUnscaled and set value = this.Transform.PerimeterUnscaled <- value
    member this.Perimeter with get () = this.Transform.Perimeter and set value = this.Transform.Perimeter <- value
    member this.Bounds = if this.Is2d then this.Transform.Bounds2d else this.Transform.Bounds3d
    member internal this.Active with get () = this.Transform.Active and set value = this.Transform.Active <- value
    member internal this.Dirty with get () = this.Transform.Dirty and set value = this.Transform.Dirty <- value
    member internal this.Invalidated with get () = this.Transform.Invalidated and set value = this.Transform.Invalidated <- value
    member this.Absolute with get () = this.Transform.Absolute and set value = this.Transform.Absolute <- value
    member this.PublishChangeEvents with get () = this.Transform.PublishChangeEvents and set value = this.Transform.PublishChangeEvents <- value
    member this.Enabled with get () = this.Transform.Enabled and set value = this.Transform.Enabled <- value
    member this.EnabledLocal with get () = this.Transform.EnabledLocal and set value = this.Transform.EnabledLocal <- value
    member this.Visible with get () = this.Transform.Visible and set value = this.Transform.Visible <- value
    member this.VisibleLocal with get () = this.Transform.VisibleLocal and set value = this.Transform.VisibleLocal <- value
    member this.CastShadow with get () = this.Transform.CastShadow and set value = this.Transform.CastShadow <- value
    member this.Pickable with get () = this.Transform.Pickable and internal set value = this.Transform.Pickable <- value
    member this.AlwaysUpdate with get () = this.Transform.AlwaysUpdate and set value = this.Transform.AlwaysUpdate <- value
    member this.AlwaysRender with get () = this.Transform.AlwaysRender and set value = this.Transform.AlwaysRender <- value
    member this.PublishUpdates with get () = this.Transform.PublishUpdates and set value = this.Transform.PublishUpdates <- value
    member this.Protected with get () = this.Transform.Protected and internal set value = this.Transform.Protected <- value
    member this.Persistent with get () = this.Transform.Persistent and set value = this.Transform.Persistent <- value
    member this.Mounted with get () = this.Transform.Mounted and set value = this.Transform.Mounted <- value
    member this.Is2d = this.Dispatcher.Is2d
    member this.Is3d = this.Dispatcher.Is3d
    member this.Physical = this.Dispatcher.Physical || Array.exists (fun (facet : Facet) -> facet.Physical) this.Facets
    member this.LightProbe = this.Dispatcher.LightProbe || Array.exists (fun (facet : Facet) -> facet.LightProbe) this.Facets
    member this.Light = this.Dispatcher.Light || Array.exists (fun (facet : Facet) -> facet.Light) this.Facets
    member this.Static with get () = this.Transform.Static and set value = this.Transform.Static <- value
    member this.Optimized imperative = this.Transform.Optimized imperative
    member internal this.VisibleInView = this.Visible || this.AlwaysRender
    member internal this.StaticInPlay = this.Static && not this.AlwaysUpdate

    member this.Presence
        with get () = if this.Absolute then Omnipresent else this.Transform.Presence
        and set value = this.Transform.Presence <- value

    member this.PresenceOverride
        with get () = this.Transform.PresenceOverride
        and set value = this.Transform.PresenceOverride <- value

    member internal this.PresenceInPlay =
        match this.PresenceOverride with
        | ValueSome presence -> if this.Absolute then Omnipresent else presence
        | ValueNone -> this.Presence

    /// Copy an entity state, invalidating the incoming reference.
    /// This is used when we want to retain an old version of an entity state in face of mutation.
    static member inline copy (entityState : EntityState) =
        let entityState' = { entityState with EntityState.Dispatcher = entityState.Dispatcher }
        Transform.invalidateFastInternal &entityState.Transform // OPTIMIZATION: invalidate fast.
        entityState'

    /// Check that there exists an xtenstion property that is a runtime property.
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
        let entityState = if entityState.Xtension.Imperative then entityState else EntityState.copy entityState
        match Xtension.trySetProperty propertyName property entityState.Xtension with
        | struct (true, xtension) ->
            entityState.Xtension <- xtension // redundant if xtension is imperative
            struct (true, entityState)
        | struct (false, _) -> struct (false, entityState)

    /// Set an xtension property with explicit type information.
    static member setProperty propertyName property (entityState : EntityState) =
        let entityState = if entityState.Xtension.Imperative then entityState else EntityState.copy entityState
        let xtension = Xtension.setProperty propertyName property entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    /// Attach an xtension property.
    static member attachProperty name property (entityState : EntityState) =
        let entityState = if entityState.Xtension.Imperative then entityState else EntityState.copy entityState
        let xtension = Xtension.attachProperty name property entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    /// Detach an xtension property.
    static member detachProperty name (entityState : EntityState) =
        let entityState = if entityState.Xtension.Imperative then entityState else EntityState.copy entityState
        let xtension = Xtension.detachProperty name entityState.Xtension
        entityState.Xtension <- xtension // redundant if xtension is imperative
        entityState

    /// Make an entity state value.
    static member make imperative mountOpt surnamesOpt overlayNameOpt (dispatcher : EntityDispatcher) =
        let mutable transform = Transform.makeDefault ()
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
          MountOpt = mountOpt
          PropagationSourceOpt = None
          OverlayNameOpt = overlayNameOpt
          FacetNames = Set.empty
          PropagatedDescriptorOpt = None
          Order = Core.getTimeStampUnique ()
          Id = id
          Surnames = surnames }

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

    /// The address of the game.
    member this.GameAddress = gameAddress

    /// Get the names of a game.
    member inline this.Names = Address.getNames this.GameAddress

    /// Get the name of a game.
    member inline this.Name = Address.getName this.GameAddress

    /// Get the latest value of a game's properties.
    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member private this.View = WorldTypes.viewGame handle WorldTypes.WorldForDebug

    /// A convenience accessor to get the universal game handle.
    static member Handle = handle

    /// Derive a screen from the game.
    static member (/) (game : Game, screenName) = let _ = game in Screen (rtoa [|Constants.Engine.GameName; screenName|])

    /// Concatenate an address with a game's address.
    static member (-->) (address : 'a Address, game : Game) =
        // HACK: anonymizes address when entity is null due to internal engine trickery.
        if isNull (game :> obj) then Address.anonymize address else acatf address game.GameAddress

    override this.ToString () =
        this.GameAddress.ToString ()

    override this.Equals that =
        match that with
        | :? Game as that -> this.GameAddress = that.GameAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.GameAddress

    interface Simulant with
        member this.SimulantAddress = gameAddress
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
    member private this.View = WorldTypes.viewScreen (this :> obj) WorldTypes.WorldForDebug

    /// Derive a group from its screen.
    static member (/) (screen : Screen, groupName) = Group (atoa<Screen, Group> screen.ScreenAddress --> ntoa groupName)

    /// Concatenate an address with a screen's address.
    static member (-->) (address : 'a Address, screen : Screen) =
        // HACK: anonymizes address when screen is null due to internal engine trickery.
        if isNull (screen :> obj) then Address.anonymize address else acatf address screen.ScreenAddress

    override this.ToString () =
        this.ScreenAddress.ToString ()

    override this.Equals that =
        match that with
        | :? Screen as that -> this.ScreenAddress = that.ScreenAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.ScreenAddress

    interface Screen IComparable with
        member this.CompareTo that =
            Address.compare this.ScreenAddress that.ScreenAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Screen as that -> (this :> Screen IComparable).CompareTo that
            | _ -> failwith "Invalid Screen comparison (comparee not of type Screen)."

    interface Simulant with
        member this.SimulantAddress = screenAddress
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
    member private this.View = WorldTypes.viewGroup (this :> obj) WorldTypes.WorldForDebug

    /// Derive an entity from its group.
    static member (/) (group : Group, entityName) = Entity (atoa<Group, Entity> group.GroupAddress --> ntoa entityName)

    /// Concatenate an address with a group's address.
    static member (-->) (address : 'a Address, group : Group) =
        // HACK: anonymizes address when group is null due to internal engine trickery.
        if isNull (group :> obj) then Address.anonymize address else acatf address group.GroupAddress

    override this.ToString () =
        this.GroupAddress.ToString ()

    override this.Equals that =
        match that with
        | :? Group as that -> this.GroupAddress = that.GroupAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.GroupAddress

    interface Simulant with
        member this.SimulantAddress = groupAddress
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
    member private this.View = WorldTypes.viewEntity (this :> obj) WorldTypes.WorldForDebug

    /// Derive an entity from its parent entity.
    static member (/) (parentEntity : Entity, entityName) = Entity (parentEntity.EntityAddress --> ntoa entityName)

    /// Concatenate an address with an entity.
    static member (-->) (address : 'a Address, entity : Entity) =
        // HACK: anonymizes address when entity is null due to internal engine trickery.
        if isNull (entity :> obj) then Address.anonymize address else acatf address entity.EntityAddress

    override this.ToString () =
        this.EntityAddress.ToString ()

    override this.Equals that =
        match that with
        | :? Entity as that -> this.EntityAddress = that.EntityAddress
        | _ -> false

    override this.GetHashCode () =
        Address.hash this.EntityAddress

    interface Simulant with
        member this.SimulantAddress = entityAddress
        end

    interface Entity IComparable with
        member this.CompareTo that =
            Address.compare this.EntityAddress that.EntityAddress

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Entity as that -> (this :> Entity IComparable).CompareTo that
            | _ -> failwith "Invalid Entity comparison (comparee not of type Entity)."

/// Describes an entity value independent of the engine.
/// Used to directly serialize an entity.
and EntityDescriptor =
    { EntityDispatcherName : string
      EntityProperties : Map<string, Symbol>
      EntityDescriptors : EntityDescriptor list }

    /// Derive a name from the descriptor.
    static member getNameOpt descriptor =
        descriptor.EntityProperties
        |> Map.tryFind Constants.Engine.NamePropertyName
        |> Option.map symbolToValue<string>

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
        dispatcher.GroupProperties
        |> Map.tryFind Constants.Engine.NamePropertyName
        |> Option.map symbolToValue<string>

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
        dispatcher.ScreenProperties
        |> Map.tryFind Constants.Engine.NamePropertyName
        |> Option.map symbolToValue<string>

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

/// Provides simulant bookkeeping information with the ImSim API.
and [<NoEquality; NoComparison>] internal SimulantImSim =
    { mutable SimulantInitializing : bool
      mutable SimulantUtilized : bool
      InitializationTime : int64
      Result : obj }

/// Provides subscription bookkeeping information with the ImSim API.
and [<NoEquality; NoComparison>] internal SubscriptionImSim =
    { mutable SubscriptionUtilized : bool
      SubscriptionId : uint64
      Results : obj }

/// Describes the type of argument used with the ImSim API.
and [<Struct>] ArgType =
    | InitializingArg
    | ReinitializingArg
    | DynamicArg

/// Describes an argument used with the ImSim API.
and [<Struct>] ArgImSim<'s when 's :> Simulant> =
    { ArgType : ArgType
      ArgLens : Lens
      ArgValue : obj }

/// The world's dispatchers (including facets).
/// NOTE: it would be nice to make this record internal, but doing so would non-trivially increases the number of
/// parameters of World.make, which is already rather long.
and [<ReferenceEquality>] Dispatchers =
    internal
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
      RendererPhysics3dOpt : DebugRenderer option
      AudioPlayer : AudioPlayer }

/// Keeps the World from occupying more than two cache lines.
and [<ReferenceEquality>] internal WorldExtension =
    { // cache line 1 (assuming 16 byte header)
      mutable ContextImSim : Address
      mutable DeclaredImSim : Address
      mutable SimulantsImSim : SUMap<Address, SimulantImSim>
      mutable SubscriptionsImSim : SUMap<string * Address * Address, SubscriptionImSim>
      JobGraph : JobGraph
      GeometryViewport : Viewport
      // cache line 2
      RasterViewport : Viewport
      OuterViewport : Viewport
      DestructionListRev : Simulant list
      Dispatchers : Dispatchers
      Plugin : NuPlugin
      PropagationTargets : UMap<Entity, Entity USet> }

/// The world state, in a functional programming sense. This type is immutable enough to allows efficient snapshots and
/// later restoration, such as for undo and redo, with very little additional code.
and [<ReferenceEquality>] WorldState =
    internal
        { // cache line 1 (assuming 16 byte header)
          EventGraph : EventGraph
          EntityCachedOpt : KeyedCache<KeyValuePair<Entity, SUMap<Entity, EntityState>>, EntityState>
          EntityStates : SUMap<Entity, EntityState>
          GroupStates : UMap<Group, GroupState>
          ScreenStates : UMap<Screen, ScreenState>
          GameState : GameState
          // cache line 2
          EntityMounts : UMap<Entity, Entity USet>
          Quadtree : Entity Quadtree
          Octree : Entity Octree
          AmbientState : World AmbientState
          Subsystems : Subsystems
          Simulants : UMap<Simulant, Simulant USet option> // OPTIMIZATION: using None instead of empty USet to descrease number of USet instances.
          EntitiesIndexed : UMap<struct (Group * Type), Entity USet> // NOTE: could even add: UMap<string, EntitySubquery * Entities USet to entry value where subqueries are populated via NuPlugin.
          WorldExtension : WorldExtension }

    override this.ToString () =
        // NOTE: too big to print in the debugger, so printing nothing.
        ""

/// The world, in a functional programming sense. Hosts the simulation state, the dependencies needed to implement a
/// game, messages to by consumed by the various engine subsystems, and general configuration data. For better
/// ergonomics, the World type keeps a mutable reference to the functional WorldState, which is updated by the engine
/// whenever the engine transforms the world state.
and [<NoEquality; NoComparison>] World =
    internal
        { mutable WorldState : WorldState }

    member internal this.EventGraph =
        this.WorldState.EventGraph

    member internal this.EntityCachedOpt =
        this.WorldState.EntityCachedOpt

    member internal this.EntityStates =
        this.WorldState.EntityStates

    member internal this.GroupStates =
        this.WorldState.GroupStates

    member internal this.ScreenStates =
        this.WorldState.ScreenStates

    member internal this.GameState =
        this.WorldState.GameState

    member internal this.EntityMounts =
        this.WorldState.EntityMounts

    member internal this.Quadtree =
        this.WorldState.Quadtree

    member internal this.Octree =
        this.WorldState.Octree

    member internal this.AmbientState =
        this.WorldState.AmbientState

    member internal this.Subsystems =
        this.WorldState.Subsystems

    member internal this.Simulants =
        this.WorldState.Simulants

    member internal this.EntitiesIndexed =
        this.WorldState.EntitiesIndexed

    member internal this.WorldExtension =
        this.WorldState.WorldExtension

    /// Get the current world state.
    member this.CurrentState =
        this.WorldState

    /// Check that the world is alive (still running).
    member this.Alive =
        AmbientState.getAlive this.AmbientState

    /// Check that the world is dead (notno longer running).
    member this.Dead =
        not this.Alive

    /// Check that the world is executing with imperative semantics where applicable.
    member this.Imperative =
        this.AmbientState.Imperative

    /// Check that the world is executing with functional semantics.
    member this.Functional =
        not this.AmbientState.Imperative

    /// Check that the world is accompanied (such as being controlled by an editing program like Gaia).
    member this.Accompanied =
        this.AmbientState.Accompanied

    /// Check that the world is unaccompanied (such as not being controlled by an editing program like Gaia).
    member this.Unaccompanied =
        not this.AmbientState.Accompanied

    /// Check that the world is advancing (not halted).
    member this.Advancing =
        this.AmbientState.Advancing

    /// Check that the world is halted (not advancing).
    member this.Halted =
        not this.AmbientState.Advancing

    /// Check that the world's frame rate is being explicitly paced based on clock progression.
    member this.FramePacing =
        this.AmbientState.FramePacing

    member internal this.AdvancementCleared =
        this.AmbientState.AdvancementCleared

    /// Get the number of updates that have transpired between this and the previous frame.
    member this.UpdateDelta =
        AmbientState.getUpdateDelta this.AmbientState

    /// Get the number of updates that have transpired since the game began advancing.
    member this.UpdateTime =
        AmbientState.getUpdateTime this.AmbientState

    /// Get the amount of clock time (in seconds) between this and the previous frame. Clock time is the primary means
    /// for scaling frame-based phenomena like speeds and impulses.
    member this.ClockDelta =
        AmbientState.getClockDelta this.AmbientState

    /// Get the amount of clock time (in seconds) that has transpired since the world began advancing. Clock time is
    /// the primary means for scaling frame-based phenomena like speeds and impulses.
    member this.ClockTime =
        AmbientState.getClockTime this.AmbientState

    /// Get the tick delta as a number of environment ticks between this and the previous frame.
    member this.TickDelta =
        AmbientState.getTickDelta this.AmbientState

    /// Get the tick time as a number of environment ticks that have transpired since the world began advancing.
    member this.TickTime =
        AmbientState.getTickTime this.AmbientState

    /// Get the polymorphic engine time between this and the previous frame.
    member this.GameDelta =
        AmbientState.getGameDelta this.AmbientState

    /// Get the polymorphic engine time that has transpired since the world began advancing.
    member this.GameTime =
        AmbientState.getGameTime this.AmbientState

    /// Get the amount of date time that has transpired between this and the previous frame. This value is independent
    /// of whether the world was or is advancing.
    member this.DateDelta =
        AmbientState.getDateDelta this.AmbientState

    /// Get the date time as of the start of this frame. This value is independent of whether the world was or is
    /// advancing.
    member this.DateTime =
        AmbientState.getDateTime this.AmbientState

    /// Get the timers.
    member this.Timers =
        AmbientState.getTimers this.AmbientState

    /// Get the current ImSim context.
    [<DebuggerBrowsable (DebuggerBrowsableState.Never)>]
    member this.ContextImSim =
        this.WorldExtension.ContextImSim

    /// Get the current ImSim Game context (throwing upon failure).
    member this.ContextGame =
        if this.WorldExtension.ContextImSim.Names.Length > 0
        then Game.Handle
        else raise (InvalidOperationException "ImSim context not of type needed to construct requested handle.")

    /// Get the current ImSim Screen context (throwing upon failure).
    member this.ContextScreen =
        match this.WorldExtension.ContextImSim with
        | :? (Screen Address) as screenAddress -> Screen screenAddress
        | :? (Group Address) as groupAddress -> Screen (Array.take 2 groupAddress.Names)
        | :? (Entity Address) as entityAddress -> Screen (Array.take 2 entityAddress.Names)
        | _ -> raise (InvalidOperationException "ImSim context not of type needed to construct requested handle.")

    /// Get the current ImSim Group context (throwing upon failure).
    member this.ContextGroup =
        match this.WorldExtension.ContextImSim with
        | :? (Group Address) as groupAddress -> Group (Array.take 3 groupAddress.Names)
        | :? (Entity Address) as entityAddress -> Group (Array.take 3 entityAddress.Names)
        | _ -> raise (InvalidOperationException "ImSim context not of type needed to construct requested handle.")

    /// Get the current ImSim Entity context (throwing upon failure).
    member this.ContextEntity =
        match this.WorldExtension.ContextImSim with
        | :? (Entity Address) as entityAddress -> Entity entityAddress
        | _ -> raise (InvalidOperationException "ImSim context not of type needed to construct requested handle.")

    /// Check that the current ImSim context is initializing this frame.
    member this.ContextInitializing =
        match this.WorldExtension.SimulantsImSim.TryGetValue this.WorldExtension.ContextImSim with
        | (true, simulantImSim) -> simulantImSim.SimulantInitializing
        | (false, _) -> false

    /// Get the recent ImSim declaration.
    [<DebuggerBrowsable (DebuggerBrowsableState.Never)>]
    member this.DeclaredImSim =
        this.WorldExtension.DeclaredImSim

    /// Get the recent ImSim Game declaration (throwing upon failure).
    member this.DeclaredGame =
        if this.WorldExtension.DeclaredImSim.Names.Length > 0
        then Game.Handle
        else raise (InvalidOperationException "ImSim declaration not of type needed to construct requested handle.")

    /// Get the recent ImSim Screen declaration (throwing upon failure).
    member this.DeclaredScreen =
        match this.WorldExtension.DeclaredImSim with
        | :? (Screen Address) as screenAddress -> Screen screenAddress
        | :? (Group Address) as groupAddress -> Screen (Array.take 2 groupAddress.Names)
        | :? (Entity Address) as entityAddress -> Screen (Array.take 2 entityAddress.Names)
        | _ -> raise (InvalidOperationException "ImSim declaration not of type needed to construct requested handle.")

    /// Get the recent ImSim Group declaration (throwing upon failure).
    member this.DeclaredGroup =
        match this.WorldExtension.DeclaredImSim with
        | :? (Group Address) as groupAddress -> Group (Array.take 3 groupAddress.Names)
        | :? (Entity Address) as entityAddress -> Group (Array.take 3 entityAddress.Names)
        | _ -> raise (InvalidOperationException "ImSim declaration not of type needed to construct requested handle.")

    /// Get the recent ImSim Entity declaration (throwing upon failure).
    member this.DeclaredEntity =
        match this.WorldExtension.DeclaredImSim with
        | :? (Entity Address) as entityAddress -> Entity entityAddress
        | _ -> raise (InvalidOperationException "ImSim declaration not of type needed to construct requested handle.")

    /// Check that the recent ImSim declaration is initializing this frame.
    member this.DeclaredInitializing =
        match this.WorldExtension.SimulantsImSim.TryGetValue this.WorldExtension.DeclaredImSim with
        | (true, simulantImSim) -> simulantImSim.SimulantInitializing
        | (false, _) -> false

    member internal this.SimulantsImSim =
        this.WorldExtension.SimulantsImSim

    member internal this.SubscriptionsImSim =
        this.WorldExtension.SubscriptionsImSim

    /// Get the currently selected screen, if any.
    member this.SelectedScreenOpt =
        this.GameState.SelectedScreenOpt

    /// Get the desired selected screen, if any.
    member this.DesiredScreen =
        this.GameState.DesiredScreen

    /// The viewport of the geometry buffer.
    member this.GeometryViewport =
        this.WorldExtension.GeometryViewport

    /// The viewport of the rasterization buffer.
    member this.RasterViewport =
        this.WorldExtension.RasterViewport

    /// The viewport of the outer (full screen) buffer.
    member this.OuterViewport =
        this.WorldExtension.OuterViewport

    /// Get the center of the 2D eye.
    member this.Eye2dCenter =
        this.GameState.Eye2dCenter

    /// Get the size of the 2D eye.
    member this.Eye2dSize =
        this.GameState.Eye2dSize

    /// Get the bounds of the 2D eye.
    member this.Eye2dBounds =
        let eyeCenter = this.Eye2dCenter
        let eyeSize = this.Eye2dSize
        box2 (eyeCenter - eyeSize * 0.5f) eyeSize

    /// Get the center of the 3D eye.
    member this.Eye3dCenter =
        this.GameState.Eye3dCenter

    /// Get the rotation of the 3D eye.
    member this.Eye3dRotation =
        this.GameState.Eye3dRotation

    /// Get the field of view of the 3D eye.
    member this.Eye3dFieldOfView =
        this.GameState.Eye3dFieldOfView

    /// Get the interior frustum of the 3D eye.
    member this.Eye3dFrustumInterior =
        this.GameState.Eye3dFrustumInterior

    /// Get the exterior frustum of the 3D eye.
    member this.Eye3dFrustumExterior =
        this.GameState.Eye3dFrustumExterior

    /// Get the exterior frustum of the 3D eye.
    member this.Eye3dFrustumImposter =
        this.GameState.Eye3dFrustumImposter

    /// Get the view frustum of the 3D eye.
    member this.Eye3dFrustumView =
        let eyeCenter = this.Eye3dCenter
        let eyeRotation = this.Eye3dRotation
        let eyeFieldOfView = this.Eye3dFieldOfView
        Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView this.RasterViewport

    override this.ToString () =
        // NOTE: too big to print in the debugger, so printing nothing.
        ""

/// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
/// specific values and configurations.
and [<AbstractClass>] NuPlugin () =

    /// Whether or not code reloading is permitted by current plugin
    abstract AllowCodeReload : bool
    default this.AllowCodeReload = true

    /// Provides a list of modes for setting game state via the editor.
    abstract EditModes : Map<string, World -> unit>
    default this.EditModes = Map.empty

    /// The packages that should be loaded at start-up in all contexts, including in audio player, renderers, and
    /// metadata. The Default package is always included.
    abstract InitialPackages : string list
    default this.InitialPackages = []

    /// Clean-up any user-defined resources of the plugin, such with shutting down a Steamworks API.
    abstract CleanUp : unit -> unit
    default this.CleanUp () = ()

    /// Invoke a user-defined callback.
    abstract Invoke : callbackName : string -> callbackArgs : obj list -> world : World -> unit
    default this.Invoke _ _ _ = ()

    /// Make a list of keyed values to hook into the engine.
    abstract MakeKeyedValues : world : World -> ((string * obj) list)
    default this.MakeKeyedValues _ = []

    /// Attempt to make an emitter of the given name.
    abstract TryMakeEmitter : time : GameTime -> lifeTimeOpt : GameTime -> particleLifeTimeOpt : GameTime -> particleRate : single -> particleMax : int -> emitterName : string -> Particles.Emitter option
    default this.TryMakeEmitter time lifeTimeOpt particleLifeTimeOpt particleRate particleMax emitterName =
        match emitterName with
        | "BasicStaticSpriteEmitter" -> Particles.BasicStaticSpriteEmitter.makeDefault time lifeTimeOpt particleLifeTimeOpt particleRate particleMax :> Particles.Emitter |> Some
        | "BasicStaticBillboardEmitter" -> Particles.BasicStaticBillboardEmitter.makeDefault time lifeTimeOpt particleLifeTimeOpt particleRate particleMax :> Particles.Emitter |> Some
        | _ -> None

    /// A call-back at the beginning of each frame.
    abstract PreProcess : world : World -> unit
    default this.PreProcess _ = ()

    /// A call-back during each frame.
    abstract PerProcess : world : World -> unit
    default this.PerProcess _ = ()

    /// A call-back at the end of each frame.
    abstract PostProcess : world : World -> unit
    default this.PostProcess _ = ()

    /// A call-back for imgui processing.
    abstract ImGuiProcess : world : World -> unit
    default this.ImGuiProcess _ = ()

    /// A call-back for imgui post-processing.
    abstract ImGuiPostProcess : world : World -> unit
    default this.ImGuiPostProcess _ = ()

    /// Birth facets / dispatchers of type 'a from plugin.
    member internal this.Birth<'a> assemblies =
        assemblies
        |> Array.map (fun (assembly : Assembly) ->
            assembly.GetTypes ()
            |> Array.filter (fun ty -> ty.IsSubclassOf typeof<'a>)
            |> Array.filter (fun ty -> not ty.IsAbstract)
            |> Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0))
            |> Array.map (fun (ty : Type) -> (ty.Name, Activator.CreateInstance ty :?> 'a)))
        |> Array.concat

    interface LateBindings

/// Lens functions.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =

    let name<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.Name

    let get<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) world =
        lens.Get world

    let getBy<'a, 'b, 's when 's :> Simulant> by (lens : Lens<'a, 's>) world : 'b =
        lens.GetBy by world

    let getByPlus<'a, 'b, 's when 's :> Simulant> by (lens : Lens<'a, 's>) world : 'b =
        lens.GetByPlus by world

    let setOpt<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        match lens.SetOpt with
        | ValueSome set -> set a world
        | ValueNone -> ()

    let trySet<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        lens.TrySet a world

    let set<'a, 's when 's :> Simulant> a (lens : Lens<'a, 's>) world =
        lens.Set a world

    let tryMapPlus<'a, 's when 's :> Simulant> mapper (lens : Lens<'a, 's>) world =
        lens.TryMapPlus mapper world

    let tryMap<'a, 's when 's :> Simulant> mapper (lens : Lens<'a, 's>) world =
        lens.TryMap mapper world

    let mapPlus<'a, 's when 's :> Simulant> mapper (lens : Lens<'a, 's>) world =
        lens.MapPlus mapper world

    let map<'a, 's when 's :> Simulant> mapper (lens : Lens<'a, 's>) world =
        lens.Map mapper world

    let changeEvent<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.ChangeEvent

    let ty<'a, 's when 's :> Simulant> (lens : Lens<'a, 's>) =
        lens.Type

    let make<'a, 's when 's :> Simulant> (name : string) (this : 's) (get : World -> 'a) set : Lens<'a, 's> =
        { Name = name; This = this; Get = get; SetOpt = ValueSome set }

    let makeReadOnly<'a, 's when 's :> Simulant> (name : string) (this : 's) (get : World -> 'a) : Lens<'a, 's> =
        { Name = name; This = this; Get = get; SetOpt = ValueNone }

/// Lens operators.
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
    let computed (lens : Lens<'a, 's>) (get : 't -> World -> 'a) (setOpt : ('a -> 't -> World -> unit) option) =
        Reflection.initPropertyNonPersistent true lens.Name
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> World) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> World))
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)

/// Signal functions.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =

    let rec [<DebuggerHidden>]
        processSignal<'model, 'message, 'command, 's when 'message :> Message and 'command :> Command and 's :> Simulant>
        (processMessage : 'model * 'message * 's * World -> Signal list * 'model)
        (processCommand : 'model * 'command * 's * World -> unit)
        (modelLens : Lens<'model, 's>)
        (signal : Signal)
        (simulant : 's)
        (world : World) =
        match signal :> obj with
        | :? 'message as message ->
            let model = Lens.get modelLens world
            let (signals, model) = processMessage (model, message, simulant, world)
            Lens.set model modelLens world
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> ()
        | :? 'command as command ->
            let model = Lens.get modelLens world
            processCommand (model, command, simulant, world)
        | _ -> failwithumf ()

    and [<DebuggerHidden>] processSignals processMessage processCommand modelLens signals simulant world =
        for signal in signals do
            processSignal processMessage processCommand modelLens signal simulant world

/// Signal operators.
[<AutoOpen>]
module SignalOperators =

    /// Signal constructor.
    let inline signal<'s when 's :> Signal> (signal : 's) = signal :> Signal

    /// Singleton signal-value pair constructor.
    let inline withSignal (signal : Signal) value = ([signal], value)

    /// Signals-value pair constructor.
    let inline withSignals (signals : Signal list) value = (signals, value)

    /// Signaless signals-value pair constructor.
    let inline just value = (([] : Signal list), value)