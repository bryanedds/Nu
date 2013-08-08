module Nu.World
open System
open FSharpx
open Nu.Core

// NOTE: one will notice that the world uses asynchrony for physics and rendering. This is
// reasonable as these systems generally do not need to cooperate with the world. As a rule, non-
// cooperative systems like these may be asynchronous. Conversely, systems that require cooperation
// with the world should be synchronous (EG - on the same thread). This is because the cost of
// cooperation is much higher with asynchrony due to the need for extra intermediate program states
// during periods of cooperation, as well as additional error-handling code to make the system
// fault-tolerant in the case that cooperation cannot be acheived. Coordination can be subtle -
// even a simple query from the world to an asynchronous system (say a segment intersection query
// to a physics system) requires cooperation, and imposes the same heavy toll.
//
// So, the rule is, consider asynchrony when there is a significant performance boost from a non-
// cooperative system. By the same token, try very, VERY hard to avoid asynchrony with cooperative
// systems, even if some nice performance is lost. At the end of the day, simplicity is priority 1.

/// Describes human input.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] HumanInput =
    | PadInput // of ...
    | KeyboardInput // of ...
    | MouseInput // of ...

/// Describes physics input from the physics engine.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] PhysicsInput =
    | Collision // of ...
    | Transformation // of ...

/// Describes physics output to the physics engine.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] PhysicsOutput =
    | CreateBody // of...
    | DestroyBody // of...
    | ApplyForce // of ...

/// Describes renderer output.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] RendererOutput =
    | Sprite // of ...
    | StaticModel // of ...
    | AnimatedModel // of ...

/// Mailbox processors for the world.
/// A reference type.
type [<ReferenceEquality>] Mailboxes =
    { HumanInput : HumanInput MailboxProcessor
      PhysicsInput : PhysicsInput MailboxProcessor
      PhysicsOutput : PhysicsOutput MailboxProcessor // NOTE: I'm not sure if we need a mailbox processor for outputs...
      RendererOutput : RendererOutput MailboxProcessor } // NOTE: same as above...

/// Specifies the address of an element in a world.
/// Note that, similar to Mu, the last node of the address is the name of the event (such as "clicked").
/// Also note that subscribing to a partial address results in listening to all messages whose
/// beginning address nodes match the partial address (sort of a wild-card).
/// A value type.
type Address = Lun list

/// Describes a mouse button.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] MouseButton =
    | MouseLeft
    | MouseRight
    | MouseCenter

/// Describes a mouse event.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] MouseEvent =
    { Button : MouseButton
      Position : Vector2 }

/// A generic message for the Nu game engine.
/// A reference type.
type [<ReferenceEquality>] Message =
    { Handled : bool
      Publisher : obj
      Data : obj }

/// Describes a game asset, such as a texture, sound, or model.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] Asset =
    { Name : Lun
      Package : Lun }

/// All assets must belong to an AssetPackage, which is a unit of asset loading.
/// In order for the renderer to render a single texture, that texture, along with all the other
/// assets in the corresponding package, must be loaded. Also, the only way to unload any of those
/// assets is to send an AssetPackageUnload message to the renderer, which unloads them all. There
/// is an AssetPackageLoad message to load a package when convenient.
/// The use of a message system for the renderer should enable streamed loading, optionally with
/// smooth fading-in of late-loaded assets (IE - assets that are already in the view frustum but are
/// still being loaded).
/// Finally, the use of AssetPackages could enforce assets to be loaded in order of size and will
/// avoid unnecessary Large Object Heap fragmentation.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] AssetPackage =
    { Name : Lun
      AssetNames : Lun list }

/// Describes a game message subscription.
/// A reference type.
type [<ReferenceEquality>] Subscription =
    Subscription of (World -> Address -> Message -> World)

/// A map of game message subscriptions.
/// A reference type due to the reference-typeness of Subscription.
and Subscriptions =
    Map<Address, Map<IComparable, Subscription>>

/// The world, in a functional programming sense.
/// A reference type with some value semantics.
and [<ReferenceEquality>] World =
    { Game : Game
      Subscriptions : Subscriptions
      Mailboxes : Mailboxes }
    member inline this.TryFind address : obj option = None // TODO: implement
    member inline this.Set address (element : 'e when 'e : not struct) = this // TODO: implement
    member inline this.Remove address = this // TODO: implement

/// Try to find an element at the given address.
let inline tryFind address (world : World) : obj option =
    world.TryFind address

/// Try to find an element at the given address.
let inline tryFindAs<'e when 'e : not struct> address (world : World) : 'e option =
    let optElement = tryFind address world
    match optElement with
    | None -> None
    | Some element ->
        match element with
        | :? 'e as elementAsE -> Some elementAsE
        | _ -> None

/// Set an element at the given address.
let inline set address (element : 'e when 'e : not struct) (world : World) =
    world.Set element address

/// Remove an element at the given address.
let inline remove address (world : World) =
    world.Remove address

/// Try to find an entity at the given address.
let inline tryFindEntity address world : Entity option =
    None // not yet implemented...

/// Try to find an actor at the given address.
let inline tryFindActor address world : Actor option =
    None // not yet implemented...

/// Try to find an actor's rigid body at the given address.
let inline tryFindActorRigidBody address world : RigidBody option =
    None // not yet implemented...

/// Publish a message to the given address.
let publish address message world : World =
    let optSubMap = Map.tryFind address world.Subscriptions
    match optSubMap with
    | None -> world
    | Some subMap ->
        Map.fold
            (fun newWorld subscriber (Subscription subscription) -> subscription newWorld address message)
            world
            subMap

/// Subscribe to messages at the given address.
let subscribe address subscription subscriber world : World =
    let sub = Subscription subscription
    let subs = world.Subscriptions
    let optSubMap = Map.tryFind address subs
    { world with
        Subscriptions =
            match optSubMap with
            | None -> let newSubMap = Map.singleton subscriber sub in Map.add address newSubMap subs
            | Some subMap -> let newSubMap = Map.add subscriber sub subMap in Map.add address newSubMap subs }

/// Unsubscribe to messages at the given address.
let unsubscribe address subscriber world : World =
    let subs = world.Subscriptions
    let optSubMap = Map.tryFind address subs
    match optSubMap with
    | None -> world
    | Some subMap ->
        let newSubMap = Map.remove subscriber subMap in
        let newSubscriptions = Map.add address newSubMap subs
        { world with Subscriptions = newSubscriptions }

/// Execute a procedure within the context of a given subscription at the given address.
let withSubscription address subscription subscriber procedure world : World =
    let newWorld = subscribe address subscription subscriber world
    let newWorld2 = procedure newWorld
    unsubscribe address subscriber newWorld2