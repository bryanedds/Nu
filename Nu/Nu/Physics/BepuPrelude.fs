namespace Nu
open System
open System.Diagnostics
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open tainicom.Aether.Physics2D.Dynamics
open BepuPhysics
open BepuPhysics.Collidables
open BepuPhysics.CollisionDetection
open BepuPhysics.Constraints
open BepuUtilities
open BepuUtilities.Collections
open BepuUtilities.Memory
open Prime

type [<Struct; StructLayout (LayoutKind.Sequential)>] PreviousCollision =
    { mutable Collidable : CollidableReference
      mutable Fresh : bool
      mutable WasTouching : bool
      mutable ContactCount : int
      mutable FeatureId0 : int
      mutable FeatureId1 : int
      mutable FeatureId2 : int
      mutable FeatureId3 : int
      mutable FeatureId4 : int
      mutable FeatureId5 : int
      mutable FeatureId6 : int
      mutable FeatureId7 : int }

//The callbacks are invoked from a multithreaded context, and we don't know how many pairs will exist. 
//Rather than attempting to synchronize all accesses, every worker thread spits out the results into a worker-local list to be processed later by the main thread flush.
type [<Struct>] PendingWorkerAdd =
    { mutable ListenerIndex : int 
      mutable Collision : PreviousCollision }

type [<Struct; StructLayout (LayoutKind.Sequential)>] Collidable =
    { mutable PreviousCollisions : PreviousCollision QuickList
      mutable CollisionCategories : Category
      mutable CollisionMask : Category
      mutable Sensor : bool }

[<AutoOpen>]
module EmptyManifoldModule =

    //HACK: need some global values to pass back via reference in order to satisfy F#'s unification system.
    let mutable private i = 0
    let mutable private f = 0.0f
    let mutable private v3 = v3Zero

    //For final events fired by the flush that still expect a manifold, we'll provide a special empty type.
    //This type never has any contacts, so there's no need for any property grabbers.
    type EmptyManifold =
        struct end
        interface IContactManifold<EmptyManifold> with
            member this.Count = 0
            member this.Convex = true
            member this.GetContact (_ : int, _ : Vector3 byref, _ : Vector3 byref, _ : single byref, _ : int byref) = raise (NotImplementedException ())
            member this.GetDepth (_ : EmptyManifold byref, _ : int) = raise (NotImplementedException ()); &f
            member this.GetFeatureId (_ : int) = raise (NotImplementedException ())
            member this.GetFeatureId (_ : EmptyManifold byref, _ : int) = raise (NotImplementedException ()); &i
            member this.GetNormal (_ : EmptyManifold byref, _ : int) = raise (NotImplementedException ()); &v3
            member this.GetOffset (_ : EmptyManifold byref, _ : int) = raise (NotImplementedException ()); &v3

//For the purpose of this code, we'll use some regular ol' interfaces rather than using the struct-implementing-interface for specialization.
//This array will be GC tracked as a result, but that should be mostly fine. If you've got hundreds of thousands of event handlers, you may want to consider alternatives.
type [<Struct>] Listener =
    { Source : CollidableReference
      Handler : IContactEventHandler
      mutable PreviousCollisions : PreviousCollision QuickList }

/// <summary>
/// Implements handlers for various collision events.
/// </summary>
and IContactEventHandler =
    interface

        /// <summary>
        /// Fires when a contact is added.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="contactOffset">Offset from the pair's local origin to the new contact.</param>
        /// <param name="contactNormal">Normal of the new contact.</param>
        /// <param name="depth">Depth of the new contact.</param>
        /// <param name="featureId">Feature id of the new contact.</param>
        /// <param name="contactIndex">Index of the new contact in the contact manifold.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnContactAdded<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference * CollidablePair * 'TManifold byref * Vector3 * Vector3 * single * int * int * int -> unit

        /// <summary>
        /// Fires when a contact is removed.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="removedFeatureId">Feature id of the contact that was removed and is no longer present in the contact manifold.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnContactRemoved<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference * CollidablePair * 'TManifold byref * int * int -> unit

        /// <summary>
        /// Fires the first time a pair is observed to be touching. Touching means that there are contacts with nonnegative depths in the manifold.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnTouchingStarted<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference * CollidablePair * 'TManifold byref * int -> unit

        /// <summary>
        /// Fires whenever a pair is observed to be touching. Touching means that there are contacts with nonnegative depths in the manifold. Will not fire for sleeping pairs.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnTouchingUpdated<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference * CollidablePair * 'TManifold byref * int -> unit


        /// <summary>
        /// Fires when a pair stops touching. Touching means that there are contacts with nonnegative depths in the manifold.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnTouchingStopped<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference  * CollidablePair  * 'TManifold byref * int -> unit

        /// <summary>
        /// Fires when a pair is observed for the first time.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnPairCreated<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference * CollidablePair  * 'TManifold byref * int -> unit

        /// <summary>
        /// Fires whenever a pair is updated. Will not fire for sleeping pairs.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        /// <param name="contactManifold">Set of remaining contacts in the collision.</param>
        /// <param name="workerIndex">Index of the worker thread that fired this event.</param>
        abstract OnPairUpdated<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>> :
            CollidableReference  * CollidablePair  * 'TManifold byref * int -> unit

        /// <summary>
        /// Fires when a pair ends.
        /// </summary>
        /// <typeparam name="'TManifold">Type of the contact manifold detected.</typeparam>
        /// <param name="eventSource">Collidable that the event was attached to.</param>
        /// <param name="pair">Collidable pair triggering the event.</param>
        abstract OnPairEnded : CollidableReference * CollidablePair -> unit

        end

/// <summary>
/// Watches a set of bodies and statics for contact changes and reports events.
/// </summary>
type [<AllowNullLiteral>] ContactEvents
    (threadDispatcher : IThreadDispatcher, threadPools : WorkerBufferPools, pool : BufferPool) as this =

    //We use a handle->index mapping in a CollidableProperty to point at our contiguously stored listeners (in the later listeners array).
    //Note that there's also IndexSets for the statics and bodies; those will be checked first before accessing the listenerIndices.
    //The CollidableProperty is quite barebones- it doesn't try to stop all invalid accesses, and the backing memory isn't guaranteed to be zero initialized.
    //IndexSets are tightly bitpacked and are cheap to access, so they're an easy way to check if a collidable can trigger an event before doing any further processing.

    //Note that simulation and listernerIndices are null until the Initialize method is called.
    let mutable simulation = Unchecked.defaultof<Simulation>
    let mutable listenerIndices = Unchecked.defaultof<CollidableProperty<int>>
    let mutable threadPools = threadPools
    let mutable pool = pool
    let mutable staticListenerFlags = IndexSet ()
    let mutable bodyListenerFlags = IndexSet ()
    let mutable listenerCount = 0
    let mutable listeners = Array.empty<Listener>
    let mutable pendingWorkerAdds = Array.empty<PendingWorkerAdd QuickList>
    let beforeCollisionDetectionHandler = TimestepperStageHandler (fun delta td -> this.SetFreshnessForCurrentActivityStatus (delta, td))

    /// <summary>
    /// Creates a new contact events stream.
    /// </summary>
    /// <param name="threadDispatcher">Thread dispatcher to pull per-thread buffer pools from, if any.</param>
    /// <param name="pool">Buffer pool used to manage resources internally. If null, the simulation's pool will be used.</param>
    new (threadDispatcher : IThreadDispatcher, pool : BufferPool) =
        ContactEvents (threadDispatcher, new WorkerBufferPools (pool, threadDispatcher.ThreadCount), pool)

    member this.GetPoolForWorker (workerIndex : int) =
        if threadDispatcher = null
        then pool :> IUnmanagedMemoryPool
        else threadPools.[workerIndex]

    /// <summary>
    /// Initializes the contact events system with a simulation.
    /// </summary>
    /// <param name="simulation">Simulation to use with the contact events code.</param>
    /// <remarks>The constructor and initialization are split because of how this class is expected to be used. 
    /// It will be passed into a simulation's constructor as a part of its contact callbacks, so there is no simulation available at the time of construction.</remarks>
    member this.Initialize (simulation_ : Simulation) =
        simulation <- simulation_
        if isNull pool then pool <- simulation.BufferPool
        threadPools <- if notNull threadDispatcher then new WorkerBufferPools (pool, threadDispatcher.ThreadCount) else null
        simulation.Timestepper.add_BeforeCollisionDetection beforeCollisionDetectionHandler
        listenerIndices <- new CollidableProperty<int> (simulation, pool)
        pendingWorkerAdds <- Array.zeroCreate (if isNull threadDispatcher then 1 else threadDispatcher.ThreadCount)

    /// <summary>
    /// Begins listening for events related to the given collidable.
    /// </summary>
    /// <param name="collidable">Collidable to monitor for events.</param>
    /// <param name="handler">Handlers to use for the collidable.</param>
    member this.Register (collidable : CollidableReference, handler : IContactEventHandler) =

        Debug.Assert (not (this.IsListener collidable), "Should only try to register listeners that weren't previously registered.")

        if collidable.Mobility = CollidableMobility.Static
        then staticListenerFlags.Add (collidable.RawHandleValue, pool)
        else bodyListenerFlags.Add (collidable.RawHandleValue, pool)

        if listenerCount > listeners.Length then Array.Resize (&listeners, listeners.Length * 2)

        //Note that allocations for the previous collision list are deferred until they actually exist.
        listeners.[listenerCount] <- { Source = collidable; Handler = handler; PreviousCollisions = QuickList () }
        listenerIndices.[collidable] <- listenerCount
        listenerCount <- inc listenerCount

    /// <summary>
    /// Begins listening for events related to the given body.
    /// </summary>
    /// <param name="body">Body to monitor for events.</param>
    /// <param name="handler">Handlers to use for the body.</param>
    member this.Register (body : BodyHandle, handler : IContactEventHandler) =
        let body = simulation.Bodies.[body]
        this.Register (body.CollidableReference, handler)

    /// <summary>
    /// Begins listening for events related to the given static.
    /// </summary>
    /// <param name="staticHandle">Static to monitor for events.</param>
    /// <param name="handler">Handlers to use for the static.</param>
    member this.Register(staticHandle : StaticHandle, handler : IContactEventHandler) =
        this.Register(CollidableReference staticHandle, handler)

    /// <summary>
    /// Stops listening for events related to the given collidable.
    /// </summary>
    /// <param name="collidable">Collidable to stop listening for.</param>
    member this.Unregister (collidable : CollidableReference) =
        Debug.Assert (this.IsListener collidable, "Should only try to unregister listeners that actually exist.")
        if collidable.Mobility = CollidableMobility.Static
        then staticListenerFlags.Remove collidable.RawHandleValue
        else bodyListenerFlags.Remove collidable.RawHandleValue
        let index = listenerIndices.[collidable]
        listenerCount <- dec listenerCount
        let removedSlot = &listeners.[index]
        if removedSlot.PreviousCollisions.Span.Allocated then
            removedSlot.PreviousCollisions.Dispose pool
        let lastSlot = &listeners.[listenerCount]
        if index < listenerCount then
            listenerIndices.[lastSlot.Source] <- index
            removedSlot <- lastSlot
        lastSlot <- Unchecked.defaultof<_>

    /// <summary>
    /// Stops listening for events related to the given body.
    /// </summary>
    /// <param name="body">Body to stop listening for.</param>
    member this.Unregister (body : BodyHandle) =
        let body = simulation.Bodies[body]
        this.Unregister (body.CollidableReference)

    /// <summary>
    /// Stops listening for events related to the given static.
    /// </summary>
    /// <param name="staticHandle">Static to stop listening for.</param>
    member this.Unregister (staticHandle : StaticHandle) =
        this.Unregister (CollidableReference staticHandle)

    /// <summary>
    /// Checks if a collidable is registered as a listener.
    /// </summary>
    /// <param name="collidable">Collidable to check.</param>
    /// <returns>True if the collidable has been registered as a listener, false otherwise.</returns>
    member this.IsListener (collidable : CollidableReference) =
        if collidable.Mobility = CollidableMobility.Static
        then staticListenerFlags.Contains collidable.RawHandleValue
        else bodyListenerFlags.Contains collidable.RawHandleValue

    /// <summary>
    /// Callback attached to the simulation's ITimestepper which executes just prior to collision detection to take a snapshot of activity states to determine which pairs we should expect updates in.
    /// </summary>
    member private this.SetFreshnessForCurrentActivityStatus (_ : single, _ : IThreadDispatcher) =
        //Every single pair tracked by the contact events has a 'freshness' flag. If the final flush sees a pair that is stale, it'll remove it
        //and any necessary events to represent the end of that pair are reported.
        //HandleManifoldForCollidable sets 'Fresh' to true for any processed pair, but pairs between sleeping or static bodies will not show up in HandleManifoldForCollidable since they're not active.
        //We don't want Flush to report that sleeping pairs have stopped colliding, so we pre-initialize any such sleeping/static pair as 'fresh'.

        //This could be multithreaded reasonably easily if there are a ton of listeners or collisions, but that would be a pretty high bar.
        //For simplicity, the code will keep it single threaded.
        let bodyHandleToLocation = simulation.Bodies.HandleToLocation
        for i in 0 .. dec listenerCount do
            let listener = &listeners.[i]
            let source = listener.Source
            let sourceExpectsUpdates = source.Mobility <> CollidableMobility.Static && bodyHandleToLocation.[source.BodyHandle.Value].SetIndex = 0
            if sourceExpectsUpdates then //If it's a body, and it's in the active set (index 0), then every pair associated with the listener should expect updates.
                for j in 0 .. dec listener.PreviousCollisions.Count do
                    listener.PreviousCollisions.[j].Fresh <- false //Pair updates will set the 'freshness' to true when they happen, so that they won't be considered 'stale' in the flush and removed.
            else //The listener is either static or sleeping. We should only expect updates if the other collidable is awake.
                for j in 0 .. dec listener.PreviousCollisions.Count do
                    listener.PreviousCollisions.[j].Fresh <-
                        listener.PreviousCollisions.[j].Collidable.Mobility = CollidableMobility.Static ||
                        bodyHandleToLocation[listener.PreviousCollisions.[j].Collidable.BodyHandle.Value].SetIndex > 0

    [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
    member private this.UpdatePreviousCollision<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>
        (collision : PreviousCollision byref, manifold : 'TManifold byref, isTouching : bool) =
        Debug.Assert (manifold.Count <= 8, "This code was built on the assumption that nonconvex manifolds will have a maximum of four contacts, but that might have changed.")
        let manifoldCount = min 8 manifold.Count
        for i in 0 .. dec manifoldCount do
            Unsafe.Add (&collision.FeatureId0, i) <- manifold.GetFeatureId i
        collision.ContactCount <- manifold.Count
        collision.Fresh <- true
        collision.WasTouching <- isTouching

    member private this.HandleManifoldForCollidable<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>
        (workerIndex : int, sourceRef : CollidableReference, otherRef : CollidableReference, pair : CollidablePair, manifold : 'TManifold byref) =

        //The "source" refers to the object that an event handler was (potentially) attached to, so we look for listeners registered for it.
        //(This function is called for both orders of the pair, so we'll catch listeners for either.)
        if this.IsListener sourceRef then

            //This collidable is registered. Is the opposing collidable present?
            let listenerIndex = listenerIndices.[sourceRef]
            let listener = &listeners.[listenerIndex]
            let mutable previousCollisionIndex = -1
            let mutable isTouching = false
            let mutable i = 0
            let mutable collisionFound = false
            while i < listener.PreviousCollisions.Count && not collisionFound do

                let mutable collision = &listener.PreviousCollisions.[i]
                if collision.Collidable.Packed = otherRef.Packed then

                    //Since the 'Packed' field contains both the handle type (dynamic, kinematic, or static) and the handle index packed into a single bitfield, an equal value guarantees we are dealing with the same collidable.
                    collisionFound <- true

                    //Sync previous collision index.
                    previousCollisionIndex <- i

                    //This manifold is associated with an existing collision.
                    //For every contact in the old collsion still present (by feature id), set a flag in this bitmask so we can know when a contact is removed.
                    let mutable previousContactsStillExist = 0
                    for contactIndex in 0 .. dec manifold.Count do

                        //We can check if each contact was already present in the previous frame by looking at contact feature ids. See the 'PreviousCollision' type for a little more info on FeatureIds.
                        let featureId = manifold.GetFeatureId contactIndex
                        let mutable previousContactIndex = 0
                        let mutable featureIdWasInPreviousCollision = false
                        while previousContactIndex < collision.ContactCount && not featureIdWasInPreviousCollision do
                            if featureId = Unsafe.Add (&collision.FeatureId0, previousContactIndex) then
                                featureIdWasInPreviousCollision <- true
                                previousContactsStillExist <- previousContactsStillExist ||| (1 <<< previousContactIndex)
                            previousContactIndex <- inc previousContactIndex

                        if not featureIdWasInPreviousCollision then
                            let mutable offset = v3Zero
                            let mutable normal = v3Zero
                            let mutable depth = 0.0f
                            let mutable unused = 0
                            manifold.GetContact (contactIndex, &offset, &normal, &depth, &unused)
                            listener.Handler.OnContactAdded (sourceRef, pair, &manifold, offset, normal, depth, featureId, contactIndex, workerIndex)

                        if manifold.GetDepth (&manifold, contactIndex) >= 0.0f then
                            isTouching <- true

                    if previousContactsStillExist <> (1 <<< collision.ContactCount) - 1 then //At least one contact that used to exist no longer does.
                        for previousContactIndex in 0 .. dec collision.ContactCount do
                            if (previousContactsStillExist &&& (1 <<< previousContactIndex)) = 0 then
                                listener.Handler.OnContactRemoved (sourceRef, pair, &manifold, Unsafe.Add (&collision.FeatureId0, previousContactIndex), workerIndex)

                    if not collision.WasTouching && isTouching then
                        listener.Handler.OnTouchingStarted (sourceRef, pair, &manifold, workerIndex)
                    elif collision.WasTouching && not isTouching then
                        listener.Handler.OnTouchingStopped (sourceRef, pair, &manifold, workerIndex)
                    if isTouching then
                        listener.Handler.OnTouchingUpdated (sourceRef, pair, &manifold, workerIndex)

                    this.UpdatePreviousCollision (&collision, &manifold, isTouching)

                i <- inc i

            if previousCollisionIndex < 0 then //There was no collision previously.
                
                let addsforWorker = &pendingWorkerAdds.[workerIndex]
                addsforWorker.EnsureCapacity (max (inc addsforWorker.Count) 64, this.GetPoolForWorker workerIndex) //EnsureCapacity will create the list if it doesn't already exist.
                let pendingAdd = &addsforWorker.AllocateUnsafely ()
                pendingAdd.ListenerIndex <- listenerIndex
                pendingAdd.Collision.Collidable <- otherRef

                listener.Handler.OnPairCreated(sourceRef, pair, &manifold, workerIndex)

                //Dispatch events for all contacts in this new manifold.
                for i in 0 .. dec manifold.Count do
                    let mutable offset = v3Zero
                    let mutable normal = v3Zero
                    let mutable depth = 0.0f
                    let mutable featureId = 0
                    manifold.GetContact (i, &offset, &normal, &depth, &featureId)
                    listener.Handler.OnContactAdded(sourceRef, pair, &manifold, offset, normal, depth, featureId, i, workerIndex)
                    if depth >= 0.0f then isTouching <- true

                if isTouching then
                    listener.Handler.OnTouchingStarted (sourceRef, pair, &manifold, workerIndex)
                    listener.Handler.OnTouchingUpdated (sourceRef, pair, &manifold, workerIndex)

                this.UpdatePreviousCollision (&pendingAdd.Collision, &manifold, isTouching)

            listener.Handler.OnPairUpdated (sourceRef, pair, &manifold, workerIndex)

    [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
    member this.HandleManifold<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>
        (workerIndex : int, pair : CollidablePair, manifold : 'TManifold byref) =
        this.HandleManifoldForCollidable (workerIndex, pair.A, pair.B, pair, &manifold)
        this.HandleManifoldForCollidable (workerIndex, pair.B, pair.A, pair, &manifold)

    member this.Flush () =

        //For simplicity, this is completely sequential. Note that it's technically possible to extract more parallelism, but the complexity cost is high and you would need
        //very large numbers of events being processed to make it worth it.

        //Remove any stale collisions. Stale collisions are those which should have received a new manifold update but did not because the manifold is no longer active.
        for i in 0 .. dec listenerCount do

            let listener = &listeners[i]
            //Note reverse order. We remove during iteration.
            for j in dec listener.PreviousCollisions.Count .. -1 .. 0 do

                let collision = &listener.PreviousCollisions.[j]
                if not collision.Fresh then

                    //Sort the references to be consistent with the direct narrow phase results.
                    let mutable pair = CollidablePair ()
                    let mutable unused = CollidableMobility.Dynamic
                    let mutable unused2 = CollidableMobility.Dynamic
                    NarrowPhase.SortCollidableReferencesForPair (listener.Source, collision.Collidable, &unused, &unused2, &pair.A, &pair.B)
                    if collision.ContactCount > 0 then
                        let mutable emptyManifold = EmptyManifold ()
                        for previousContactCount in 0 .. dec collision.ContactCount do
                            listener.Handler.OnContactRemoved (listener.Source, pair, &emptyManifold, Unsafe.Add (&collision.FeatureId0, previousContactCount), 0)
                        if collision.WasTouching then
                            listener.Handler.OnTouchingStopped (listener.Source, pair, &emptyManifold, 0)

                    listener.Handler.OnPairEnded (collision.Collidable, pair)
                    listener.PreviousCollisions.FastRemoveAt j //This collision was not updated since the last flush despite being active. It should be removed.
                    if  listener.PreviousCollisions.Count = 0 then
                        listener.PreviousCollisions.Dispose pool
                        listener.PreviousCollisions <- QuickList ()

                else collision.Fresh <- false

        for i in 0 .. dec pendingWorkerAdds.Length do
            let pendingAdds = &pendingWorkerAdds.[i]
            for j in 0 .. dec pendingAdds.Count do
                let add = &pendingAdds.[j]
                let collisions = &listeners.[add.ListenerIndex].PreviousCollisions
                collisions.EnsureCapacity (max 8 (inc collisions.Count), pool) //Ensure capacity will initialize the slot if necessary.
                collisions.AllocateUnsafely () <- pendingAdds[j].Collision
            if pendingAdds.Span.Allocated then
                pendingAdds.Dispose (this.GetPoolForWorker i)
            pendingAdds <- QuickList () //We rely on zeroing out the count for lazy initialization.

        if notNull threadPools then threadPools.Clear ()

    member this.Dispose() =
        if bodyListenerFlags.Flags.Allocated then
            bodyListenerFlags.Dispose pool
        if staticListenerFlags.Flags.Allocated then
            staticListenerFlags.Dispose pool
        listenerIndices.Dispose ()
        simulation.Timestepper.remove_BeforeCollisionDetection beforeCollisionDetectionHandler
        if notNull threadPools then threadPools.Dispose ()
        for i in 0 .. dec pendingWorkerAdds.Length do
            Debug.Assert (not pendingWorkerAdds[i].Span.Allocated, "The pending worker adds should have been disposed by the previous flush.")

type [<Struct>] PoseIntegratorCallbacks =

    val Gravity : Vector3
    val mutable GravityWideDelta : Vector3Wide

    new (gravity) =
        { Gravity = gravity
          GravityWideDelta = Vector3Wide () }

    interface IPoseIntegratorCallbacks with
        member this.AngularIntegrationMode = AngularIntegrationMode.Nonconserving
        member this.AllowSubstepsForUnconstrainedBodies = false
        member this.IntegrateVelocityForKinematics = false
        member this.Initialize (_ : Simulation) = ()
        member this.PrepareForIntegration (clockDelta : single) = this.GravityWideDelta <- Vector3Wide.Broadcast (this.Gravity * clockDelta)
        member this.IntegrateVelocity (_ : Vector<int>, _ : Vector3Wide, _ : QuaternionWide, _ : BodyInertiaWide, _ : Vector<int>, _ : int, _ : Vector<single>, velocity : BodyVelocityWide byref) = velocity.Linear <- velocity.Linear + this.GravityWideDelta

type NarrowPhaseCallbacks =
    struct
        val mutable ContactEvents : ContactEvents
        end

    interface INarrowPhaseCallbacks with

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, _ : CollidablePair, _ : int, _ : int) =
            true

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold (_ : int, _ : CollidablePair, _ : int, _ : int, _ : ConvexContactManifold byref) =
            true

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>
            (workerIndex : int, pair : CollidablePair, manifold : 'TManifold byref, pairMaterial : PairMaterialProperties byref) =
            pairMaterial.FrictionCoefficient <- 1f
            pairMaterial.MaximumRecoveryVelocity <- 2f
            pairMaterial.SpringSettings <- SpringSettings (30.0f, 1.0f)
            this.ContactEvents.HandleManifold (workerIndex, pair, &manifold)
            true

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, a : CollidableReference, b : CollidableReference, _ : single byref) =
            a.Mobility = CollidableMobility.Dynamic ||
            b.Mobility = CollidableMobility.Dynamic

        member this.Initialize (simulation : Simulation) =
            this.ContactEvents.Initialize simulation

        member this.Dispose () =
            ()