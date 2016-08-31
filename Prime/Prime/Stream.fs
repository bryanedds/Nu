// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open LanguagePrimitives
open Prime

/// Describes a participant property.
type [<NoEquality; NoComparison>] PropertyTag<'s, 'a, 'w> =
    { This : 's
      Name : string
      Get : 'w -> 'a
      OptSet : ('a -> 'w -> 'w) option }

    member this.MapGet mapper =
        { this with Get = mapper this.Get }

    member this.MapSet mapper =
        { this with OptSet = match this.OptSet with Some set -> Some (fun value -> set (mapper value)) | None -> None }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PropertyTag =

    let mapGet mapper (property : PropertyTag<_, _, _>) =
        property.MapGet mapper

    let mapSet mapper (property : PropertyTag<_, _, _>) =
        property.MapSet mapper

    let makeReadOnly this name get =
        { This = this; Name = name; Get = get; OptSet = None }

    let make this name get set =
        { This = this; Name = name; Get = get; OptSet = Some set }

/// A stream in the functional reactive style.
type [<ReferenceEquality>] Stream<'a, 'w when 'w :> 'w EventWorld> =
    { Subscribe : 'w -> 'a Address * ('w -> 'w) * 'w }

module Stream =

    /// Make a stream of an event at the given address.
    let [<DebuggerHidden; DebuggerStepThrough>] stream<'a, 'w when 'w :> 'w EventWorld>
        (eventAddress : 'a Address) : Stream<'a, 'w> =
        let subscribe = fun (world : 'w) ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let unsubscribe = fun world -> EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "stream" evt.Trace
                let world = EventWorld.publish<'a, Participant, 'w> evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Combine a stream with a stream of the events from the given address. Combination is in
    /// 'product form', which is defined as a pair of the data of the combined events. Think of it
    /// as 'zip' for event streams.
    /// TODO: unit test for this!
    let [<DebuggerHidden; DebuggerStepThrough>] product
        (stream : Stream<'a, 'w>) (stream' : Stream<'b, 'w>) : Stream<'a * 'b, 'w> =
        let subscribe = fun world ->

            // initialize event state, subscription keys and addresses
            let stateKey = makeGuid ()
            let state = (List.empty<'a>, List.empty<'b>)
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream'.Subscribe world
            let subscriptionAddress'' = ntoa<'a * 'b> !!(scstring subscriptionKey'')
            
            // unsubscribe from 'a and 'b events, and remove event state
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                let world = EventWorld.unsubscribe<'w> subscriptionKey' world
                EventWorld.removeEventState stateKey world

            // subscription for 'a events
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record4 "Stream" "product" "'a" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let aList = evt.Data :: aList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publish<'a * 'b, Participant, _> (a, b) subscriptionAddress'' eventTrace evt.Publisher world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscription for 'b events
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record4 "Stream" "product" "'b" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let bList = evt.Data :: bList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publish<'a * 'b, Participant, _> (a, b) subscriptionAddress'' eventTrace evt.Publisher world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscripe 'a and 'b events
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription subscriptionAddress (world.GetNullParticipant ()) world
            let world = EventWorld.subscribe5<'b, Participant, 'w> subscriptionKey subscription' subscriptionAddress' (world.GetNullParticipant ()) world
            (subscriptionAddress'', unsubscribe, world)

        // fin
        { Subscribe = subscribe }

    /// Combine a stream with a stream of events from the given address. Combination is in 'sum
    /// form', which is defined as an Either of the data of the combined events, where only data
    /// from the most recent event is available at a time.
    let [<DebuggerHidden; DebuggerStepThrough>] sum
        (stream : Stream<'a, 'w>) (stream' : Stream<'b, 'w>) : Stream<Either<'a, 'b>, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream'.Subscribe world
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> !!(scstring subscriptionKey'')
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                EventWorld.unsubscribe<'w> subscriptionKey' world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "sum" evt.Trace
                let eventData = Left evt.Data
                let world = EventWorld.publish<Either<'a, 'b>, Participant, _> eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "sum" evt.Trace
                let eventData = Right evt.Data
                let world = EventWorld.publish<Either<'a, 'b>, Participant, _> eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'b, Participant, 'w> subscriptionKey' subscription' subscriptionAddress' (world.GetNullParticipant ()) world
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription subscriptionAddress (world.GetNullParticipant ()) world
            (subscriptionAddress'', unsubscribe, world)
        { Subscribe = subscribe }

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filter
        (pred : Event<'a, Participant> -> 'w -> bool) (stream : Stream<'a, 'w>) =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let world =
                    if pred evt world then
                        let eventTrace = EventTrace.record "Stream" "filter" evt.Trace
                        EventWorld.publish<'a, Participant, 'w> evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Map a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map
        (mapper : Event<'a, Participant> -> 'w -> 'b) (stream : Stream<'a, 'w>) : Stream<'b, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'b> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "map" evt.Trace
                let world = EventWorld.publish<'b, Participant, 'w> (mapper evt world) subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track4
        (tracker : 'c -> Event<'a, Participant> -> 'w -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (stream : Stream<'a, 'w>) : Stream<'b, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'b> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "track4" evt.Trace
                        let eventData = transformer state
                        EventWorld.publish<'b, Participant, 'w> eventData subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track2
        (tracker : 'a -> Event<'a, Participant> -> 'w -> 'a * bool) (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey None world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let optState = EventWorld.getEventState stateKey world
                let state = match optState with Some state -> state | None -> evt.Data
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "track2" evt.Trace
                        EventWorld.publish<'a, Participant, 'w> state subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track
        (tracker : 'b -> 'w -> 'b * bool) (state : 'b) (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "track" evt.Trace
                        EventWorld.publish<'a, Participant, 'w> evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Subscribe to a stream, handling each event with the given 'handleEvent' procedure,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribePlus handleEvent (subscriber : 's) stream world =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (address, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let world = EventWorld.subscribe5<'a, 's, 'w> subscriptionKey handleEvent address subscriber world
            (subscriptionAddress, unsubscribe, world)
        let stream = { Subscribe = subscribe }
        stream.Subscribe world |> _bc

    /// Subscribe to a stream, handling each event with the given 'handleEvent' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribe handleEvent subscriber stream world =
        subscribePlus handleEvent subscriber stream world |> snd

    /// Terminate a stream when an event at the given address is raised.
    let [<DebuggerHidden; DebuggerStepThrough>] until
        (eventAddress : unit Address) (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        let subscribe = fun world ->
            let eventKey = makeGuid ()
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress', unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                EventWorld.unsubscribe<'w> eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = EventWorld.subscribe5 eventKey handleEvent eventAddress (world.GetNullParticipant ()) world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "until" evt.Trace
                let world = EventWorld.publish<'a, Participant, 'w> evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, Participant, 'w> subscriptionKey subscription eventAddress' (world.GetNullParticipant ()) world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Terminate a stream when the subscriber is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] lifetime<'s, 'a, 'w when 's :> Participant and 'w :> 'w EventWorld>
        (subscriber : 's) (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        let removingEventAddress = ltoa<unit> [!!typeof<'s>.Name; !!"Removing"] ->>- subscriber.ParticipantAddress
        until removingEventAddress stream

    /// Subscribe to a stream until the subscriber is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] monitorWithUnsub eventAddress subscriber stream world =
        (stream |> lifetime subscriber |> subscribePlus eventAddress subscriber) world

    /// Subscribe to a stream until the subscriber is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] monitor eventAddress subscriber stream world =
        monitorWithUnsub eventAddress subscriber stream world |> snd
    
    (* Advanced Combinators *)

    /// Scan over a stream, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan4 (f : 'b -> Event<'a, Participant> -> 'w -> 'b) g s (stream : Stream<'a, 'w>) : Stream<'c, 'w> =
        track4 (fun b a w -> (f b a w, true)) g s stream
        
    /// Scan over a stream, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan2 (f : 'a -> Event<'a, Participant> -> 'w -> 'a) (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        track2 (fun a a2 w -> (f a a2 w, true)) stream
        
    /// Scan over a stream, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan (f : 'b -> Event<'a, Participant> -> 'w -> 'b) s (stream : Stream<'a, 'w>) : Stream<'b, 'w> =
        scan4 f id s stream

    /// Transform a stream into a running average of its event's numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline average (stream : Stream<'a, 'w>) : Stream<'a, 'w> =
        scan4
            (fun (avg : 'a, den : 'a) a _ ->
                let den' = den + one ()
                let dod' = den / den'
                let avg' = avg * dod' + a.Data / den
                (avg', den'))
            fst
            (zero (), zero ())
            stream

    /// Transform a stream into a running map from its event's data to keys as defined by 'f'.
    let [<DebuggerHidden; DebuggerStepThrough>] organize f (stream : Stream<'a, 'w>) : Stream<('a * 'b) option * Map<'b, 'a>, 'w> =
        scan
            (fun (_, m) a world ->
                let b = f a world
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            stream

    /// Transform a stream into a running set of its event's unique data as defined via 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] groupBy by (stream : Stream<'a, 'w>) : Stream<'b * bool * 'b Set, 'w> =
        scan
            (fun (_, _, set) a world ->
                let b = by a world
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            stream

    /// Transform a stream into a running set of its event's unique data.
    let [<DebuggerHidden; DebuggerStepThrough>] group (stream : Stream<'a, 'w>) : Stream<'a * bool * 'a Set, 'w> =
        groupBy (fun a _ -> a.Data) stream

    /// Transform a stream into a running sum of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline sumN stream = scan2 (fun n a _ -> n + a.Data) stream

    /// Transform a stream into a running product of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline productN stream = scan2 (fun n a _ -> n * a.Data) stream
    
    /// Transform a stream of pairs into its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] first stream = map (fun a _ -> fst a.Data) stream
    
    /// Transform a stream of pairs into its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] second stream = map (fun a _ -> snd a.Data) stream
    
    /// Transform a stream's pairs by a mapping of its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapFirst mapper stream = map (fun a _ -> (mapper ^ fst a.Data, snd a.Data)) stream
    
    /// Transform a stream of pairs by a mapping of its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapSecond mapper stream = map (fun a _ -> (fst a.Data, mapper ^ snd a.Data)) stream
    
    /// Transform a stream by duplicating its data into pairs.
    let [<DebuggerHidden; DebuggerStepThrough>] duplicate stream = map (fun a _ -> (a.Data, a.Data)) stream
    
    /// Take only the first n events from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] take n stream = track (fun m _ -> (m + 1, m < n)) 0 stream
    
    /// Skip the first n events in a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] skip n stream = track (fun m _ -> (m + 1, m >= n)) 0 stream
    
    /// Take only the first event from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] head stream = take 1 stream
    
    /// Skip the first event of a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] tail stream = skip 1 stream
    
    /// Take only the nth event from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] nth n stream = stream |> skip n |> head
    
    /// Take only the first event from a stream that satisfies 'p'.
    let [<DebuggerHidden; DebuggerStepThrough>] search p stream = stream |> filter p |> head
    
    /// Filter out the None data values from a stream and strip the Some constructor from
    /// the remaining values.
    let [<DebuggerHidden; DebuggerStepThrough>] choose (stream : Stream<'a option, 'w>) =
        stream |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    
    /// Transform a stream into a running maximum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] max stream = scan2 (fun n a _ -> if n < a.Data then a.Data else n) stream
    
    /// Transform a stream into a running minimum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] min stream = scan2 (fun n a _ -> if a.Data < n then a.Data else n) stream

    /// Filter out the events with non-unique data as defined via 'by' from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinctBy by stream = stream |> organize by |> first |> choose
    
    /// Filter out the events with non-unique data from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinct stream = distinctBy (fun a -> a.Data) stream

[<AutoOpen>]
module StreamOperators =

    // open related module
    open Stream

    /// Pipe-right arrow that provides special precedence for streams.
    let (-|>) = (|>)

    /// Make a stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( !-- ) (property : PropertyTag<'a, 'b, 'w>) =
        let changeEventAddress = ltoa<ParticipantChangeData<'a, 'w>> [!!typeof<'a>.Name; !!"Change"; !!property.Name] ->>- property.This.ParticipantAddress
        stream changeEventAddress

    /// Propagate the event data of a stream to a property in the observing participant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] ( --> ) stream (property : PropertyTag<'a, 'b, 'w>) =
        subscribe (fun a world ->
            let world =
                if world.ContainsParticipant a.Subscriber then
                    match property.OptSet with
                    | Some set -> set a.Data world
                    | None -> world // TODO: log info here about property not being set-able?
                else world
            (Cascade, world))
            property.This
            stream

    /// Propagate a property value from the given source participant to a property in the given destination participant.
    let [<DebuggerHidden; DebuggerStepThrough>] ( -!> )
        (sourceProperty : PropertyTag<'a, 'b, 'w>)
        (destinationProperty : PropertyTag<'s, 'b, 'w>) =
        !-- sourceProperty -|> map (fun _ world -> sourceProperty.Get world) --> destinationProperty