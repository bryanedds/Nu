// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Diagnostics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Stream =

    /// Take only one event from a stream per update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate (stream : Stream<'a, World>) =
        stream |>
        Stream.trackEvent4
            (fun (a, current) _ world ->
                let previous = current
                let current = World.getUpdateCount world
                ((a, current), previous < current))
            id (Unchecked.defaultof<'a>, -1L) |>
        Stream.first

    /// Take events from a stream only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking stream =
        Stream.filterEvent (fun _ -> World.isTicking) stream

    /// Take events from a stream only when the simulant is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] isSimulantSelected simulant stream =
        Stream.filterEvent (fun _ -> World.isSimulantSelected simulant) stream

    /// Take events from a stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenIdling) stream
    
    /// Take events from a stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenTransitioning) stream

    /// Filter a stream of options for actual values.
    /// NOTE: this will be in Prime in the next version.
    let [<DebuggerHidden; DebuggerStepThrough>] definitize (stream : Stream<'a option, World>) =
        stream |>
        Stream.filter Option.isSome |>
        Stream.map Option.get

    /// Filter events with unchanging data.
    /// NOTE: this will be in Prime in the next version.
    let [<DebuggerHidden; DebuggerStepThrough>] optimize (stream : Stream<_, World>) =
        Stream.fold
            (fun (_, l) a ->
                match l with
                | [] -> (Some a, [a])
                | x :: _ -> if a = x then (None, [a]) else (Some a, [a]))
            (None, [])
            stream |>
        Stream.map fst |>
        definitize

    /// Add a state value to the stream.
    /// NOTE: this will be in Prime in the next version.
    let [<DebuggerHidden; DebuggerStepThrough>] insert state stream =
        stream |>
        Stream.fold (fun (stateOpt, _) a -> (Some (Option.getOrDefault state stateOpt), a)) (None, Unchecked.defaultof<_>) |>
        Stream.map (mapFst Option.get)

    /// Make a Guid deterministically.
    /// HACK: this is an ugly hack to create a deterministic sequance of guids.
    /// Limited to creating 255 guids.
    /// NOTE: this will be in Prime in the next version.
    let makeGuidDeterministic offset (guid : Guid) =
        let arr = guid.ToByteArray ()
        arr.[15] <- arr.[15] + byte offset                    
        Guid arr

    /// Transform a stream into existing entities.
    let [<DebuggerHidden; DebuggerStepThrough>] entities (mapper : 'a -> EntityLayout) (stream : Stream<'a list, World>) (layer : Layer) =
        stream |>
        insert (makeGuid ()) |>
        Stream.map (fun (guid, list) ->
            List.mapi (fun i a -> PartialComparable.make (layer => scstring (makeGuidDeterministic i guid)) (mapper a)) list |>
            Set.ofList) |>
        Stream.fold (fun (p, _, _) c -> (c, Set.difference c p, Set.difference p c)) (Set.empty, Set.empty, Set.empty) |>
        Stream.mapEffect (fun evt world ->
            let (current, added, removed) = evt.Data
            let world =
                Seq.fold (fun world entityAndLayout ->
                    let (entity : Entity, layout) = PartialComparable.unmake entityAndLayout
                    if not (entity.GetExists world)
                    then World.expandEntity (Some entity.EntityName) layout layer world |> snd
                    else world)
                    world added
            let world =
                Seq.fold (fun world entityAndLayout ->
                    let (entity : Entity, _) = PartialComparable.unmake entityAndLayout
                    World.destroyEntity entity world)
                    world removed
            (current, world))

[<AutoOpen>]
module StreamOperators =

    /// Stream sequencing operator.
    let (---) = (|>)

    /// Make a stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] (!--) (lens : Lens<'b, World>) = !-- lens

    /// Propagate the event data of a stream to a value in the observing participant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] (-|>) stream (lens : Lens<'b, World>) = stream -|> lens

    // Propagate a value from the given source participant to a value in the given destination
    // participant, but with update-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] (-/>) stream lens = Stream.noMoreThanOncePerUpdate stream -|> lens