// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Threading
open System.Threading.Tasks
open Prime
open Nu

/// A renderer process that may or may not be threaded.
type RendererProcess =
    interface
        abstract Started : bool
        abstract Terminated : bool
        abstract Start : unit -> unit
        abstract EnqueueMessage2d : RenderMessage2d -> unit
        abstract EnqueueMessage3d : RenderMessage3d -> unit
        abstract ClearMessages : unit -> unit
        abstract SubmitMessages : Vector2 -> Vector2 -> Vector3 -> Quaternion -> Vector2i -> unit
        abstract Swap : unit -> unit
        abstract Terminate : unit -> unit
        end

/// A non-threaded renderer.
type RendererInline (createRenderer2d, createRenderer3d) =

    let mutable started = false
    let mutable terminated = false
    let mutable messages2d = SegmentedList.make ()
    let mutable messages3d = SegmentedList.make ()
    let mutable renderersOpt = Option<Renderer2d * Renderer3d>.None

    interface RendererProcess with

        member this.Started =
            started

        member this.Terminated =
            terminated

        member this.Start () =
            match renderersOpt with
            | Some _ -> raise (InvalidOperationException "Redundant Start calls.")
            | None ->
                let renderer3d = createRenderer3d { ShouldInitializeContext = true; ShouldBeginFrame = true; ShouldEndFrame = false }
                let renderer2d = createRenderer2d { ShouldInitializeContext = false; ShouldBeginFrame = false; ShouldEndFrame = true }
                renderersOpt <- Some (renderer2d, renderer3d)
                started <- true

        member this.EnqueueMessage2d message =
            match renderersOpt with
            | Some _ -> SegmentedList.add message messages2d
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.EnqueueMessage3d message =
            match renderersOpt with
            | Some _ -> SegmentedList.add message messages3d
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.ClearMessages () =
            messages2d <- SegmentedList.make ()

        member this.SubmitMessages eyePosition2d eyeSize2d eyePosition3d eyeRotation3d windowSize =
            match renderersOpt with
            | Some (renderer2d, renderer3d) ->
                renderer3d.Render eyePosition3d eyeRotation3d windowSize messages3d
                SegmentedList.clear messages3d
                renderer2d.Render eyePosition2d eyeSize2d windowSize messages2d
                SegmentedList.clear messages2d
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.Swap () =
            match renderersOpt with
            | Some (renderer2d, _) -> renderer2d.Swap ()
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.Terminate () =
            match renderersOpt with
            | Some (renderer2d, renderer3d) ->
                renderer3d.CleanUp ()
                renderer2d.CleanUp ()
                renderersOpt <- None
                terminated <- true
            | None -> raise (InvalidOperationException "Redundant Terminate calls.")

/// A threaded renderer.
type RendererThread (createRenderer2d, createRenderer3d) =

    let mutable taskOpt = None
    let [<VolatileField>] mutable started = false
    let [<VolatileField>] mutable terminated = false
    let [<VolatileField>] mutable messages2d = SegmentedList.make ()
    let [<VolatileField>] mutable messages3d = SegmentedList.make ()
    let [<VolatileField>] mutable submissionOpt = Option<RenderMessage2d SegmentedList * RenderMessage3d SegmentedList * Vector2 * Vector2 * Vector3 * Quaternion * Vector2i>.None
    let [<VolatileField>] mutable swap = false
    let cachedSpriteMessagesLock = obj ()
    let cachedSpriteMessages = Queue ()
    let mutable cachedSpriteMessagesCapacity = Constants.Render.SpriteMessagesPrealloc

    let allocSpriteMessage () =
        lock cachedSpriteMessagesLock (fun () ->
            if cachedSpriteMessages.Count = 0 then
                for _ in 0 .. dec cachedSpriteMessagesCapacity do
                    let spriteDescriptor = CachedSpriteDescriptor { CachedSprite = Unchecked.defaultof<_> }
                    let cachedSpriteMessage = RenderLayeredMessage2d { Elevation = 0.0f; Horizon = 0.0f; AssetTag = Unchecked.defaultof<_>; RenderDescriptor = spriteDescriptor }
                    cachedSpriteMessages.Enqueue cachedSpriteMessage
                cachedSpriteMessagesCapacity <- cachedSpriteMessagesCapacity * 2
                cachedSpriteMessages.Dequeue ()
            else cachedSpriteMessages.Dequeue ())

    let freeSpriteMessages messages =
        lock cachedSpriteMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderLayeredMessage2d layeredMessage ->
                    match layeredMessage.RenderDescriptor with
                    | CachedSpriteDescriptor _ -> cachedSpriteMessages.Enqueue message
                    | _ -> ()
                | _ -> ())

    member private this.Run () =

        // create 3d renderer
        let renderer3d = createRenderer3d { ShouldInitializeContext = true; ShouldBeginFrame = true; ShouldEndFrame = false } : Renderer3d

        // create 2d renderer
        let renderer2d = createRenderer2d { ShouldInitializeContext = false; ShouldBeginFrame = false; ShouldEndFrame = true } : Renderer2d

        // mark as started
        started <- true

        // loop until terminated
        while not terminated do

            // loop until submission exists
            while Option.isNone submissionOpt && not terminated do Thread.Yield () |> ignore<bool>

            // guard against early termination
            if not terminated then

                // receive submission
                let (messages2d, messages3d, eyePosition2d, eyeSize2d, eyePosition3d, eyeRotation3d, windowSize) = Option.get submissionOpt
                submissionOpt <- None

                // render 3d
                renderer3d.Render eyePosition3d eyeRotation3d windowSize messages3d

                // render 2d
                renderer2d.Render eyePosition2d eyeSize2d windowSize messages2d
            
                // recover cached sprite messages
                freeSpriteMessages messages2d

                // loop until swap is requested
                while not swap && not terminated do Thread.Yield () |> ignore<bool>

                // guard against early termination
                if not terminated then

                    // swap
                    renderer2d.Swap ()

                    // complete swap request
                    swap <- false

        // clean up
        renderer2d.CleanUp ()

    interface RendererProcess with

        member this.Started =
            started

        member this.Terminated =
            terminated

        member this.Start () =

            // validate state
            if Option.isSome taskOpt then raise (InvalidOperationException "Render process already started.")

            // start task
            let task = new Task ((fun () -> this.Run ()), TaskCreationOptions.LongRunning)
            taskOpt <- Some task
            task.Start ()

            // wait for task to finish starting
            while not started do Thread.Yield () |> ignore<bool>

        member this.EnqueueMessage2d message =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match message with
            | RenderLayeredMessage2d layeredMessage ->
                match layeredMessage.RenderDescriptor with
                | SpriteDescriptor sprite ->
                    let cachedSpriteMessage = allocSpriteMessage ()
                    match cachedSpriteMessage with
                    | RenderLayeredMessage2d cachedLayeredMessage ->
                        match cachedLayeredMessage.RenderDescriptor with
                        | CachedSpriteDescriptor descriptor ->
                            cachedLayeredMessage.Elevation <- layeredMessage.Elevation
                            cachedLayeredMessage.Horizon <- layeredMessage.Horizon
                            cachedLayeredMessage.AssetTag <- layeredMessage.AssetTag
                            descriptor.CachedSprite.Transform <- sprite.Transform
                            descriptor.CachedSprite.InsetOpt <- sprite.InsetOpt
                            descriptor.CachedSprite.Image <- sprite.Image
                            descriptor.CachedSprite.Color <- sprite.Color
                            descriptor.CachedSprite.Blend <- sprite.Blend
                            descriptor.CachedSprite.Glow <- sprite.Glow
                            descriptor.CachedSprite.Flip <- sprite.Flip
                            SegmentedList.add cachedSpriteMessage messages2d
                        | _ -> failwithumf ()
                    | _ -> failwithumf ()
                | _ -> SegmentedList.add message messages2d
            | _ -> SegmentedList.add message messages2d

        member this.EnqueueMessage3d message =
            SegmentedList.add message messages3d

        member this.ClearMessages () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            messages2d <- SegmentedList.make ()

        member this.SubmitMessages eyePosition2d eyeSize2d eyePosition3d eyeRotation3d eyeMargin =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            while swap do Thread.Yield () |> ignore<bool>
            let messages2dTemp = Interlocked.Exchange (&messages2d, SegmentedList.make ())
            let messages3dTemp = Interlocked.Exchange (&messages3d, SegmentedList.make ())
            submissionOpt <- Some (messages2dTemp, messages3dTemp, eyePosition2d, eyeSize2d, eyePosition3d, eyeRotation3d, eyeMargin)

        member this.Swap () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            if swap then raise (InvalidOperationException "Redundant Swap calls.")
            swap <- true

        member this.Terminate () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let task = Option.get taskOpt
            if terminated then raise (InvalidOperationException "Redundant Terminate calls.")
            terminated <- true
            task.Wait ()
            taskOpt <- None