// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
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
        abstract EnqueueMessage3d : RenderMessage3d -> unit
        abstract EnqueueMessage2d : RenderMessage2d -> unit
        abstract ClearMessages : unit -> unit
        abstract SubmitMessages : Vector3 -> Quaternion -> Vector2 -> Vector2 -> Vector2i -> unit
        abstract Swap : unit -> unit
        abstract Terminate : unit -> unit
        end

/// A non-threaded render process.
type RendererInline (createRenderer3d, createRenderer2d) =

    let mutable started = false
    let mutable terminated = false
    let mutable messages3d = List ()
    let mutable messages2d = List ()
    let mutable renderersOpt = Option<Renderer3d * Renderer2d>.None

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
                renderersOpt <- Some (renderer3d, renderer2d)
                started <- true

        member this.EnqueueMessage3d message =
            match renderersOpt with
            | Some _ -> messages3d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.EnqueueMessage2d message =
            match renderersOpt with
            | Some _ -> messages2d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.ClearMessages () =
            messages3d.Clear ()
            messages2d.Clear ()

        member this.SubmitMessages eyeCenter3d eyeRotation3d eyeCenter2d eyeSize2d windowSize =
            match renderersOpt with
            | Some (renderer3d, renderer2d) ->
                renderer3d.Render eyeCenter3d eyeRotation3d windowSize messages3d
                messages3d.Clear ()
                renderer2d.Render eyeCenter2d eyeSize2d windowSize messages2d
                messages2d.Clear ()
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.Swap () =
            match renderersOpt with
            | Some (_, renderer2d) -> renderer2d.Swap ()
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.Terminate () =
            match renderersOpt with
            | Some (renderer3d, renderer2d) ->
                renderer3d.CleanUp ()
                renderer2d.CleanUp ()
                renderersOpt <- None
                terminated <- true
            | None -> raise (InvalidOperationException "Redundant Terminate calls.")

/// A threaded render process.
type RendererThread (createRenderer3d, createRenderer2d) =

    let mutable taskOpt = None
    let [<VolatileField>] mutable started = false
    let [<VolatileField>] mutable terminated = false
    let [<VolatileField>] mutable submissionOpt = Option<RenderMessage3d List * RenderMessage2d List * Vector3 * Quaternion * Vector2 * Vector2 * Vector2i>.None
    let [<VolatileField>] mutable swap = false
    let mutable messageBufferIndex = 0
    let messageBuffers3d = [|List (); List ()|]
    let messageBuffers2d = [|List (); List ()|]
    let cachedSpriteMessagesLock = obj ()
    let cachedSpriteMessages = System.Collections.Generic.Queue ()
    let mutable cachedSpriteMessagesCapacity = Constants.Render.SpriteMessagesPrealloc
    let cachedStaticModelMessagesLock = obj ()
    let cachedStaticModelMessages = System.Collections.Generic.Queue ()
    let mutable cachedStaticModelMessagesCapacity = Constants.Render.StaticModelMessagesPrealloc

    let allocStaticModelMessage () =
        lock cachedStaticModelMessagesLock (fun () ->
            if cachedStaticModelMessages.Count = 0 then
                for _ in 0 .. dec cachedStaticModelMessagesCapacity do
                    let staticModelDescriptor =
                        { CachedStaticModelAbsolute = Unchecked.defaultof<_>
                          CachedStaticModelAffineMatrix = Unchecked.defaultof<_>
                          CachedStaticModelInsetOpt = Unchecked.defaultof<_>
                          CachedStaticModelRenderMaterial = Unchecked.defaultof<_>
                          CachedStaticModelRenderType = Unchecked.defaultof<_>
                          CachedStaticModel = Unchecked.defaultof<_> }
                    let cachedStaticModelMessage = RenderCachedStaticModelMessage staticModelDescriptor
                    cachedStaticModelMessages.Enqueue cachedStaticModelMessage
                cachedStaticModelMessagesCapacity <- cachedStaticModelMessagesCapacity * 2
                cachedStaticModelMessages.Dequeue ()
            else cachedStaticModelMessages.Dequeue ())

    let freeStaticModelMessages messages =
        lock cachedStaticModelMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderCachedStaticModelMessage _ -> cachedStaticModelMessages.Enqueue message
                | _ -> ())

    let allocSpriteMessage () =
        lock cachedSpriteMessagesLock (fun () ->
            if cachedSpriteMessages.Count = 0 then
                for _ in 0 .. dec cachedSpriteMessagesCapacity do
                    let spriteDescriptor = CachedSpriteDescriptor { CachedSprite = Unchecked.defaultof<_> }
                    let cachedSpriteMessage = RenderLayeredMessage2d { Elevation = 0.0f; Horizon = 0.0f; AssetTag = Unchecked.defaultof<_>; RenderDescriptor2d = spriteDescriptor }
                    cachedSpriteMessages.Enqueue cachedSpriteMessage
                cachedSpriteMessagesCapacity <- cachedSpriteMessagesCapacity * 2
                cachedSpriteMessages.Dequeue ()
            else cachedSpriteMessages.Dequeue ())

    let freeSpriteMessages messages =
        lock cachedSpriteMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderLayeredMessage2d layeredMessage ->
                    match layeredMessage.RenderDescriptor2d with
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
            while Option.isNone submissionOpt && not terminated do Thread.Sleep 1

            // guard against early termination
            if not terminated then

                // receie submission
                let (messages3d, messages2d, eyeCenter3d, eyeRotation3d, eyeCenter2d, eyeSize2d, windowSize) = Option.get submissionOpt
                submissionOpt <- None

                // render 3d
                renderer3d.Render eyeCenter3d eyeRotation3d windowSize messages3d
                
                // recover cached static model messages
                freeStaticModelMessages messages3d

                // render 2d
                renderer2d.Render eyeCenter2d eyeSize2d windowSize messages2d
            
                // recover cached sprite messages
                freeSpriteMessages messages2d

                // loop until swap is requested
                while not swap && not terminated do Thread.Sleep 1

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
            while not started do Thread.Sleep 1

        member this.EnqueueMessage3d message =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match message with
            | RenderStaticModelMessage (absolute, affineMatrix, insetOpt, renderMaterial, renderType, staticModel) ->
                let cachedStaticModelMessage = allocStaticModelMessage ()
                match cachedStaticModelMessage with
                | RenderCachedStaticModelMessage cachedDescriptor ->
                    cachedDescriptor.CachedStaticModelAbsolute <- absolute
                    cachedDescriptor.CachedStaticModelAffineMatrix <- affineMatrix
                    cachedDescriptor.CachedStaticModelInsetOpt <- insetOpt
                    cachedDescriptor.CachedStaticModelRenderMaterial <- renderMaterial
                    cachedDescriptor.CachedStaticModelRenderType <- renderType
                    cachedDescriptor.CachedStaticModel <- staticModel
                    messageBuffers3d.[messageBufferIndex].Add cachedStaticModelMessage
                | _ -> failwithumf ()
            | _ -> messageBuffers3d.[messageBufferIndex].Add message

        member this.EnqueueMessage2d message =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match message with
            | RenderLayeredMessage2d layeredMessage ->
                match layeredMessage.RenderDescriptor2d with
                | SpriteDescriptor sprite ->
                    let cachedSpriteMessage = allocSpriteMessage ()
                    match cachedSpriteMessage with
                    | RenderLayeredMessage2d cachedLayeredMessage ->
                        match cachedLayeredMessage.RenderDescriptor2d with
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
                            messageBuffers2d.[messageBufferIndex].Add cachedSpriteMessage 
                        | _ -> failwithumf ()
                    | _ -> failwithumf ()
                | _ -> messageBuffers2d.[messageBufferIndex].Add message
            | _ -> messageBuffers2d.[messageBufferIndex].Add message

        member this.ClearMessages () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()

        member this.SubmitMessages eyeCenter3d eyeRotation3d eyeCenter2d eyeSize2d eyeMargin =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            while swap do Thread.Sleep 1
            let messages3d = messageBuffers3d.[messageBufferIndex]
            let messages2d = messageBuffers2d.[messageBufferIndex]
            messageBufferIndex <- if messageBufferIndex = 0 then 1 else 0
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()
            submissionOpt <- Some (messages3d, messages2d, eyeCenter3d, eyeRotation3d, eyeCenter2d, eyeSize2d, eyeMargin)

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