// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open System.Threading
open SDL2
open ImGuiNET
open Prime

/// A renderer process that may or may not be threaded.
type RendererProcess =
    interface
        /// Start the rendering process.
        abstract Start : ImFontAtlasPtr -> Window option -> Viewport -> Viewport -> Viewport -> unit
        /// The current configuration of the 3d renderer.
        abstract Renderer3dConfig : Renderer3dConfig
        /// Attempt to get a texture id that can be used to visually represent the specified asset.
        abstract TryGetImGuiTextureId : AssetTag -> uint32 voption
        /// Enqueue a 3d rendering message.
        abstract EnqueueMessage3d : RenderMessage3d -> unit
        /// Potential fast-path for rendering static models.
        abstract RenderStaticModelFast : Matrix4x4 inref * bool * Presence * Box2 voption * MaterialProperties inref * StaticModel AssetTag * RenderType * RenderPass -> unit
        /// Potential fast-path for rendering static model surfaces.
        abstract RenderStaticModelSurfaceFast : Matrix4x4 inref * bool * Presence * Box2 voption * MaterialProperties inref * Material inref * StaticModel AssetTag * int * RenderType * RenderPass -> unit
        /// Potential fast-path for rendering animated models.
        abstract RenderAnimatedModelFast : Matrix4x4 inref * bool * Presence * Box2 voption * MaterialProperties inref * Matrix4x4 array * AnimatedModel AssetTag * RenderPass -> unit
        /// Enqueue a 2d rendering message.
        abstract EnqueueMessage2d : RenderMessage2d -> unit
        /// Potential fast-path for rendering layered sprite.
        abstract RenderLayeredSpriteFast : single * single * AssetTag * Transform inref * Box2 ValueOption inref * Box2 ValueOption inref * Image AssetTag * Color inref * Blend * Color inref * Flip -> unit
        /// Enqueue an ImGui rendering message.
        abstract EnqueueMessageImGui : RenderMessageImGui -> unit
        /// Clear enqueued render messages.
        abstract ClearMessages : unit -> unit
        /// Submit enqueued render messages for processing.
        abstract SubmitMessages : Frustum -> Frustum -> Frustum -> Box3 -> Vector3 -> Quaternion -> single -> Vector2 -> Vector2 -> Vector2i -> Viewport -> Viewport -> Viewport -> ImDrawDataPtr -> unit
        /// Request to swap the underlying render buffer.
        abstract Swap : unit -> unit
        /// Terminate the rendering process, blocking until termination is complete.
        abstract Terminate : unit -> unit
        end

/// A non-threaded render process.
type RendererInline () =

    let mutable started = false
    let mutable terminated = false
    let mutable windowOpt = Option<Window>.None
    let mutable glFinishRequired = false
    let mutable messages3d = List ()
    let mutable messages2d = List ()
    let mutable messagesImGui = List ()
    let mutable renderersOpt = Option<Renderer3d * Renderer2d * RendererImGui>.None
    let assetTextureRequests = ConcurrentDictionary<AssetTag, unit> HashIdentity.Structural
    let assetTextureOpts = ConcurrentDictionary<AssetTag, uint32 voption> HashIdentity.Structural

    interface RendererProcess with

        member this.Start fonts windowOpt_ geometryViewport rasterViewport outerViewport =

            // assign windowOpt
            windowOpt <- windowOpt_

            // ensure renderers not already created
            match renderersOpt with
            | None ->

                // create renderers
                match windowOpt with
                | Some window ->
                
                    // create gl context
                    let (glFinishRequired', glContext) = match window with SglWindow window -> OpenGL.Hl.CreateSglContextInitial window.SglWindow
                    OpenGL.Hl.Assert ()
                    glFinishRequired <- glFinishRequired'

                    // initialize gl context
                    OpenGL.Hl.InitContext Constants.OpenGL.HlDebug
                    OpenGL.Hl.Assert ()

                    // create 3d renderer
                    let renderer3d = GlRenderer3d.make glContext window geometryViewport rasterViewport :> Renderer3d
                    OpenGL.Hl.Assert ()

                    // create 2d renderer
                    let renderer2d = GlRenderer2d.make rasterViewport :> Renderer2d
                    OpenGL.Hl.Assert ()

                    // create imgui renderer
                    let rendererImGui = GlRendererImGui.make assetTextureRequests assetTextureOpts fonts outerViewport :> RendererImGui
                    OpenGL.Hl.Assert ()

                    // fin
                    renderersOpt <- Some (renderer3d, renderer2d, rendererImGui)

                // no renderers
                | None -> renderersOpt <- None

                // fin
                started <- true

            // fail on already created
            | Some _ -> raise (InvalidOperationException "Redundant Start calls.")

        member this.Renderer3dConfig =
            match renderersOpt with
            | Some (renderer3d, _, _) -> renderer3d.RendererConfig
            | None -> Renderer3dConfig.defaultConfig

        member this.TryGetImGuiTextureId assetTag =
            assetTextureRequests.[assetTag] <- ()
            match assetTextureOpts.TryGetValue assetTag with
            | (true, textureIdOpt) -> textureIdOpt
            | (false, _) -> ValueNone

        member this.EnqueueMessage3d message =
            match renderersOpt with
            | Some _ -> messages3d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderStaticModelFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, staticModel, renderType, renderPass) =
            match renderersOpt with
            | Some _ -> messages3d.Add (RenderStaticModel { ModelMatrix = modelMatrix; CastShadow = castShadow; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; StaticModel = staticModel; RenderType = renderType; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderStaticModelSurfaceFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, material, staticModel, surfaceIndex, renderType, renderPass) =
            match renderersOpt with
            | Some _ -> messages3d.Add (RenderStaticModelSurface { ModelMatrix = modelMatrix; CastShadow = castShadow; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; Material = material; StaticModel = staticModel; SurfaceIndex = surfaceIndex; RenderType = renderType; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderAnimatedModelFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, boneTransforms, animatedModel, renderPass) =
            match renderersOpt with
            | Some _ -> messages3d.Add (RenderAnimatedModel { ModelMatrix = modelMatrix; CastShadow = castShadow; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; BoneTransforms = boneTransforms; AnimatedModel = animatedModel; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.EnqueueMessage2d message =
            match renderersOpt with
            | Some _ -> messages2d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderLayeredSpriteFast (elevation, horizon, assetTag, transform, insetOpt, clipOpt, image, color, blend, emission, flip) =
            match renderersOpt with
            | Some _ -> messages2d.Add (LayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = assetTag; RenderOperation2d = RenderSprite { Transform = transform; InsetOpt = insetOpt; ClipOpt = clipOpt; Image = image; Color = color; Blend = blend; Emission = emission; Flip = flip }})
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.EnqueueMessageImGui message =
            match renderersOpt with
            | Some _ -> messagesImGui.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.ClearMessages () =
            messages3d.Clear ()
            messages2d.Clear ()
            messagesImGui.Clear ()

        member this.SubmitMessages frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye3dFieldOfView eye2dCenter eye2dSize windowSize geometryViewport rasterViewport outerViewport drawData =
            match renderersOpt with
            | Some (renderer3d, renderer2d, rendererImGui) ->

                // begin frame
                OpenGL.Hl.BeginFrame (windowSize, outerViewport.Bounds)
                OpenGL.Hl.Assert ()

                // render 3d
                renderer3d.Render frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye3dFieldOfView geometryViewport rasterViewport messages3d
                messages3d.Clear ()
                OpenGL.Hl.Assert ()

                // render 2d
                renderer2d.Render eye2dCenter eye2dSize rasterViewport messages2d
                messages2d.Clear ()
                OpenGL.Hl.Assert ()

                // render imgui
                rendererImGui.Render outerViewport drawData messagesImGui
                messagesImGui.Clear ()
                OpenGL.Hl.Assert ()

                // end frame
                OpenGL.Hl.EndFrame ()
                OpenGL.Hl.Assert ()

            | None -> ()

        member this.Swap () =
            match windowOpt with
            | Some (SglWindow window) ->
                if glFinishRequired then OpenGL.Gl.Finish ()
                SDL.SDL_GL_SwapWindow window.SglWindow
            | None -> ()

        member this.Terminate () =
            match renderersOpt with
            | Some (renderer3d, renderer2d, rendererImGui) ->
                renderer3d.CleanUp ()
                renderer2d.CleanUp ()
                rendererImGui.CleanUp ()
                renderersOpt <- None
                terminated <- true
            | None -> ()

/// A threaded render process.
type RendererThread () =

    let [<VolatileField>] mutable threadOpt = None
    let [<VolatileField>] mutable started = false
    let [<VolatileField>] mutable terminated = false
    let [<VolatileField>] mutable submissionOpt = Option<Frustum * Frustum * Frustum * Box3 * RenderMessage3d List * RenderMessage2d List * RenderMessageImGui List * Vector3 * Quaternion * single * Vector2 * Vector2 * Vector2i * Viewport * Viewport * Viewport * ImDrawDataPtr>.None
    let [<VolatileField>] mutable swapRequested = false
    let [<VolatileField>] mutable swapCompleted = false
    let [<VolatileField>] mutable renderer3dConfig = Renderer3dConfig.defaultConfig
    let [<VolatileField>] mutable messageBufferIndex = 0
    let messageBuffers3d = [|List (); List ()|]
    let messageBuffers2d = [|List (); List ()|]
    let messageBuffersImGui = [|List (); List ()|]
    let assetTextureRequests = ConcurrentDictionary<AssetTag, unit> HashIdentity.Structural
    let assetTextureOpts = ConcurrentDictionary<AssetTag, uint32 voption> HashIdentity.Structural
    let cachedStaticModelMessagesLock = obj ()
    let cachedStaticModelMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedStaticModelMessagesCapacity = Constants.Render.StaticModelMessagesPrealloc
    let cachedStaticModelSurfaceMessagesLock = obj ()
    let cachedStaticModelSurfaceMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedStaticModelSurfaceMessagesCapacity = Constants.Render.StaticModelSurfaceMessagesPrealloc
    let cachedAnimatedModelMessagesLock = obj ()
    let cachedAnimatedModelMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedAnimatedModelMessagesCapacity = Constants.Render.AnimatedModelMessagesPrealloc
    let cachedSpriteMessagesLock = obj ()
    let cachedSpriteMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedSpriteMessagesCapacity = Constants.Render.SpriteMessagesPrealloc

    let allocStaticModelMessage () =
        lock cachedStaticModelMessagesLock (fun () ->
            if cachedStaticModelMessages.Count = 0 then
                for _ in 0 .. dec cachedStaticModelMessagesCapacity do
                    let staticModelDescriptor =
                        { CachedStaticModelMatrix = Unchecked.defaultof<_>
                          CachedStaticModelCastShadow = Unchecked.defaultof<_>
                          CachedStaticModelPresence = Unchecked.defaultof<_>
                          CachedStaticModelInsetOpt = Unchecked.defaultof<_>
                          CachedStaticModelMaterialProperties = Unchecked.defaultof<_>
                          CachedStaticModel = Unchecked.defaultof<_>
                          CachedStaticModelRenderType = Unchecked.defaultof<_>
                          CachedStaticModelRenderPass = Unchecked.defaultof<_> }
                    let cachedStaticModelMessage = RenderCachedStaticModel staticModelDescriptor
                    cachedStaticModelMessages.Enqueue cachedStaticModelMessage
                cachedStaticModelMessagesCapacity <- cachedStaticModelMessagesCapacity * 2
                cachedStaticModelMessages.Dequeue ()
            else cachedStaticModelMessages.Dequeue ())

    let allocStaticModelSurfaceMessage () =
        lock cachedStaticModelSurfaceMessagesLock (fun () ->
            if cachedStaticModelSurfaceMessages.Count = 0 then
                for _ in 0 .. dec cachedStaticModelSurfaceMessagesCapacity do
                    let staticModelSurfaceDescriptor =
                        { CachedStaticModelSurfaceMatrix = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceCastShadow = Unchecked.defaultof<_>
                          CachedStaticModelSurfacePresence = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceInsetOpt = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceMaterialProperties = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceMaterial = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceModel = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceIndex = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceRenderType = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceRenderPass = Unchecked.defaultof<_> }
                    let cachedStaticModelSurfaceMessage = RenderCachedStaticModelSurface staticModelSurfaceDescriptor
                    cachedStaticModelSurfaceMessages.Enqueue cachedStaticModelSurfaceMessage
                cachedStaticModelSurfaceMessagesCapacity <- cachedStaticModelSurfaceMessagesCapacity * 2
                cachedStaticModelSurfaceMessages.Dequeue ()
            else cachedStaticModelSurfaceMessages.Dequeue ())

    let freeStaticModelMessages messages =
        lock cachedStaticModelMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderCachedStaticModel _ -> cachedStaticModelMessages.Enqueue message
                | _ -> ())

    let freeStaticModelSurfaceMessages messages =
        lock cachedStaticModelSurfaceMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderCachedStaticModelSurface _ -> cachedStaticModelSurfaceMessages.Enqueue message
                | _ -> ())

    let allocAnimatedModelMessage () =
        lock cachedAnimatedModelMessagesLock (fun () ->
            if cachedAnimatedModelMessages.Count = 0 then
                for _ in 0 .. dec cachedAnimatedModelMessagesCapacity do
                    let animatedModelDescriptor =
                        { CachedAnimatedModelMatrix = Unchecked.defaultof<_>
                          CachedAnimatedModelCastShadow = Unchecked.defaultof<_>
                          CachedAnimatedModelPresence = Unchecked.defaultof<_>
                          CachedAnimatedModelInsetOpt = Unchecked.defaultof<_>
                          CachedAnimatedModelMaterialProperties = Unchecked.defaultof<_>
                          CachedAnimatedModelBoneTransforms = Unchecked.defaultof<_>
                          CachedAnimatedModel = Unchecked.defaultof<_>
                          CachedAnimatedModelRenderPass = Unchecked.defaultof<_> }
                    let cachedAnimatedModelMessage = RenderCachedAnimatedModel animatedModelDescriptor
                    cachedAnimatedModelMessages.Enqueue cachedAnimatedModelMessage
                cachedAnimatedModelMessagesCapacity <- cachedAnimatedModelMessagesCapacity * 2
                cachedAnimatedModelMessages.Dequeue ()
            else cachedAnimatedModelMessages.Dequeue ())

    let freeAnimatedModelMessages messages =
        lock cachedAnimatedModelMessagesLock (fun () ->
            for message in messages do
                match message with
                | RenderCachedAnimatedModel _ -> cachedAnimatedModelMessages.Enqueue message
                | _ -> ())

    let allocSpriteMessage () =
        lock cachedSpriteMessagesLock (fun () ->
            if cachedSpriteMessages.Count = 0 then
                for _ in 0 .. dec cachedSpriteMessagesCapacity do
                    let spriteDescriptor = RenderCachedSprite { CachedSprite = Unchecked.defaultof<_> }
                    let cachedSpriteMessage = LayeredOperation2d { Elevation = 0.0f; Horizon = 0.0f; AssetTag = Unchecked.defaultof<_>; RenderOperation2d = spriteDescriptor }
                    cachedSpriteMessages.Enqueue cachedSpriteMessage
                cachedSpriteMessagesCapacity <- cachedSpriteMessagesCapacity * 2
                cachedSpriteMessages.Dequeue ()
            else cachedSpriteMessages.Dequeue ())

    let freeSpriteMessages messages =
        lock cachedSpriteMessagesLock (fun () ->
            for message in messages do
                match message with
                | LayeredOperation2d opertion ->
                    match opertion.RenderOperation2d with
                    | RenderCachedSprite _ -> cachedSpriteMessages.Enqueue message
                    | _ -> ()
                | _ -> ())

    member private this.Run fonts window geometryViewport rasterViewport outerViewport =

        // create gl context
        let (glFinishRequired, glContext) = match window with SglWindow window -> OpenGL.Hl.CreateSglContextInitial window.SglWindow
        OpenGL.Hl.Assert ()

        // initialize gl context
        OpenGL.Hl.InitContext Constants.OpenGL.HlDebug
        OpenGL.Hl.Assert ()

        // create 3d renderer
        let renderer3d = GlRenderer3d.make glContext window geometryViewport rasterViewport :> Renderer3d
        OpenGL.Hl.Assert ()

        // create 2d renderer
        let renderer2d = GlRenderer2d.make rasterViewport :> Renderer2d
        OpenGL.Hl.Assert ()

        // create imgui renderer
        let rendererImGui = GlRendererImGui.make assetTextureRequests assetTextureOpts fonts outerViewport :> RendererImGui

        // mark as started
        started <- true

        // loop until terminated
        while not terminated do

            // wait until submission is provided
            while Option.isNone submissionOpt && not terminated do Thread.Yield () |> ignore<bool>
            let (frustumInterior, frustumExterior, frustumImposter, lightBox, messages3d, messages2d, messagesImGui, eye3dCenter, eye3dRotation, eye3dFieldOfView, eye2dCenter, eye2dSize, windowSize, geometryViewport, rasterViewport, outerViewport, drawData) = Option.get submissionOpt
            submissionOpt <- None

            // guard against early termination
            if not terminated then

                // begin frame
                OpenGL.Hl.BeginFrame (windowSize, outerViewport.Bounds)
                OpenGL.Hl.Assert ()

                // render 3d
                renderer3d.Render frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye3dFieldOfView geometryViewport rasterViewport messages3d
                freeStaticModelMessages messages3d
                freeStaticModelSurfaceMessages messages3d
                freeAnimatedModelMessages messages3d
                renderer3dConfig <- renderer3d.RendererConfig
                OpenGL.Hl.Assert ()

                // render 2d
                renderer2d.Render eye2dCenter eye2dSize rasterViewport messages2d
                freeSpriteMessages messages2d
                OpenGL.Hl.Assert ()

                // render imgui
                rendererImGui.Render outerViewport drawData messagesImGui
                OpenGL.Hl.Assert ()

                // end frame
                OpenGL.Hl.EndFrame ()
                OpenGL.Hl.Assert ()

                // guard against early termination
                if not terminated then
            
                    // wait until swap is requested
                    while not swapRequested && not terminated do Thread.Yield () |> ignore<bool>
                    swapRequested <- false

                    // guard against early termination
                    if not terminated then

                        // notify swap is completed
                        swapCompleted <- true

                        // swap, optionally finishing
                        if glFinishRequired then OpenGL.Gl.Finish ()
                        match window with SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow

        // clean up
        renderer3d.CleanUp ()
        renderer2d.CleanUp ()
        rendererImGui.CleanUp ()

    interface RendererProcess with

        member this.Start fonts windowOpt geometryViewport rasterViewport outerViewport =

            // validate state
            if Option.isSome threadOpt then raise (InvalidOperationException "Render process already started.")

            // attempt to start thread
            match windowOpt with
            | Some window ->

                // start real thread
                let thread = Thread (ThreadStart (fun () -> this.Run fonts window geometryViewport rasterViewport outerViewport))
                threadOpt <- Some thread
                thread.IsBackground <- true
                thread.Start ()

            | None ->

                // start empty thread
                let thread = Thread (ThreadStart (fun () ->
                    started <- true
                    while not terminated do
                        while Option.isNone submissionOpt && not terminated do Thread.Yield () |> ignore<bool>
                        submissionOpt <- None
                        if not terminated then
                            while not swapRequested && not terminated do ()
                            swapRequested <- false
                            if not terminated then
                                swapCompleted <- true))
                threadOpt <- Some thread
                thread.IsBackground <- true
                thread.Start ()

            // wait for thread to finish starting
            while not started do Thread.Yield () |> ignore<bool>

        member this.Renderer3dConfig =
            renderer3dConfig

        member this.TryGetImGuiTextureId assetTag =
            assetTextureRequests.[assetTag] <- ()
            match assetTextureOpts.TryGetValue assetTag with
            | (true, textureIdOpt) -> textureIdOpt
            | (false, _) -> ValueNone

        member this.EnqueueMessage3d message =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match message with
            | RenderStaticModel rsm ->
                let cachedStaticModelMessage = allocStaticModelMessage ()
                match cachedStaticModelMessage with
                | RenderCachedStaticModel cachedMessage ->
                    cachedMessage.CachedStaticModelMatrix <- rsm.ModelMatrix
                    cachedMessage.CachedStaticModelCastShadow <- rsm.CastShadow
                    cachedMessage.CachedStaticModelPresence <- rsm.Presence
                    cachedMessage.CachedStaticModelInsetOpt <- ValueOption.ofOption rsm.InsetOpt
                    cachedMessage.CachedStaticModelMaterialProperties <- rsm.MaterialProperties
                    cachedMessage.CachedStaticModel <- rsm.StaticModel
                    cachedMessage.CachedStaticModelRenderType <- rsm.RenderType
                    cachedMessage.CachedStaticModelRenderPass <- rsm.RenderPass
                    messageBuffers3d.[messageBufferIndex].Add cachedStaticModelMessage
                | _ -> failwithumf ()
            | RenderStaticModelSurface rsms ->
                let cachedStaticModelSurfaceMessage = allocStaticModelSurfaceMessage ()
                match cachedStaticModelSurfaceMessage with
                | RenderCachedStaticModelSurface cachedMessage ->
                    cachedMessage.CachedStaticModelSurfaceMatrix <- rsms.ModelMatrix
                    cachedMessage.CachedStaticModelSurfaceCastShadow <- rsms.CastShadow
                    cachedMessage.CachedStaticModelSurfacePresence <- rsms.Presence
                    cachedMessage.CachedStaticModelSurfaceInsetOpt <- ValueOption.ofOption rsms.InsetOpt
                    cachedMessage.CachedStaticModelSurfaceMaterialProperties <- rsms.MaterialProperties
                    cachedMessage.CachedStaticModelSurfaceMaterial <- rsms.Material
                    cachedMessage.CachedStaticModelSurfaceModel <- rsms.StaticModel
                    cachedMessage.CachedStaticModelSurfaceIndex <- rsms.SurfaceIndex
                    cachedMessage.CachedStaticModelSurfaceRenderType <- rsms.RenderType
                    cachedMessage.CachedStaticModelSurfaceRenderPass <- rsms.RenderPass
                    messageBuffers3d.[messageBufferIndex].Add cachedStaticModelSurfaceMessage
                | _ -> failwithumf ()
            | RenderAnimatedModel ram ->
                let cachedAnimatedModelMessage = allocAnimatedModelMessage ()
                match cachedAnimatedModelMessage with
                | RenderCachedAnimatedModel cachedMessage ->
                    cachedMessage.CachedAnimatedModelMatrix <- ram.ModelMatrix
                    cachedMessage.CachedAnimatedModelCastShadow <- ram.CastShadow
                    cachedMessage.CachedAnimatedModelPresence <- ram.Presence
                    cachedMessage.CachedAnimatedModelInsetOpt <- ValueOption.ofOption ram.InsetOpt
                    cachedMessage.CachedAnimatedModelMaterialProperties <- ram.MaterialProperties
                    cachedMessage.CachedAnimatedModelBoneTransforms <- ram.BoneTransforms
                    cachedMessage.CachedAnimatedModel <- ram.AnimatedModel
                    cachedMessage.CachedAnimatedModelRenderPass <- ram.RenderPass
                    messageBuffers3d.[messageBufferIndex].Add cachedAnimatedModelMessage
                | _ -> failwithumf ()
            | _ -> messageBuffers3d.[messageBufferIndex].Add message

        member this.RenderStaticModelFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, staticModel, renderType, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let cachedStaticModelMessage = allocStaticModelMessage ()
            match cachedStaticModelMessage with
            | RenderCachedStaticModel cachedMessage ->
                cachedMessage.CachedStaticModelMatrix <- modelMatrix
                cachedMessage.CachedStaticModelCastShadow <- castShadow
                cachedMessage.CachedStaticModelPresence <- presence
                cachedMessage.CachedStaticModelInsetOpt <- insetOpt
                cachedMessage.CachedStaticModelMaterialProperties <- materialProperties
                cachedMessage.CachedStaticModel <- staticModel
                cachedMessage.CachedStaticModelRenderType <- renderType
                cachedMessage.CachedStaticModelRenderPass <- renderPass
                messageBuffers3d.[messageBufferIndex].Add cachedStaticModelMessage
            | _ -> failwithumf ()

        member this.RenderStaticModelSurfaceFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, material, staticModel, surfaceIndex, renderType, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let cachedStaticModelSurfaceMessage = allocStaticModelSurfaceMessage ()
            match cachedStaticModelSurfaceMessage with
            | RenderCachedStaticModelSurface cachedMessage ->
                cachedMessage.CachedStaticModelSurfaceMatrix <- modelMatrix
                cachedMessage.CachedStaticModelSurfaceCastShadow <- castShadow
                cachedMessage.CachedStaticModelSurfacePresence <- presence
                cachedMessage.CachedStaticModelSurfaceInsetOpt <- insetOpt
                cachedMessage.CachedStaticModelSurfaceMaterialProperties <- materialProperties
                cachedMessage.CachedStaticModelSurfaceMaterial <- material
                cachedMessage.CachedStaticModelSurfaceModel <- staticModel
                cachedMessage.CachedStaticModelSurfaceIndex <- surfaceIndex
                cachedMessage.CachedStaticModelSurfaceRenderType <- renderType
                cachedMessage.CachedStaticModelSurfaceRenderPass <- renderPass
                messageBuffers3d.[messageBufferIndex].Add cachedStaticModelSurfaceMessage
            | _ -> failwithumf ()

        member this.RenderAnimatedModelFast (modelMatrix, castShadow, presence, insetOpt, materialProperties, boneTransforms, animatedModel, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let cachedAnimatedModelMessage = allocAnimatedModelMessage ()
            match cachedAnimatedModelMessage with
            | RenderCachedAnimatedModel cachedMessage ->
                cachedMessage.CachedAnimatedModelMatrix <- modelMatrix
                cachedMessage.CachedAnimatedModelCastShadow <- castShadow
                cachedMessage.CachedAnimatedModelPresence <- presence
                cachedMessage.CachedAnimatedModelInsetOpt <- insetOpt
                cachedMessage.CachedAnimatedModelMaterialProperties <- materialProperties
                cachedMessage.CachedAnimatedModelBoneTransforms <- boneTransforms
                cachedMessage.CachedAnimatedModel <- animatedModel
                cachedMessage.CachedAnimatedModelRenderPass <- renderPass
                messageBuffers3d.[messageBufferIndex].Add cachedAnimatedModelMessage
            | _ -> failwithumf ()

        member this.EnqueueMessage2d message =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match message with
            | LayeredOperation2d operation ->
                match operation.RenderOperation2d with
                | RenderSprite sprite ->
                    let cachedSpriteMessage = allocSpriteMessage ()
                    match cachedSpriteMessage with
                    | LayeredOperation2d cachedOperation ->
                        match cachedOperation.RenderOperation2d with
                        | RenderCachedSprite descriptor ->
                            cachedOperation.Elevation <- operation.Elevation
                            cachedOperation.Horizon <- operation.Horizon
                            cachedOperation.AssetTag <- operation.AssetTag
                            descriptor.CachedSprite.Transform <- sprite.Transform
                            descriptor.CachedSprite.InsetOpt <- sprite.InsetOpt
                            descriptor.CachedSprite.ClipOpt <- sprite.ClipOpt
                            descriptor.CachedSprite.Image <- sprite.Image
                            descriptor.CachedSprite.Color <- sprite.Color
                            descriptor.CachedSprite.Blend <- sprite.Blend
                            descriptor.CachedSprite.Emission <- sprite.Emission
                            descriptor.CachedSprite.Flip <- sprite.Flip
                            messageBuffers2d.[messageBufferIndex].Add cachedSpriteMessage 
                        | _ -> failwithumf ()
                    | _ -> failwithumf ()
                | _ -> messageBuffers2d.[messageBufferIndex].Add message
            | _ -> messageBuffers2d.[messageBufferIndex].Add message

        member this.RenderLayeredSpriteFast (elevation, horizon, assetTag, transform, insetOpt, clipOpt, image, color, blend, emission, flip) =
            let cachedSpriteMessage = allocSpriteMessage ()
            match cachedSpriteMessage with
            | LayeredOperation2d cachedOperation ->
                match cachedOperation.RenderOperation2d with
                | RenderCachedSprite descriptor ->
                    cachedOperation.Elevation <- elevation
                    cachedOperation.Horizon <- horizon
                    cachedOperation.AssetTag <- assetTag
                    descriptor.CachedSprite.Transform <- transform
                    descriptor.CachedSprite.InsetOpt <- insetOpt
                    descriptor.CachedSprite.ClipOpt <- clipOpt
                    descriptor.CachedSprite.Image <- image
                    descriptor.CachedSprite.Color <- color
                    descriptor.CachedSprite.Blend <- blend
                    descriptor.CachedSprite.Emission <- emission
                    descriptor.CachedSprite.Flip <- flip
                    messageBuffers2d.[messageBufferIndex].Add cachedSpriteMessage 
                | _ -> failwithumf ()
            | _ -> failwithumf ()

        member this.EnqueueMessageImGui message =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            messageBuffersImGui.[messageBufferIndex].Add message

        member this.ClearMessages () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()
            messageBuffersImGui.[messageBufferIndex].Clear ()

        member this.SubmitMessages frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye3dFieldOfView eye2dCenter eye2dSize eyeMargin geometryViewport rasterViewport outerViewport drawData =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let messages3d = messageBuffers3d.[messageBufferIndex]
            let messages2d = messageBuffers2d.[messageBufferIndex]
            let messagesImGui = messageBuffersImGui.[messageBufferIndex]
            messageBufferIndex <- if messageBufferIndex = 0 then 1 else 0
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()
            messageBuffersImGui.[messageBufferIndex].Clear ()
            submissionOpt <- Some (frustumInterior, frustumExterior, frustumImposter, lightBox, messages3d, messages2d, messagesImGui, eye3dCenter, eye3dRotation, eye3dFieldOfView, eye2dCenter, eye2dSize, eyeMargin, geometryViewport, rasterViewport, outerViewport, drawData)

        member this.Swap () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            swapRequested <- true
            while not swapCompleted && not terminated do Thread.Yield () |> ignore<bool>
            swapCompleted <- false

        member this.Terminate () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let thread = Option.get threadOpt
            if terminated then raise (InvalidOperationException "Redundant Terminate calls.")
            terminated <- true
            thread.Join ()
            threadOpt <- None