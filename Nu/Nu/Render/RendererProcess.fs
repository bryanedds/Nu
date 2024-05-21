// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Threading
open FSharp.NativeInterop
open SDL2
open Vortice.Vulkan
open type Vortice.Vulkan.Vulkan
open ImGuiNET
open Prime
open System.Runtime.InteropServices
open System.Collections.Generic

/// A renderer process that may or may not be threaded.
type RendererProcess =
    interface
        /// Start the rendering process.
        abstract Start : ImFontAtlasPtr -> Window option -> unit
        /// The current configuration of the 3d renderer.
        abstract Renderer3dConfig : Renderer3dConfig
        /// Enqueue a 3d rendering message.
        abstract EnqueueMessage3d : RenderMessage3d -> unit
        /// Potential fast-path for rendering static models.
        abstract RenderStaticModelFast : bool * Matrix4x4 inref * Presence * Box2 voption * MaterialProperties inref * StaticModel AssetTag * RenderType * RenderPass -> unit
        /// Potential fast-path for rendering static model surfaces.
        abstract RenderStaticModelSurfaceFast : bool * Matrix4x4 inref * Presence * Box2 voption * MaterialProperties inref * Material inref * StaticModel AssetTag * int * RenderType * RenderPass -> unit
        /// Potential fast-path for rendering animated models.
        abstract RenderAnimatedModelFast : bool * Matrix4x4 inref * Presence * Box2 voption * MaterialProperties inref * Matrix4x4 array * AnimatedModel AssetTag * RenderPass -> unit
        /// Enqueue a 2d rendering message.
        abstract EnqueueMessage2d : RenderMessage2d -> unit
        /// Potential fast-path for rendering layered sprite.
        abstract RenderLayeredSpriteFast : single * single * AssetTag * Transform inref * Box2 ValueOption inref * Image AssetTag * Color inref * Blend * Color inref * Flip -> unit
        /// Clear enqueued render messages.
        abstract ClearMessages : unit -> unit
        /// Submit enqueued render messages for processing.
        abstract SubmitMessages : Frustum -> Frustum -> Frustum -> Box3 -> Vector3 -> Quaternion -> Vector2 -> Vector2 -> Vector2i -> ImDrawDataPtr -> unit
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
    let mutable messages3d = List ()
    let mutable messages2d = List ()
    let mutable renderersOpt = Option<Renderer3d * Renderer2d * RendererImGui>.None

    interface RendererProcess with

        member this.Start fonts windowOpt_ =
            ()

            //// assign windowOpt
            //windowOpt <- windowOpt_
            //
            //// ensure renderers not already created
            //match renderersOpt with
            //| None ->
            //
            //    // create renderers
            //    match windowOpt with
            //    | Some window ->
            //    
            //        // create gl context
            //        let glContext = match window with SglWindow window -> OpenGL.Hl.CreateSglContextInitial window.SglWindow
            //        OpenGL.Hl.Assert ()
            //
            //        // initialize gl context
            //        OpenGL.Hl.InitContext ()
            //        OpenGL.Hl.Assert ()
            //
            //        // create 3d renderer
            //        let renderer3d = GlRenderer3d.make glContext window :> Renderer3d
            //        OpenGL.Hl.Assert ()
            //
            //        // create 2d renderer
            //        let renderer2d = GlRenderer2d.make window :> Renderer2d
            //        OpenGL.Hl.Assert ()
            //
            //        // create imgui renderer
            //        let rendererImGui = GlRendererImGui.make fonts :> RendererImGui
            //        OpenGL.Hl.Assert ()
            //
            //        // fin
            //        renderersOpt <- Some (renderer3d, renderer2d, rendererImGui)
            //
            //    // create stub renderers
            //    | None ->
            //        let renderer3d = StubRenderer3d.make () :> Renderer3d
            //        let renderer2d = StubRenderer2d.make () :> Renderer2d
            //        let rendererImGui = StubRendererImGui.make fonts :> RendererImGui
            //        renderersOpt <- Some (renderer3d, renderer2d, rendererImGui)
            //        OpenGL.Hl.Assert ()
            //
            //    // fin
            //    started <- true
            //
            //// fail on already created
            //| Some _ -> raise (InvalidOperationException "Redundant Start calls.")

        member this.Renderer3dConfig =
            match renderersOpt with
            | Some (renderer3d, _, _) -> renderer3d.RendererConfig
            | None -> Renderer3dConfig.defaultConfig

        member this.EnqueueMessage3d message =
            match renderersOpt with
            | Some _ -> ()//messages3d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderStaticModelFast (absolute, modelMatrix, presence, insetOpt, materialProperties, staticModel, renderType, renderPass) =
            match renderersOpt with
            | Some _ -> ()//messages3d.Add (RenderStaticModel { Absolute = absolute; ModelMatrix = modelMatrix; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; StaticModel = staticModel; RenderType = renderType; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderStaticModelSurfaceFast (absolute, modelMatrix, presence, insetOpt, materialProperties, material, staticModel, surfaceIndex, renderType, renderPass) =
            match renderersOpt with
            | Some _ -> ()//messages3d.Add (RenderStaticModelSurface { Absolute = absolute; ModelMatrix = modelMatrix; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; Material = material; StaticModel = staticModel; SurfaceIndex = surfaceIndex; RenderType = renderType; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderAnimatedModelFast (absolute, modelMatrix, presence, insetOpt, materialProperties, boneTransforms, animatedModel, renderPass) =
            match renderersOpt with
            | Some _ -> ()//messages3d.Add (RenderAnimatedModel { Absolute = absolute; ModelMatrix = modelMatrix; Presence = presence; InsetOpt = Option.ofValueOption insetOpt; MaterialProperties = materialProperties; BoneTransforms = boneTransforms; AnimatedModel = animatedModel; RenderPass = renderPass })
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.EnqueueMessage2d message =
            match renderersOpt with
            | Some _ -> ()//messages2d.Add message 
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.RenderLayeredSpriteFast (elevation, horizon, assetTag, transform, insetOpt, image, color, blend, emission, flip) =
            match renderersOpt with
            | Some _ -> ()//messages2d.Add (LayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = assetTag; RenderOperation2d = RenderSprite { Transform = transform; InsetOpt = insetOpt; Image = image; Color = color; Blend = blend; Emission = emission; Flip = flip }})
            | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")

        member this.ClearMessages () =
            messages3d.Clear ()
            messages2d.Clear ()

        member this.SubmitMessages frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye2dCenter eye2dSize windowSize drawData =
            ()
    //        match renderersOpt with
    //        | Some (renderer3d, renderer2d, rendererImGui) ->
    //            
    //            // begin frame
    //            OpenGL.Hl.BeginFrame (Constants.Render.ViewportOffset windowSize, windowSize)
    //            OpenGL.Hl.Assert ()
    //
    //            // render 3d
    //            renderer3d.Render frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation windowSize messages3d
    //            messages3d.Clear ()
    //            OpenGL.Hl.Assert ()
    //
    //            // render 2d
    //            renderer2d.Render eye2dCenter eye2dSize windowSize messages2d
    //            messages2d.Clear ()
    //            OpenGL.Hl.Assert ()
    //
    //            // render imgui
    //            rendererImGui.Render drawData
    //            OpenGL.Hl.Assert ()
    //
    //            // end frame
    //            OpenGL.Hl.EndFrame ()
    //            OpenGL.Hl.Assert ()
    //
    //        | None -> raise (InvalidOperationException "Renderers are not yet or are no longer valid.")
            
        member this.Swap () =
            ()
    //        match windowOpt with
    //        | Some window ->
    //            match window with
    //            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
    //        | None -> ()
            
        member this.Terminate () =
            ()
    //        match renderersOpt with
    //        | Some (renderer3d, renderer2d, rendererImGui) ->
    //            renderer3d.CleanUp ()
    //            renderer2d.CleanUp ()
    //            rendererImGui.CleanUp ()
    //            renderersOpt <- None
    //            terminated <- true
    //        | None -> raise (InvalidOperationException "Redundant Terminate calls.")
            
/// A threaded render process.
type RendererThread () =

    let [<VolatileField>] mutable threadOpt = None
    let [<VolatileField>] mutable started = false
    let [<VolatileField>] mutable terminated = false
    let [<VolatileField>] mutable submissionOpt = Option<Frustum * Frustum * Frustum * Box3 * RenderMessage3d List * RenderMessage2d List * Vector3 * Quaternion * Vector2 * Vector2 * Vector2i * ImDrawDataPtr>.None
    let [<VolatileField>] mutable renderer3dConfig = Renderer3dConfig.defaultConfig
    let [<VolatileField>] mutable swap = false
    let [<VolatileField>] mutable messageBufferIndex = 0
    let messageBuffers3d = [|List (); List ()|]
    let messageBuffers2d = [|List (); List ()|]
    let cachedSpriteMessagesLock = obj ()
    let cachedSpriteMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedSpriteMessagesCapacity = Constants.Render.SpriteMessagesPrealloc
    let cachedStaticModelMessagesLock = obj ()
    let cachedStaticModelMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedStaticModelMessagesCapacity = Constants.Render.StaticModelMessagesPrealloc
    let cachedStaticModelSurfaceMessagesLock = obj ()
    let cachedStaticModelSurfaceMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedStaticModelSurfaceMessagesCapacity = Constants.Render.StaticModelSurfaceMessagesPrealloc
    let cachedAnimatedModelMessagesLock = obj ()
    let cachedAnimatedModelMessages = System.Collections.Generic.Queue ()
    let [<VolatileField>] mutable cachedAnimatedModelMessagesCapacity = Constants.Render.AnimatedModelMessagesPrealloc

    let allocStaticModelMessage () =
        lock cachedStaticModelMessagesLock (fun () ->
            if cachedStaticModelMessages.Count = 0 then
                for _ in 0 .. dec cachedStaticModelMessagesCapacity do
                    let staticModelDescriptor =
                        { CachedStaticModelAbsolute = Unchecked.defaultof<_>
                          CachedStaticModelMatrix = Unchecked.defaultof<_>
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
                        { CachedStaticModelSurfaceAbsolute = Unchecked.defaultof<_>
                          CachedStaticModelSurfaceMatrix = Unchecked.defaultof<_>
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
                        { CachedAnimatedModelAbsolute = Unchecked.defaultof<_>
                          CachedAnimatedModelMatrix = Unchecked.defaultof<_>
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

    member private this.Run fonts windowOpt =

        // core vulkan handles
        let mutable instance = Unchecked.defaultof<VkInstance>
        let mutable physicalDevice = Unchecked.defaultof<VkPhysicalDevice>
        let mutable graphicsQueueFamily = 0u
        let mutable presentQueueFamily = 0u
        let mutable device = Unchecked.defaultof<VkDevice>
        let mutable graphicsQueue = Unchecked.defaultof<VkQueue>
        let mutable presentQueue = Unchecked.defaultof<VkQueue>
        let mutable surface = Unchecked.defaultof<VkSurfaceKHR>
        let mutable swapChain = Unchecked.defaultof<VkSwapchainKHR>
        let mutable swapChainExtent = Unchecked.defaultof<VkExtent2D>
        let mutable swapChainImageFormat = Unchecked.defaultof<VkFormat>
        let mutable swapChainImages = Array.empty<VkImage>
        let mutable swapChainImageViews = Array.empty<VkImageView>
        let mutable renderPass = Unchecked.defaultof<VkRenderPass>
        let mutable swapChainFramebuffers = Array.empty<VkFramebuffer>
        let mutable commandPool = Unchecked.defaultof<VkCommandPool>
        let mutable commandBuffer = Unchecked.defaultof<VkCommandBuffer>
        let mutable imageAvailableSemaphore = Unchecked.defaultof<VkSemaphore>
        let mutable renderFinishedSemaphore = Unchecked.defaultof<VkSemaphore>
        let mutable inFlightFence = Unchecked.defaultof<VkFence>
        
        // create renderers
        let (renderer3d, renderer2d, rendererImGui) =
            match windowOpt with
            | Some window ->

                let window = match window with SglWindow window -> window.SglWindow

                // loads vulkan. NOTE: this is not an actual vulkan function. wrapper features prefixed like this are misleading and should be avoided where practical.
                let result = vkInitialize ()

                do
                    // get available instance layers
                    let mutable layerCount = 0u
                    let result = vkEnumerateInstanceLayerProperties (Interop.AsPointer &layerCount, NativePtr.nullPtr)
                    let mutable layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
                    use layersHnd = layers.AsMemory().Pin()
                    let layersNptr = NativePtr.ofVoidPtr<VkLayerProperties> layersHnd.Pointer
                    let result = vkEnumerateInstanceLayerProperties (Interop.AsPointer &layerCount, layersNptr)

                    // check if validation layer exists
                    let validationLayer = "VK_LAYER_KHRONOS_validation"
                    let validationLayerExists = Array.exists (fun (x : VkLayerProperties) -> x.GetLayerName () = validationLayer) layers
                    if not validationLayerExists then printfn "Could not find %s. Note for Bryan: this is to be expected as you presumably have not installed the vulkan sdk. I would like you to keep the sdk uninstalled as long as possible so we can see what the difference is. From what I understand, windows users may be able to get by without it, linux not so much. Validation should only matter to nu users if they write their own vulkan code. \n" validationLayer

                    // get validation layer
                    // TODO: maybe try setting up message callback later?
                    use _ = validationLayer.AsMemory().Pin()
                    let vlayerPointer = validationLayer.GetUtf8Span().GetPointer()
                    let vlayerArray = [|vlayerPointer|]
                    use vlayerArrayHnd = vlayerArray.AsMemory().Pin()
                    let vlayerArrayNptr = NativePtr.ofVoidPtr<nativeptr<sbyte>> vlayerArrayHnd.Pointer

                    // get sdl extensions
                    let mutable sdlExtensionCount = 0u
                    let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, null)
                    let sdlExtensionsOut = Array.zeroCreate<nativeint> (int sdlExtensionCount)
                    use _ = sdlExtensionsOut.AsMemory().Pin()
                    let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, sdlExtensionsOut)
                    let sdlExtensions = Array.zeroCreate<nativeptr<sbyte>> (int sdlExtensionCount)
                    for i in [0 .. dec (int sdlExtensionCount)] do sdlExtensions[i] <- NativePtr.ofNativeInt<sbyte> sdlExtensionsOut[i]
                    use sdlExtensionsHnd = sdlExtensions.AsMemory().Pin()
                    let sdlExtensionsNptr = NativePtr.ofVoidPtr<nativeptr<sbyte>> sdlExtensionsHnd.Pointer

                    // print sdl extensions
                    printfn "SDL extensions:"
                    for i in [0 .. dec (int sdlExtensionCount)] do
                        let span = Interop.GetUtf8Span sdlExtensions[i]
                        printfn "%s" (span.GetString ())

                    // populate createinstance info
                    let mutable instanceCreateInfo = VkInstanceCreateInfo ()
                    instanceCreateInfo.enabledExtensionCount <- sdlExtensionCount
                    instanceCreateInfo.ppEnabledExtensionNames <- sdlExtensionsNptr
                
                    if validationLayerExists then
                        instanceCreateInfo.enabledLayerCount <- 1u
                        instanceCreateInfo.ppEnabledLayerNames <- vlayerArrayNptr
                    
                    // create vulkan instance
                    let result = vkCreateInstance (Interop.AsPointer &instanceCreateInfo, NativePtr.nullPtr, &instance)
                    printfn "vkCreateInstance returned %s." (result.ToString ())

                    ()

                // another wrapper only function; loads instance commands
                vkLoadInstanceOnly instance

                // get surface from sdl
                let result = SDL.SDL_Vulkan_CreateSurface (window, instance, &(Interop.As<VkSurfaceKHR, uint64> &surface))
                printfn "SDL_Vulkan_CreateSurface returned %s." (result.ToString ())

                do
                    // get available physical devices
                    let mutable deviceCount = 0u
                    let result = vkEnumeratePhysicalDevices (instance, Interop.AsPointer &deviceCount, NativePtr.nullPtr)
                    printfn "Vulkan found %i physical devices on your computer." deviceCount
                    let mutable devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
                    use devicesHnd = devices.AsMemory().Pin()
                    let devicesNptr = NativePtr.ofVoidPtr<VkPhysicalDevice> devicesHnd.Pointer
                    let result = vkEnumeratePhysicalDevices (instance, Interop.AsPointer &deviceCount, devicesNptr)

                    // select physical device
                    physicalDevice <- devices[0]
                    let mutable physicalDeviceProperties = Unchecked.defaultof<VkPhysicalDeviceProperties>
                    vkGetPhysicalDeviceProperties (physicalDevice, &physicalDeviceProperties)
                    printfn "Using physical device %s." (physicalDeviceProperties.GetDeviceName ())

                    ()

                do
                    // get queue families
                    let mutable queueFamilyCount = 0u
                    let result = vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, Interop.AsPointer &queueFamilyCount, NativePtr.nullPtr)
                    let mutable queueFamilies = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
                    use queueFamiliesHnd = queueFamilies.AsMemory().Pin()
                    let queueFamiliesNptr = NativePtr.ofVoidPtr<VkQueueFamilyProperties> queueFamiliesHnd.Pointer
                    let result = vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, Interop.AsPointer &queueFamilyCount, queueFamiliesNptr)
                    
                    for i in [0 .. dec (int queueFamilyCount)] do
                        if queueFamilies[i].queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then graphicsQueueFamily <- uint i
                        let mutable presentSupport = VkBool32.False
                        let result = vkGetPhysicalDeviceSurfaceSupportKHR (physicalDevice, uint i, surface, &presentSupport)
                        if (presentSupport = VkBool32.True) then presentQueueFamily <- uint i
                    
                    ()
                
                do
                    // get unique queue family array
                    let uniqueQueueFamiliesSet = new HashSet<uint> ()
                    uniqueQueueFamiliesSet.Add graphicsQueueFamily
                    uniqueQueueFamiliesSet.Add presentQueueFamily
                    let uniqueQueueFamilies = Array.zeroCreate<uint> uniqueQueueFamiliesSet.Count
                    uniqueQueueFamiliesSet.CopyTo (uniqueQueueFamilies)
                    
                    // populate queue create infos
                    let mutable queuePriority = 1.0f
                    let queueCreateInfos = Array.zeroCreate<VkDeviceQueueCreateInfo> uniqueQueueFamilies.Length
                    use queueCreateInfosHnd = queueCreateInfos.AsMemory().Pin()
                    let queueCreateInfosNptr = NativePtr.ofVoidPtr<VkDeviceQueueCreateInfo> queueCreateInfosHnd.Pointer

                    for i in [0 .. dec (uniqueQueueFamilies.Length)] do
                        let mutable queueCreateInfo = VkDeviceQueueCreateInfo ()
                        queueCreateInfo.queueFamilyIndex <- uniqueQueueFamilies[i]
                        queueCreateInfo.queueCount <- 1u
                        queueCreateInfo.pQueuePriorities <- Interop.AsPointer &queuePriority
                        queueCreateInfos[i] <- queueCreateInfo
                    
                    // get swapchain extension
                    let swapChainName = VK_KHR_SWAPCHAIN_EXTENSION_NAME
                    use _ = swapChainName.AsMemory().Pin()
                    let swapChainNamePtr = swapChainName.GetUtf8Span().GetPointer()
                    let extensionArray = [|swapChainNamePtr|]
                    use extensionArrayHnd = extensionArray.AsMemory().Pin()
                    let extensionArrayNptr = NativePtr.ofVoidPtr<nativeptr<sbyte>> extensionArrayHnd.Pointer
                    
                    // populate createdevice info
                    let mutable deviceFeatures = VkPhysicalDeviceFeatures ()
                    let mutable deviceCreateInfo = VkDeviceCreateInfo ()
                    deviceCreateInfo.pQueueCreateInfos <- queueCreateInfosNptr
                    deviceCreateInfo.queueCreateInfoCount <- uint queueCreateInfos.Length
                    deviceCreateInfo.pEnabledFeatures <- Interop.AsPointer &deviceFeatures
                    deviceCreateInfo.enabledExtensionCount <- 1u
                    deviceCreateInfo.ppEnabledExtensionNames <- extensionArrayNptr

                    // create logical device
                    let result = vkCreateDevice (physicalDevice, Interop.AsPointer &deviceCreateInfo, NativePtr.nullPtr, &device)
                    printfn "vkCreateDevice returned %s." (result.ToString ())

                    ()

                // like load instance; vulkan should be fully loaded now!
                vkLoadDevice device
                
                // get queue handles
                let result = vkGetDeviceQueue (device, graphicsQueueFamily, 0u, &graphicsQueue)
                let result = vkGetDeviceQueue (device, presentQueueFamily, 0u, &presentQueue)

                // get surface capabilities
                let mutable surfaceCapabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
                let result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR (physicalDevice, surface, &surfaceCapabilities)
                
                // set swapchain extent and format
                swapChainExtent <- surfaceCapabilities.currentExtent
                swapChainImageFormat <- VK_FORMAT_B8G8R8A8_SRGB
                
                do
                    // populate create swapchain info
                    let mutable swapChainCreateInfo = VkSwapchainCreateInfoKHR ()
                    swapChainCreateInfo.surface <- surface
                    swapChainCreateInfo.minImageCount <- surfaceCapabilities.minImageCount
                    printfn "min swapchain images: %i" surfaceCapabilities.minImageCount
                    swapChainCreateInfo.imageFormat <- swapChainImageFormat
                    swapChainCreateInfo.imageColorSpace <- VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
                    swapChainCreateInfo.imageExtent <- swapChainExtent
                    swapChainCreateInfo.imageArrayLayers <- 1u
                    swapChainCreateInfo.imageUsage <- VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT

                    let indicesArray = [|graphicsQueueFamily; presentQueueFamily|]
                    use indicesArrayHnd = indicesArray.AsMemory().Pin()
                    let indicesArrayNptr = NativePtr.ofVoidPtr<uint32> indicesArrayHnd.Pointer

                    if (graphicsQueueFamily = presentQueueFamily) then
                        swapChainCreateInfo.imageSharingMode <- VK_SHARING_MODE_EXCLUSIVE
                        swapChainCreateInfo.queueFamilyIndexCount <- 0u
                        swapChainCreateInfo.pQueueFamilyIndices <- NativePtr.nullPtr
                    else
                        swapChainCreateInfo.imageSharingMode <- VK_SHARING_MODE_CONCURRENT
                        swapChainCreateInfo.queueFamilyIndexCount <- 2u
                        swapChainCreateInfo.pQueueFamilyIndices <- indicesArrayNptr

                    swapChainCreateInfo.preTransform <- surfaceCapabilities.currentTransform
                    swapChainCreateInfo.compositeAlpha <- VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                    swapChainCreateInfo.presentMode <- VK_PRESENT_MODE_FIFO_KHR
                    swapChainCreateInfo.clipped <- VkBool32.True

                    // create swapchain
                    let result = vkCreateSwapchainKHR (device, Interop.AsPointer &swapChainCreateInfo, NativePtr.nullPtr, &swapChain)
                    printfn "vkCreateSwapchainKHR returned %s." (result.ToString ())

                do
                    // get swapchain images
                    let mutable swapChainImageCount = 0u
                    let result = vkGetSwapchainImagesKHR (device, swapChain, Interop.AsPointer &swapChainImageCount, NativePtr.nullPtr)
                    Array.Resize<VkImage> (&swapChainImages, int swapChainImageCount)
                    use swapChainImagesHnd = swapChainImages.AsMemory().Pin()
                    let swapChainImagesNptr = NativePtr.ofVoidPtr<VkImage> swapChainImagesHnd.Pointer
                    let result = vkGetSwapchainImagesKHR (device, swapChain, Interop.AsPointer &swapChainImageCount, swapChainImagesNptr)
                    printfn "vkGetSwapchainImagesKHR returned %s." (result.ToString ())

                    ()

                do
                    // setup swapchain image views
                    Array.Resize<VkImageView> (&swapChainImageViews, swapChainImages.Length)
                    use _ = swapChainImageViews.AsMemory().Pin()

                    for i in [0 .. dec (swapChainImageViews.Length)] do
                        let mutable imageViewCreateInfo = VkImageViewCreateInfo ()
                        imageViewCreateInfo.image <- swapChainImages[i]
                        imageViewCreateInfo.viewType <- VK_IMAGE_VIEW_TYPE_2D
                        imageViewCreateInfo.format <- swapChainImageFormat
                        imageViewCreateInfo.components.r <- VK_COMPONENT_SWIZZLE_IDENTITY
                        imageViewCreateInfo.components.g <- VK_COMPONENT_SWIZZLE_IDENTITY
                        imageViewCreateInfo.components.b <- VK_COMPONENT_SWIZZLE_IDENTITY
                        imageViewCreateInfo.components.a <- VK_COMPONENT_SWIZZLE_IDENTITY
                        imageViewCreateInfo.subresourceRange.aspectMask <- VK_IMAGE_ASPECT_COLOR_BIT
                        imageViewCreateInfo.subresourceRange.baseMipLevel <- 0u
                        imageViewCreateInfo.subresourceRange.levelCount <- 1u
                        imageViewCreateInfo.subresourceRange.baseArrayLayer <- 0u
                        imageViewCreateInfo.subresourceRange.layerCount <- 1u

                        let result = vkCreateImageView (device, Interop.AsPointer &imageViewCreateInfo, NativePtr.nullPtr, &swapChainImageViews[i])
                        printfn "vkCreateImageView returned %s." (result.ToString ())

                    ()

                // setup renderpass
                let mutable colorAttachment = VkAttachmentDescription ()
                colorAttachment.format <- swapChainImageFormat
                colorAttachment.samples <- VK_SAMPLE_COUNT_1_BIT
                colorAttachment.loadOp <- VK_ATTACHMENT_LOAD_OP_CLEAR
                colorAttachment.storeOp <- VK_ATTACHMENT_STORE_OP_STORE
                colorAttachment.stencilLoadOp <- VK_ATTACHMENT_LOAD_OP_DONT_CARE
                colorAttachment.stencilStoreOp <- VK_ATTACHMENT_STORE_OP_DONT_CARE
                colorAttachment.initialLayout <- VK_IMAGE_LAYOUT_UNDEFINED
                colorAttachment.finalLayout <- VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

                let mutable colorAttachmentRef = VkAttachmentReference ()
                colorAttachmentRef.attachment <- 0u
                colorAttachmentRef.layout <- VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

                let mutable subpass = VkSubpassDescription ()
                subpass.pipelineBindPoint <- VK_PIPELINE_BIND_POINT_GRAPHICS
                subpass.colorAttachmentCount <- 1u
                subpass.pColorAttachments <- Interop.AsPointer &colorAttachmentRef

                let mutable dependency = VkSubpassDependency ()
                dependency.srcSubpass <- VK_SUBPASS_EXTERNAL
                dependency.dstSubpass <- 0u
                dependency.srcStageMask <- VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                dependency.srcAccessMask <- VK_ACCESS_NONE
                dependency.dstStageMask <- VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                dependency.dstAccessMask <- VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT

                let mutable renderPassInfo = VkRenderPassCreateInfo ()
                renderPassInfo.attachmentCount <- 1u
                renderPassInfo.pAttachments <- Interop.AsPointer &colorAttachment
                renderPassInfo.subpassCount <- 1u
                renderPassInfo.pSubpasses <- Interop.AsPointer &subpass
                renderPassInfo.dependencyCount <- 1u
                renderPassInfo.pDependencies <- Interop.AsPointer &dependency

                let result = vkCreateRenderPass (device, Interop.AsPointer &renderPassInfo, NativePtr.nullPtr, &renderPass)
                printfn "vkCreateRenderPass returned %s." (result.ToString ())

                do
                    // setup swapchain framebuffers
                    Array.Resize<VkFramebuffer> (&swapChainFramebuffers, swapChainImageViews.Length)
                    use _ = swapChainFramebuffers.AsMemory().Pin()

                    for i in [0 .. dec (swapChainImageViews.Length)] do
                        let mutable imageView = swapChainImageViews[i]
                        let mutable framebufferInfo = VkFramebufferCreateInfo ()
                        framebufferInfo.renderPass <- renderPass
                        framebufferInfo.attachmentCount <- 1u
                        framebufferInfo.pAttachments <- Interop.AsPointer &imageView
                        framebufferInfo.width <- swapChainExtent.width
                        framebufferInfo.height <- swapChainExtent.height
                        framebufferInfo.layers <- 1u

                        let result = vkCreateFramebuffer (device, Interop.AsPointer &framebufferInfo, NativePtr.nullPtr, &swapChainFramebuffers[i])
                        printfn "vkCreateFramebuffer returned %s." (result.ToString ())

                    ()

                // setup command buffer
                let mutable poolInfo = VkCommandPoolCreateInfo ()
                poolInfo.flags <- VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                poolInfo.queueFamilyIndex <- graphicsQueueFamily

                let result = vkCreateCommandPool (device, Interop.AsPointer &poolInfo, NativePtr.nullPtr, &commandPool)
                printfn "vkCreateCommandPool returned %s." (result.ToString ())

                let mutable allocInfo = VkCommandBufferAllocateInfo ()
                allocInfo.commandPool <- commandPool
                allocInfo.level <- VK_COMMAND_BUFFER_LEVEL_PRIMARY
                allocInfo.commandBufferCount <- 1u

                let result = vkAllocateCommandBuffers (device, Interop.AsPointer &allocInfo, Interop.AsPointer &commandBuffer)
                printfn "vkAllocateCommandBuffers returned %s." (result.ToString ())

                // setup synchronization primatives
                let mutable semaphoreInfo = VkSemaphoreCreateInfo ()
                let mutable fenceInfo = VkFenceCreateInfo ()
                fenceInfo.flags <- VK_FENCE_CREATE_SIGNALED_BIT
                let result = vkCreateSemaphore (device, Interop.AsPointer &semaphoreInfo, NativePtr.nullPtr, &imageAvailableSemaphore)
                let result = vkCreateSemaphore (device, Interop.AsPointer &semaphoreInfo, NativePtr.nullPtr, &renderFinishedSemaphore)
                let result = vkCreateFence (device, Interop.AsPointer &fenceInfo, NativePtr.nullPtr, &inFlightFence)

                // create gl context
                //let glContext = match window with SglWindow window -> OpenGL.Hl.CreateSglContextInitial window.SglWindow
                OpenGL.Hl.Assert ()

                // initialize gl context
                //OpenGL.Hl.InitContext ()
                OpenGL.Hl.Assert ()

                // create 3d renderer
                //let renderer3d = GlRenderer3d.make glContext window :> Renderer3d
                let renderer3d = StubRenderer3d.make () :> Renderer3d
                OpenGL.Hl.Assert ()

                // create 2d renderer
                //let renderer2d = GlRenderer2d.make window :> Renderer2d
                let renderer2d = StubRenderer2d.make () :> Renderer2d
                OpenGL.Hl.Assert ()

                // create imgui renderer
                //let rendererImGui = GlRendererImGui.make fonts :> RendererImGui
                let rendererImGui = StubRendererImGui.make fonts :> RendererImGui

                // fin
                (renderer3d, renderer2d, rendererImGui)

            // create stub renderers
            | None ->
                let renderer3d = StubRenderer3d.make () :> Renderer3d
                let renderer2d = StubRenderer2d.make () :> Renderer2d
                let rendererImGui = StubRendererImGui.make fonts :> RendererImGui
                (renderer3d, renderer2d, rendererImGui)

        // mark as started
        started <- true

        // loop until terminated
        while not terminated do

            // loop until submission exists
            while Option.isNone submissionOpt && not terminated do Thread.Sleep 1

            // guard against early termination
            if not terminated then

                // receie submission
                let (frustumInterior, frustumExterior, frustumImposter, lightBox, messages3d, messages2d, eye3dCenter, eye3dRotation, eye2dCenter, eye2dSize, windowSize, drawData) = Option.get submissionOpt
                submissionOpt <- None
                
                // wait for previous cycle to finish
                let result = vkWaitForFences (device, 1u, Interop.AsPointer &inFlightFence, VkBool32.True, UInt64.MaxValue)
                let result = vkResetFences (device, 1u, Interop.AsPointer &inFlightFence)

                // acquire image from swapchain to draw onto
                let mutable imageIndex = 0u
                let result = vkAcquireNextImageKHR (device, swapChain, UInt64.MaxValue, imageAvailableSemaphore, inFlightFence, &imageIndex)

                // record command buffer
                let result = vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None)

                let mutable beginInfo = VkCommandBufferBeginInfo ()
                beginInfo.flags <- VkCommandBufferUsageFlags.None
                beginInfo.pInheritanceInfo <- NativePtr.nullPtr

                let result = vkBeginCommandBuffer (commandBuffer, Interop.AsPointer &beginInfo)

                let mutable clearColor = VkClearValue (1.0f, 1.0f, 1.0f, 1.0f)            
                let mutable renderPassInfo = VkRenderPassBeginInfo ()
                renderPassInfo.renderPass <- renderPass
                renderPassInfo.framebuffer <- swapChainFramebuffers[int imageIndex]
                renderPassInfo.renderArea.offset <- VkOffset2D.Zero
                renderPassInfo.renderArea.extent <- swapChainExtent
                renderPassInfo.clearValueCount <- 1u
                renderPassInfo.pClearValues <- Interop.AsPointer &clearColor

                vkCmdBeginRenderPass (commandBuffer, Interop.AsPointer &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE)
                vkCmdSetBlendConstants (commandBuffer, 1.0f, 1.0f, 1.0f, 1.0f)
                vkCmdEndRenderPass commandBuffer
                let result = vkEndCommandBuffer commandBuffer

                // submit command buffer
                let mutable flags = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                let mutable submitInfo = VkSubmitInfo ()
                submitInfo.waitSemaphoreCount <- 1u
                submitInfo.pWaitSemaphores <- Interop.AsPointer &imageAvailableSemaphore
                submitInfo.pWaitDstStageMask <- Interop.AsPointer &flags // may have to comment out
                submitInfo.commandBufferCount <- 1u
                submitInfo.pCommandBuffers <- Interop.AsPointer &commandBuffer
                submitInfo.signalSemaphoreCount <- 1u
                submitInfo.pSignalSemaphores <- Interop.AsPointer &renderFinishedSemaphore

                let result = vkQueueSubmit (graphicsQueue, 1u, Interop.AsPointer &submitInfo, inFlightFence)

                // present image back to swapchain to appear on screen
                let mutable presentInfo = VkPresentInfoKHR ()
                presentInfo.waitSemaphoreCount <- 1u
                presentInfo.pWaitSemaphores <- Interop.AsPointer &renderFinishedSemaphore
                presentInfo.swapchainCount <- 1u
                presentInfo.pSwapchains <- Interop.AsPointer &swapChain
                presentInfo.pImageIndices <- Interop.AsPointer &imageIndex
                presentInfo.pResults <- NativePtr.nullPtr

                let result = vkQueuePresentKHR (presentQueue, Interop.AsPointer &presentInfo)
                
                // begin frame
                //OpenGL.Hl.BeginFrame (Constants.Render.ViewportOffset windowSize, windowSize)
                OpenGL.Hl.Assert ()

                //// render 3d
                //renderer3d.Render frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation windowSize messages3d
                //freeStaticModelMessages messages3d
                //freeStaticModelSurfaceMessages messages3d
                //freeAnimatedModelMessages messages3d
                //renderer3dConfig <- renderer3d.RendererConfig
                //OpenGL.Hl.Assert ()
                //
                //// render 2d
                //renderer2d.Render eye2dCenter eye2dSize windowSize messages2d
                //freeSpriteMessages messages2d
                //OpenGL.Hl.Assert ()

                // render imgui
                rendererImGui.Render drawData
                OpenGL.Hl.Assert ()

                // end frame
                //OpenGL.Hl.EndFrame ()
                OpenGL.Hl.Assert ()

                // loop until swap is requested
                while not terminated && not swap do Thread.Sleep 1

                // guard against early termination
                if not terminated then

                    // attempt to swap
                    match windowOpt with
                    | Some window -> ()//match window with SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
                    | None -> ()

                    // complete swap request
                    swap <- false

        // clean up
        renderer2d.CleanUp ()

    interface RendererProcess with

        member this.Renderer3dConfig =
            renderer3dConfig        

        member this.Start fonts windowOpt =

            // validate state
            if Option.isSome threadOpt then raise (InvalidOperationException "Render process already started.")

            // start thread
            let thread = Thread (ThreadStart (fun () -> this.Run fonts windowOpt))
            threadOpt <- Some thread
            thread.IsBackground <- true
            thread.Start ()

            // wait for thread to finish starting
            while not started do Thread.Yield () |> ignore<bool>

        member this.EnqueueMessage3d message =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            ()
    //        match message with
    //        | RenderStaticModel rsm ->
    //            let cachedStaticModelMessage = allocStaticModelMessage ()
    //            match cachedStaticModelMessage with
    //            | RenderCachedStaticModel cachedMessage ->
    //                cachedMessage.CachedStaticModelAbsolute <- rsm.Absolute
    //                cachedMessage.CachedStaticModelMatrix <- rsm.ModelMatrix
    //                cachedMessage.CachedStaticModelPresence <- rsm.Presence
    //                cachedMessage.CachedStaticModelInsetOpt <- ValueOption.ofOption rsm.InsetOpt
    //                cachedMessage.CachedStaticModelMaterialProperties <- rsm.MaterialProperties
    //                cachedMessage.CachedStaticModel <- rsm.StaticModel
    //                cachedMessage.CachedStaticModelRenderType <- rsm.RenderType
    //                cachedMessage.CachedStaticModelRenderPass <- rsm.RenderPass
    //                messageBuffers3d.[messageBufferIndex].Add cachedStaticModelMessage
    //            | _ -> failwithumf ()
    //        | RenderStaticModelSurface rsms ->
    //            let cachedStaticModelSurfaceMessage = allocStaticModelSurfaceMessage ()
    //            match cachedStaticModelSurfaceMessage with
    //            | RenderCachedStaticModelSurface cachedMessage ->
    //                cachedMessage.CachedStaticModelSurfaceAbsolute <- rsms.Absolute
    //                cachedMessage.CachedStaticModelSurfaceMatrix <- rsms.ModelMatrix
    //                cachedMessage.CachedStaticModelSurfacePresence <- rsms.Presence
    //                cachedMessage.CachedStaticModelSurfaceInsetOpt <- ValueOption.ofOption rsms.InsetOpt
    //                cachedMessage.CachedStaticModelSurfaceMaterialProperties <- rsms.MaterialProperties
    //                cachedMessage.CachedStaticModelSurfaceMaterial <- rsms.Material
    //                cachedMessage.CachedStaticModelSurfaceModel <- rsms.StaticModel
    //                cachedMessage.CachedStaticModelSurfaceIndex <- rsms.SurfaceIndex
    //                cachedMessage.CachedStaticModelSurfaceRenderType <- rsms.RenderType
    //                cachedMessage.CachedStaticModelSurfaceRenderPass <- rsms.RenderPass
    //                messageBuffers3d.[messageBufferIndex].Add cachedStaticModelSurfaceMessage
    //            | _ -> failwithumf ()
    //        | RenderAnimatedModel ram ->
    //            let cachedAnimatedModelMessage = allocAnimatedModelMessage ()
    //            match cachedAnimatedModelMessage with
    //            | RenderCachedAnimatedModel cachedMessage ->
    //                cachedMessage.CachedAnimatedModelAbsolute <- ram.Absolute
    //                cachedMessage.CachedAnimatedModelMatrix <- ram.ModelMatrix
    //                cachedMessage.CachedAnimatedModelPresence <- ram.Presence
    //                cachedMessage.CachedAnimatedModelInsetOpt <- ValueOption.ofOption ram.InsetOpt
    //                cachedMessage.CachedAnimatedModelMaterialProperties <- ram.MaterialProperties
    //                cachedMessage.CachedAnimatedModelBoneTransforms <- ram.BoneTransforms
    //                cachedMessage.CachedAnimatedModel <- ram.AnimatedModel
    //                cachedMessage.CachedAnimatedModelRenderPass <- ram.RenderPass
    //                messageBuffers3d.[messageBufferIndex].Add cachedAnimatedModelMessage
    //            | _ -> failwithumf ()
    //        | _ -> messageBuffers3d.[messageBufferIndex].Add message

        member this.RenderStaticModelFast (absolute, modelMatrix, presence, insetOpt, materialProperties, staticModel, renderType, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            ()
    //        let cachedStaticModelMessage = allocStaticModelMessage ()
    //        match cachedStaticModelMessage with
    //        | RenderCachedStaticModel cachedMessage ->
    //            cachedMessage.CachedStaticModelAbsolute <- absolute
    //            cachedMessage.CachedStaticModelMatrix <- modelMatrix
    //            cachedMessage.CachedStaticModelPresence <- presence
    //            cachedMessage.CachedStaticModelInsetOpt <- insetOpt
    //            cachedMessage.CachedStaticModelMaterialProperties <- materialProperties
    //            cachedMessage.CachedStaticModel <- staticModel
    //            cachedMessage.CachedStaticModelRenderType <- renderType
    //            cachedMessage.CachedStaticModelRenderPass <- renderPass
    //            messageBuffers3d.[messageBufferIndex].Add cachedStaticModelMessage
    //        | _ -> failwithumf ()

        member this.RenderStaticModelSurfaceFast (absolute, modelMatrix, presence, insetOpt, materialProperties, material, staticModel, surfaceIndex, renderType, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            ()
    //        let cachedStaticModelSurfaceMessage = allocStaticModelSurfaceMessage ()
    //        match cachedStaticModelSurfaceMessage with
    //        | RenderCachedStaticModelSurface cachedMessage ->
    //            cachedMessage.CachedStaticModelSurfaceAbsolute <- absolute
    //            cachedMessage.CachedStaticModelSurfaceMatrix <- modelMatrix
    //            cachedMessage.CachedStaticModelSurfacePresence <- presence
    //            cachedMessage.CachedStaticModelSurfaceInsetOpt <- insetOpt
    //            cachedMessage.CachedStaticModelSurfaceMaterialProperties <- materialProperties
    //            cachedMessage.CachedStaticModelSurfaceMaterial <- material
    //            cachedMessage.CachedStaticModelSurfaceModel <- staticModel
    //            cachedMessage.CachedStaticModelSurfaceIndex <- surfaceIndex
    //            cachedMessage.CachedStaticModelSurfaceRenderType <- renderType
    //            cachedMessage.CachedStaticModelSurfaceRenderPass <- renderPass
    //            messageBuffers3d.[messageBufferIndex].Add cachedStaticModelSurfaceMessage
    //        | _ -> failwithumf ()

        member this.RenderAnimatedModelFast (absolute, modelMatrix, presence, insetOpt, materialProperties, boneTransforms, animatedModel, renderPass) =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            ()
    //        let cachedAnimatedModelMessage = allocAnimatedModelMessage ()
    //        match cachedAnimatedModelMessage with
    //        | RenderCachedAnimatedModel cachedMessage ->
    //            cachedMessage.CachedAnimatedModelAbsolute <- absolute
    //            cachedMessage.CachedAnimatedModelMatrix <- modelMatrix
    //            cachedMessage.CachedAnimatedModelPresence <- presence
    //            cachedMessage.CachedAnimatedModelInsetOpt <- insetOpt
    //            cachedMessage.CachedAnimatedModelMaterialProperties <- materialProperties
    //            cachedMessage.CachedAnimatedModelBoneTransforms <- boneTransforms
    //            cachedMessage.CachedAnimatedModel <- animatedModel
    //            cachedMessage.CachedAnimatedModelRenderPass <- renderPass
    //            messageBuffers3d.[messageBufferIndex].Add cachedAnimatedModelMessage
    //        | _ -> failwithumf ()

        member this.EnqueueMessage2d message =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            ()
    //        match message with
    //        | LayeredOperation2d operation ->
    //            match operation.RenderOperation2d with
    //            | RenderSprite sprite ->
    //                let cachedSpriteMessage = allocSpriteMessage ()
    //                match cachedSpriteMessage with
    //                | LayeredOperation2d cachedOperation ->
    //                    match cachedOperation.RenderOperation2d with
    //                    | RenderCachedSprite descriptor ->
    //                        cachedOperation.Elevation <- operation.Elevation
    //                        cachedOperation.Horizon <- operation.Horizon
    //                        cachedOperation.AssetTag <- operation.AssetTag
    //                        descriptor.CachedSprite.Transform <- sprite.Transform
    //                        descriptor.CachedSprite.InsetOpt <- sprite.InsetOpt
    //                        descriptor.CachedSprite.Image <- sprite.Image
    //                        descriptor.CachedSprite.Color <- sprite.Color
    //                        descriptor.CachedSprite.Blend <- sprite.Blend
    //                        descriptor.CachedSprite.Emission <- sprite.Emission
    //                        descriptor.CachedSprite.Flip <- sprite.Flip
    //                        messageBuffers2d.[messageBufferIndex].Add cachedSpriteMessage 
    //                    | _ -> failwithumf ()
    //                | _ -> failwithumf ()
    //            | _ -> messageBuffers2d.[messageBufferIndex].Add message
    //        | _ -> messageBuffers2d.[messageBufferIndex].Add message

        member this.RenderLayeredSpriteFast (elevation, horizon, assetTag, transform, insetOpt, image, color, blend, emission, flip) =
            ()
    //        let cachedSpriteMessage = allocSpriteMessage ()
    //        match cachedSpriteMessage with
    //        | LayeredOperation2d cachedOperation ->
    //            match cachedOperation.RenderOperation2d with
    //            | RenderCachedSprite descriptor ->
    //                cachedOperation.Elevation <- elevation
    //                cachedOperation.Horizon <- horizon
    //                cachedOperation.AssetTag <- assetTag
    //                descriptor.CachedSprite.Transform <- transform
    //                descriptor.CachedSprite.InsetOpt <- insetOpt
    //                descriptor.CachedSprite.Image <- image
    //                descriptor.CachedSprite.Color <- color
    //                descriptor.CachedSprite.Blend <- blend
    //                descriptor.CachedSprite.Emission <- emission
    //                descriptor.CachedSprite.Flip <- flip
    //                messageBuffers2d.[messageBufferIndex].Add cachedSpriteMessage 
    //            | _ -> failwithumf ()
    //        | _ -> failwithumf ()

        member this.ClearMessages () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()

        member this.SubmitMessages frustumInterior frustumExterior frustumImposter lightBox eye3dCenter eye3dRotation eye2dCenter eye2dSize eyeMargin drawData =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let messages3d = messageBuffers3d.[messageBufferIndex]
            let messages2d = messageBuffers2d.[messageBufferIndex]
            messageBufferIndex <- if messageBufferIndex = 0 then 1 else 0
            messageBuffers3d.[messageBufferIndex].Clear ()
            messageBuffers2d.[messageBufferIndex].Clear ()
            submissionOpt <- Some (frustumInterior, frustumExterior, frustumImposter, lightBox, messages3d, messages2d, eye3dCenter, eye3dRotation, eye2dCenter, eye2dSize, eyeMargin, drawData)

        member this.Swap () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            swap <- true
            while swap do Thread.Sleep 1

        member this.Terminate () =
            if Option.isNone threadOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let thread = Option.get threadOpt
            if terminated then raise (InvalidOperationException "Redundant Terminate calls.")
            terminated <- true
            thread.Join ()
            threadOpt <- None