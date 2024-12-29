// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module Pipeline =

    /// An abstraction of a rendering pipeline.
    type Pipeline =
        { Pipeline : VkPipeline
          DescriptorPool : VkDescriptorPool
          DescriptorSet : VkDescriptorSet
          PipelineLayout : VkPipelineLayout
          DescriptorSetLayout : VkDescriptorSetLayout
          mutable VertexBuffer : Hl.AllocatedBuffer
          mutable IndexBuffer : Hl.AllocatedBuffer }

        /// Create the descriptor set layout.
        static member private createDescriptorSetLayout layoutBindings device =
            let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
            use layoutBindingsPin = new ArrayPin<_> (layoutBindings)
            let mutable info = VkDescriptorSetLayoutCreateInfo ()
            info.bindingCount <- uint layoutBindings.Length
            info.pBindings <- layoutBindingsPin.Pointer
            Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
            descriptorSetLayout

        /// Create the pipeline layout.
        static member private createPipelineLayout descriptorSetLayout pushConstantRanges device =
            let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
            let mutable descriptorSetLayout = descriptorSetLayout
            use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
            let mutable info = VkPipelineLayoutCreateInfo ()
            info.setLayoutCount <- 1u
            info.pSetLayouts <- asPointer &descriptorSetLayout
            info.pushConstantRangeCount <- uint pushConstantRanges.Length
            info.pPushConstantRanges <- pushConstantRangesPin.Pointer
            Vulkan.vkCreatePipelineLayout (device, &info, nullPtr, &pipelineLayout) |> Hl.check
            pipelineLayout
        
        /// Create the descriptor pool.
        static member private createDescriptorPool (layoutBindings : VkDescriptorSetLayoutBinding array) device =
            
            // handle
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            
            // derive pool sizes from layout bindings
            let poolSizes = Array.zeroCreate<VkDescriptorPoolSize> layoutBindings.Length
            use poolSizesPin = new ArrayPin<_> (poolSizes)
            for i in [0 .. dec layoutBindings.Length] do
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- layoutBindings.[i].descriptorType
                poolSize.descriptorCount <- layoutBindings.[i].descriptorCount
                poolSizes.[i] <- poolSize
            
            // create descriptor pool
            let mutable info = VkDescriptorPoolCreateInfo ()
            info.maxSets <- 1u
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
            descriptorPool

        /// Create the descriptor set.
        static member private createDescriptorSet descriptorSetLayout descriptorPool device =
            let mutable descriptorSet = Unchecked.defaultof<VkDescriptorSet>
            let mutable descriptorSetLayout = descriptorSetLayout
            let mutable info = VkDescriptorSetAllocateInfo ()
            info.descriptorPool <- descriptorPool
            info.descriptorSetCount <- 1u
            info.pSetLayouts <- asPointer &descriptorSetLayout
            Vulkan.vkAllocateDescriptorSets (device, asPointer &info, asPointer &descriptorSet) |> Hl.check
            descriptorSet
        
        /// Create a Pipeline.
        static member create shaderPath pushConstantRanges layoutBindings (vulkanGlobal : Hl.VulkanGlobal) =
            
            
            let device = vulkanGlobal.Device
            
            
            let descriptorSetLayout = Pipeline.createDescriptorSetLayout layoutBindings device
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayout pushConstantRanges device
            let descriptorPool = Pipeline.createDescriptorPool layoutBindings device
            let descriptorSet = Pipeline.createDescriptorSet descriptorSetLayout descriptorPool device


            ()