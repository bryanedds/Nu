// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan

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

        /// Create a Pipeline.
        static member create shaderPath pushConstants layoutBindings vulkanGlobal =
            

            ()