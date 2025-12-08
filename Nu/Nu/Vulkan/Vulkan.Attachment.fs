// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    /// Create general-purpose color attachment with optional linear filters.
    let CreateColorAttachment (resolutionX, resolutionY, filtered) =
        
        // TODO: DJL: create attachment texture with format rgba16f (as well as fallback format mechanism)
        // once VulkanTexture has been appropriately modified for frames in flight aware writing.

        ()