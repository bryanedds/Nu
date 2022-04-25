// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Nu

// 3d rendering implemented with OpenGL.
// Deferred, PBR, SSAO (or some variant such as SSDO), Variance Shadow Map (Non-Cascading for now), w/ FSAA.
// In addition, I'll try to implement spheroidal reflection probes. Potentially light probes as well.
// Not sure if I'll also implement a full scene mirror render pass for large water body reflection as that's quite expensive.
// Additionally, there appears to be a bias constant that can be used with VSMs to fix up light leaks, so consider that.

/// Describes the material of a 3d surface.
type Material =
    interface // same derived type indicates two materials can potentially be batched
        inherit IComparable<Material> // 0 indicates two materials can be drawn in the same batch with the same parameters
        abstract Bounds : Box3 // allows for sorting of translucent surfaces
        abstract Translucent : bool // can affect order in which materials are drawn such as in deferred rendering and may disallow batching
        abstract RenderMany : Material seq -> unit // does actual batched opengl calls
        end

/// A message to the 3d renderer.
type [<NoEquality; NoComparison>] RenderMessage3d =
    | MaterialDescriptor of Material
    | MaterialsDescriptor of Material array
    | RenderCallback3d of (Matrix4x4 * Matrix4x4 * Vector3 * Vector3 * Vector3 * Renderer -> unit)
    | RenderPass of (Material array * Matrix4x4 * Matrix4x4 * Vector3 * Vector3 * Vector3 * Renderer -> unit)