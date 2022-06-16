// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

// 3d rendering notes:
// I may borrow some sky dome code from here or related - https://github.com/shff/opengl_sky/blob/master/main.mm
// There appears to be a bias constant that can be used with VSMs to fix up light leaks, so consider that.

/// Renders a material of 3d surfaces.
type RenderMaterial =
    interface // same derived type indicates two materials can potentially be batched
        inherit IComparable // CompareTo of 0 indicates two materials can be drawn in the same batch with the same parameters
        abstract Bounds : Box3 // allows for z-sorting of translucent surfaces
        abstract Transparent : bool // can affect order in which materials are drawn such as in deferred rendering and may disallow batching
        abstract RenderMany : RenderMaterial array * Matrix4x4 byref * Matrix4x4 byref * Vector3 * Vector3 * Vector3 * Renderer -> unit // does actual batched opengl calls
        end

/// A collection of render materials in a pass.
type RenderMaterials =
    { RenderMaterialsOpaque : Map<RenderMaterial, RenderMaterial array>
      RenderMaterialsTransparent : RenderMaterial array }

/// Describes a 3d render pass.
type [<CustomEquality; CustomComparison>] RenderPassDescriptor3d =
    { RenderPassOrder : int64
      RenderPass3d : RenderMaterials * Matrix4x4 * Renderer3d -> unit }
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? RenderPassDescriptor3d as that -> this.RenderPassOrder.CompareTo that.RenderPassOrder
            | _ -> -1
    override this.Equals (that : obj) =
        match that with
        | :? RenderPassDescriptor3d as that -> this.RenderPassOrder = that.RenderPassOrder
        | _ -> false
    override this.GetHashCode () = hash this.RenderPassOrder

/// A message to the 3d renderer.
and [<NoEquality; NoComparison>] RenderMessage3d =
    | RenderMaterialDescriptor of RenderMaterial
    | RenderMaterialsDescriptor of RenderMaterial array
    | RenderCallbackDescriptor3d of (Matrix4x4 * Matrix4x4 * Vector3 * Vector3 * Vector3 * Renderer3d -> unit)
    | RenderPrePassDescriptor3d of RenderPassDescriptor3d
    | RenderPostPassDescriptor3d of RenderPassDescriptor3d

/// The 3d renderer. Represents the 3d rendering system in Nu generally.
and Renderer3d () = class end