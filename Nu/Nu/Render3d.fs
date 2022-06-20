// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

// 3d rendering notes:
// I may borrow some sky dome code from here or related - https://github.com/shff/opengl_sky/blob/master/main.mm
// There appears to be a bias constant that can be used with VSMs to fix up light leaks, so consider that.

/// Renders a material of 3d surfaces.
type RenderMaterial =
    interface // same derived type indicates two materials can potentially be batched
        inherit IEquatable<RenderMaterial> // equality indicates two materials have the same draw state and can be batched
        abstract Bounds : Box3 // allows for z-sorting of translucent surfaces
        abstract Transparent : bool // can affect order in which materials are drawn such as in deferred rendering and may disallow batching
        abstract RenderMany : obj * Matrix4x4 byref * Matrix4x4 byref * Vector3 * uint * Renderer -> unit // does actual batched opengl calls
        end

type [<CustomEquality; NoComparison; Struct>] PhysicallyBasedMaterial =
    { mutable HashCode : int
      Model : Matrix4x4
      Bounds : Box3
      Transparent : bool
      AlbedoTexture : uint
      MetalnessTexture : uint
      RoughnessTexture : uint
      NormalTexture : uint
      LightPositions : Vector3 array
      LightColors : Vector3 array
      PhysicallyBasedSurface : OpenGL.Hl.PhysicallyBasedSurface }

    static member inline hash state =
        hash state.Transparent ^^^
        hash state.PhysicallyBasedSurface ^^^
        hash state.AlbedoTexture * hash state.MetalnessTexture * hash state.RoughnessTexture * hash state.NormalTexture ^^^
        hash state.LightPositions ^^^
        hash state.LightColors

    static member inline create model bounds transparent albedoTexture metalnessTexture roughnessTexture normalTexture lightPositions lightColors surface =
        let mutable result =
            { HashCode = 0
              Model = model
              Bounds = bounds
              Transparent = transparent
              PhysicallyBasedSurface = surface
              AlbedoTexture = albedoTexture
              MetalnessTexture = metalnessTexture
              RoughnessTexture = roughnessTexture
              NormalTexture = normalTexture
              LightPositions = lightPositions
              LightColors = lightColors }
        result.HashCode <- PhysicallyBasedMaterial.hash result
        result

    static member inline equals left right =
        left.HashCode = right.HashCode &&
        left.Transparent = right.Transparent &&
        left.PhysicallyBasedSurface.IndexBuffer = right.PhysicallyBasedSurface.IndexBuffer &&
        left.PhysicallyBasedSurface.VertexBuffer = right.PhysicallyBasedSurface.VertexBuffer &&
        left.PhysicallyBasedSurface.PhysicallyBasedVao = right.PhysicallyBasedSurface.PhysicallyBasedVao &&
        left.AlbedoTexture = right.AlbedoTexture &&
        left.MetalnessTexture = right.MetalnessTexture &&
        left.RoughnessTexture = right.RoughnessTexture &&
        left.NormalTexture = right.NormalTexture &&
        left.LightPositions = right.LightPositions &&
        left.LightColors = right.LightColors

    member this.Equals that =
        PhysicallyBasedMaterial.equals this that

    override this.Equals (thatObj : obj) =
        match thatObj with
        | :? PhysicallyBasedMaterial as that -> PhysicallyBasedMaterial.equals this that
        | _ -> false

    override this.GetHashCode () =
        PhysicallyBasedMaterial.hash this

    interface RenderMaterial with
        member this.Bounds = this.Bounds
        member this.Transparent = this.Transparent
        member this.RenderMany (materialsObj, view, projection, eyePosition, ambientOcclusionTexture, modelRow0Buffer, modelRow1Buffer, modelRow2Buffer, modelRow3Buffer, renderer) =
            render.DrawPhysicallyBasedSurfaces ()
            match materialsObj with
            | :? (PhysicallyBasedMaterial array) as materials ->
                if materials.Length > 0 then
                    let material0 = &materials.[0]
                    OpenGL.Hl.DrawSurfaces
                        (material0.PhysicallyBasedSurface, eyePosition, (), view.ToArray (), projection.ToArray (),
                         material0.AlbedoTexture, material0.MetalnessTexture, material0.RoughnessTexture, material0.NormalTexture, ambientOcclusionTexture,
                         [||], [||],
                         modelRow0Buffer, modelRow1Buffer, modelRow2Buffer, modelRow3Buffer, )
                    ()
            | _ -> failwithumf ()

/// A collection of render materials in a pass.
type [<NoEquality; NoComparison>] RenderMaterials =
    { RenderMaterialsOpaque : Dictionary<RenderMaterial, RenderMaterial array>
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