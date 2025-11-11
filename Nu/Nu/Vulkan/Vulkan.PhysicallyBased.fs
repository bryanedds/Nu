// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module PhysicallyBased =

    /// Describes the configurable properties of a physically-based material.
    type PhysicallyBasedMaterialProperties =
        { Albedo : Color
          Roughness : single
          Metallic : single
          AmbientOcclusion : single
          Emission : single
          Height : single
          IgnoreLightMaps : bool
          OpaqueDistance : single
          FinenessOffset : single
          ScatterType : ScatterType
          SpecularScalar : single
          RefractiveIndex : single }

        /// The empty material properties.
        static member empty =
            { Albedo = Color.Zero
              Roughness = 0.0f
              Metallic = 0.0f
              AmbientOcclusion = 0.0f
              Emission = 0.0f
              Height = 0.0f
              IgnoreLightMaps = false
              OpaqueDistance = 0.0f
              FinenessOffset = 0.0f
              ScatterType = NoScatter
              SpecularScalar = 0.0f
              RefractiveIndex = 0.0f }

    /// Describes a physically-based material.
    type PhysicallyBasedMaterial =
        { AlbedoTexture : Texture.Texture
          RoughnessTexture : Texture.Texture
          MetallicTexture : Texture.Texture
          AmbientOcclusionTexture : Texture.Texture
          EmissionTexture : Texture.Texture
          NormalTexture : Texture.Texture
          HeightTexture : Texture.Texture
          SubdermalTexture : Texture.Texture
          FinenessTexture : Texture.Texture
          ScatterTexture : Texture.Texture
          TwoSided : bool
          Clipped : bool
          Names : string }

        /// The empty material.
        static member empty =
            { AlbedoTexture = Texture.EmptyTexture
              RoughnessTexture = Texture.EmptyTexture
              MetallicTexture = Texture.EmptyTexture
              AmbientOcclusionTexture = Texture.EmptyTexture
              EmissionTexture = Texture.EmptyTexture
              NormalTexture = Texture.EmptyTexture
              HeightTexture = Texture.EmptyTexture
              SubdermalTexture = Texture.EmptyTexture
              FinenessTexture = Texture.EmptyTexture
              ScatterTexture = Texture.EmptyTexture
              TwoSided = false
              Clipped = false
              Names = "" }

    /// Describes some physically-based geometry that's loaded into VRAM.
    type PhysicallyBasedGeometry =
        { Bounds : Box3
          PrimitiveTopology : VkPrimitiveTopology
          ElementCount : int
          Vertices : Vector3 array
          Indices : int array
          mutable TrianglesCached : Vector3 array option
          VertexBuffer : Buffer.Buffer
          InstanceBuffer : Buffer.Buffer
          IndexBuffer : Buffer.Buffer }

        /// Lazily access triangles, building them from Vertices and Indices if needed.
        member this.Triangles =
            match this.TrianglesCached with
            | None ->
                assert (this.PrimitiveTopology = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST) // should hold since we use Assimp.PostProcessSteps.Triangulate
                let triangles =
                    [|for points in Array.chunkBySize 3 this.Indices do
                        this.Vertices.[points.[0]]
                        this.Vertices.[points.[1]]
                        this.Vertices.[points.[2]]|]
                this.TrianglesCached <- Some triangles
                triangles
            | Some triangles -> triangles

    /// Describes a renderable physically-based surface.
    type [<CustomEquality; NoComparison>] PhysicallyBasedSurface =
        { HashCode : int
          SurfaceNames : string array
          SurfaceMatrixIsIdentity : bool // OPTIMIZATION: avoid matrix multiply when unnecessary.
          SurfaceMatrix : Matrix4x4
          SurfaceBounds : Box3
          SurfaceMaterialProperties : PhysicallyBasedMaterialProperties
          SurfaceMaterial : PhysicallyBasedMaterial
          SurfaceMaterialIndex : int
          SurfaceNode : Assimp.Node
          PhysicallyBasedGeometry : PhysicallyBasedGeometry }

        static member extractPresence presenceDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.PresenceOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue presenceDefault material.PresenceOpt
                | Some _ | None -> presenceDefault
            | ValueSome presence -> presence

        static member extractRenderStyle renderStyleDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RenderStyleOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue renderStyleDefault material.RenderStyleOpt
                | Some _ | None -> renderStyleDefault
            | ValueSome renderStyle -> renderStyle

        static member extractIgnoreLightMaps ignoreLightMapsDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.IgnoreLightMapsOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue ignoreLightMapsDefault material.IgnoreLightMapsOpt
                | Some _ | None -> ignoreLightMapsDefault
            | ValueSome ignoreLightMaps -> ignoreLightMaps

        static member extractOpaqueDistance opaqueDistanceDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.OpaqueDistanceOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue opaqueDistanceDefault material.OpaqueDistanceOpt
                | Some _ | None -> opaqueDistanceDefault
            | ValueSome opaqueDistance -> opaqueDistance

        static member extractFinenessOffset finenessOffsetDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.FinenessOffsetOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue finenessOffsetDefault material.FinenessOffsetOpt
                | Some _ | None -> finenessOffsetDefault
            | ValueSome finenessOffset -> finenessOffset

        static member extractScatterType scatterTypeDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.ScatterTypeOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue scatterTypeDefault material.ScatterTypeOpt
                | Some _ | None -> scatterTypeDefault
            | ValueSome scatterType -> scatterType

        static member extractSpecularScalar specularScalarDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.SpecularScalarOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue specularScalarDefault material.SpecularScalarOpt
                | Some _ | None -> specularScalarDefault
            | ValueSome specularScalar -> specularScalar

        static member extractRefractiveIndex refractiveIndexDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RefractiveIndexOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue refractiveIndexDefault material.RefractiveIndexOpt
                | Some _ | None -> refractiveIndexDefault
            | ValueSome refractiveIndex -> refractiveIndex

        static member extractNavShape shapeDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.NavShapeOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue shapeDefault material.NavShapeOpt
                | Some _ | None -> shapeDefault
            | ValueSome shape -> shape

        static member inline hash surface =
            surface.HashCode

        static member equals left right =
            refEq left right || // OPTIMIZATION: first check ref equality.
            left.HashCode = right.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
            left.SurfaceMaterial.AlbedoTexture = right.SurfaceMaterial.AlbedoTexture &&
            left.SurfaceMaterial.RoughnessTexture = right.SurfaceMaterial.RoughnessTexture &&
            left.SurfaceMaterial.MetallicTexture = right.SurfaceMaterial.MetallicTexture &&
            left.SurfaceMaterial.AmbientOcclusionTexture = right.SurfaceMaterial.AmbientOcclusionTexture &&
            left.SurfaceMaterial.EmissionTexture = right.SurfaceMaterial.EmissionTexture &&
            left.SurfaceMaterial.NormalTexture = right.SurfaceMaterial.NormalTexture &&
            left.SurfaceMaterial.HeightTexture = right.SurfaceMaterial.HeightTexture &&
            left.SurfaceMaterial.SubdermalTexture = right.SurfaceMaterial.SubdermalTexture &&
            left.SurfaceMaterial.FinenessTexture = right.SurfaceMaterial.FinenessTexture &&
            left.SurfaceMaterial.ScatterTexture = right.SurfaceMaterial.ScatterTexture &&
            left.SurfaceMaterial.TwoSided = right.SurfaceMaterial.TwoSided &&
            left.SurfaceMaterial.Clipped = right.SurfaceMaterial.Clipped &&
            left.SurfaceMaterial.Names = right.SurfaceMaterial.Names &&
            refEq left.PhysicallyBasedGeometry right.PhysicallyBasedGeometry

        static member comparer =
            HashIdentity.FromFunctions PhysicallyBasedSurface.hash PhysicallyBasedSurface.equals

        static member make names (surfaceMatrix : Matrix4x4) bounds properties material materialIndex surfaceNode geometry =
            let hashCode =
                (hash material.AlbedoTexture) ^^^
                (hash material.RoughnessTexture <<< 2) ^^^
                (hash material.MetallicTexture <<< 4) ^^^
                (hash material.AmbientOcclusionTexture <<< 6) ^^^
                (hash material.EmissionTexture <<< 8) ^^^
                (hash material.NormalTexture <<< 10) ^^^
                (hash material.HeightTexture <<< 12) ^^^
                (hash material.SubdermalTexture <<< 14) ^^^
                (hash material.FinenessTexture <<< 16) ^^^
                (hash material.ScatterTexture <<< 18) ^^^
                (hash material.TwoSided <<< 20) ^^^
                (hash material.Clipped <<< 22) ^^^
                (hash material.Names <<< 24) ^^^
                Runtime.CompilerServices.RuntimeHelpers.GetHashCode geometry <<< 24
            { HashCode = hashCode
              SurfaceNames = names
              SurfaceMatrixIsIdentity = surfaceMatrix.IsIdentity
              SurfaceMatrix = surfaceMatrix
              SurfaceBounds = bounds
              SurfaceMaterialProperties = properties
              SurfaceMaterial = material
              SurfaceMaterialIndex = materialIndex
              SurfaceNode = surfaceNode
              PhysicallyBasedGeometry = geometry }

        member this.Equals that =
            PhysicallyBasedSurface.equals this that

        override this.Equals (thatObj : obj) =
            match thatObj with
            | :? PhysicallyBasedSurface as that -> PhysicallyBasedSurface.equals this that
            | _ -> false

        override this.GetHashCode () =
            this.HashCode

    module internal PhysicallyBasedSurfaceFns =
        let extractPresence = PhysicallyBasedSurface.extractPresence
        let extractRenderStyle = PhysicallyBasedSurface.extractRenderStyle
        let extractIgnoreLightMaps = PhysicallyBasedSurface.extractIgnoreLightMaps
        let extractOpaqueDistance = PhysicallyBasedSurface.extractOpaqueDistance
        let extractFinenessOffset = PhysicallyBasedSurface.extractFinenessOffset
        let extractScatterType = PhysicallyBasedSurface.extractScatterType
        let extractSpecularScalar = PhysicallyBasedSurface.extractSpecularScalar
        let extractRefractiveIndex = PhysicallyBasedSurface.extractRefractiveIndex
        let extractNavShape = PhysicallyBasedSurface.extractNavShape
        let hash = PhysicallyBasedSurface.hash
        let equals = PhysicallyBasedSurface.equals
        let comparer = PhysicallyBasedSurface.comparer
        let make = PhysicallyBasedSurface.make

    /// A light probe inside a physically-based static model.
    type PhysicallyBasedLightProbe =
        { LightProbeNames : string array
          LightProbeMatrixIsIdentity : bool
          LightProbeMatrix : Matrix4x4
          LightProbeBounds : Box3 }

    /// A light inside a physically-based static model.
    type PhysicallyBasedLight =
        { LightNames : string array
          LightMatrixIsIdentity : bool
          LightMatrix : Matrix4x4
          LightColor : Color
          LightBrightness : single
          LightAttenuationLinear : single
          LightAttenuationQuadratic : single
          LightCutoff : single
          LightType : LightType
          LightDesireShadows : bool }

    /// A part of a physically-based hierarchy.
    type PhysicallyBasedPart =
        | PhysicallyBasedNode of string array
        | PhysicallyBasedLightProbe of PhysicallyBasedLightProbe
        | PhysicallyBasedLight of PhysicallyBasedLight
        | PhysicallyBasedSurface of PhysicallyBasedSurface

    /// A physically-based model.
    type PhysicallyBasedModel =
        { Animated : bool
          Bounds : Box3
          LightProbes : PhysicallyBasedLightProbe array
          Lights : PhysicallyBasedLight array
          Surfaces : PhysicallyBasedSurface array
          SceneOpt : Assimp.Scene option
          PhysicallyBasedHierarchy : PhysicallyBasedPart array TreeNode }