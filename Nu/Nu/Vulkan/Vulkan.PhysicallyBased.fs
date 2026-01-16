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

    /// A set of physically-based attachments that support a given viewport.
    type PhysicallyBasedAttachments =
        { CompositionAttachments : Texture.Texture * Texture.Texture }
    
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
          SubsurfaceCutoff : single
          SubsurfaceCutoffMargin : single
          RefractiveIndex : single
          ClearCoat : single
          ClearCoatRoughness : single }

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
              SubsurfaceCutoff = 0.0f
              SubsurfaceCutoffMargin = 0.0f
              RefractiveIndex = 0.0f
              ClearCoat = 0.0f
              ClearCoatRoughness = 0.0f }

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
          ClearCoatTexture : Texture.Texture
          ClearCoatRoughnessTexture : Texture.Texture
          ClearCoatNormalTexture : Texture.Texture
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
              ClearCoatTexture = Texture.EmptyTexture
              ClearCoatRoughnessTexture = Texture.EmptyTexture
              ClearCoatNormalTexture = Texture.EmptyTexture
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
                assert (this.PrimitiveTopology = VkPrimitiveTopology.TriangleList) // should hold since we use Assimp.PostProcessSteps.Triangulate
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

        static member extractSubsurfaceCutoff subsurfaceCutoffDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.SubsurfaceCutoffOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue subsurfaceCutoffDefault material.SubsurfaceCutoffOpt
                | Some _ | None -> subsurfaceCutoffDefault
            | ValueSome subsurfaceCutoff -> subsurfaceCutoff

        static member extractSubsurfaceCutoffMargin subsurfaceCutoffMarginDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.SubsurfaceCutoffMarginOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue subsurfaceCutoffMarginDefault material.SubsurfaceCutoffMarginOpt
                | Some _ | None -> subsurfaceCutoffMarginDefault
            | ValueSome subsurfaceCutoffMargin -> subsurfaceCutoffMargin

        static member extractRefractiveIndex refractiveIndexDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RefractiveIndexOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue refractiveIndexDefault material.RefractiveIndexOpt
                | Some _ | None -> refractiveIndexDefault
            | ValueSome refractiveIndex -> refractiveIndex

        static member extractClearCoat clearCoatDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.ClearCoatOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue clearCoatDefault material.ClearCoatOpt
                | Some _ | None -> clearCoatDefault
            | ValueSome clearCoat -> clearCoat

        static member extractClearCoatRoughness clearCoatRoughnessDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.ClearCoatRoughnessOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue clearCoatRoughnessDefault material.ClearCoatRoughnessOpt
                | Some _ | None -> clearCoatRoughnessDefault
            | ValueSome clearCoatRoughness -> clearCoatRoughness

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
        let extractSubsurfaceCutoff = PhysicallyBasedSurface.extractSubsurfaceCutoff
        let extractSubsurfaceCutoffMargin = PhysicallyBasedSurface.extractSubsurfaceCutoffMargin
        let extractRefractiveIndex = PhysicallyBasedSurface.extractRefractiveIndex
        let extractClearCoat = PhysicallyBasedSurface.extractClearCoat
        let extractClearCoatRoughness = PhysicallyBasedSurface.extractClearCoatRoughness
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

    /// Describes a physically-based pipeline that's loaded into GPU.
    type PhysicallyBasedPipeline =
        { ViewUniform : Buffer.Buffer
          ProjectionUniform : Buffer.Buffer
          ViewProjectionUniform : Buffer.Buffer
          EyeCenterUniform : Buffer.Buffer
          ViewInverseUniform : Buffer.Buffer
          ProjectionInverseUniform : Buffer.Buffer
          LightCutoffMarginUniform : Buffer.Buffer
          LightAmbientColorUniform : Buffer.Buffer
          LightAmbientBrightnessUniform : Buffer.Buffer
          LightAmbientBoostCutoffUniform : Buffer.Buffer
          LightAmbientBoostScalarUniform : Buffer.Buffer
          LightShadowSamplesUniform : Buffer.Buffer
          LightShadowBiasUniform : Buffer.Buffer
          LightShadowSampleScalarUniform : Buffer.Buffer
          LightShadowExponentUniform : Buffer.Buffer
          LightShadowDensityUniform : Buffer.Buffer
          FogEnabledUniform : Buffer.Buffer
          FogTypeUniform : Buffer.Buffer
          FogStartUniform : Buffer.Buffer
          FogFinishUniform : Buffer.Buffer
          FogDensityUniform : Buffer.Buffer
          FogColorUniform : Buffer.Buffer
          SsvfEnabledUniform : Buffer.Buffer
          SsvfIntensityUniform : Buffer.Buffer
          SsvfStepsUniform : Buffer.Buffer
          SsvfAsymmetryUniform : Buffer.Buffer
          SsrrEnabledUniform : Buffer.Buffer
          SsrrIntensityUniform : Buffer.Buffer
          SsrrDetailUniform : Buffer.Buffer
          SsrrRefinementsMaxUniform : Buffer.Buffer
          SsrrRayThicknessUniform : Buffer.Buffer
          SsrrDistanceCutoffUniform : Buffer.Buffer
          SsrrDistanceCutoffMarginUniform : Buffer.Buffer
          SsrrEdgeHorizontalMarginUniform : Buffer.Buffer
          SsrrEdgeVerticalMarginUniform : Buffer.Buffer
          ShadowNearUniform : Buffer.Buffer
          BonesUniform : Buffer.Buffer
          LightMapOriginsUniform : Buffer.Buffer
          LightMapMinsUniform : Buffer.Buffer
          LightMapSizesUniform : Buffer.Buffer
          LightMapAmbientColorsUniform : Buffer.Buffer
          LightMapAmbientBrightnessesUniform : Buffer.Buffer
          LightMapsCountUniform : Buffer.Buffer
          LightMapSingletonBlendMarginUniform : Buffer.Buffer
          LightOriginsUniform : Buffer.Buffer
          LightDirectionsUniform : Buffer.Buffer
          LightColorsUniform : Buffer.Buffer
          LightBrightnessesUniform : Buffer.Buffer
          LightAttenuationLinearsUniform : Buffer.Buffer
          LightAttenuationQuadraticsUniform : Buffer.Buffer
          LightCutoffsUniform : Buffer.Buffer
          LightTypesUniform : Buffer.Buffer
          LightConeInnersUniform : Buffer.Buffer
          LightConeOutersUniform : Buffer.Buffer
          LightDesireFogsUniform : Buffer.Buffer
          LightShadowIndicesUniform : Buffer.Buffer
          LightsCountUniform : Buffer.Buffer
          ShadowMatricesUniform : Buffer.Buffer
          Pipeline : Pipeline.Pipeline }

    /// Create the attachments required for physically-based rendering.
    let CreatePhysicallyBasedAttachments (geometryViewport : Viewport, vkc) =
        
        // create composition attachments
        let compositionAttachments = Attachment.CreateColorAttachments (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, vkc)

        // make record
        { CompositionAttachments = compositionAttachments }

    /// Update the size of the attachments. Must be used every frame.
    let UpdatePhysicallyBasedAttachmentsSize (geometryViewport : Viewport, attachments : PhysicallyBasedAttachments, vkc) =
        Attachment.UpdateColorAttachmentsSize (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, attachments.CompositionAttachments, vkc)
    
    /// Destroy the physically-based attachments.
    let DestroyPhysicallyBasedAttachments (attachments : PhysicallyBasedAttachments, vkc) =
        Attachment.DestroyColorAttachments (attachments.CompositionAttachments, vkc)
    
    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if vkcOpt = None.
    let CreatePhysicallyBasedMaterial (vkcOpt, dirPath, defaultMaterial, textureClient : Texture.TextureClient, material : Assimp.Material) =

        // compute the directory string to prefix to a local asset file path
        let dirPrefix = if dirPath <> "" then dirPath + "/" else ""

        // attempt to load albedo info
        let albedo =
            if material.HasColorDiffuse
            then color material.ColorDiffuse.R material.ColorDiffuse.G material.ColorDiffuse.B material.ColorDiffuse.A
            else Constants.Render.AlbedoDefault
        let mutable (_, albedoTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.BaseColor, 0)
        let mutable (_, albedoTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.Diffuse, 0)
        let mutable albedoTextureSlotFilePath =
            if isNull albedoTextureSlotA.FilePath then
                if isNull albedoTextureSlotB.FilePath then ""
                else albedoTextureSlotB.FilePath
            else albedoTextureSlotA.FilePath
        if albedoTextureSlotFilePath <> "" then
            albedoTextureSlotFilePath <- PathF.Normalize albedoTextureSlotFilePath
            let individualPaths = albedoTextureSlotFilePath.Split "/"
            let possibleFilePaths =
                [|for i in dec individualPaths.Length .. -1 .. 0 do
                    let possibleFilePath = String.join "/" (Array.skip i individualPaths)
                    possibleFilePath
                    if PathF.GetExtensionLower possibleFilePath = ".psd" then PathF.ChangeExtension (possibleFilePath, ".png")
                    PathF.ChangeExtension (possibleFilePath, ".dds")|]
            let mutable found = false
            let mutable i = 0
            while not found && i < possibleFilePaths.Length do
                let possibleFilePath = possibleFilePaths.[i]
                if File.Exists (dirPrefix + possibleFilePath) then
                    albedoTextureSlotFilePath <- possibleFilePath
                    found <- true
                else i <- inc i
        let albedoTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression albedoTextureSlotFilePath, dirPrefix + albedoTextureSlotFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ -> defaultMaterial.AlbedoTexture
            | None -> defaultMaterial.AlbedoTexture

        // infer possible substitute texture names
        let albedoTextureDirName =              match albedoTextureSlotFilePath with null -> "" | filePath -> PathF.GetDirectoryName filePath
        let albedoTextureFileName =             PathF.GetFileName albedoTextureSlotFilePath
        let substitutionPrefix =                if albedoTextureDirName <> "" then albedoTextureDirName + "/" else ""
        let has_bc =                            albedoTextureFileName.Contains "_bc"
        let has_d =                             albedoTextureFileName.Contains "_d"
        let hasBaseColor =                      albedoTextureFileName.Contains "BaseColor"
        let hasDiffuse =                        albedoTextureFileName.Contains "Diffuse"
        let hasAlbedo =                         albedoTextureFileName.Contains "Albedo"
        let mTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_m")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_m")                        else ""
        let g_mTextureFilePath =                if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m")                     elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m")                      else ""
        let g_m_aoTextureFilePath =             if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m_ao")                  elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m_ao")                   else ""
        let gTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g")                        else ""
        let sTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_s")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_s")                        else ""
        let aoTextureFilePath =                 if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_ao")                      elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_ao")                       else ""
        let eTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_e")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_e")                        else ""
        let nTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_n")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_n")                        else ""
        let hTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_h")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_h")                        else ""
        let subdermalTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_subdermal")               elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_subdermal")                else ""
        let finenessTextureFilePath =           if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_fineness")                elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_fineness")                 else ""
        let scatterTextureFilePath =            if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_scatter")                 elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_scatter")                  else ""
        let clearCoatTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat")              elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat")               else ""
        let clearCoatRoughnessTextureFilePath = if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat_roughness")    elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat_roughness")     else ""
        let clearCoatNormalTextureFilePath =    if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat_normal")       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat_normal")        else ""
        let rmTextureFilePath =                 if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RM")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RM")                   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RM")                    else ""
        let rmaTextureFilePath =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RMA")                elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RMA")                  elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RMA")                   else ""
        let roughnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Roughness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Roughness")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Roughness")             else ""
        let metallicTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metallic")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metallic")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metallic")              else ""
        let metalnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metalness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metalness")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metalness")             else ""
        let ambientOcclusionTextureFilePath =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AmbientOcclusion")     elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AmbientOcclusion")      else ""
        let occlusionTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Occlusion")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Occlusion")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Occlusion")             else ""
        let aoTextureFilePath' =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AO")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AO")                   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AO")                    else ""
        let normalTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Normal")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Normal")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Normal")                else ""
        let emissionTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emission")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emission")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emission")              else ""
        let heightTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Height")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Height")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Height")                else ""
        let subdermalTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Subdermal")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Subdermal")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Subdermal")             else ""
        let finenessTextureFilePath' =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Fineness")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Fineness")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Fineness")              else ""
        let scatterTextureFilePath' =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Scatter")            elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Scatter")              elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Scatter")               else ""
        let clearCoatTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoat")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoat")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoat")             else ""
        let clearCoatRoughnessTextureFilePath' =if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoatRoughness") elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoatRoughness")   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoatRoughness")    else ""
        let clearCoatNormalTextureFilePath' =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoatNormal")    elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoatNormal")      elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoatNormal")       else ""

        // attempt to load roughness info
        let roughness = Constants.Render.RoughnessDefault
        let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
        if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
        roughnessTextureSlot.FilePath <- roughnessTextureSlot.FilePath // trim
        let roughnessTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression roughnessTextureSlot.FilePath, dirPrefix + roughnessTextureSlot.FilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression gTextureFilePath, dirPrefix + gTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression sTextureFilePath, dirPrefix + sTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_mTextureFilePath, dirPrefix + g_mTextureFilePath, Texture.RenderThread, vkc) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath, Texture.RenderThread, vkc) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression roughnessTextureFilePath, dirPrefix + roughnessTextureFilePath, Texture.RenderThread, vkc) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmTextureFilePath, dirPrefix + rmTextureFilePath, Texture.RenderThread, vkc) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath, Texture.RenderThread, vkc) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.RoughnessTexture
            | None -> defaultMaterial.RoughnessTexture

        // attempt to load metallic info
        let metallic = Constants.Render.MetallicDefault
        let mutable (_, metallicTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Metalness, 0)
        if isNull metallicTextureSlot.FilePath
        then metallicTextureSlot.FilePath <- "" // ensure not null
        else metallicTextureSlot.FilePath <- PathF.Normalize metallicTextureSlot.FilePath
        let metallicTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metallicTextureSlot.FilePath, dirPrefix + metallicTextureSlot.FilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression mTextureFilePath, dirPrefix + mTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_mTextureFilePath, dirPrefix + g_mTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath, Texture.RenderThread, vkc) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metallicTextureFilePath, dirPrefix + metallicTextureFilePath, Texture.RenderThread, vkc) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metalnessTextureFilePath, dirPrefix + metalnessTextureFilePath, Texture.RenderThread, vkc) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmTextureFilePath, dirPrefix + rmTextureFilePath, Texture.RenderThread, vkc) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath, Texture.RenderThread, vkc) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.MetallicTexture
            | None -> defaultMaterial.MetallicTexture

        // attempt to load ambient occlusion info
        let ambientOcclusion = Constants.Render.AmbientOcclusionDefault
        let mutable (_, ambientOcclusionTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.Ambient, 0)
        let mutable (_, ambientOcclusionTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.AmbientOcclusion, 0)
        let ambientOcclusionTextureSlotFilePath =
            if isNull ambientOcclusionTextureSlotA.FilePath then
                if isNull ambientOcclusionTextureSlotB.FilePath then ""
                else ambientOcclusionTextureSlotB.FilePath
            else ambientOcclusionTextureSlotA.FilePath
        let ambientOcclusionTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression ambientOcclusionTextureSlotFilePath, dirPrefix + ambientOcclusionTextureSlotFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression aoTextureFilePath, dirPrefix + aoTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression ambientOcclusionTextureFilePath, dirPrefix + ambientOcclusionTextureFilePath, Texture.RenderThread, vkc) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression occlusionTextureFilePath, dirPrefix + occlusionTextureFilePath, Texture.RenderThread, vkc) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression aoTextureFilePath', dirPrefix + aoTextureFilePath', Texture.RenderThread, vkc) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath, Texture.RenderThread, vkc) with
                                        | Right texture -> texture
                                        | Left _ -> defaultMaterial.AmbientOcclusionTexture
            | None -> defaultMaterial.AmbientOcclusionTexture

        // attempt to load emission info
        let emission = Constants.Render.EmissionDefault
        let mutable (_, emissionTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Emissive, 0)
        if isNull emissionTextureSlot.FilePath
        then emissionTextureSlot.FilePath <- "" // ensure not null
        else emissionTextureSlot.FilePath <- PathF.Normalize emissionTextureSlot.FilePath
        let emissionTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression emissionTextureSlot.FilePath, dirPrefix + emissionTextureSlot.FilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression eTextureFilePath, dirPrefix + eTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression emissionTextureFilePath, dirPrefix + emissionTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.EmissionTexture
            | None -> defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression normalTextureSlot.FilePath, dirPrefix + normalTextureSlot.FilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression nTextureFilePath, dirPrefix + nTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression normalTextureFilePath, dirPrefix + normalTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.NormalTexture
            | None -> defaultMaterial.NormalTexture

        // attempt to load height info
        let height = Constants.Render.HeightDefault
        let mutable (_, heightTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
        if isNull heightTextureSlot.FilePath
        then heightTextureSlot.FilePath <- "" // ensure not null
        else heightTextureSlot.FilePath <- PathF.Normalize heightTextureSlot.FilePath
        let heightTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression heightTextureSlot.FilePath, dirPrefix + heightTextureSlot.FilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression hTextureFilePath, dirPrefix + hTextureFilePath, Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression heightTextureFilePath, dirPrefix + heightTextureFilePath, Texture.RenderThread, vkc) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.HeightTexture
            | None -> defaultMaterial.HeightTexture

        // compute ignore light maps
        let ignoreLightMaps =
            match material.IgnoreLightMapsOpt with
            | ValueSome ignoreLightMaps -> ignoreLightMaps
            | ValueNone -> Constants.Render.IgnoreLightMapsDefault

        // compute opaque distance
        let opaqueDistance =
            match material.OpaqueDistanceOpt with
            | ValueSome opqaqueDistance -> opqaqueDistance
            | ValueNone -> Constants.Render.OpaqueDistanceDefault

        // attempt to load subdermal info
        let subdermalTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression subdermalTextureFilePath, dirPrefix + subdermalTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression subdermalTextureFilePath', dirPrefix + subdermalTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.SubdermalTexture
            | None -> defaultMaterial.SubdermalTexture

        // attempt to load fineness info
        let finenessOffset = Constants.Render.FinenessOffsetDefault
        let finenessTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression finenessTextureFilePath, dirPrefix + finenessTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression finenessTextureFilePath', dirPrefix + finenessTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.FinenessTexture
            | None -> defaultMaterial.FinenessTexture

        // attempt to load scatter info
        let scatterType = Constants.Render.ScatterTypeDefault
        let scatterTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression scatterTextureFilePath, dirPrefix + scatterTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression scatterTextureFilePath', dirPrefix + scatterTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ScatterTexture
            | None -> defaultMaterial.ScatterTexture

        // attempt to load specular scalar info
        let specularScalar =
            match material.SpecularScalarOpt with
            | ValueSome specularScalar -> specularScalar
            | ValueNone -> Constants.Render.SpecularScalarDefault

        // attempt to load subsurface cutoff info
        let subsurfaceCutoff =
            match material.SubsurfaceCutoffOpt with
            | ValueSome subsurfaceCutoff -> subsurfaceCutoff
            | ValueNone -> Constants.Render.SubsurfaceCutoffDefault

        // attempt to load subsurface cutoff margin info
        let subsurfaceCutoffMargin =
            match material.SubsurfaceCutoffMarginOpt with
            | ValueSome subsurfaceCutoffMargin -> subsurfaceCutoffMargin
            | ValueNone -> Constants.Render.SubsurfaceCutoffMarginDefault

        // attempt to load refractive index info
        let refractiveIndex =
            match material.RefractiveIndexOpt with
            | ValueSome refractiveIndex -> refractiveIndex
            | ValueNone -> Constants.Render.RefractiveIndexDefault

        // attempt to load clear coat info
        let clearCoat = Constants.Render.ClearCoatDefault
        let clearCoatTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatTextureFilePath, dirPrefix + clearCoatTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatTextureFilePath', dirPrefix + clearCoatTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatTexture
            | None -> defaultMaterial.ClearCoatTexture

        // attempt to load clear coat roughness info
        let clearCoatRoughness = Constants.Render.ClearCoatRoughnessDefault
        let clearCoatRoughnessTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatRoughnessTextureFilePath, dirPrefix + clearCoatRoughnessTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatRoughnessTextureFilePath', dirPrefix + clearCoatRoughnessTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatRoughnessTexture
            | None -> defaultMaterial.ClearCoatRoughnessTexture

        // attempt to load clear coat normal info
        let clearCoatNormalTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatNormalTextureFilePath, dirPrefix + clearCoatNormalTextureFilePath, Texture.RenderThread, vkc) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression clearCoatNormalTextureFilePath', dirPrefix + clearCoatNormalTextureFilePath', Texture.RenderThread, vkc) with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatNormalTexture
            | None -> defaultMaterial.ClearCoatNormalTexture

        // compute two-sidedness
        let twoSided =
            match material.TwoSidedOpt with
            | ValueSome twoSided -> twoSided
            | ValueNone -> material.IsTwoSided

        // compute clippedness
        let clipped =
            match material.ClippedOpt with
            | ValueSome clipped -> clipped
            | ValueNone -> false

        // compose names when not rendering so that surfaces can be correlated without textures
        let names =
            match vkcOpt with
            | Some _ ->
                albedoTextureSlotFilePath + "/" +
                roughnessTextureSlot.FilePath + "/" +
                metallicTextureSlot.FilePath + "/" +
                ambientOcclusionTextureSlotA.FilePath + "/" +
                ambientOcclusionTextureSlotB.FilePath + "/" +
                emissionTextureSlot.FilePath + "/" +
                normalTextureSlot.FilePath + "/" +
                heightTextureSlot.FilePath
            | None -> ""

        // make properties
        let properties =
            { Albedo = color albedo.R albedo.G albedo.B albedo.A
              Roughness = roughness
              Metallic = metallic
              AmbientOcclusion = ambientOcclusion
              Emission = emission
              Height = height
              IgnoreLightMaps = ignoreLightMaps
              OpaqueDistance = opaqueDistance
              FinenessOffset = finenessOffset
              ScatterType = scatterType
              SubsurfaceCutoff = subsurfaceCutoff
              SubsurfaceCutoffMargin = subsurfaceCutoffMargin
              SpecularScalar = specularScalar
              RefractiveIndex = refractiveIndex
              ClearCoat = clearCoat
              ClearCoatRoughness = clearCoatRoughness }

        // make material
        let material =
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              SubdermalTexture = subdermalTexture
              FinenessTexture = finenessTexture
              ScatterTexture = scatterTexture
              ClearCoatTexture = clearCoatTexture
              ClearCoatRoughnessTexture = clearCoatRoughnessTexture
              ClearCoatNormalTexture = clearCoatNormalTexture
              TwoSided = twoSided
              Clipped = clipped
              Names = names }

        // fin
        (properties, material)

    /// Create physically-based static mesh from an assimp mesh.
    let CreatePhysicallyBasedStaticMesh (indexData, mesh : Assimp.Mesh) =

        // populate vertex data and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 8
            let position = if i < mesh.VertexCount then mesh.Vertices.[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels.[0].Capacity then mesh.TextureCoordinateChannels.[0].[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals.[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData.[v] <- position.X
            vertexData.[v+1] <- position.Y
            vertexData.[v+2] <- position.Z
            vertexData.[v+3] <- texCoords.X
            vertexData.[v+4] <- 1.0f - texCoords.Y
            vertexData.[v+5] <- normal.X
            vertexData.[v+6] <- normal.Y
            vertexData.[v+7] <- normal.Z
            positionMin.X <- min positionMin.X position.X
            positionMin.Y <- min positionMin.Y position.Y
            positionMin.Z <- min positionMin.Z position.Z
            positionMax.X <- max positionMax.X position.X
            positionMax.Y <- max positionMax.Y position.Y
            positionMax.Z <- max positionMax.Z position.Z
        let bounds = box3 positionMin (positionMax - positionMin)

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based animated mesh from an assimp mesh.
    let CreatePhysicallyBasedAnimatedMesh (indexData, mesh : Assimp.Mesh) =

        // populate vertex data (except bone) and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 16)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 16
            let position = if i < mesh.VertexCount then mesh.Vertices.[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels.[0].Capacity then mesh.TextureCoordinateChannels.[0].[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals.[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData.[v] <- position.X
            vertexData.[v+1] <- position.Y
            vertexData.[v+2] <- position.Z
            vertexData.[v+3] <- texCoords.X
            vertexData.[v+4] <- 1.0f - texCoords.Y
            vertexData.[v+5] <- normal.X
            vertexData.[v+6] <- normal.Y
            vertexData.[v+7] <- normal.Z
            vertexData.[v+8] <- -1.0f
            vertexData.[v+9] <- -1.0f
            vertexData.[v+10] <- -1.0f
            vertexData.[v+11] <- -1.0f
            vertexData.[v+12] <- 0.0f
            vertexData.[v+13] <- 0.0f
            vertexData.[v+14] <- 0.0f
            vertexData.[v+15] <- 0.0f
            positionMin.X <- min positionMin.X position.X
            positionMin.Y <- min positionMin.Y position.Y
            positionMin.Z <- min positionMin.Z position.Z
            positionMax.X <- max positionMax.X position.X
            positionMax.Y <- max positionMax.Y position.Y
            positionMax.Z <- max positionMax.Z position.Z
        let bounds = box3 positionMin (positionMax - positionMin)

        // populate vertex bone data
        for boneIndex in 0 .. dec mesh.Bones.Count do
            let weights = mesh.Bones.[boneIndex].VertexWeights
            let weightsCount = mesh.Bones.[boneIndex].VertexWeights.Count
            for weightIndex in 0 .. dec weightsCount do
                let vertexId = weights.[weightIndex].VertexID
                let vertexOffset = vertexId * 16
                let weight = weights.[weightIndex].Weight
                if weight > 0.0f then

                    // find a free slot to specify the current index and weight (free slots are designated as -1.0f index above)
                    let mutable found = false
                    let mutable i = 0
                    while not found && i < Constants.Render.BonesInfluenceMax do
                        if vertexData.[vertexOffset+8+i] = single boneIndex then // already found
                            found <- true
                        elif vertexData.[vertexOffset+8+i] < 0.0f then // found free slot
                            vertexData.[vertexOffset+8+i] <- single boneIndex
                            vertexData.[vertexOffset+12+i] <- weight
                            found <- true
                        else i <- inc i

                    // when all slots are allocated, replace the index and weight of the lowest-weight entry iff the current weight is higher
                    if not found then
                        let mutable lowestOpt = ValueNone
                        for i in 0 .. dec Constants.Render.BonesInfluenceMax do
                            match lowestOpt with
                            | ValueSome lowest ->
                                if vertexData.[vertexOffset+12+i] < vertexData.[vertexOffset+12+lowest] then
                                    lowestOpt <- ValueSome i
                            | ValueNone -> lowestOpt <- ValueSome i
                        match lowestOpt with
                        | ValueSome lowest ->
                            if vertexData.[vertexOffset+12+lowest] < weight then
                                vertexData.[vertexOffset+8+lowest] <- single boneIndex
                                vertexData.[vertexOffset+12+lowest] <- weight
                        | ValueNone -> failwithumf ()

        // fin
        (vertexData, indexData, bounds)
    
    let StaticTexCoordsOffset = (3 (*position*)) * sizeof<single>
    let StaticNormalOffset =    (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let StaticVertexSize =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    
    /// Create physically-based static geometry from a mesh.
    let CreatePhysicallyBasedStaticGeometry (vkcOpt, primitiveTopology, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match vkcOpt with
            | Some vkc ->

                // create buffers
                let vertexBuffer = Buffer.Buffer.createVertexStagedFromMemory vertexData vkc
                let instanceBuffer = Buffer.Buffer.create (Constants.Render.InstanceFieldCount * sizeof<single>) (Buffer.Vertex true) vkc
                let indexBuffer = Buffer.Buffer.createIndexStagedFromMemory indexData vkc

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                Buffer.Buffer.uploadArray 0 0 0 instanceData instanceBuffer vkc
                
                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

            // fake buffers
            | None ->

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 8)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 8
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, Unchecked.defaultof<Buffer.Buffer>, Unchecked.defaultof<Buffer.Buffer>, Unchecked.defaultof<Buffer.Buffer>)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = primitiveTopology
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create physically-based static geometry from an assimp mesh.
    let CreatePhysicallyBasedStaticGeometryFromMesh (vkcOpt, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedStaticMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedStaticGeometry (vkcOpt, VkPrimitiveTopology.TriangleList, vertexData.AsMemory (), indexData.AsMemory (), bounds)
    
    /// Create physically-based animated geometry from a mesh.
    let CreatePhysicallyBasedAnimatedGeometry (vkcOpt, primitiveTopology, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match vkcOpt with
            | Some vkc ->

                // create buffers
                let vertexBuffer = Buffer.Buffer.createVertexStagedFromMemory vertexData vkc
                let instanceBuffer = Buffer.Buffer.create (Constants.Render.InstanceFieldCount * sizeof<single>) (Buffer.Vertex true) vkc
                let indexBuffer = Buffer.Buffer.createIndexStagedFromMemory indexData vkc

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                Buffer.Buffer.uploadArray 0 0 0 instanceData instanceBuffer vkc
                
                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

            // fake buffers
            | None ->

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 16)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 16
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, Unchecked.defaultof<Buffer.Buffer>, Unchecked.defaultof<Buffer.Buffer>, Unchecked.defaultof<Buffer.Buffer>)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = primitiveTopology
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create physically-based animated geometry from an assimp mesh.
    let CreatePhysicallyBasedAnimatedGeometryFromMesh (vkcOpt, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedAnimatedMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedAnimatedGeometry (vkcOpt, VkPrimitiveTopology.TriangleList, vertexData.AsMemory (), indexData.AsMemory (), bounds)
    
    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if vkcOpt = None.
    let TryCreatePhysicallyBasedMaterials (vkcOpt, dirPath, defaultMaterial, textureClient, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = CreatePhysicallyBasedMaterial (vkcOpt, dirPath, defaultMaterial, textureClient, scene.Materials.[i])
                propertiesAndMaterials.[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
    let CreatePhysicallyBasedStaticGeometries (vkcOpt, scene : Assimp.Scene) =
        let meshAndGeometryLists = Dictionary<int * int * Assimp.BoundingBox, (Assimp.Mesh * PhysicallyBasedGeometry) List> HashIdentity.Structural
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let mutable found = false
            let meshAndGeometryListOpt = Dictionary.tryFind (mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox) meshAndGeometryLists
            match meshAndGeometryListOpt with
            | Some (meshAndGeometry : (Assimp.Mesh * PhysicallyBasedGeometry) List) ->
                let mutable enr = meshAndGeometry.GetEnumerator ()
                while not found && enr.MoveNext () do
                    let (meshCached, geometryCached) = enr.Current
                    if  Enumerable.SequenceEqual (meshCached.Vertices, mesh.Vertices) && 
                        Enumerable.SequenceEqual (meshCached.TextureCoordinateChannels.[0], mesh.TextureCoordinateChannels.[0]) && 
                        Enumerable.SequenceEqual (meshCached.Normals, mesh.Normals) then
                        geometries.Add geometryCached
                        found <- true
            | None -> ()
            if not found then
                let geometry = CreatePhysicallyBasedStaticGeometryFromMesh (vkcOpt, indexData, mesh)
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists.[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in CreatePhysicallyBasedStaticGeometries?
    let CreatePhysicallyBasedAnimatedGeometries (vkcOpt, scene : Assimp.Scene) =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let geometry = CreatePhysicallyBasedAnimatedGeometryFromMesh (vkcOpt, indexData, mesh)
            geometries.Add geometry
        geometries
    
    /// Create a physically-based pipeline.
    let CreatePhysicallyBasedPipeline (lightMapsMax, lightsMax, shaderPath, blends, vertexBindings, colorAttachmentFormat, depthTestOpt, vkc) =

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                shaderPath blends vertexBindings
                
                // descriptor set 0: common; per frame; not descriptor indexed
                [|Pipeline.descriptorSet false
                    [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexFragmentStage 1 // view
                      Pipeline.descriptor 1 Hl.UniformBuffer Hl.VertexFragmentStage 1 // projection
                      Pipeline.descriptor 2 Hl.UniformBuffer Hl.VertexStage 1 // viewProjection
                      Pipeline.descriptor 3 Hl.UniformBuffer Hl.FragmentStage 1 // eyeCenter
                      Pipeline.descriptor 4 Hl.UniformBuffer Hl.FragmentStage 1 // viewInverse
                      Pipeline.descriptor 5 Hl.UniformBuffer Hl.FragmentStage 1 // projectionInverse
                      Pipeline.descriptor 6 Hl.UniformBuffer Hl.FragmentStage 1 // lightCutoffMargin
                      Pipeline.descriptor 7 Hl.UniformBuffer Hl.FragmentStage 1 // lightAmbientColor
                      Pipeline.descriptor 8 Hl.UniformBuffer Hl.FragmentStage 1 // lightAmbientBrightness
                      Pipeline.descriptor 9 Hl.UniformBuffer Hl.FragmentStage 1 // lightAmbientBoostCutoff
                      Pipeline.descriptor 10 Hl.UniformBuffer Hl.FragmentStage 1 // lightAmbientBoostScalar
                      Pipeline.descriptor 11 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowSamples
                      Pipeline.descriptor 12 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowBias
                      Pipeline.descriptor 13 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowSampleScalar
                      Pipeline.descriptor 14 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowExponent
                      Pipeline.descriptor 15 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowDensity
                      Pipeline.descriptor 16 Hl.UniformBuffer Hl.FragmentStage 1 // fogEnabled
                      Pipeline.descriptor 17 Hl.UniformBuffer Hl.FragmentStage 1 // fogType
                      Pipeline.descriptor 18 Hl.UniformBuffer Hl.FragmentStage 1 // fogStart
                      Pipeline.descriptor 19 Hl.UniformBuffer Hl.FragmentStage 1 // fogFinish
                      Pipeline.descriptor 20 Hl.UniformBuffer Hl.FragmentStage 1 // fogDensity
                      Pipeline.descriptor 21 Hl.UniformBuffer Hl.FragmentStage 1 // fogColor
                      Pipeline.descriptor 22 Hl.UniformBuffer Hl.FragmentStage 1 // ssvfEnabled
                      Pipeline.descriptor 23 Hl.UniformBuffer Hl.FragmentStage 1 // ssvfIntensity
                      Pipeline.descriptor 24 Hl.UniformBuffer Hl.FragmentStage 1 // ssvfSteps
                      Pipeline.descriptor 25 Hl.UniformBuffer Hl.FragmentStage 1 // ssvfAsymmetry
                      Pipeline.descriptor 26 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrEnabled
                      Pipeline.descriptor 27 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrIntensity
                      Pipeline.descriptor 28 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrDetail
                      Pipeline.descriptor 29 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrRefinementsMax
                      Pipeline.descriptor 30 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrRayThickness
                      Pipeline.descriptor 31 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrDistanceCutoff
                      Pipeline.descriptor 32 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrDistanceCutoffMargin
                      Pipeline.descriptor 33 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrEdgeHorizontalMargin
                      Pipeline.descriptor 34 Hl.UniformBuffer Hl.FragmentStage 1 // ssrrEdgeVerticalMargin
                      Pipeline.descriptor 35 Hl.CombinedImageSampler Hl.FragmentStage 1 // depthTexture
                      Pipeline.descriptor 36 Hl.CombinedImageSampler Hl.FragmentStage 1 // colorTexture
                      Pipeline.descriptor 37 Hl.CombinedImageSampler Hl.FragmentStage 1 // brdfTexture
                      Pipeline.descriptor 38 Hl.CombinedImageSampler Hl.FragmentStage 1 // irradianceMap
                      Pipeline.descriptor 39 Hl.CombinedImageSampler Hl.FragmentStage 1 // environmentFilterMap
                      Pipeline.descriptor 40 Hl.UniformBuffer Hl.FragmentStage 1|] // shadowNear

                  // descriptor set 1: position-specific; per draw; descriptor indexed
                  Pipeline.descriptorSet true
                    [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage 1 // bones
                      Pipeline.descriptor 1 Hl.CombinedImageSampler Hl.FragmentStage 1 // albedoTexture
                      Pipeline.descriptor 2 Hl.CombinedImageSampler Hl.FragmentStage 1 // roughnessTexture
                      Pipeline.descriptor 3 Hl.CombinedImageSampler Hl.FragmentStage 1 // metallicTexture
                      Pipeline.descriptor 4 Hl.CombinedImageSampler Hl.FragmentStage 1 // ambientOcclusionTexture
                      Pipeline.descriptor 5 Hl.CombinedImageSampler Hl.FragmentStage 1 // emissionTexture
                      Pipeline.descriptor 6 Hl.CombinedImageSampler Hl.FragmentStage 1 // normalTexture
                      Pipeline.descriptor 7 Hl.CombinedImageSampler Hl.FragmentStage 1 // heightTexture
                      Pipeline.descriptor 8 Hl.CombinedImageSampler Hl.FragmentStage 1 // subdermalTexture
                      Pipeline.descriptor 9 Hl.CombinedImageSampler Hl.FragmentStage 1 // finenessTexture
                      Pipeline.descriptor 10 Hl.CombinedImageSampler Hl.FragmentStage 1 // scatterTexture
                      Pipeline.descriptor 11 Hl.CombinedImageSampler Hl.FragmentStage 1 // clearCoatTexture
                      Pipeline.descriptor 12 Hl.CombinedImageSampler Hl.FragmentStage 1 // clearCoatRoughnessTexture
                      Pipeline.descriptor 13 Hl.CombinedImageSampler Hl.FragmentStage 1 // clearCoatNormalTexture
                      Pipeline.descriptor 14 Hl.CombinedImageSampler Hl.FragmentStage lightMapsMax // irradianceMaps
                      Pipeline.descriptor 15 Hl.CombinedImageSampler Hl.FragmentStage lightMapsMax // environmentFilterMaps
                      Pipeline.descriptor 16 Hl.CombinedImageSampler Hl.FragmentStage 1 // shadowTextures
                      Pipeline.descriptor 17 Hl.CombinedImageSampler Hl.FragmentStage Constants.Render.ShadowMapsMax // shadowMaps
                      Pipeline.descriptor 18 Hl.CombinedImageSampler Hl.FragmentStage Constants.Render.ShadowCascadesMax // shadowCascades
                      Pipeline.descriptor 19 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapOrigins
                      Pipeline.descriptor 20 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapMins
                      Pipeline.descriptor 21 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapSizes
                      Pipeline.descriptor 22 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapAmbientColors
                      Pipeline.descriptor 23 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapAmbientBrightnesses
                      Pipeline.descriptor 24 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapsCount
                      Pipeline.descriptor 25 Hl.UniformBuffer Hl.FragmentStage 1 // lightMapSingletonBlendMargin
                      Pipeline.descriptor 26 Hl.UniformBuffer Hl.FragmentStage 1 // lightOrigins
                      Pipeline.descriptor 27 Hl.UniformBuffer Hl.FragmentStage 1 // lightDirections
                      Pipeline.descriptor 28 Hl.UniformBuffer Hl.FragmentStage 1 // lightColors
                      Pipeline.descriptor 29 Hl.UniformBuffer Hl.FragmentStage 1 // lightBrightnesses
                      Pipeline.descriptor 30 Hl.UniformBuffer Hl.FragmentStage 1 // lightAttenuationLinears
                      Pipeline.descriptor 31 Hl.UniformBuffer Hl.FragmentStage 1 // lightAttenuationQuadratics
                      Pipeline.descriptor 32 Hl.UniformBuffer Hl.FragmentStage 1 // lightCutoffs
                      Pipeline.descriptor 33 Hl.UniformBuffer Hl.FragmentStage 1 // lightTypes
                      Pipeline.descriptor 34 Hl.UniformBuffer Hl.FragmentStage 1 // lightConeInners
                      Pipeline.descriptor 35 Hl.UniformBuffer Hl.FragmentStage 1 // lightConeOuters
                      Pipeline.descriptor 36 Hl.UniformBuffer Hl.FragmentStage 1 // lightDesireFogs
                      Pipeline.descriptor 37 Hl.UniformBuffer Hl.FragmentStage 1 // lightShadowIndices
                      Pipeline.descriptor 38 Hl.UniformBuffer Hl.FragmentStage 1 // lightsCount
                      Pipeline.descriptor 39 Hl.UniformBuffer Hl.FragmentStage 1|]|] // shadowMatrices
                
                [|Pipeline.pushConstant 0 sizeof<int> Hl.FragmentStage|]
                colorAttachmentFormat depthTestOpt vkc

        // create set 0 uniform buffers
        let viewUniform = Buffer.Buffer.create (16 * sizeof<single>) Buffer.Uniform vkc
        let projectionUniform = Buffer.Buffer.create (16 * sizeof<single>) Buffer.Uniform vkc
        let viewProjectionUniform = Buffer.Buffer.create (16 * sizeof<single>) Buffer.Uniform vkc
        let eyeCenterUniform = Buffer.Buffer.create (3 * sizeof<single>) Buffer.Uniform vkc
        let viewInverseUniform = Buffer.Buffer.create (16 * sizeof<single>) Buffer.Uniform vkc
        let projectionInverseUniform = Buffer.Buffer.create (16 * sizeof<single>) Buffer.Uniform vkc
        let lightCutoffMarginUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightAmbientColorUniform = Buffer.Buffer.create (3 * sizeof<single>) Buffer.Uniform vkc
        let lightAmbientBrightnessUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightAmbientBoostCutoffUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightAmbientBoostScalarUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightShadowSamplesUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let lightShadowBiasUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightShadowSampleScalarUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightShadowExponentUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightShadowDensityUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let fogEnabledUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let fogTypeUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let fogStartUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let fogFinishUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let fogDensityUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let fogColorUniform = Buffer.Buffer.create (4 * sizeof<single>) Buffer.Uniform vkc
        let ssvfEnabledUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let ssvfIntensityUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssvfStepsUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let ssvfAsymmetryUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrEnabledUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let ssrrIntensityUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrDetailUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrRefinementsMaxUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let ssrrRayThicknessUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrDistanceCutoffUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrDistanceCutoffMarginUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrEdgeHorizontalMarginUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let ssrrEdgeVerticalMarginUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let shadowNearUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        
        // create set 1 uniform buffers
        let bonesUniform = Buffer.Buffer.create (64 * Constants.Render.BonesMax) Buffer.Uniform vkc
        let lightMapOriginsUniform = Buffer.Buffer.create (16 * lightMapsMax) Buffer.Uniform vkc
        let lightMapMinsUniform = Buffer.Buffer.create (16 * lightMapsMax) Buffer.Uniform vkc
        let lightMapSizesUniform = Buffer.Buffer.create (16 * lightMapsMax) Buffer.Uniform vkc
        let lightMapAmbientColorsUniform = Buffer.Buffer.create (16 * lightMapsMax) Buffer.Uniform vkc
        let lightMapAmbientBrightnessesUniform = Buffer.Buffer.create (16 * lightMapsMax) Buffer.Uniform vkc
        let lightMapsCountUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let lightMapSingletonBlendMarginUniform = Buffer.Buffer.create sizeof<single> Buffer.Uniform vkc
        let lightOriginsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightDirectionsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightColorsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightBrightnessesUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightAttenuationLinearsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightAttenuationQuadraticsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightCutoffsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightTypesUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightConeInnersUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightConeOutersUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightDesireFogsUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightShadowIndicesUniform = Buffer.Buffer.create (16 * lightsMax) Buffer.Uniform vkc
        let lightsCountUniform = Buffer.Buffer.create sizeof<int> Buffer.Uniform vkc
        let shadowMatricesUniform = Buffer.Buffer.create (64 * Constants.Render.ShadowTexturesMax * Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels) Buffer.Uniform vkc
        
        // make PhysicallyBasedPipeline
        let physicallyBasedPipeline =
            { ViewUniform = viewUniform
              ProjectionUniform = projectionUniform
              ViewProjectionUniform = viewProjectionUniform
              EyeCenterUniform = eyeCenterUniform
              ViewInverseUniform = viewInverseUniform
              ProjectionInverseUniform = projectionInverseUniform
              LightCutoffMarginUniform = lightCutoffMarginUniform
              LightAmbientColorUniform = lightAmbientColorUniform
              LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
              LightAmbientBoostCutoffUniform = lightAmbientBoostCutoffUniform
              LightAmbientBoostScalarUniform = lightAmbientBoostScalarUniform
              LightShadowSamplesUniform = lightShadowSamplesUniform
              LightShadowBiasUniform = lightShadowBiasUniform
              LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
              LightShadowExponentUniform = lightShadowExponentUniform
              LightShadowDensityUniform = lightShadowDensityUniform
              FogEnabledUniform = fogEnabledUniform
              FogTypeUniform = fogTypeUniform
              FogStartUniform = fogStartUniform
              FogFinishUniform = fogFinishUniform
              FogDensityUniform = fogDensityUniform
              FogColorUniform = fogColorUniform
              SsvfEnabledUniform = ssvfEnabledUniform
              SsvfIntensityUniform = ssvfIntensityUniform
              SsvfStepsUniform = ssvfStepsUniform
              SsvfAsymmetryUniform = ssvfAsymmetryUniform
              SsrrEnabledUniform = ssrrEnabledUniform
              SsrrIntensityUniform = ssrrIntensityUniform
              SsrrDetailUniform = ssrrDetailUniform
              SsrrRefinementsMaxUniform = ssrrRefinementsMaxUniform
              SsrrRayThicknessUniform = ssrrRayThicknessUniform
              SsrrDistanceCutoffUniform = ssrrDistanceCutoffUniform
              SsrrDistanceCutoffMarginUniform = ssrrDistanceCutoffMarginUniform
              SsrrEdgeHorizontalMarginUniform = ssrrEdgeHorizontalMarginUniform
              SsrrEdgeVerticalMarginUniform = ssrrEdgeVerticalMarginUniform
              ShadowNearUniform = shadowNearUniform
              BonesUniform = bonesUniform
              LightMapOriginsUniform = lightMapOriginsUniform
              LightMapMinsUniform = lightMapMinsUniform
              LightMapSizesUniform = lightMapSizesUniform
              LightMapAmbientColorsUniform = lightMapAmbientColorsUniform
              LightMapAmbientBrightnessesUniform = lightMapAmbientBrightnessesUniform
              LightMapsCountUniform = lightMapsCountUniform
              LightMapSingletonBlendMarginUniform = lightMapSingletonBlendMarginUniform
              LightOriginsUniform = lightOriginsUniform
              LightDirectionsUniform = lightDirectionsUniform
              LightColorsUniform = lightColorsUniform
              LightBrightnessesUniform = lightBrightnessesUniform
              LightAttenuationLinearsUniform = lightAttenuationLinearsUniform
              LightAttenuationQuadraticsUniform = lightAttenuationQuadraticsUniform
              LightCutoffsUniform = lightCutoffsUniform
              LightTypesUniform = lightTypesUniform
              LightConeInnersUniform = lightConeInnersUniform
              LightConeOutersUniform = lightConeOutersUniform
              LightDesireFogsUniform = lightDesireFogsUniform
              LightShadowIndicesUniform = lightShadowIndicesUniform
              LightsCountUniform = lightsCountUniform
              ShadowMatricesUniform = shadowMatricesUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedPipeline
    
    /// Destroy PhysicallyBasedPipeline.
    let DestroyPhysicallyBasedPipeline physicallyBasedPipeline vkc =
        Buffer.Buffer.destroy physicallyBasedPipeline.ViewUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ProjectionUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ViewProjectionUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.EyeCenterUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ViewInverseUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ProjectionInverseUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightCutoffMarginUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAmbientColorUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAmbientBrightnessUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAmbientBoostCutoffUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAmbientBoostScalarUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowSamplesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowBiasUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowSampleScalarUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowExponentUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowDensityUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogEnabledUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogTypeUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogStartUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogFinishUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogDensityUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.FogColorUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsvfEnabledUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsvfIntensityUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsvfStepsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsvfAsymmetryUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrEnabledUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrIntensityUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrDetailUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrRefinementsMaxUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrRayThicknessUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrDistanceCutoffUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrDistanceCutoffMarginUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrEdgeHorizontalMarginUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.SsrrEdgeVerticalMarginUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ShadowNearUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.BonesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapOriginsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapMinsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapSizesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapAmbientColorsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapAmbientBrightnessesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapsCountUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightMapSingletonBlendMarginUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightOriginsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightDirectionsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightColorsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightBrightnessesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAttenuationLinearsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightAttenuationQuadraticsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightCutoffsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightTypesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightConeInnersUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightConeOutersUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightDesireFogsUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightShadowIndicesUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.LightsCountUniform vkc
        Buffer.Buffer.destroy physicallyBasedPipeline.ShadowMatricesUniform vkc
        Pipeline.Pipeline.destroy physicallyBasedPipeline.Pipeline vkc
    
    /// Begin the process of drawing with a forward pipeline.
    let BeginPhysicallyBasedForwardPipeline
        (view : single array,
         projection : single array,
         viewProjection : single array,
         eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         lightCutoffMargin : single,
         lightAmbientColor : Color,
         lightAmbientBrightness : single,
         lightAmbientBoostCutoff : single,
         lightAmbientBoostScalar : single,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         fogEnabled : int,
         fogType : int,
         fogStart : single,
         fogFinish : single,
         fogDensity : single,
         fogColor : Color,
         ssvfEnabled : int,
         ssvfIntensity : single,
         ssvfSteps : int,
         ssvfAsymmetry : single,
         ssrrEnabled : int,
         ssrrIntensity : single,
         ssrrDetail : single,
         ssrrRefinementsMax : int,
         ssrrRayThickness : single,
         ssrrDistanceCutoff : single,
         ssrrDistanceCutoffMargin : single,
         ssrrEdgeHorizontalMargin : single,
         ssrrEdgeVerticalMargin : single,
         depthTexture : Texture.Texture,
         colorTexture : Texture.Texture,
         brdfTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         shadowNear : single,
         pipeline : PhysicallyBasedPipeline,
         _ : uint,
         vkc : Hl.VulkanContext) =

        // upload common uniforms
        Buffer.Buffer.uploadArray 0 0 16 view pipeline.ViewUniform vkc
        Buffer.Buffer.uploadArray 0 0 16 projection pipeline.ProjectionUniform vkc
        Buffer.Buffer.uploadArray 0 0 16 viewProjection pipeline.ViewProjectionUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|eyeCenter.X; eyeCenter.Y; eyeCenter.Z|] pipeline.EyeCenterUniform vkc
        Buffer.Buffer.uploadArray 0 0 16 viewInverse pipeline.ViewInverseUniform vkc
        Buffer.Buffer.uploadArray 0 0 16 projectionInverse pipeline.ProjectionInverseUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightCutoffMargin|] pipeline.LightCutoffMarginUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightAmbientColor.R; lightAmbientColor.G; lightAmbientColor.B|] pipeline.LightAmbientColorUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightAmbientBrightness|] pipeline.LightAmbientBrightnessUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightAmbientBoostCutoff|] pipeline.LightAmbientBoostCutoffUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightAmbientBoostScalar|] pipeline.LightAmbientBoostScalarUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightShadowSamples|] pipeline.LightShadowSamplesUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightShadowBias|] pipeline.LightShadowBiasUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightShadowSampleScalar|] pipeline.LightShadowSampleScalarUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightShadowExponent|] pipeline.LightShadowExponentUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|lightShadowDensity|] pipeline.LightShadowDensityUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogEnabled|] pipeline.FogEnabledUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogType|] pipeline.FogTypeUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogStart|] pipeline.FogStartUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogFinish|] pipeline.FogFinishUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogDensity|] pipeline.FogDensityUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|fogColor.R; fogColor.G; fogColor.B; fogColor.A|] pipeline.FogColorUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssvfEnabled|] pipeline.SsvfEnabledUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssvfIntensity|] pipeline.SsvfIntensityUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssvfSteps|] pipeline.SsvfStepsUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssvfAsymmetry|] pipeline.SsvfAsymmetryUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrEnabled|] pipeline.SsrrEnabledUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrIntensity|] pipeline.SsrrIntensityUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrDetail|] pipeline.SsrrDetailUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrRefinementsMax|] pipeline.SsrrRefinementsMaxUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrRayThickness|] pipeline.SsrrRayThicknessUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrDistanceCutoff|] pipeline.SsrrDistanceCutoffUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrDistanceCutoffMargin|] pipeline.SsrrDistanceCutoffMarginUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrEdgeHorizontalMargin|] pipeline.SsrrEdgeHorizontalMarginUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|ssrrEdgeVerticalMargin|] pipeline.SsrrEdgeVerticalMarginUniform vkc
        Buffer.Buffer.uploadArray 0 0 0 [|shadowNear|] pipeline.ShadowNearUniform vkc

        // update common uniform descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 0 pipeline.ViewUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 1 pipeline.ProjectionUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 2 pipeline.ViewProjectionUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 3 pipeline.EyeCenterUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 4 pipeline.ViewInverseUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 5 pipeline.ProjectionInverseUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 6 pipeline.LightCutoffMarginUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 7 pipeline.LightAmbientColorUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 8 pipeline.LightAmbientBrightnessUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 9 pipeline.LightAmbientBoostCutoffUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 10 pipeline.LightAmbientBoostScalarUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 11 pipeline.LightShadowSamplesUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 12 pipeline.LightShadowBiasUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 13 pipeline.LightShadowSampleScalarUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 14 pipeline.LightShadowExponentUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 15 pipeline.LightShadowDensityUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 16 pipeline.FogEnabledUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 17 pipeline.FogTypeUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 18 pipeline.FogStartUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 19 pipeline.FogFinishUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 20 pipeline.FogDensityUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 21 pipeline.FogColorUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 22 pipeline.SsvfEnabledUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 23 pipeline.SsvfIntensityUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 24 pipeline.SsvfStepsUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 25 pipeline.SsvfAsymmetryUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 26 pipeline.SsrrEnabledUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 27 pipeline.SsrrIntensityUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 28 pipeline.SsrrDetailUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 29 pipeline.SsrrRefinementsMaxUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 30 pipeline.SsrrRayThicknessUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 31 pipeline.SsrrDistanceCutoffUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 32 pipeline.SsrrDistanceCutoffMarginUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 33 pipeline.SsrrEdgeHorizontalMarginUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 34 pipeline.SsrrEdgeVerticalMarginUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 40 pipeline.ShadowNearUniform pipeline.Pipeline vkc

        // bind common textures
        Pipeline.Pipeline.writeDescriptorTexture 0 0 35 depthTexture pipeline.Pipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 0 0 36 colorTexture pipeline.Pipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 0 0 37 brdfTexture pipeline.Pipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 0 0 38 irradianceMap pipeline.Pipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 0 0 39 environmentFilterMap pipeline.Pipeline vkc

    /// Draw a batch of physically-based forward surfaces.
    let DrawPhysicallyBasedForwardSurfaces
        (drawIndex : int,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         irradianceMaps : Texture.Texture array,
         environmentFilterMaps : Texture.Texture array,
         shadowTextureArray : Texture.Texture,
         shadowMaps : Texture.Texture array,
         shadowCascades : Texture.Texture array,
         lightMapOrigins : Vector3 array,
         lightMapMins : Vector3 array,
         lightMapSizes : Vector3 array,
         lightMapAmbientColors : Color array,
         lightMapAmbientBrightnesses : single array,
         lightMapsCount : int,
         lightMapSingletonBlendMargin : single,
         lightOrigins : Vector3 array,
         lightDirections : Vector3 array,
         lightColors : Color array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightTypes : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightDesireFogs : int array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         depthTest : DepthTest,
         blending : bool,
         viewport : Viewport,
         colorAttachment : Texture.Texture,
         depthAttachment : Texture.Texture,
         pipeline : PhysicallyBasedPipeline,
         vkc : Hl.VulkanContext) =

        // only draw when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then
            
            // upload position-specific uniforms
            let bones = Array.concat bones
            let shadowMatrices = Array.concat shadowMatrices
            Buffer.Buffer.uploadArray drawIndex 0 16 bones pipeline.BonesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightMapOrigins pipeline.LightMapOriginsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightMapMins pipeline.LightMapMinsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightMapSizes pipeline.LightMapSizesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightMapAmbientColors pipeline.LightMapAmbientColorsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightMapAmbientBrightnesses pipeline.LightMapAmbientBrightnessesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 0 [|lightMapsCount|] pipeline.LightMapsCountUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 0 [|lightMapSingletonBlendMargin|] pipeline.LightMapSingletonBlendMarginUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightOrigins pipeline.LightOriginsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightDirections pipeline.LightDirectionsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightColors pipeline.LightColorsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightBrightnesses pipeline.LightBrightnessesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightAttenuationLinears pipeline.LightAttenuationLinearsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightAttenuationQuadratics pipeline.LightAttenuationQuadraticsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightCutoffs pipeline.LightCutoffsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightTypes pipeline.LightTypesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightConeInners pipeline.LightConeInnersUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightConeOuters pipeline.LightConeOutersUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightDesireFogs pipeline.LightDesireFogsUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 lightShadowIndices pipeline.LightShadowIndicesUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 0 [|lightsCount|] pipeline.LightsCountUniform vkc
            Buffer.Buffer.uploadArray drawIndex 0 16 shadowMatrices pipeline.ShadowMatricesUniform vkc

            // update position-specific uniform descriptors
            Pipeline.Pipeline.updateDescriptorsUniform 1 0 pipeline.BonesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 19 pipeline.LightMapOriginsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 20 pipeline.LightMapMinsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 21 pipeline.LightMapSizesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 22 pipeline.LightMapAmbientColorsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 23 pipeline.LightMapAmbientBrightnessesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 24 pipeline.LightMapsCountUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 25 pipeline.LightMapSingletonBlendMarginUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 26 pipeline.LightOriginsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 27 pipeline.LightDirectionsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 28 pipeline.LightColorsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 29 pipeline.LightBrightnessesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 30 pipeline.LightAttenuationLinearsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 31 pipeline.LightAttenuationQuadraticsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 32 pipeline.LightCutoffsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 33 pipeline.LightTypesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 34 pipeline.LightConeInnersUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 35 pipeline.LightConeOutersUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 36 pipeline.LightDesireFogsUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 37 pipeline.LightShadowIndicesUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 38 pipeline.LightsCountUniform pipeline.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 39 pipeline.ShadowMatricesUniform pipeline.Pipeline vkc
        
            // bind position-specific textures
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 1 material.AlbedoTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 2 material.RoughnessTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 3 material.MetallicTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 4 material.AmbientOcclusionTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 5 material.EmissionTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 6 material.NormalTexture pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 7 material.HeightTexture pipeline.Pipeline vkc
            for i in 0 .. dec (min irradianceMaps.Length Constants.Render.LightMapsMaxForward) do
                Pipeline.Pipeline.writeDescriptorTexture (drawIndex * Constants.Render.LightMapsMaxForward + i) 1 14 irradianceMaps.[i] pipeline.Pipeline vkc
            for i in 0 .. dec (min environmentFilterMaps.Length Constants.Render.LightMapsMaxForward) do
                Pipeline.Pipeline.writeDescriptorTexture (drawIndex * Constants.Render.LightMapsMaxForward + i) 1 15 environmentFilterMaps.[i] pipeline.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture drawIndex 1 16 shadowTextureArray pipeline.Pipeline vkc
            for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
                Pipeline.Pipeline.writeDescriptorTexture (drawIndex * Constants.Render.ShadowMapsMax + i) 1 17 shadowMaps.[i] pipeline.Pipeline vkc
            for i in 0 .. dec (min shadowCascades.Length Constants.Render.ShadowCascadesMax) do
                Pipeline.Pipeline.writeDescriptorTexture (drawIndex * Constants.Render.ShadowCascadesMax + i) 1 18 shadowCascades.[i] pipeline.Pipeline vkc
        
            // update instance buffer
            use instanceFieldsPin = new ArrayPin<_> (instanceFields)
            Buffer.Buffer.upload drawIndex 0 0 (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc
        
            // make viewport and scissor
            // TODO: DJL: see if some of the init stuff can/should be moved to BeginPhysicallyBasedForwardPipeline.
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let mutable scissor = renderArea

            // only draw if scissor (and therefore also viewport) is valid
            if Hl.validateRect scissor then

                // init render
                let cb = vkc.RenderCommandBuffer
                let mutable rendering = Hl.makeRenderingInfo colorAttachment.ImageView (Some depthAttachment.ImageView) renderArea None
                Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

                // bind pipeline
                let blend = if blending then Pipeline.Transparent else Pipeline.NoBlend
                let vkPipeline = Pipeline.Pipeline.getVkPipeline blend (not material.TwoSided) pipeline.Pipeline
                Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

                // set viewport and scissor
                Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
                
                // set depth test state
                Vulkan.vkCmdSetDepthTestEnable (cb, (not depthTest.IsAlwaysPassTest))
                Vulkan.vkCmdSetDepthCompareOp (cb, (Pipeline.depthTestToVkCompareOp depthTest))
            
                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                Vulkan.vkCmdBindVertexBuffers (cb, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                Vulkan.vkCmdBindIndexBuffer (cb, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                // TODO: DJL: try to move set 0 (common) binding to BeginPhysicallyBasedForwardPipeline.
                let mutable descriptorSet0 = pipeline.Pipeline.VkDescriptorSet 0
                let mutable descriptorSet1 = pipeline.Pipeline.VkDescriptorSet 1
                Vulkan.vkCmdBindDescriptorSets
                    (cb, VkPipelineBindPoint.Graphics,
                     pipeline.Pipeline.PipelineLayout, 0u,
                     1u, asPointer &descriptorSet0,
                     0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets
                    (cb, VkPipelineBindPoint.Graphics,
                     pipeline.Pipeline.PipelineLayout, 1u,
                     1u, asPointer &descriptorSet1,
                     0u, nullPtr)

                // draw
                Vulkan.vkCmdDrawIndexed (cb, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)
                Hl.reportDrawCall surfacesCount
            
                // end render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

    /// End the process of drawing with a forward pipeline.
    let EndPhysicallyBasedForwardPipeline (_ : PhysicallyBasedPipeline, _ : uint) =
        () // nothing to do

    /// Destroy physically-based geometry resources.
    let DestroyPhysicallyBasedGeometry (geometry, vkc) =
        Buffer.Buffer.destroy geometry.VertexBuffer vkc
        Buffer.Buffer.destroy geometry.InstanceBuffer vkc
        Buffer.Buffer.destroy geometry.IndexBuffer vkc

    /// Destroy physically-based model resources.
    let DestroyPhysicallyBasedModel (model : PhysicallyBasedModel, vkc) =
        for surface in model.Surfaces do
            DestroyPhysicallyBasedGeometry (surface.PhysicallyBasedGeometry, vkc)

    /// Memoizes physically-based scene loads.
    type PhysicallyBasedSceneClient () =

        /// Attempt to create physically-based model from a model file with assimp.
        /// Thread-safe if vkcOpt = None.
        member this.TryCreatePhysicallyBasedModel (vkcOpt, filePath, defaultMaterial, textureClient) =

            // attempt to import from assimp scene
            match AssimpContext.TryGetScene filePath with
            | Right scene ->
                let dirPath = PathF.GetDirectoryName filePath
                match TryCreatePhysicallyBasedMaterials (vkcOpt, dirPath, defaultMaterial, textureClient, scene) with
                | Right materials ->
                    let animated = scene.Animations.Count <> 0
                    let geometries =
                        if animated
                        then CreatePhysicallyBasedAnimatedGeometries (vkcOpt, scene)
                        else CreatePhysicallyBasedStaticGeometries (vkcOpt, scene)

                    // collect light nodes
                    let lightNodes =
                        [|for i in 0 .. dec scene.LightCount do
                            let light = scene.Lights.[i]
                            let node = scene.RootNode.FindNode light.Name
                            yield (light, node)|]

                    // construct bounds and hierarchy
                    // TODO: P1: consider sanitizing incoming names. Corrupted or incompatible names cause subtle hierarchy bugs.
                    let lightProbes = SList.make ()
                    let lights = SList.make ()
                    let surfaces = SList.make ()
                    let mutable bounds = box3Zero
                    let hierarchy =
                        scene.RootNode.Map ([||], m4Identity, fun node names transform ->

                            [|// collect node
                              yield PhysicallyBasedNode names

                              // attempt to collect light probe
                              let lastNameLower = Array.last(names).ToLowerInvariant()
                              if lastNameLower.Contains "probe" && not (lastNameLower.Contains "probes") then
                                let names = Array.append names [|"LightProbe"|]
                                let lightProbeOrigin = transform.Translation
                                let lightProbeBounds =
                                    box3
                                        (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + lightProbeOrigin)
                                        (v3Dup Constants.Render.LightProbeSizeDefault)
                                let lightProbe =
                                    { LightProbeNames = names
                                      LightProbeMatrixIsIdentity = transform.IsIdentity
                                      LightProbeMatrix = transform
                                      LightProbeBounds = lightProbeBounds }
                                lightProbes.Add lightProbe
                                yield PhysicallyBasedLightProbe lightProbe

                              // collect light
                              // NOTE: this is an n^2 algorithm to deal with nodes having no light information
                              for i in 0 .. dec lightNodes.Length do
                                let (light, lightNode) = lightNodes.[i]
                                if lightNode = node then
                                    let names = Array.append names [|"Light" + if i > 0 then string i else ""|]
                                    let lightMatrix = Assimp.ExportMatrix node.TransformWorld
                                    let color = color (min 1.0f light.ColorDiffuse.R) (min 1.0f light.ColorDiffuse.G) (min 1.0f light.ColorDiffuse.B) 1.0f
                                    let lightType =
                                        match light.LightType with
                                        | Assimp.LightSourceType.Spot -> SpotLight (light.AngleInnerCone, light.AngleOuterCone)
                                        | _ -> PointLight // default to point light
                                    let physicallyBasedLight =
                                        { LightNames = names
                                          LightMatrixIsIdentity = lightMatrix.IsIdentity
                                          LightMatrix = lightMatrix
                                          LightColor = color
                                          LightBrightness = Constants.Render.BrightnessDefault // TODO: figure out if we can populate this properly.
                                          LightAttenuationLinear = if light.AttenuationLinear > 0.0f then light.AttenuationLinear else Constants.Render.AttenuationLinearDefault
                                          LightAttenuationQuadratic = if light.AttenuationQuadratic > 0.0f then light.AttenuationQuadratic else Constants.Render.AttenuationQuadraticDefault
                                          LightCutoff = Constants.Render.LightCutoffDefault // TODO: figure out if we can populate this properly.
                                          LightType = lightType
                                          LightDesireShadows = false }
                                    lights.Add physicallyBasedLight
                                    yield PhysicallyBasedLight physicallyBasedLight

                              // collect surfaces
                              for i in 0 .. dec node.MeshIndices.Count do
                                let names = Array.append names [|"Geometry" + if i > 0 then string (inc i) else ""|]
                                let meshIndex = node.MeshIndices.[i]
                                let materialIndex = scene.Meshes.[meshIndex].MaterialIndex
                                let (properties, material) = materials.[materialIndex]
                                let geometry = geometries.[meshIndex]
                                let surface = PhysicallyBasedSurface.make names transform geometry.Bounds properties material materialIndex node geometry
                                bounds <- bounds.Combine (geometry.Bounds.Transform transform)
                                surfaces.Add surface
                                yield PhysicallyBasedSurface surface|]
                            |> TreeNode)

                    // fin
                    Right
                        { Animated = animated
                          Bounds = bounds
                          LightProbes = Array.ofSeq lightProbes
                          Lights = Array.ofSeq lights
                          Surfaces = Array.ofSeq surfaces
                          SceneOpt = Some scene
                          PhysicallyBasedHierarchy = hierarchy }

                // error
                | Left error -> Left ("Could not load materials for static model in file name '" + filePath + "' due to: " + error)
            | Left error -> Left error


    /// Physically-based pipelines.
    type PhysicallyBasedPipelines =
        { ForwardStaticPipeline : PhysicallyBasedPipeline }

    let CreatePhysicallyBasedPipelines (lightMapsMax, lightsMax, colorAttachmentFormat, depthAttachmentFormat, vkc) =

        // create forward static pipeline
        let forwardStaticPipeline =
            CreatePhysicallyBasedPipeline
                (Constants.Render.LightMapsMaxForward,
                 Constants.Render.LightsMaxForward,
                 Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath,
                 [|Pipeline.NoBlend; Pipeline.Transparent|],
                 [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                     [|Pipeline.attribute 0 Hl.Single3 0
                       Pipeline.attribute 1 Hl.Single2 StaticTexCoordsOffset
                       Pipeline.attribute 2 Hl.Single3 StaticNormalOffset|]
                   Pipeline.vertex 1 (Constants.Render.InstanceFieldCount * sizeof<single>) VkVertexInputRate.Instance
                     [|Pipeline.attribute 3 Hl.Single4 0
                       Pipeline.attribute 4 Hl.Single4 (4 * sizeof<single>)
                       Pipeline.attribute 5 Hl.Single4 (8 * sizeof<single>)
                       Pipeline.attribute 6 Hl.Single4 (12 * sizeof<single>)
                       Pipeline.attribute 7 Hl.Single4 (16 * sizeof<single>)
                       Pipeline.attribute 8 Hl.Single4 (20 * sizeof<single>)
                       Pipeline.attribute 9 Hl.Single4 (24 * sizeof<single>)
                       Pipeline.attribute 10 Hl.Single4 (28 * sizeof<single>)
                       Pipeline.attribute 11 Hl.Single4 (32 * sizeof<single>)
                       Pipeline.attribute 12 Hl.Single4 (36 * sizeof<single>)|]|],
                 colorAttachmentFormat,
                 (Some (Pipeline.depthTest depthAttachmentFormat)),
                 vkc)
        
        // create PhysicallyBasedPipelines
        let physicallyBasedPipelines =
            { ForwardStaticPipeline = forwardStaticPipeline }

        // fin
        physicallyBasedPipelines
    
    let DestroyPhysicallyBasedPipelines physicallyBasedPipelines vkc =
        DestroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardStaticPipeline vkc