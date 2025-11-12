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

    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if renderable = false.
    let CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureClient : Texture.TextureClient, material : Assimp.Material) =

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
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression albedoTextureSlotFilePath, dirPrefix + albedoTextureSlotFilePath) with
                | Right texture -> texture
                | Left _ -> defaultMaterial.AlbedoTexture
            else defaultMaterial.AlbedoTexture

        // infer possible substitute texture names
        let albedoTextureDirName =              match albedoTextureSlotFilePath with null -> "" | filePath -> PathF.GetDirectoryName filePath
        let albedoTextureFileName =             PathF.GetFileName albedoTextureSlotFilePath
        let substitutionPrefix =                if albedoTextureDirName <> "" then albedoTextureDirName + "/" else ""
        let has_bc =                            albedoTextureFileName.Contains "_bc"
        let has_d =                             albedoTextureFileName.Contains "_d"
        let hasBaseColor =                      albedoTextureFileName.Contains "BaseColor"
        let hasDiffuse =                        albedoTextureFileName.Contains "Diffuse"
        let hasAlbedo =                         albedoTextureFileName.Contains "Albedo"
        let mTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_m")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_m")                    else ""
        let g_mTextureFilePath =                if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m")                     elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m")                  else ""
        let g_m_aoTextureFilePath =             if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m_ao")                  elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m_ao")               else ""
        let gTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g")                    else ""
        let sTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_s")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_s")                    else ""
        let aoTextureFilePath =                 if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_ao")                      elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_ao")                   else ""
        let eTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_e")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_e")                    else ""
        let nTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_n")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_n")                    else ""
        let hTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_h")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_h")                    else ""
        let subdermalTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_subdermal")               elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_subdermal")            else ""
        let finenessTextureFilePath =           if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_fineness")                elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_fineness")             else ""
        let scatterTextureFilePath =            if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_scatter")                 elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_scatter")              else ""
        let rmTextureFilePath =                 if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RM")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RM")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RM")                else ""
        let rmaTextureFilePath =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RMA")                elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RMA")              elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RMA")               else ""
        let roughnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Roughness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Roughness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Roughness")         else ""
        let metallicTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metallic")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metallic")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metallic")          else ""
        let metalnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metalness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metalness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metalness")         else ""
        let ambientOcclusionTextureFilePath =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AmbientOcclusion") elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AmbientOcclusion")  else ""
        let occlusionTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Occlusion")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Occlusion")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Occlusion")         else ""
        let aoTextureFilePath' =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AO")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AO")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AO")                else ""
        let normalTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Normal")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Normal")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Normal")            else ""
        let emissionTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emission")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emission")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emission")          else ""
        let heightTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Height")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Height")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Height")            else ""
        let subdermalTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Subdermal")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Subdermal")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Subdermal")         else ""
        let finenessTextureFilePath' =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Fineness")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Fineness")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Fineness")          else ""
        let scatterTextureFilePath' =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Scatter")            elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Scatter")          elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Scatter")           else ""

        // attempt to load roughness info
        let roughness = Constants.Render.RoughnessDefault
        let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
        if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
        roughnessTextureSlot.FilePath <- roughnessTextureSlot.FilePath // trim
        let roughnessTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression roughnessTextureSlot.FilePath, dirPrefix + roughnessTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression gTextureFilePath, dirPrefix + gTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression sTextureFilePath, dirPrefix + sTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_mTextureFilePath, dirPrefix + g_mTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression roughnessTextureFilePath, dirPrefix + roughnessTextureFilePath) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmTextureFilePath, dirPrefix + rmTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.RoughnessTexture
            else defaultMaterial.RoughnessTexture

        // attempt to load metallic info
        let metallic = Constants.Render.MetallicDefault
        let mutable (_, metallicTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Metalness, 0)
        if isNull metallicTextureSlot.FilePath
        then metallicTextureSlot.FilePath <- "" // ensure not null
        else metallicTextureSlot.FilePath <- PathF.Normalize metallicTextureSlot.FilePath
        let metallicTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metallicTextureSlot.FilePath, dirPrefix + metallicTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression mTextureFilePath, dirPrefix + mTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_mTextureFilePath, dirPrefix + g_mTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metallicTextureFilePath, dirPrefix + metallicTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression metalnessTextureFilePath, dirPrefix + metalnessTextureFilePath) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmTextureFilePath, dirPrefix + rmTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.MetallicTexture
            else defaultMaterial.MetallicTexture

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
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression ambientOcclusionTextureSlotFilePath, dirPrefix + ambientOcclusionTextureSlotFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression aoTextureFilePath, dirPrefix + aoTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression g_m_aoTextureFilePath, dirPrefix + g_m_aoTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression ambientOcclusionTextureFilePath, dirPrefix + ambientOcclusionTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression occlusionTextureFilePath, dirPrefix + occlusionTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression aoTextureFilePath', dirPrefix + aoTextureFilePath') with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression rmaTextureFilePath, dirPrefix + rmaTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ -> defaultMaterial.AmbientOcclusionTexture
            else defaultMaterial.AmbientOcclusionTexture

        // attempt to load emission info
        let emission = Constants.Render.EmissionDefault
        let mutable (_, emissionTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Emissive, 0)
        if isNull emissionTextureSlot.FilePath
        then emissionTextureSlot.FilePath <- "" // ensure not null
        else emissionTextureSlot.FilePath <- PathF.Normalize emissionTextureSlot.FilePath
        let emissionTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression emissionTextureSlot.FilePath, dirPrefix + emissionTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression eTextureFilePath, dirPrefix + eTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression emissionTextureFilePath, dirPrefix + emissionTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.EmissionTexture
            else defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression normalTextureSlot.FilePath, dirPrefix + normalTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression nTextureFilePath, dirPrefix + nTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression normalTextureFilePath, dirPrefix + normalTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.NormalTexture
            else defaultMaterial.NormalTexture

        // attempt to load height info
        let height = Constants.Render.HeightDefault
        let mutable (_, heightTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
        if isNull heightTextureSlot.FilePath
        then heightTextureSlot.FilePath <- "" // ensure not null
        else heightTextureSlot.FilePath <- PathF.Normalize heightTextureSlot.FilePath
        let heightTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression heightTextureSlot.FilePath, dirPrefix + heightTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression hTextureFilePath, dirPrefix + hTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression heightTextureFilePath, dirPrefix + heightTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.HeightTexture
            else defaultMaterial.HeightTexture

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
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression subdermalTextureFilePath, dirPrefix + subdermalTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression subdermalTextureFilePath', dirPrefix + subdermalTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.SubdermalTexture
            else defaultMaterial.SubdermalTexture

        // attempt to load fineness info
        let finenessOffset = Constants.Render.FinenessOffsetDefault
        let finenessTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression finenessTextureFilePath, dirPrefix + finenessTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression finenessTextureFilePath', dirPrefix + finenessTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.FinenessTexture
            else defaultMaterial.FinenessTexture

        // attempt to load scatter info
        let scatterType = Constants.Render.ScatterTypeDefault
        let scatterTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression scatterTextureFilePath, dirPrefix + scatterTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, Texture.InferCompression scatterTextureFilePath', dirPrefix + scatterTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ScatterTexture
            else defaultMaterial.ScatterTexture

        // attempt to load specular scalar info
        let specularScalar =
            match material.SpecularScalarOpt with
            | ValueSome specularScalar -> specularScalar
            | ValueNone -> Constants.Render.SpecularScalarDefault

        // attempt to load refractive index info
        let refractiveIndex =
            match material.RefractiveIndexOpt with
            | ValueSome refractiveIndex -> refractiveIndex
            | ValueNone -> Constants.Render.RefractiveIndexDefault

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
            if not renderable then
                albedoTextureSlotFilePath + "/" +
                roughnessTextureSlot.FilePath + "/" +
                metallicTextureSlot.FilePath + "/" +
                ambientOcclusionTextureSlotA.FilePath + "/" +
                ambientOcclusionTextureSlotB.FilePath + "/" +
                emissionTextureSlot.FilePath + "/" +
                normalTextureSlot.FilePath + "/" +
                heightTextureSlot.FilePath
            else ""

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
              SpecularScalar = specularScalar
              RefractiveIndex = refractiveIndex }

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
              TwoSided = twoSided
              Clipped = clipped
              Names = names }

        // fin
        (properties, material)

    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if renderable = false.
    let TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureClient, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureClient, scene.Materials.[i])
                propertiesAndMaterials.[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
    let CreatePhysicallyBasedStaticGeometries (renderable, scene : Assimp.Scene) =
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
                let geometry = CreatePhysicallyBasedStaticGeometryFromMesh (renderable, indexData, mesh)
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists.[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in CreatePhysicallyBasedStaticGeometries?
    let CreatePhysicallyBasedAnimatedGeometries (renderable, scene : Assimp.Scene) =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let geometry = CreatePhysicallyBasedAnimatedGeometryFromMesh (renderable, indexData, mesh)
            geometries.Add geometry
        geometries
    
    /// Memoizes physically-based scene loads.
    type PhysicallyBasedSceneClient () =

        let scenes = Dictionary HashIdentity.Structural

        /// Memoized scenes.
        member this.Scenes = scenes

        /// Attempt to create physically-based model from a model file with assimp.
        /// Thread-safe if renderable = false.
        member this.TryCreatePhysicallyBasedModel (renderable, filePath, defaultMaterial, textureClient) =

            // attempt to memoize scene
            let sceneEir =
                match scenes.TryGetValue filePath with
                | (false, _) ->

                    // attempt to create scene
                    use assimp = new Assimp.AssimpContext ()
                    try let scene = assimp.ImportFile (filePath, Constants.Assimp.PostProcessSteps)
                        scene.IndexDatasToMetadata () // avoid polluting memory with face data
                        scene.ClearColorData () // avoid polluting memory with unused color data
                        scenes.[filePath] <- scene
                        Right scene
                    with exn ->
                        Left ("Could not load assimp scene from '" + filePath + "' due to: " + scstring exn)

                // already exists
                | (true, scene) -> Right scene

            // attempt to import from assimp scene
            match sceneEir with
            | Right scene ->
                let dirPath = PathF.GetDirectoryName filePath
                match TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureClient, scene) with
                | Right materials ->
                    let animated = scene.Animations.Count <> 0
                    let geometries =
                        if animated
                        then CreatePhysicallyBasedAnimatedGeometries (renderable, scene)
                        else CreatePhysicallyBasedStaticGeometries (renderable, scene)

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