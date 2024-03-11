// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module PhysicallyBased =

    /// Describes the configurable properties of a physically-based material.
    type [<Struct>] PhysicallyBasedMaterialProperties =
        { Albedo : Color
          Roughness : single
          Metallic : single
          AmbientOcclusion : single
          Emission : single
          Height : single
          IgnoreLightMaps : bool
          OpaqueDistance : single }

    /// Describes a physically-based material.
    type [<Struct>] PhysicallyBasedMaterial =
        { AlbedoMetadata : Texture.TextureMetadata
          AlbedoTexture : Texture.Texture
          RoughnessTexture : Texture.Texture
          MetallicTexture : Texture.Texture
          AmbientOcclusionTexture : Texture.Texture
          EmissionTexture : Texture.Texture
          NormalTexture : Texture.Texture
          HeightTexture : Texture.Texture
          TwoSided : bool }

    /// Describes some physically-based geometry that's loaded into VRAM.
    type PhysicallyBasedGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          Indices : int array
          VertexBuffer : uint
          InstanceBuffer : uint
          IndexBuffer : uint
          PhysicallyBasedVao : uint }

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
            | None ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    Option.defaultValue presenceDefault material.PresenceOpt
                | Some _ | None -> presenceDefault
            | Some presence -> presence

        static member extractRenderStyle renderStyleDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RenderStyleOpt with
            | None ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    Option.defaultValue renderStyleDefault material.RenderStyleOpt
                | Some _ | None -> renderStyleDefault
            | Some renderStyle -> renderStyle

        static member extractIgnoreLightMaps ignoreLightMapsDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.IgnoreLightMapsOpt with
            | None ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    Option.defaultValue ignoreLightMapsDefault material.IgnoreLightMapsOpt
                | Some _ | None -> ignoreLightMapsDefault
            | Some ignoreLightMaps -> ignoreLightMaps

        static member extractOpaqueDistance opaqueDistanceDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.OpaqueDistanceOpt with
            | None ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    Option.defaultValue opaqueDistanceDefault material.OpaqueDistanceOpt
                | Some _ | None -> opaqueDistanceDefault
            | Some opaqueDistance -> opaqueDistance

        static member extractNavShape shapeDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.NavShapeOpt with
            | None ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    Option.defaultValue shapeDefault material.NavShapeOpt
                | Some _ | None -> shapeDefault
            | Some shape -> shape

        static member inline hash surface =
            surface.HashCode

        static member equals left right =
            left.SurfaceMaterial.AlbedoTexture.TextureId = right.SurfaceMaterial.AlbedoTexture.TextureId &&
            left.SurfaceMaterial.RoughnessTexture.TextureId = right.SurfaceMaterial.RoughnessTexture.TextureId &&
            left.SurfaceMaterial.MetallicTexture.TextureId = right.SurfaceMaterial.MetallicTexture.TextureId &&
            left.SurfaceMaterial.AmbientOcclusionTexture.TextureId = right.SurfaceMaterial.AmbientOcclusionTexture.TextureId &&
            left.SurfaceMaterial.EmissionTexture.TextureId = right.SurfaceMaterial.EmissionTexture.TextureId &&
            left.SurfaceMaterial.NormalTexture.TextureId = right.SurfaceMaterial.NormalTexture.TextureId &&
            left.SurfaceMaterial.HeightTexture.TextureId = right.SurfaceMaterial.HeightTexture.TextureId &&
            left.SurfaceMaterial.TwoSided = right.SurfaceMaterial.TwoSided &&
            left.PhysicallyBasedGeometry.PrimitiveType = right.PhysicallyBasedGeometry.PrimitiveType &&
            left.PhysicallyBasedGeometry.PhysicallyBasedVao = right.PhysicallyBasedGeometry.PhysicallyBasedVao

        static member make names (surfaceMatrix : Matrix4x4) bounds properties material materialIndex surfaceNode geometry =
            let hashCode =
                (int material.AlbedoTexture.TextureId) ^^^
                (int material.RoughnessTexture.TextureId <<< 2) ^^^
                (int material.MetallicTexture.TextureId <<< 4) ^^^
                (int material.AmbientOcclusionTexture.TextureId <<< 6) ^^^
                (int material.EmissionTexture.TextureId <<< 8) ^^^
                (int material.NormalTexture.TextureId <<< 10) ^^^
                (int material.HeightTexture.TextureId <<< 12) ^^^
                (hash material.TwoSided <<< 14) ^^^
                (int geometry.PrimitiveType <<< 16) ^^^
                (int geometry.PhysicallyBasedVao <<< 18)
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
        let extractNavShape = PhysicallyBasedSurface.extractNavShape
        let hash = PhysicallyBasedSurface.hash
        let equals = PhysicallyBasedSurface.equals
        let comparer = HashIdentity.FromFunctions hash equals

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

    /// Describes a physically-based shader that's loaded into GPU.
    type PhysicallyBasedShader =
        { ViewUniform : int
          ProjectionUniform : int
          BonesUniforms : int array
          EyeCenterUniform : int
          LightCutoffMarginUniform : int
          LightAmbientColorUniform : int
          LightAmbientBrightnessUniform : int
          LightShadowBiasAcneUniform : int
          LightShadowBiasBleedUniform : int
          AlbedoTextureUniform : int
          RoughnessTextureUniform : int
          MetallicTextureUniform : int
          AmbientOcclusionTextureUniform : int
          EmissionTextureUniform : int
          NormalTextureUniform : int
          HeightTextureUniform : int
          BrdfTextureUniform : int
          IrradianceMapUniform : int
          EnvironmentFilterMapUniform : int
          IrradianceMapsUniforms : int array
          EnvironmentFilterMapsUniforms : int array
          ShadowTexturesUniforms : int array
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          LightMapsCountUniform : int
          LightOriginsUniform : int
          LightDirectionsUniform : int
          LightColorsUniform : int
          LightBrightnessesUniform : int
          LightAttenuationLinearsUniform : int
          LightAttenuationQuadraticsUniform : int
          LightCutoffsUniform : int
          LightDirectionalsUniform : int
          LightConeInnersUniform : int
          LightConeOutersUniform : int
          LightShadowIndicesUniform : int
          LightsCountUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedShader : uint }

    /// Describes a physically-based deferred terrain shader that's loaded into GPU.
    type PhysicallyBasedDeferredTerrainShader =
        { ViewUniform : int
          ProjectionUniform : int
          EyeCenterUniform : int
          LayersCountUniform : int
          AlbedoTexturesUniforms : int array
          RoughnessTexturesUniforms : int array
          AmbientOcclusionTexturesUniforms : int array
          NormalTexturesUniforms : int array
          HeightTexturesUniforms : int array
          PhysicallyBasedShader : uint }

    /// Describes a light mapping pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredLightMappingShader =
        { PositionTextureUniform : int
          NormalPlusTextureUniform : int
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          LightMapsCountUniform : int
          PhysicallyBasedDeferredLightMappingShader : uint }

    /// Describes an irradiance pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredIrradianceShader =
        { PositionTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          IrradianceMapUniform : int
          IrradianceMapsUniforms : int array
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          PhysicallyBasedDeferredIrradianceShader : uint }

    /// Describes an environment filter pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredEnvironmentFilterShader =
        { EyeCenterUniform : int
          PositionTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          EnvironmentFilterMapUniform : int
          EnvironmentFilterMapsUniforms : int array
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          PhysicallyBasedDeferredEnvironmentFilterShader : uint }

    /// Describes an ssao pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredSsaoShader =
        { ViewUniform : int
          ProjectionUniform : int
          PositionTextureUniform : int
          NormalPlusTextureUniform : int
          SsaoResolution : int
          SsaoIntensity : int
          SsaoBias : int
          SsaoRadius : int
          SsaoDistanceMax : int
          SsaoSampleCount : int
          PhysicallyBasedDeferredSsaoShader : uint }

    /// Describes the lighting pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredLightingShader =
        { EyeCenterUniform : int
          LightCutoffMarginUniform : int
          LightAmbientColorUniform : int
          LightAmbientBrightnessUniform : int
          LightShadowBiasAcneUniform : int
          LightShadowBiasBleedUniform : int
          PositionTextureUniform : int
          AlbedoTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          BrdfTextureUniform : int
          IrradianceTextureUniform : int
          EnvironmentFilterTextureUniform : int
          SsaoTextureUniform : int
          ShadowTexturesUniforms : int array
          LightOriginsUniform : int
          LightDirectionsUniform : int
          LightColorsUniform : int
          LightBrightnessesUniform : int
          LightAttenuationLinearsUniform : int
          LightAttenuationQuadraticsUniform : int
          LightCutoffsUniform : int
          LightDirectionalsUniform : int
          LightConeInnersUniform : int
          LightConeOutersUniform : int
          LightShadowIndicesUniform : int
          LightsCountUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedDeferredLightingShader : uint }

    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if renderable = false.
    let CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureMemo, material : Assimp.Material) =

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
        let (albedoMetadata, albedoTexture) =
            if renderable then
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + albedoTextureSlotFilePath, textureMemo) with
                | Right (textureMetadata, texture) -> (textureMetadata, texture)
                | Left _ -> (defaultMaterial.AlbedoMetadata, defaultMaterial.AlbedoTexture)
            else (defaultMaterial.AlbedoMetadata, defaultMaterial.AlbedoTexture)

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
        let rmTextureFilePath =                 if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RM")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RM")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RM")                else ""
        let rmaTextureFilePath =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RMA")                elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RMA")              elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RMA")               else ""
        let roughnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Roughness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Roughness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Roughness")         else ""
        let metallicTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metallic")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metallic")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metallic")          else ""
        let metalnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metalness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metalness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metalness")         else ""
        let ambientOcclusionTextureFilePath =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AmbientOcclusion") elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AmbientOcclusion")  else ""
        let aoTextureFilePath' =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AO")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AO")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AO")                else ""
        let normalTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Normal")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Normal")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Normal")            else ""
        let emissionTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emission")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emission")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emission")          else ""
        let heightTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Height")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Height")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Height")            else ""

        // attempt to load roughness info
        let roughness = Constants.Render.RoughnessDefault
        let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
        if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
        roughnessTextureSlot.FilePath <- roughnessTextureSlot.FilePath // trim
        let roughnessTexture =
            if renderable then
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + roughnessTextureSlot.FilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + gTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + sTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
                        | Left _ ->
                            match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + g_mTextureFilePath, textureMemo) with
                            | Right (_, texture) -> texture
                            | Left _ ->
                                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + g_m_aoTextureFilePath, textureMemo) with
                                | Right (_, texture) -> texture
                                | Left _ ->
                                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + roughnessTextureFilePath, textureMemo) with
                                    | Right (_, texture) -> texture
                                    | Left _ ->
                                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + rmTextureFilePath, textureMemo) with
                                        | Right (_, texture) -> texture
                                        | Left _ ->
                                            match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + rmaTextureFilePath, textureMemo) with
                                            | Right (_, texture) -> texture
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
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + metallicTextureSlot.FilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + mTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + g_mTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
                        | Left _ ->
                            match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + g_m_aoTextureFilePath, textureMemo) with
                            | Right (_, texture) -> texture
                            | Left _ ->
                                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + metallicTextureFilePath, textureMemo) with
                                | Right (_, texture) -> texture
                                | Left _ ->
                                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + metalnessTextureFilePath, textureMemo) with
                                    | Right (_, texture) -> texture
                                    | Left _ ->
                                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + rmTextureFilePath, textureMemo) with
                                        | Right (_, texture) -> texture
                                        | Left _ ->
                                            match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + rmaTextureFilePath, textureMemo) with
                                            | Right (_, texture) -> texture
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
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + ambientOcclusionTextureSlotFilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + aoTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + g_m_aoTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
                        | Left _ ->
                            match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + ambientOcclusionTextureFilePath, textureMemo) with
                            | Right (_, texture) -> texture
                            | Left _ ->
                                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + aoTextureFilePath', textureMemo) with
                                | Right (_, texture) -> texture
                                | Left _ ->
                                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + rmaTextureFilePath, textureMemo) with
                                    | Right (_, texture) -> texture
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
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + emissionTextureSlot.FilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + eTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + emissionTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
                        | Left _ -> defaultMaterial.EmissionTexture
            else defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            if renderable then
                match Texture.TryCreateTextureFilteredMemoized (false, dirPrefix + normalTextureSlot.FilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (false, dirPrefix + nTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (false, dirPrefix + normalTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
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
                match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + heightTextureSlot.FilePath, textureMemo) with
                | Right (_, texture) -> texture
                | Left _ ->
                    match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + hTextureFilePath, textureMemo) with
                    | Right (_, texture) -> texture
                    | Left _ ->
                        match Texture.TryCreateTextureFilteredMemoized (true, dirPrefix + heightTextureFilePath, textureMemo) with
                        | Right (_, texture) -> texture
                        | Left _ -> defaultMaterial.HeightTexture
            else defaultMaterial.HeightTexture

        // compute ignore light maps
        let ignoreLightMaps =
            match material.IgnoreLightMapsOpt with
            | Some ignoreLightMaps -> ignoreLightMaps
            | None -> Constants.Render.IgnoreLightMapsDefault

        // compute opaque distance
        let opaqueDistance =
            match material.OpaqueDistanceOpt with
            | Some opqaqueDistance -> opqaqueDistance
            | None -> Constants.Render.OpaqueDistanceDefault

        // compute two-sidedness
        let twoSided =
            match material.TwoSidedOpt with
            | Some twoSided -> twoSided
            | None -> material.IsTwoSided

        // make properties
        let properties =
            { Albedo = color albedo.R albedo.G albedo.B albedo.A
              Roughness = roughness
              Metallic = metallic
              AmbientOcclusion = ambientOcclusion
              Emission = emission
              Height = height
              IgnoreLightMaps = ignoreLightMaps
              OpaqueDistance = opaqueDistance }

        // make material
        let material =
            { AlbedoMetadata = albedoMetadata
              AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TwoSided = twoSided }

        // fin
        (properties, material)

    /// Attempt to create physically-based static mesh from an assimp mesh.
    let TryCreatePhysicallyBasedStaticMesh (indexData, mesh : Assimp.Mesh) =

        // ensure required data is available
        if  mesh.HasVertices &&
            mesh.HasNormals &&
            mesh.HasTextureCoords 0 then

            // attempt to populate geometry data
            if mesh.Vertices.Count = mesh.Normals.Count && mesh.Vertices.Count = mesh.TextureCoordinateChannels.[0].Count then

                // populate vertex data and bounds
                let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
                let mutable positionMin = v3Zero
                let mutable positionMax = v3Zero
                for i in 0 .. dec mesh.Vertices.Count do
                    let v = i * 8
                    let position = mesh.Vertices.[i]
                    let texCoords = mesh.TextureCoordinateChannels.[0].[i]
                    let normal = mesh.Normals.[i]
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
                Right (vertexData, indexData, bounds)

            // error
            else Left "Vertex / normal / tex coords count mismatch."

        // error
        else Left "Mesh is missing vertices, normals, or texCoords."

    /// Attempt to create physically-based animated mesh from an assimp mesh.
    let TryCreatePhysicallyBasedAnimatedMesh (indexData, mesh : Assimp.Mesh) =

        // ensure required data is available
        if  mesh.HasVertices &&
            mesh.HasNormals &&
            mesh.HasTextureCoords 0 then

            // attempt to populate geometry data
            if mesh.Vertices.Count = mesh.Normals.Count && mesh.Vertices.Count = mesh.TextureCoordinateChannels.[0].Count then

                // populate vertex data (except bone) and bounds
                let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 16)
                let mutable positionMin = v3Zero
                let mutable positionMax = v3Zero
                for i in 0 .. dec mesh.Vertices.Count do
                    let v = i * 16
                    let position = mesh.Vertices.[i]
                    let texCoords = mesh.TextureCoordinateChannels.[0].[i]
                    let normal = mesh.Normals.[i]
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
                        let weight = weights.[weightIndex].Weight
                        if weight > 0.0f then
                            let mutable found = false
                            let mutable i = 0
                            while not found && i < Constants.Render.BonesInfluenceMax do
                                let v = vertexId * 16
                                if vertexData.[v+8+i] = single boneIndex then // already found
                                    found <- true
                                elif vertexData.[v+8+i] < 0.0f then // found free slot
                                    vertexData.[v+8+i] <- single boneIndex
                                    vertexData.[v+12+i] <- weight
                                    found <- true
                                else i <- inc i

                // fin
                Right (vertexData, indexData, bounds)
                    
            // error
            else Left ("Vertex / normal / tex coords count mismatch.")

        // error
        else Left "Mesh is missing vertices, normals, or texCoords."

    /// Create a mesh for a physically-based quad.
    let CreatePhysicallyBasedQuadMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +1.0f; -1.0f; +0.0f;        1.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -1.0f; +1.0f; +0.0f;        0.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -1.0f -1.0f 0.0f) (v3 2.0f 2.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based particle.
    let CreatePhysicallyBasedParticleMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based billboard.
    let CreatePhysicallyBasedBillboardMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based cube.
    let CreatePhysicallyBasedCubeMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)

                // back face
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-left
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-right         
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-right
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-left
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-left

                // front face
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-right
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-right
                -0.5f; +0.5f; +0.5f;        0.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-left

                // left face
                -0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // top-right
                -0.5f; +0.5f; -0.5f;        1.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // top-left
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // bottom-right
                -0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // top-right

                // right face
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // top-left
                +0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // top-right         
                +0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // top-left
                +0.5f; -0.5f; +0.5f;        0.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // bottom-left     

                // bottom face
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-right
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-right

                // top face
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-left
                +0.5f; +0.5f ;+0.5f;        1.0f; 0.0f;          0.0f; +1.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-right     
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;          0.0f; +1.0f;  0.0f; // bottom-right
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-left
                -0.5f; +0.5f; +0.5f;        0.0f; 0.0f;          0.0f; +1.0f;  0.0f  // bottom-left     
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -0.5f) v3One

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based static geometry from a mesh.
    let CreatePhysicallyBasedStaticGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                let strideSize = Constants.Render.InstanceFieldCount * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceDataPtr = GCHandle.Alloc (m4Identity.ToArray (), GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0)
                Gl.VertexAttribDivisor (3u, 1u)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (4u, 1u)
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (5u, 1u)
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

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
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Attempt to create physically-based static geometry from an assimp mesh.
    let TryCreatePhysicallyBasedStaticGeometry (renderable, indexData, mesh : Assimp.Mesh) =
        match TryCreatePhysicallyBasedStaticMesh (indexData, mesh) with
        | Right (vertexData, indexData, bounds) -> Right (CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds))
        | Left error -> Left error

    /// Create physically-based animated geometry from a mesh.
    let CreatePhysicallyBasedAnimatedGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let boneIdsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                let weightsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*) + 4 (*weights*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint boneIdsOffset)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint weightsOffset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                let strideSize = Constants.Render.InstanceFieldCount * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceDataPtr = GCHandle.Alloc (m4Identity.ToArray (), GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0)
                Gl.VertexAttribDivisor (5u, 1u)
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Gl.EnableVertexAttribArray 11u
                Gl.VertexAttribPointer (11u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (11u, 1u)
                Gl.EnableVertexAttribArray 12u
                Gl.VertexAttribPointer (12u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (12u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

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
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Attempt to create physically-based animated geometry from an assimp mesh.
    let TryCreatePhysicallyBasedAnimatedGeometry (renderable, indexData, mesh : Assimp.Mesh) =
        match TryCreatePhysicallyBasedAnimatedMesh (indexData, mesh) with
        | Right (vertexData, indexData, bounds) -> Right (CreatePhysicallyBasedAnimatedGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds))
        | Left error -> Left error

    /// Create physically-based terrain geometry from a mesh.
    let CreatePhysicallyBasedTerrainGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let tintOffset =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                let blendsOffset =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*)) * sizeof<single>
                let blends2Offset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*) + 4 (*blends2*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint tintOffset)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint blendsOffset)
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint blends2Offset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                let strideSize = Constants.Render.InstanceFieldCount * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceDataPtr = GCHandle.Alloc (m4Identity.ToArray (), GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0) // model fields
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Gl.EnableVertexAttribArray 11u
                Gl.VertexAttribPointer (11u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (11u, 1u)
                Gl.EnableVertexAttribArray 12u
                Gl.VertexAttribPointer (12u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (12u, 1u)
                Gl.EnableVertexAttribArray 13u
                Gl.VertexAttribPointer (13u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (13u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 19)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 19
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Create physically-based quad.
    let CreatePhysicallyBasedQuad renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedQuadMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based billboard.
    let CreatePhysicallyBasedBillboard renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedBillboardMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based cube.
    let CreatePhysicallyBasedCube renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedCubeMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create a physically-based surface.
    let CreatePhysicallyBasedSurface (surfaceNames, surfaceMetadata, surfaceMatrix, surfaceBounds, properties, material, materialIndex, geometry) =
        PhysicallyBasedSurface.make surfaceNames surfaceMetadata surfaceMatrix surfaceBounds properties material materialIndex geometry

    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if renderable = false.
    let TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureMemo, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureMemo, scene.Materials.[i])
                propertiesAndMaterials.[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Attempt to create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and de-duplicated here, which does have some run-time cost.
    let TryCreatePhysicallyBasedStaticGeometries (renderable, filePath, scene : Assimp.Scene) =
        let meshAndGeometryLists = Dictionary<int * int * Assimp.BoundingBox, (Assimp.Mesh * PhysicallyBasedGeometry) List> HashIdentity.Structural
        let mutable errorOpt = None
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            if Option.isNone errorOpt then
                let mutable found = false
                let meshAndGeometryListOpt = Dictionary.tryFind (mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox) meshAndGeometryLists
                match meshAndGeometryListOpt with
                | Some (meshAndGeometry : (Assimp.Mesh * PhysicallyBasedGeometry) List) ->
                    let mutable enr = meshAndGeometry.GetEnumerator ()
                    while not found && enr.MoveNext () do
                        let (meshCached, geometryCached) = enr.Current
                        if  Linq.Enumerable.SequenceEqual (meshCached.Vertices, mesh.Vertices) && 
                            Linq.Enumerable.SequenceEqual (meshCached.TextureCoordinateChannels.[0], mesh.TextureCoordinateChannels.[0]) && 
                            Linq.Enumerable.SequenceEqual (meshCached.Normals, mesh.Normals) then
                            geometries.Add geometryCached
                            found <- true
                | None -> ()
                if not found then
                    match TryCreatePhysicallyBasedStaticGeometry (renderable, indexData, mesh) with
                    | Right geometry ->
                        match meshAndGeometryListOpt with
                        | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                        | None -> meshAndGeometryLists.[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                        geometries.Add geometry
                    | Left error -> errorOpt <- Some ("Could not load static geometries for mesh in file name '" + filePath + "' due to: " + error)
        match errorOpt with
        | Some error -> Left error
        | None -> Right geometries

    /// Attempt to create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in TryCreatePhysicallyBasedStaticGeometries?
    let TryCreatePhysicallyBasedAnimatedGeometries (renderable, filePath, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            if Option.isNone errorOpt then
                match TryCreatePhysicallyBasedAnimatedGeometry (renderable, indexData, mesh) with
                | Right geometry -> geometries.Add geometry
                | Left error -> errorOpt <- Some ("Could not load animated geometries for mesh in file name '" + filePath + "' due to: " + error)
        match errorOpt with
        | Some error -> Left error
        | None -> Right geometries

    /// Attempt to create physically-based model from a model file with assimp.
    /// Thread-safe if renderable = false.
    let TryCreatePhysicallyBasedModel (renderable, filePath, defaultMaterial, textureMemo, assimpSceneMemo : OpenGL.Assimp.AssimpSceneMemo) =

        // attempt to memoize scene
        let sceneEir =
            match assimpSceneMemo.AssimpScenes.TryGetValue filePath with
            | (false, _) ->

                // attempt to create scene
                use assimp = new Assimp.AssimpContext ()
                try let scene = assimp.ImportFile (filePath, Constants.Assimp.PostProcessSteps)
                    scene.IndexDatasToMetadata () // avoid polluting memory with face data
                    assimpSceneMemo.AssimpScenes.[filePath] <- scene
                    Right scene
                with exn ->
                    Left ("Could not load assimp scene from '" + filePath + "' due to: " + scstring exn)

            // already exists
            | (true, scene) -> Right scene

        // attempt to import from assimp scene
        match sceneEir with
        | Right scene ->
            let dirPath = PathF.GetDirectoryName filePath
            match TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureMemo, scene) with
            | Right materials ->
                let animated = scene.Animations.Count <> 0
                let geometriesEir =
                    if animated
                    then TryCreatePhysicallyBasedAnimatedGeometries (renderable, filePath, scene)
                    else TryCreatePhysicallyBasedStaticGeometries (renderable, filePath, scene)
                match geometriesEir with
                | Right geometries ->

                    // collect light nodes
                    let lightNodes =
                        [|for i in 0 .. dec scene.LightCount do
                            let light = scene.Lights.[i]
                            let node = scene.RootNode.FindNode light.Name
                            yield (light, node)|]

                    // construct bounds and hierarchy
                    // TODO: sanitize incoming names. Corrupted or incompatible names cause subtle hierarchy bugs.
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
                                yield PhysicallyBasedSurface surface|] |>
                            TreeNode)

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
                | Left error -> Left error
            | Left error -> Left ("Could not load materials for static model in file name '" + filePath + "' due to: " + error)
        | Left error -> Left error

    /// Create a physically-based shader.
    let CreatePhysicallyBasedShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let bonesUniforms =
            Array.init Constants.Render.BonesMax $ fun i ->
                Gl.GetUniformLocation (shader, "bones[" + string i + "]")
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightAmbientColorUniform = Gl.GetUniformLocation (shader, "lightAmbientColor")
        let lightAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightAmbientBrightness")
        let lightShadowBiasAcneUniform = Gl.GetUniformLocation (shader, "lightShadowBiasAcne")
        let lightShadowBiasBleedUniform = Gl.GetUniformLocation (shader, "lightShadowBiasBleed")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let roughnessTextureUniform = Gl.GetUniformLocation (shader, "roughnessTexture")
        let metallicTextureUniform = Gl.GetUniformLocation (shader, "metallicTexture")
        let ambientOcclusionTextureUniform = Gl.GetUniformLocation (shader, "ambientOcclusionTexture")
        let emissionTextureUniform = Gl.GetUniformLocation (shader, "emissionTexture")
        let normalTextureUniform = Gl.GetUniformLocation (shader, "normalTexture")
        let heightTextureUniform = Gl.GetUniformLocation (shader, "heightTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let irradianceMapsUniforms =
            Array.init Constants.Render.LightMapsMaxForward $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")
        let environmentFilterMapsUniforms =
            Array.init Constants.Render.LightMapsMaxForward $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")
        let lightOriginsUniform = Gl.GetUniformLocation (shader, "lightOrigins")
        let lightDirectionsUniform = Gl.GetUniformLocation (shader, "lightDirections")
        let lightColorsUniform = Gl.GetUniformLocation (shader, "lightColors")
        let lightBrightnessesUniform = Gl.GetUniformLocation (shader, "lightBrightnesses")
        let lightAttenuationLinearsUniform = Gl.GetUniformLocation (shader, "lightAttenuationLinears")
        let lightAttenuationQuadraticsUniform = Gl.GetUniformLocation (shader, "lightAttenuationQuadratics")
        let lightCutoffsUniform = Gl.GetUniformLocation (shader, "lightCutoffs")
        let lightDirectionalsUniform = Gl.GetUniformLocation (shader, "lightDirectionals")
        let lightConeInnersUniform = Gl.GetUniformLocation (shader, "lightConeInners")
        let lightConeOutersUniform = Gl.GetUniformLocation (shader, "lightConeOuters")
        let lightShadowIndicesUniform = Gl.GetUniformLocation (shader, "lightShadowIndices")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowMatricesUniforms =
            Array.init Constants.Render.ShadowsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          BonesUniforms = bonesUniforms
          EyeCenterUniform = eyeCenterUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightAmbientColorUniform = lightAmbientColorUniform
          LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
          LightShadowBiasAcneUniform = lightShadowBiasAcneUniform
          LightShadowBiasBleedUniform = lightShadowBiasBleedUniform
          AlbedoTextureUniform = albedoTextureUniform
          RoughnessTextureUniform = roughnessTextureUniform
          MetallicTextureUniform = metallicTextureUniform
          AmbientOcclusionTextureUniform = ambientOcclusionTextureUniform
          EmissionTextureUniform = emissionTextureUniform
          NormalTextureUniform = normalTextureUniform
          HeightTextureUniform = heightTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          ShadowTexturesUniforms = shadowTexturesUniforms
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          LightMapsCountUniform = lightMapsCountUniform
          LightOriginsUniform = lightOriginsUniform
          LightDirectionsUniform = lightDirectionsUniform
          LightColorsUniform = lightColorsUniform
          LightBrightnessesUniform = lightBrightnessesUniform
          LightAttenuationLinearsUniform = lightAttenuationLinearsUniform
          LightAttenuationQuadraticsUniform = lightAttenuationQuadraticsUniform
          LightCutoffsUniform = lightCutoffsUniform
          LightDirectionalsUniform = lightDirectionalsUniform
          LightConeInnersUniform = lightConeInnersUniform
          LightConeOutersUniform = lightConeOutersUniform
          LightShadowIndicesUniform = lightShadowIndicesUniform
          LightsCountUniform = lightsCountUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedShader = shader }

    /// Create a physically-based terrain shader.
    let CreatePhysicallyBasedTerrainShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let layersCountUniform = Gl.GetUniformLocation (shader, "layersCount")
        let albedoTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "albedoTextures[" + string i + "]")
        let roughnessTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "roughnessTextures[" + string i + "]")
        let ambientOcclusionTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "ambientOcclusionTextures[" + string i + "]")
        let normalTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "normalTextures[" + string i + "]")
        let heightTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "heightTextures[" + string i + "]")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          EyeCenterUniform = eyeCenterUniform
          LayersCountUniform = layersCountUniform
          AlbedoTexturesUniforms = albedoTexturesUniforms
          RoughnessTexturesUniforms = roughnessTexturesUniforms
          AmbientOcclusionTexturesUniforms = ambientOcclusionTexturesUniforms
          NormalTexturesUniforms = normalTexturesUniforms
          HeightTexturesUniforms = heightTexturesUniforms
          PhysicallyBasedShader = shader } : PhysicallyBasedDeferredTerrainShader

    /// Create a physically-based shader for the light mapping pass of deferred rendering.
    let CreatePhysicallyBasedDeferredLightMappingShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")

        // make shader record
        { PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          LightMapsCountUniform = lightMapsCountUniform
          PhysicallyBasedDeferredLightMappingShader = shader }

    /// Create a physically-based shader for the irradiance pass of deferred rendering.
    let CreatePhysicallyBasedDeferredIrradianceShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let irradianceMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")

        // make shader record
        { PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          PhysicallyBasedDeferredIrradianceShader = shader }

    /// Create a physically-based shader for the environment filter pass of deferred rendering.
    let CreatePhysicallyBasedDeferredEnvironmentFilterShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let environmentFilterMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          PositionTextureUniform = positionTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          PhysicallyBasedDeferredEnvironmentFilterShader = shader }

    /// Create a physically-based shader for the ssao pass of deferred rendering.
    let CreatePhysicallyBasedDeferredSsaoShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let ssaoResolution = Gl.GetUniformLocation (shader, "ssaoResolution")
        let ssaoIntensity = Gl.GetUniformLocation (shader, "ssaoIntensity")
        let ssaoBias = Gl.GetUniformLocation (shader, "ssaoBias")
        let ssaoRadius = Gl.GetUniformLocation (shader, "ssaoRadius")
        let ssaoDistanceMax = Gl.GetUniformLocation (shader, "ssaoDistanceMax")
        let ssaoSampleCount = Gl.GetUniformLocation (shader, "ssaoSampleCount")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          SsaoResolution = ssaoResolution
          SsaoIntensity = ssaoIntensity
          SsaoBias = ssaoBias
          SsaoRadius = ssaoRadius
          SsaoDistanceMax = ssaoDistanceMax
          SsaoSampleCount = ssaoSampleCount
          PhysicallyBasedDeferredSsaoShader = shader }

    /// Create a physically-based shader for the lighting pass of deferred rendering.
    let CreatePhysicallyBasedDeferredLightingShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightAmbientColorUniform = Gl.GetUniformLocation (shader, "lightAmbientColor")
        let lightAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightAmbientBrightness")
        let lightShadowBiasAcneUniform = Gl.GetUniformLocation (shader, "lightShadowBiasAcne")
        let lightShadowBiasBleedUniform = Gl.GetUniformLocation (shader, "lightShadowBiasBleed")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let irradianceTextureUniform = Gl.GetUniformLocation (shader, "irradianceTexture")
        let environmentFilterTextureUniform = Gl.GetUniformLocation (shader, "environmentFilterTexture")
        let ssaoTextureUniform = Gl.GetUniformLocation (shader, "ssaoTexture")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let lightOriginsUniform = Gl.GetUniformLocation (shader, "lightOrigins")
        let lightDirectionsUniform = Gl.GetUniformLocation (shader, "lightDirections")
        let lightColorsUniform = Gl.GetUniformLocation (shader, "lightColors")
        let lightBrightnessesUniform = Gl.GetUniformLocation (shader, "lightBrightnesses")
        let lightAttenuationLinearsUniform = Gl.GetUniformLocation (shader, "lightAttenuationLinears")
        let lightAttenuationQuadraticsUniform = Gl.GetUniformLocation (shader, "lightAttenuationQuadratics")
        let lightCutoffsUniform = Gl.GetUniformLocation (shader, "lightCutoffs")
        let lightDirectionalsUniform = Gl.GetUniformLocation (shader, "lightDirectionals")
        let lightConeInnersUniform = Gl.GetUniformLocation (shader, "lightConeInners")
        let lightConeOutersUniform = Gl.GetUniformLocation (shader, "lightConeOuters")
        let lightShadowIndicesUniform = Gl.GetUniformLocation (shader, "lightShadowIndices")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowMatricesUniforms =
            Array.init Constants.Render.ShadowsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightAmbientColorUniform = lightAmbientColorUniform
          LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
          LightShadowBiasAcneUniform = lightShadowBiasAcneUniform
          LightShadowBiasBleedUniform = lightShadowBiasBleedUniform
          PositionTextureUniform = positionTextureUniform
          AlbedoTextureUniform = albedoTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          IrradianceTextureUniform = irradianceTextureUniform
          EnvironmentFilterTextureUniform = environmentFilterTextureUniform
          SsaoTextureUniform = ssaoTextureUniform
          ShadowTexturesUniforms = shadowTexturesUniforms
          LightOriginsUniform = lightOriginsUniform
          LightDirectionsUniform = lightDirectionsUniform
          LightColorsUniform = lightColorsUniform
          LightBrightnessesUniform = lightBrightnessesUniform
          LightAttenuationLinearsUniform = lightAttenuationLinearsUniform
          LightAttenuationQuadraticsUniform = lightAttenuationQuadraticsUniform
          LightCutoffsUniform = lightCutoffsUniform
          LightDirectionalsUniform = lightDirectionalsUniform
          LightConeInnersUniform = lightConeInnersUniform
          LightConeOutersUniform = lightConeOutersUniform
          LightShadowIndicesUniform = lightShadowIndicesUniform
          LightsCountUniform = lightsCountUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedDeferredLightingShader = shader }

    /// Create the shaders for physically-based shadow rendering.
    let CreatePhysicallyBasedShadowShaders (shaderStaticShadowFilePath, shaderAnimatedShadowFilePath, shaderTerrainShadowFilePath) =
        let shaderStaticShadow = CreatePhysicallyBasedShader shaderStaticShadowFilePath
        Hl.Assert ()
        let shaderAnimatedShadow = CreatePhysicallyBasedShader shaderAnimatedShadowFilePath
        Hl.Assert ()
        let shaderTerrainShadow = CreatePhysicallyBasedTerrainShader shaderTerrainShadowFilePath
        (shaderStaticShadow, shaderAnimatedShadow, shaderTerrainShadow)

    /// Create the shaders for physically-based deferred rendering.
    let CreatePhysicallyBasedDeferredShaders (shaderStaticFilePath, shaderAnimatedFilePath, terrainShaderFilePath, shaderLightMappingFilePath, shaderIrradianceFilePath, shaderEnvironmentFilterFilePath, shaderSsaoFilePath, shaderLightingFilePath) =
        let shaderStatic = CreatePhysicallyBasedShader shaderStaticFilePath
        Hl.Assert ()
        let shaderAnimated = CreatePhysicallyBasedShader shaderAnimatedFilePath
        Hl.Assert ()
        let shaderTerrain = CreatePhysicallyBasedTerrainShader terrainShaderFilePath
        Hl.Assert ()
        let shaderLightMapping = CreatePhysicallyBasedDeferredLightMappingShader shaderLightMappingFilePath
        Hl.Assert ()
        let shaderIrradiance = CreatePhysicallyBasedDeferredIrradianceShader shaderIrradianceFilePath
        Hl.Assert ()
        let shaderEnvironmentFilter = CreatePhysicallyBasedDeferredEnvironmentFilterShader shaderEnvironmentFilterFilePath
        Hl.Assert ()
        let shaderSsao = CreatePhysicallyBasedDeferredSsaoShader shaderSsaoFilePath
        Hl.Assert ()
        let shaderLighting = CreatePhysicallyBasedDeferredLightingShader shaderLightingFilePath
        (shaderStatic, shaderAnimated, shaderTerrain, shaderLightMapping, shaderIrradiance, shaderEnvironmentFilter, shaderSsao, shaderLighting)

    /// Draw the filter box pass using a physically-based surface.
    let DrawFilterBoxSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBoxShader) =

        // setup shader
        Gl.UseProgram shader.FilterBoxShader
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.InputTextureUniform, inputTexture.TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the filter gaussian pass using a physically-based surface.
    let DrawFilterGaussianSurface
        (scale : Vector2,
         inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterGaussianShader) =

        // setup shader
        Gl.UseProgram shader.FilterGaussianShader
        Gl.Uniform2 (shader.ScaleUniform, scale.X, scale.Y)
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.InputTextureUniform, inputTexture.TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the filter fxaa pass using a physically-based surface.
    let DrawFilterFxaaSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterFxaaShader) =

        // setup shader
        Gl.UseProgram shader.FilterFxaaShader
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.InputTextureUniform, inputTexture.TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw a batch of physically-based surfaces' shadows.
    let DrawPhysicallyBasedShadowSurfaces
        (batchPhase : BatchPhase,
         view : single array,
         projection : single array,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // start batch
        if batchPhase.Starting then

            // setup state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            Hl.Assert ()

        // update instance buffer
        let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
        try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
            Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
            Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
            Hl.Assert ()
        finally instanceFieldsPtr.Free ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
        Hl.ReportDrawCall surfacesCount
        Hl.Assert ()

        // stop batch
        if batchPhase.Stopping then

            // teardown geometry
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown state
            Gl.DepthFunc DepthFunction.Less
            Gl.Disable EnableCap.DepthTest

    /// Draw a batch of physically-based deferred surfaces.
    let DrawPhysicallyBasedDeferredSurfaces
        (batchPhase : BatchPhase,
         view : single array,
         projection : single array,
         bones : single array array,
         eyeCenter : Vector3,
         surfacesCount : int,
         instanceFields : single array,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // setup dynamic state
        if not material.TwoSided then
            Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // start batch
        if batchPhase.Starting then

            // setup static state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
            Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.AlbedoTextureUniform, material.AlbedoTexture.TextureHandle)
        Gl.UniformHandleARB (shader.RoughnessTextureUniform, material.RoughnessTexture.TextureHandle)
        Gl.UniformHandleARB (shader.MetallicTextureUniform, material.MetallicTexture.TextureHandle)
        Gl.UniformHandleARB (shader.AmbientOcclusionTextureUniform, material.AmbientOcclusionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.EmissionTextureUniform, material.EmissionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalTextureUniform, material.NormalTexture.TextureHandle)
        Gl.UniformHandleARB (shader.HeightTextureUniform, material.HeightTexture.TextureHandle)
        Hl.Assert ()

        // update instance buffer
        let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
        try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
            Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
            Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
            Hl.Assert ()
        finally instanceFieldsPtr.Free ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
        Hl.ReportDrawCall surfacesCount
        Hl.Assert ()

        // stop batch
        if batchPhase.Stopping then

            // teardown geometry in general
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown static state
            Gl.DepthFunc DepthFunction.Less
            Gl.Disable EnableCap.DepthTest
            Hl.Assert ()

        // teardown dynamic state
        if not material.TwoSided then
            Gl.Disable EnableCap.CullFace

    /// Draw a batch of physically-based forward surfaces.
    let DrawPhysicallyBasedForwardSurfaces
        (blending : bool,
         view : single array,
         projection : single array,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         eyeCenter : Vector3,
         lightCutoffMargin : single,
         lightAmbientColor : single array,
         lightAmbientBrightness : single,
         lightShadowBiasAcne : single,
         lightShadowBiasBleed : single,
         brdfTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         irradianceMaps : Texture.Texture array,
         environmentFilterMaps : Texture.Texture array,
         shadowTextures : Texture.Texture array,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         lightMapsCount : int,
         lightOrigins : single array,
         lightDirections : single array,
         lightColors : single array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightDirectionals : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        if blending then
            Gl.BlendEquation BlendEquationMode.FuncAdd
            Gl.BlendFunc (BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha)
            Gl.Enable EnableCap.Blend
        if not material.TwoSided then Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
            Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
        if lightAmbientColor.Length = 3 then
            Gl.Uniform3 (shader.LightAmbientColorUniform, lightAmbientColor)
        Gl.Uniform1 (shader.LightAmbientBrightnessUniform, lightAmbientBrightness)
        Gl.Uniform1 (shader.LightShadowBiasAcneUniform, lightShadowBiasAcne)
        Gl.Uniform1 (shader.LightShadowBiasBleedUniform, lightShadowBiasBleed)
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
        Gl.Uniform3 (shader.LightOriginsUniform, lightOrigins)
        Gl.Uniform3 (shader.LightDirectionsUniform, lightDirections)
        Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        Gl.Uniform1 (shader.LightBrightnessesUniform, lightBrightnesses)
        Gl.Uniform1 (shader.LightAttenuationLinearsUniform, lightAttenuationLinears)
        Gl.Uniform1 (shader.LightAttenuationQuadraticsUniform, lightAttenuationQuadratics)
        Gl.Uniform1 (shader.LightCutoffsUniform, lightCutoffs)
        Gl.Uniform1 (shader.LightDirectionalsUniform, lightDirectionals)
        Gl.Uniform1 (shader.LightConeInnersUniform, lightConeInners)
        Gl.Uniform1 (shader.LightConeOutersUniform, lightConeOuters)
        Gl.Uniform1 (shader.LightShadowIndicesUniform, lightShadowIndices)
        Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
        for i in 0 .. dec (min Constants.Render.ShadowsMax shadowMatrices.Length) do
            Gl.UniformMatrix4 (shader.ShadowMatricesUniforms.[i], false, shadowMatrices.[i])
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.AlbedoTextureUniform, material.AlbedoTexture.TextureHandle)
        Gl.UniformHandleARB (shader.RoughnessTextureUniform, material.RoughnessTexture.TextureHandle)
        Gl.UniformHandleARB (shader.MetallicTextureUniform, material.MetallicTexture.TextureHandle)
        Gl.UniformHandleARB (shader.AmbientOcclusionTextureUniform, material.AmbientOcclusionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.EmissionTextureUniform, material.EmissionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalTextureUniform, material.NormalTexture.TextureHandle)
        Gl.UniformHandleARB (shader.HeightTextureUniform, material.HeightTexture.TextureHandle)
        Gl.UniformHandleARB (shader.BrdfTextureUniform, brdfTexture.TextureHandle)
        Gl.UniformHandleARB (shader.IrradianceMapUniform, irradianceMap.TextureHandle)
        Gl.UniformHandleARB (shader.EnvironmentFilterMapUniform, environmentFilterMap.TextureHandle)
        for i in 0 .. dec (min irradianceMaps.Length Constants.Render.LightMapsMaxForward) do
            Gl.UniformHandleARB (shader.IrradianceMapsUniforms.[i], irradianceMaps.[i].TextureHandle)
        for i in 0 .. dec (min environmentFilterMaps.Length Constants.Render.LightMapsMaxForward) do
            Gl.UniformHandleARB (shader.EnvironmentFilterMapsUniforms.[i], environmentFilterMaps.[i].TextureHandle)
        for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowsMax) do
            Gl.UniformHandleARB (shader.ShadowTexturesUniforms.[i], shadowTextures.[i].TextureHandle)
        Hl.Assert ()

        // update instance buffer
        let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
        try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
            Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
            Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
            Hl.Assert ()
        finally instanceFieldsPtr.Free ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
        Hl.ReportDrawCall surfacesCount
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest
        if blending then
            Gl.Disable EnableCap.Blend
            Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)
            Gl.BlendEquation BlendEquationMode.FuncAdd
        if not material.TwoSided then Gl.Disable EnableCap.CullFace

    let DrawPhysicallyBasedTerrain
        (view : single array,
         projection : single array,
         eyeCenter : Vector3,
         instanceFields : single array,
         elementsCount : int,
         materials : PhysicallyBasedMaterial array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredTerrainShader) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // enforce layer limit
        let layersCount = min materials.Length Constants.Render.TerrainLayersMax
        
        // setup shader
        Gl.UseProgram shader.PhysicallyBasedShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform1 (shader.LayersCountUniform, layersCount)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.AlbedoTexturesUniforms.[i], i)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.RoughnessTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.AmbientOcclusionTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 2)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.NormalTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 3)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.HeightTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 4)
        Hl.Assert ()

        // setup textures
        for i in 0 .. dec layersCount do
            Gl.UniformHandleARB (shader.AlbedoTexturesUniforms.[i], materials.[i].AlbedoTexture.TextureHandle)
        for i in 0 .. dec layersCount do
            Gl.UniformHandleARB (shader.RoughnessTexturesUniforms.[i], materials.[i].RoughnessTexture.TextureHandle)
        for i in 0 .. dec layersCount do
            Gl.UniformHandleARB (shader.AmbientOcclusionTexturesUniforms.[i], materials.[i].AmbientOcclusionTexture.TextureHandle)
        for i in 0 .. dec layersCount do
            Gl.UniformHandleARB (shader.NormalTexturesUniforms.[i], materials.[i].NormalTexture.TextureHandle)
        for i in 0 .. dec layersCount do
            Gl.UniformHandleARB (shader.AmbientOcclusionTexturesUniforms.[i], materials.[i].AmbientOcclusionTexture.TextureHandle)
        Hl.Assert ()

        // update instance buffer
        let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
        try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
            Gl.BufferData (BufferTarget.ArrayBuffer, uint (Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
            Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
            Hl.Assert ()
        finally instanceFieldsPtr.Free ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, elementsCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()
        
        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest
        Gl.Disable EnableCap.CullFace

    /// Draw the light mapping pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightMappingSurface
        (positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         lightMapsCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightMappingShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightMappingShader
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.PositionTextureUniform, positionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalPlusTextureUniform, normalPlusTexture.TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.ValidateProgram shader.PhysicallyBasedDeferredLightMappingShader
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the irradiance pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredIrradianceSurface
        (positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         irradianceMaps : Texture.Texture array,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredIrradianceShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredIrradianceShader
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.PositionTextureUniform, positionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalPlusTextureUniform, normalPlusTexture.TextureHandle)
        Gl.UniformHandleARB (shader.LightMappingTextureUniform, lightMappingTexture.TextureHandle)
        Gl.UniformHandleARB (shader.IrradianceMapUniform, irradianceMap.TextureHandle)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.UniformHandleARB (shader.IrradianceMapsUniforms.[i], irradianceMaps.[i].TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the environment filter pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredEnvironmentFilterSurface
        (eyeCenter : Vector3,
         positionTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         environmentFilterMaps : Texture.Texture array,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredEnvironmentFilterShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredEnvironmentFilterShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.PositionTextureUniform, positionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.MaterialTextureUniform, materialTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalPlusTextureUniform, normalPlusTexture.TextureHandle)
        Gl.UniformHandleARB (shader.LightMappingTextureUniform, lightMappingTexture.TextureHandle)
        Gl.UniformHandleARB (shader.EnvironmentFilterMapUniform, environmentFilterMap.TextureHandle)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.UniformHandleARB (shader.EnvironmentFilterMapsUniforms.[i], environmentFilterMaps.[i].TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the ssao pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredSsaoSurface
        (view : single array,
         projection : single array,
         positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         ssaoResolution : int array,
         ssaoIntensity : single,
         ssaoBias : single,
         ssaoRadius : single,
         ssaoDistanceMax : single,
         ssaoSampleCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredSsaoShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredSsaoShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform2 (shader.SsaoResolution, ssaoResolution)
        Gl.Uniform1 (shader.SsaoIntensity, ssaoIntensity)
        Gl.Uniform1 (shader.SsaoBias, ssaoBias)
        Gl.Uniform1 (shader.SsaoRadius, ssaoRadius)
        Gl.Uniform1 (shader.SsaoDistanceMax, ssaoDistanceMax)
        Gl.Uniform1 (shader.SsaoSampleCount, ssaoSampleCount)
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.PositionTextureUniform, positionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalPlusTextureUniform, normalPlusTexture.TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the lighting pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3,
         lightCutoffMargin : single,
         lightAmbientColor : single array,
         lightAmbientBrightness : single,
         lightShadowBiasAcne : single,
         lightShadowBiasBleed : single,
         positionTexture : Texture.Texture,
         albedoTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         brdfTexture : Texture.Texture,
         irradianceTexture : Texture.Texture,
         environmentFilterTexture : Texture.Texture,
         ssaoTexture : Texture.Texture,
         shadowTextures : Texture.Texture array,
         lightOrigins : single array,
         lightDirections : single array,
         lightColors : single array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightDirectionals : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightingShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightingShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
        Gl.Uniform3 (shader.LightAmbientColorUniform, lightAmbientColor)
        Gl.Uniform1 (shader.LightAmbientBrightnessUniform, lightAmbientBrightness)
        Gl.Uniform1 (shader.LightShadowBiasAcneUniform, lightShadowBiasAcne)
        Gl.Uniform1 (shader.LightShadowBiasBleedUniform, lightShadowBiasBleed)
        Gl.Uniform3 (shader.LightOriginsUniform, lightOrigins)
        Gl.Uniform3 (shader.LightDirectionsUniform, lightDirections)
        Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        Gl.Uniform1 (shader.LightBrightnessesUniform, lightBrightnesses)
        Gl.Uniform1 (shader.LightAttenuationLinearsUniform, lightAttenuationLinears)
        Gl.Uniform1 (shader.LightAttenuationQuadraticsUniform, lightAttenuationQuadratics)
        Gl.Uniform1 (shader.LightCutoffsUniform, lightCutoffs)
        Gl.Uniform1 (shader.LightDirectionalsUniform, lightDirectionals)
        Gl.Uniform1 (shader.LightConeInnersUniform, lightConeInners)
        Gl.Uniform1 (shader.LightConeOutersUniform, lightConeOuters)
        Gl.Uniform1 (shader.LightShadowIndicesUniform, lightShadowIndices)
        Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
        for i in 0 .. dec (min Constants.Render.ShadowsMax shadowMatrices.Length) do
            Gl.UniformMatrix4 (shader.ShadowMatricesUniforms.[i], false, shadowMatrices.[i])
        Hl.Assert ()

        // setup textures
        Gl.UniformHandleARB (shader.PositionTextureUniform, positionTexture.TextureHandle)
        Gl.UniformHandleARB (shader.AlbedoTextureUniform, albedoTexture.TextureHandle)
        Gl.UniformHandleARB (shader.MaterialTextureUniform, materialTexture.TextureHandle)
        Gl.UniformHandleARB (shader.NormalPlusTextureUniform, normalPlusTexture.TextureHandle)
        Gl.UniformHandleARB (shader.BrdfTextureUniform, brdfTexture.TextureHandle)
        Gl.UniformHandleARB (shader.IrradianceTextureUniform, irradianceTexture.TextureHandle)
        Gl.UniformHandleARB (shader.EnvironmentFilterTextureUniform, environmentFilterTexture.TextureHandle)
        Gl.UniformHandleARB (shader.SsaoTextureUniform, ssaoTexture.TextureHandle)
        for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowsMax) do
            Gl.UniformHandleARB (shader.ShadowTexturesUniforms[i], shadowTextures[i].TextureHandle)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Destroy physically-based geometry resources.
    let DestroyPhysicallyBasedGeometry geometry =
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.DeleteBuffers [|geometry.VertexBuffer|]
        Gl.DeleteBuffers [|geometry.InstanceBuffer|]
        Gl.DeleteBuffers [|geometry.IndexBuffer|]
        Gl.BindVertexArray 0u
        Gl.DeleteVertexArrays [|geometry.PhysicallyBasedVao|]

    /// Destroy physically-based model resources.
    let DestroyPhysicallyBasedModel (model : PhysicallyBasedModel) =
        for surface in model.Surfaces do
            DestroyPhysicallyBasedGeometry surface.PhysicallyBasedGeometry