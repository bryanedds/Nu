// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open SDL2
open Prime
open Nu

//////////////////////////////////////////////////////////////////////////////////////////
// TODO: introduce records for a bunch of the tuples in this file!                      //
// TODO: account for Blend in billboards (at least alpha, overwrite, and additive)      //
// TODO: account for Flip in billboards.                                                //
// TODO: optimize billboard rendering with some sort of batch renderer.                 //
//////////////////////////////////////////////////////////////////////////////////////////

/// Materials used for rendering models.
type [<StructuralEquality; NoComparison; Struct>] RenderMaterial =
    { AlbedoOpt : Color voption
      MetalnessOpt : single voption
      RoughnessOpt : single voption
      EmissionOpt : single voption
      AmbientOcclusionOpt : single voption }

    static member empty =
        { AlbedoOpt = ValueNone
          MetalnessOpt = ValueNone
          RoughnessOpt = ValueNone
          EmissionOpt = ValueNone
          AmbientOcclusionOpt = ValueNone }

/// Describes billboard-based particles.
type [<NoEquality; NoComparison>] BillboardParticlesDescriptor =
    { Absolute : bool
      RenderMaterial : RenderMaterial
      AlbedoImage : Image AssetTag
      MetalnessImage : Image AssetTag
      RoughnessImage : Image AssetTag
      EmissionImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      NormalImage : Image AssetTag
      MinFilterOpt : OpenGL.TextureMinFilter voption
      MagFilterOpt : OpenGL.TextureMagFilter voption
      RenderType : RenderType
      Particles : Particle SegmentedArray }

/// A collection of render tasks in a pass.
and [<ReferenceEquality>] RenderTasks =
    { RenderSkyBoxes : CubeMap AssetTag SegmentedList
      RenderLights : SortableLight SegmentedList
      RenderSurfacesDeferredAbsolute : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Box2 * RenderMaterial) SegmentedList>
      RenderSurfacesDeferredRelative : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Box2 * RenderMaterial) SegmentedList>
      RenderSurfacesForwardAbsolute : struct (single * single * Matrix4x4 * Box2 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardRelative : struct (single * single * Matrix4x4 * Box2 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardAbsoluteSorted : struct (Matrix4x4 * Box2 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardRelativeSorted : struct (Matrix4x4 * Box2 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList }

/// The parameters for completing a render pass.
and [<ReferenceEquality>] RenderPassParameters3d =
    { EyeCenter : Vector3
      EyeRotation : Quaternion
      ViewAbsolute : Matrix4x4
      ViewRelative : Matrix4x4
      ViewSkyBox : Matrix4x4
      Viewport : Viewport
      Projection : Matrix4x4
      RenderTasks : RenderTasks
      Renderer3d : Renderer3d }

/// A 3d render pass message.
and [<CustomEquality; CustomComparison>] RenderPassMessage3d =
    { RenderPassOrder : int64
      RenderPassParameters3d : RenderPassParameters3d -> unit }
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? RenderPassMessage3d as that -> this.RenderPassOrder.CompareTo that.RenderPassOrder
            | _ -> failwithumf ()
    override this.Equals (that : obj) =
        match that with
        | :? RenderPassMessage3d as that -> this.RenderPassOrder = that.RenderPassOrder
        | _ -> false
    override this.GetHashCode () = hash this.RenderPassOrder

/// An internally cached static model used to avoid GC promotion of static model messages.
and [<ReferenceEquality>] CachedStaticModelMessage =
    { mutable CachedStaticModelAbsolute : bool
      mutable CachedStaticModelAffineMatrix : Matrix4x4
      mutable CachedStaticModelInsetOpt : Box2 voption
      mutable CachedStaticModelRenderMaterial : RenderMaterial
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModel : StaticModel AssetTag }

/// Describes a user-defined static model surface.
and [<ReferenceEquality>] StaticModelSurfaceDescriptor =
    { Positions : Vector3 array
      TexCoordses : Vector2 array
      Normals : Vector3 array
      Indices : int array
      AffineMatrix : Matrix4x4
      Bounds : Box3
      Albedo : Color
      AlbedoImage : Image AssetTag
      Metalness : single
      MetalnessImage : Image AssetTag
      Roughness : single
      RoughnessImage : Image AssetTag
      Emission : single
      EmissionImage : Image AssetTag
      AmbientOcclusion : single
      AmbientOcclusionImage : Image AssetTag
      NormalImage : Image AssetTag
      TextureMinFilterOpt : OpenGL.TextureMinFilter voption
      TextureMagFilterOpt : OpenGL.TextureMagFilter voption
      TwoSided : bool }

/// A message to the 3d renderer.
and [<ReferenceEquality>] RenderMessage3d =
    | CreateUserDefinedStaticModel of StaticModelSurfaceDescriptor array * Box3 * StaticModel AssetTag
    | DestroyUserDefinedStaticModel of StaticModel AssetTag
    | RenderSkyBox of CubeMap AssetTag
    | RenderLight3d of Vector3 * Color * single * single * LightType
    | RenderBillboard of bool * Matrix4x4 * Box2 voption * RenderMaterial * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * OpenGL.TextureMinFilter voption * OpenGL.TextureMagFilter voption * RenderType
    | RenderBillboards of bool * (Matrix4x4 * Box2 voption) SegmentedList * RenderMaterial * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * OpenGL.TextureMinFilter voption * OpenGL.TextureMagFilter voption * RenderType
    | RenderBillboardParticles of bool * RenderMaterial * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * Image AssetTag * OpenGL.TextureMinFilter voption * OpenGL.TextureMagFilter voption * RenderType * Particle SegmentedArray
    | RenderStaticModelSurface of bool * Matrix4x4 * Box2 voption * RenderMaterial * RenderType * StaticModel AssetTag * int
    | RenderStaticModel of bool * Matrix4x4 * Box2 voption * RenderMaterial * RenderType * StaticModel AssetTag
    | RenderStaticModels of bool * (Matrix4x4 * Box2 voption * RenderMaterial) SegmentedList * RenderType * StaticModel AssetTag
    | RenderCachedStaticModel of CachedStaticModelMessage
    | RenderUserDefinedStaticModel of bool * Matrix4x4 * Box2 voption * RenderMaterial * RenderType * StaticModelSurfaceDescriptor array * Box3
    | RenderPostPass3d of RenderPassMessage3d
    | LoadRenderPackage3d of string
    | UnloadRenderPackage3d of string
    | ReloadRenderAssets3d

/// A sortable light.
/// OPTIMIZATION: mutable field for caching distance squared.
and [<ReferenceEquality>] SortableLight =
    { SortableLightOrigin : Vector3
      SortableLightColor : Color
      SortableLightBrightness : single
      SortableLightIntensity : single
      mutable SortableLightDistanceSquared : single }

    /// Sort lights into array for uploading to OpenGL.
    /// TODO: consider getting rid of allocation here.
    static member sortLightsIntoArrays position lights =
        let lightOrigins = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax * 3)
        let lightColors = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax * 4)
        let lightBrightnesses = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax)
        let lightIntensities = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax)
        for light in lights do
            light.SortableLightDistanceSquared <- (light.SortableLightOrigin - position).MagnitudeSquared
        let lightsSorted = lights |> Seq.toArray |> Array.sortBy (fun light -> light.SortableLightDistanceSquared)
        for i in 0 .. dec Constants.Render.ShaderLightsMax do
            if i < lightsSorted.Length then
                let p = i * 3
                let c = i * 4
                let b = i
                let n = i
                let light = lightsSorted.[i]
                lightOrigins.[p] <- light.SortableLightOrigin.X
                lightOrigins.[p+1] <- light.SortableLightOrigin.Y
                lightOrigins.[p+2] <- light.SortableLightOrigin.Z
                lightBrightnesses.[b] <- light.SortableLightBrightness
                lightIntensities.[n] <- light.SortableLightIntensity
                lightColors.[c] <- light.SortableLightColor.R
                lightColors.[c+1] <- light.SortableLightColor.G
                lightColors.[c+2] <- light.SortableLightColor.B
                lightColors.[c+3] <- light.SortableLightColor.A
        (lightOrigins, lightColors, lightBrightnesses, lightIntensities)

/// The 3d renderer. Represents the 3d rendering system in Nu generally.
and Renderer3d =
    inherit Renderer
    /// The physically-based shader.
    abstract PhysicallyBasedShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
    /// Render a frame of the game.
    abstract Render : Vector3 -> Quaternion -> Vector2i -> RenderMessage3d List -> unit
    /// Swap a rendered frame of the game.
    abstract Swap : unit -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The mock implementation of Renderer3d.
type [<ReferenceEquality>] MockRenderer3d =
    private
        { MockRenderer3d : unit }

    interface Renderer3d with
        member renderer.PhysicallyBasedShader = Unchecked.defaultof<_>
        member renderer.Render _ _ _ _ = ()
        member renderer.Swap () = ()
        member renderer.CleanUp () = ()

    static member make () =
        { MockRenderer3d = () }

/// The internally used package state for the 3d OpenGL renderer.
type [<ReferenceEquality>] private GlPackageState3d =
    { TextureMemo : OpenGL.Texture.TextureMemo
      CubeMapMemo : OpenGL.CubeMap.CubeMapMemo }

/// The OpenGL implementation of Renderer3d.
type [<ReferenceEquality>] GlRenderer3d =
    private
        { RenderWindow : Window
          RenderAssimp : Assimp.AssimpContext
          RenderSkyBoxShader : OpenGL.SkyBox.SkyBoxShader
          RenderIrradianceShader : OpenGL.SkyBox.SkyBoxShader
          RenderEnvironmentFilterShader : OpenGL.SkyBox.EnvironmentFilterShader
          RenderPhysicallyBasedForwardShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          RenderPhysicallyBasedDeferredShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          RenderPhysicallyBasedDeferred2Shader : OpenGL.PhysicallyBased.PhysicallyBasedDeferred2Shader
          RenderIrradianceFramebuffer : uint * uint
          RenderEnvironmentFilterFramebuffer : uint * uint
          RenderGeometryFramebuffer : uint * uint * uint * uint * uint
          RenderSkyBoxGeometry : OpenGL.SkyBox.SkyBoxGeometry
          RenderBillboardGeometry : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          RenderPhysicallyBasedQuad : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          RenderIrradianceMap : uint
          RenderEnvironmentFilterMap : uint
          RenderBrdfTexture : uint
          RenderPhysicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial
          mutable RenderModelsFields : single array
          mutable RenderTexCoordsOffsetsFields : single array
          mutable RenderAlbedosFields : single array
          mutable RenderMaterialsFields : single array
          mutable RenderUserDefinedStaticModelFields : single array
          RenderTasks : RenderTasks
          RenderPackages : Packages<RenderAsset, GlPackageState3d>
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          RenderMessages : RenderMessage3d List
          RenderShouldBeginFrame : bool
          RenderShouldEndFrame : bool }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset packageState renderAsset renderer =
        GlRenderer3d.invalidateCaches renderer
        match renderAsset with
        | TextureAsset (filePath, _, _) ->
            OpenGL.Texture.DeleteTextureMemoized filePath packageState.TextureMemo
            OpenGL.Hl.Assert ()
        | FontAsset (_, _, font) ->
            SDL_ttf.TTF_CloseFont font
        | CubeMapAsset (cubeMapFilePaths, _, _) ->
            OpenGL.CubeMap.DeleteCubeMapMemoized cubeMapFilePaths packageState.CubeMapMemo
            OpenGL.Hl.Assert ()
        | StaticModelAsset (_, staticModel) ->
            OpenGL.PhysicallyBased.DestroyPhysicallyBasedStaticModel staticModel
            OpenGL.Hl.Assert ()

    static member private tryLoadTextureAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match OpenGL.Texture.TryCreateTextureMemoizedFiltered (asset.FilePath, packageState.TextureMemo) with
        | Right (textureMetadata, texture) ->
            Some (asset.FilePath, textureMetadata, texture)
        | Left error ->
            Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
            None

    static member private tryLoadCubeMapAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match File.ReadAllLines asset.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
        | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
            let dirPath = Path.GetDirectoryName asset.FilePath
            let faceRightFilePath = dirPath + "/" + faceRightFilePath |> fun str -> str.Trim ()
            let faceLeftFilePath = dirPath + "/" + faceLeftFilePath |> fun str -> str.Trim ()
            let faceTopFilePath = dirPath + "/" + faceTopFilePath |> fun str -> str.Trim ()
            let faceBottomFilePath = dirPath + "/" + faceBottomFilePath |> fun str -> str.Trim ()
            let faceBackFilePath = dirPath + "/" + faceBackFilePath |> fun str -> str.Trim ()
            let faceFrontFilePath = dirPath + "/" + faceFrontFilePath |> fun str -> str.Trim ()
            let cubeMapMemoKey = (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)
            match OpenGL.CubeMap.TryCreateCubeMapMemoized (cubeMapMemoKey, packageState.CubeMapMemo) with
            | Right cubeMap -> Some (cubeMapMemoKey, cubeMap, ref None)
            | Left error -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to: " + error); None
        | _ -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line."); None

    static member private tryLoadStaticModelAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (true, asset.FilePath, renderer.RenderPhysicallyBasedMaterial, packageState.TextureMemo, renderer.RenderAssimp) with
        | Right staticModel -> Some staticModel
        | Left error -> Log.debug ("Could not load static model '" + asset.FilePath + "' due to: " + error); None

    static member private tryLoadRenderAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tif" | ".tiff" ->
            match GlRenderer3d.tryLoadTextureAsset packageState asset renderer with
            | Some (filePath, metadata, texture) -> Some (TextureAsset (filePath, metadata, texture))
            | None -> None
        | ".cbm" ->
            match GlRenderer3d.tryLoadCubeMapAsset packageState asset renderer with
            | Some (cubeMapMemoKey, cubeMap, opt) -> Some (CubeMapAsset (cubeMapMemoKey, cubeMap, opt))
            | None -> None
        | ".fbx" | ".obj" ->
            match GlRenderer3d.tryLoadStaticModelAsset packageState asset renderer with
            | Some model -> Some (StaticModelAsset (false, model))
            | None -> None
        | _ -> None

    // TODO: split this into two functions instead of passing reloading boolean.
    static member private tryLoadRenderPackage reloading packageName renderer =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Render3d) packageName assetGraph with
            | Right assets ->

                // find or create render package
                let renderPackage =
                    match Dictionary.tryFind packageName renderer.RenderPackages with
                    | Some renderPackage -> renderPackage
                    | None ->
                        let renderPackageState = { TextureMemo = OpenGL.Texture.TextureMemo.make (); CubeMapMemo = OpenGL.CubeMap.CubeMapMemo.make () }
                        let renderPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = renderPackageState }
                        renderer.RenderPackages.[packageName] <- renderPackage
                        renderPackage

                // reload assets if specified
                if reloading then
                    OpenGL.Texture.RecreateTexturesMemoized true renderPackage.PackageState.TextureMemo
                    OpenGL.Hl.Assert ()
                    OpenGL.CubeMap.RecreateCubeMapsMemoized renderPackage.PackageState.CubeMapMemo
                    OpenGL.Hl.Assert ()
                    for asset in assets do
                        match renderPackage.Assets.TryGetValue asset.AssetTag.AssetName with
                        | (true, renderAsset) ->
                            match renderAsset with
                            | TextureAsset _ -> () // already reloaded via texture memo
                            | FontAsset _ -> () // not yet used in 3d renderer
                            | CubeMapAsset _ -> () // already reloaded via cube map memo
                            | StaticModelAsset (userDefined, staticModel) ->
                                OpenGL.PhysicallyBased.DestroyPhysicallyBasedStaticModel staticModel
                                OpenGL.Hl.Assert ()
                                match GlRenderer3d.tryLoadStaticModelAsset renderPackage.PackageState asset renderer with
                                | Some staticModel -> renderPackage.Assets.[asset.AssetTag.AssetName] <- StaticModelAsset (userDefined, staticModel)
                                | None -> ()
                        | (false, _) -> ()

                // otherwise create assets
                else
                    for asset in assets do
                        match GlRenderer3d.tryLoadRenderAsset renderPackage.PackageState asset renderer with
                        | Some renderAsset -> renderPackage.Assets.[asset.AssetTag.AssetName] <- renderAsset
                        | None -> ()

            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member tryGetRenderAsset (assetTag : obj AssetTag) renderer =
        if  renderer.RenderPackageCachedOpt :> obj |> notNull &&
            fst renderer.RenderPackageCachedOpt = assetTag.PackageName then
            if  renderer.RenderAssetCachedOpt :> obj |> notNull &&
                fst renderer.RenderAssetCachedOpt = assetTag.AssetName then
                ValueSome (snd renderer.RenderAssetCachedOpt)
            else
                let assets = snd renderer.RenderPackageCachedOpt
                match assets.TryGetValue assetTag.AssetName with
                | (true, asset) ->
                    renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
        else
            match Dictionary.tryFind assetTag.PackageName renderer.RenderPackages with
            | Some package ->
                renderer.RenderPackageCachedOpt <- (assetTag.PackageName, package.Assets)
                match package.Assets.TryGetValue assetTag.AssetName with
                | (true, asset) ->
                    renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
            | None ->
                Log.info ("Loading Render3d package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                GlRenderer3d.tryLoadRenderPackage false assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, package) ->
                    renderer.RenderPackageCachedOpt <- (assetTag.PackageName, package.Assets)
                    match package.Assets.TryGetValue assetTag.AssetName with
                    | (true, asset) ->
                        renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                        ValueSome asset
                    | (false, _) -> ValueNone
                | (false, _) -> ValueNone

    static member private tryDestroyUserDefinedStaticModel assetTag renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            GlRenderer3d.tryLoadRenderPackage false assetTag.PackageName renderer

        // free any existing user-created static model, also determining if target asset can be user-created
        match renderer.RenderPackages.TryGetValue assetTag.PackageName with
        | (true, package) ->
            match package.Assets.TryGetValue assetTag.AssetName with
            | (true, asset) ->
                match asset with
                | StaticModelAsset (userDefined, _) when userDefined -> GlRenderer3d.freeRenderAsset package.PackageState asset renderer
                | _ -> ()
            | (false, _) -> ()
        | (false, _) -> ()

    static member private tryCreateUserDefinedStaticModel surfaceDescriptors bounds (assetTag : StaticModel AssetTag) renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            GlRenderer3d.tryLoadRenderPackage false assetTag.PackageName renderer

        // determine if target asset can be created
        let canCreateUserDefinedStaticModel =
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) -> not (package.Assets.ContainsKey assetTag.AssetName)
            | (false, _) -> true

        // ensure the user can create the static model
        if canCreateUserDefinedStaticModel then

            // create surfaces
            let surfaces = List ()
            for surfaceDescriptor in surfaceDescriptors do

                // get albedo metadata and texture
                let (albedoMetadata, albedoTexture) =
                    match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.AlbedoImage) renderer with
                    | ValueSome (TextureAsset (_, textureMetadata, texture)) -> (textureMetadata, texture)
                    | _ -> (renderer.RenderPhysicallyBasedMaterial.AlbedoMetadata, renderer.RenderPhysicallyBasedMaterial.AlbedoTexture)

                // create material
                let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
                    { Albedo = surfaceDescriptor.Albedo
                      AlbedoMetadata = albedoMetadata
                      AlbedoTexture = albedoTexture
                      Metalness = surfaceDescriptor.Metalness
                      MetalnessTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.MetalnessImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.MetalnessTexture
                      Roughness = surfaceDescriptor.Roughness
                      RoughnessTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.RoughnessImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.RoughnessTexture
                      Emission = surfaceDescriptor.Emission
                      EmissionTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.EmissionImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.EmissionTexture
                      AmbientOcclusion = surfaceDescriptor.AmbientOcclusion
                      AmbientOcclusionTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.AmbientOcclusionImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.AmbientOcclusionTexture
                      NormalTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.NormalImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.NormalTexture
                      TextureMinFilterOpt = surfaceDescriptor.TextureMinFilterOpt
                      TextureMagFilterOpt = surfaceDescriptor.TextureMagFilterOpt
                      TwoSided = surfaceDescriptor.TwoSided }

                // create vertex data, truncating it when required
                let vertexCount = surfaceDescriptor.Positions.Length
                let elementCount = vertexCount * 8
                if  renderer.RenderUserDefinedStaticModelFields.Length < elementCount then
                    renderer.RenderUserDefinedStaticModelFields <- Array.zeroCreate elementCount // TODO: grow this by power of two.
                let vertexData = renderer.RenderUserDefinedStaticModelFields.AsMemory (0, elementCount)
                let mutable i = 0
                try
                    let vertexData = vertexData.Span
                    while i < vertexCount do
                        let u = i * 8
                        vertexData.[u] <- surfaceDescriptor.Positions.[i].X
                        vertexData.[u+1] <- surfaceDescriptor.Positions.[i].Y
                        vertexData.[u+2] <- surfaceDescriptor.Positions.[i].Z
                        vertexData.[u+3] <- surfaceDescriptor.TexCoordses.[i].X
                        vertexData.[u+4] <- surfaceDescriptor.TexCoordses.[i].Y
                        vertexData.[u+5] <- surfaceDescriptor.Normals.[i].X
                        vertexData.[u+6] <- surfaceDescriptor.Normals.[i].Y
                        vertexData.[u+7] <- surfaceDescriptor.Normals.[i].Z
                        i <- inc i
                with :? IndexOutOfRangeException ->
                    Log.debug "Vertex data truncated due to an unequal count among surface descriptor Positions, TexCoordses, and Normals."

                // create index data
                let indexData = surfaceDescriptor.Indices.AsMemory ()

                // create geometry
                let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedGeometry (true, vertexData, indexData, surfaceDescriptor.Bounds)

                // create surface
                let surface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], surfaceDescriptor.AffineMatrix, surfaceDescriptor.Bounds, material, geometry)
                surfaces.Add surface

            // create static model
            let surfaces = Seq.toArray surfaces
            let hierarchy = TreeNode (Array.map OpenGL.PhysicallyBased.PhysicallyBasedSurface surfaces)
            let staticModel : OpenGL.PhysicallyBased.PhysicallyBasedStaticModel =
                { Bounds = bounds
                  Lights = [||]
                  Surfaces = surfaces
                  PhysicallyBasedStaticHierarchy = hierarchy }

            // assign static model as appropriate render package asset
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) ->
                package.Assets.[assetTag.AssetName] <- StaticModelAsset (true, staticModel)
            | (false, _) ->
                let packageState = { TextureMemo = OpenGL.Texture.TextureMemo.make (); CubeMapMemo = OpenGL.CubeMap.CubeMapMemo.make () }
                let package = { Assets = Dictionary.singleton StringComparer.Ordinal assetTag.AssetName (StaticModelAsset (true, staticModel)); PackageState = packageState }
                renderer.RenderPackages.[assetTag.PackageName] <- package

        // attempted to replace a loaded asset
        else Log.debug ("Cannot replace a loaded asset '" + scstring assetTag + "' with a user-created static model.")

    static member private handleLoadRenderPackage hintPackageName renderer =
        GlRenderer3d.tryLoadRenderPackage false hintPackageName renderer

    static member private handleUnloadRenderPackage hintPackageName renderer =
        GlRenderer3d.invalidateCaches renderer
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some package ->
            OpenGL.Texture.DeleteTexturesMemoized package.PackageState.TextureMemo
            OpenGL.CubeMap.DeleteCubeMapsMemoized package.PackageState.CubeMapMemo
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        GlRenderer3d.invalidateCaches renderer
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        for packageName in packageNames do
            GlRenderer3d.tryLoadRenderPackage true packageName renderer

    static member private categorizeBillboardSurface
        (absolute,
         eyeRotation : Quaternion,
         affineMatrix,
         insetOpt : Box2 voption,
         albedoMetadata : OpenGL.Texture.TextureMetadata,
         renderMaterial,
         renderType,
         billboardSurface,
         renderer) =
        let texCoordsOffset =
            match insetOpt with
            | ValueSome inset ->
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | ValueNone -> box2 v2Zero v2One // shouldn't we still be using borders?
        let eyeForward =
            (Vector3.Transform (v3Forward, eyeRotation)).WithY 0.0f
        if v3Neq eyeForward v3Zero then
            let billboardAngle = if Vector3.Dot (eyeForward, v3Right) >= 0.0f then -eyeForward.AngleBetween v3Forward else eyeForward.AngleBetween v3Forward
            let billboardRotation = Matrix4x4.CreateFromQuaternion (Quaternion.CreateFromAxisAngle (v3Up, billboardAngle))
            let billboardMatrix = affineMatrix * billboardRotation
            match renderType with
            | DeferredRenderType ->
                if absolute then
                    match renderer.RenderTasks.RenderSurfacesDeferredAbsolute.TryGetValue billboardSurface with
                    | (true, renderTasks) -> SegmentedList.add struct (billboardMatrix, texCoordsOffset, renderMaterial) renderTasks
                    | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Add (billboardSurface, SegmentedList.singleton (billboardMatrix, texCoordsOffset, renderMaterial))
                else
                    match renderer.RenderTasks.RenderSurfacesDeferredRelative.TryGetValue billboardSurface with
                    | (true, renderTasks) -> SegmentedList.add struct (billboardMatrix, texCoordsOffset, renderMaterial) renderTasks
                    | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredRelative.Add (billboardSurface, SegmentedList.singleton (billboardMatrix, texCoordsOffset, renderMaterial))
            | ForwardRenderType (sort, subsort) ->
                if absolute
                then SegmentedList.add struct (sort, subsort, billboardMatrix, texCoordsOffset, renderMaterial, billboardSurface) renderer.RenderTasks.RenderSurfacesForwardAbsolute
                else SegmentedList.add struct (sort, subsort, billboardMatrix, texCoordsOffset, renderMaterial, billboardSurface) renderer.RenderTasks.RenderSurfacesForwardRelative

    static member private categorizeStaticModelSurface
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 voption,
         renderMaterial : RenderMaterial inref,
         renderType : RenderType,
         ignoreSurfaceMatrix,
         surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         renderer) =
        let surfaceMatrix =
            if ignoreSurfaceMatrix || surface.SurfaceMatrixIsIdentity
            then modelMatrix
            else surface.SurfaceMatrix * modelMatrix
        let texCoordsOffset =
            match insetOpt with
            | ValueSome inset ->
                let albedoMetadata = surface.SurfaceMaterial.AlbedoMetadata
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | ValueNone -> box2 v2Zero v2Zero
        match renderType with
        | DeferredRenderType ->
            if modelAbsolute then
                match renderer.RenderTasks.RenderSurfacesDeferredAbsolute.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, texCoordsOffset, renderMaterial) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Add (surface, SegmentedList.singleton (surfaceMatrix, texCoordsOffset, renderMaterial))
            else
                match renderer.RenderTasks.RenderSurfacesDeferredRelative.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, texCoordsOffset, renderMaterial) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredRelative.Add (surface, SegmentedList.singleton (surfaceMatrix, texCoordsOffset, renderMaterial))
        | ForwardRenderType (sort, subsort) ->
            if modelAbsolute
            then SegmentedList.add struct (sort, subsort, surfaceMatrix, texCoordsOffset, renderMaterial, surface) renderer.RenderTasks.RenderSurfacesForwardAbsolute
            else SegmentedList.add struct (sort, subsort, surfaceMatrix, texCoordsOffset, renderMaterial, surface) renderer.RenderTasks.RenderSurfacesForwardRelative

    static member private categorizeStaticModelSurfaceByIndex
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 voption,
         renderMaterial : RenderMaterial inref,
         renderType : RenderType,
         staticModel : StaticModel AssetTag,
         surfaceIndex,
         renderer) =
        match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize staticModel) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces.[surfaceIndex]
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, insetOpt, &renderMaterial, renderType, true, surface, renderer)
            | _ -> Log.trace "Cannot render static model surface with a non-model asset."
        | _ -> Log.info ("Cannot render static model surface due to unloadable assets for '" + scstring staticModel + "'.")

    static member private categorizeStaticModel
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 voption,
         renderMaterial : RenderMaterial inref,
         renderType,
         staticModel : StaticModel AssetTag,
         renderer) =
        match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize staticModel) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                for light in modelAsset.Lights do
                    let lightMatrix = light.LightMatrix * modelMatrix
                    let light =
                        { SortableLightOrigin = lightMatrix.Translation
                          SortableLightColor = light.LightColor
                          SortableLightBrightness = light.LightBrightness
                          SortableLightIntensity = light.LightIntensity
                          SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderer.RenderTasks.RenderLights
                for surface in modelAsset.Surfaces do
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, insetOpt, &renderMaterial, renderType, false, surface, renderer)
            | _ -> Log.trace "Cannot render static model with a non-model asset."
        | _ -> Log.info ("Cannot render static model due to unloadable assets for '" + scstring staticModel + "'.")

    /// Get the physically-based shader.
    static member getPhysicallyBasedShader renderer =
        renderer.RenderPhysicallyBasedForwardShader

    /// Create an irradiance map for a sky box.
    static member createIrradianceMap currentViewportOffset currentFramebuffer renderbuffer framebuffer shader skyBoxSurface =
        OpenGL.SkyBox.CreateIrradianceMap
            (currentViewportOffset,
             currentFramebuffer,
             Constants.Render.IrradianceMapResolution,
             Constants.Render.IrradianceMapResolution,
             renderbuffer,
             framebuffer,
             shader,
             skyBoxSurface)

    /// Create an environment filter map for a sky box.
    static member createEnvironmentFilterMap currentViewportOffset currentFramebuffer renderbuffer framebuffer shader skyBoxSurface =
        OpenGL.SkyBox.CreateEnvironmentFilterMap
            (currentViewportOffset,
             currentFramebuffer,
             Constants.Render.EnvironmentFilterResolution,
             Constants.Render.EnvironmentFilterResolution,
             renderbuffer,
             framebuffer,
             shader,
             skyBoxSurface)

    static member private sortSurfaces eyeCenter (surfaces : struct (single * single * Matrix4x4 * Box2 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList) =
        surfaces |>
        Seq.map (fun struct (sort, subsort, model, texCoordsOffset, renderMaterial, surface) -> struct (sort, subsort, model, texCoordsOffset, renderMaterial, surface, (model.Translation - eyeCenter).MagnitudeSquared)) |>
        Seq.toArray |> // TODO: use a preallocated array to avoid allocating on the LOH.
        Array.sortByDescending (fun struct (sort, subsort, _, _, _, _, distanceSquared) -> struct (sort, distanceSquared, subsort)) |>
        Array.map (fun struct (_, _, model, texCoordsOffset, renderMaterialOpt, surface, _) -> struct (model, texCoordsOffset, renderMaterialOpt, surface))

    static member private renderPhysicallyBasedSurfaces
        eyeCenter
        viewArray
        projectionArray
        (parameters : struct (Matrix4x4 * Box2 * RenderMaterial) SegmentedList)
        blending
        irradianceMap
        environmentFilterMap
        brdfTexture
        lightOrigins
        lightColors
        lightBrightnesses
        lightIntensities
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface)
        shader
        renderer =

        // ensure there are surfaces to render
        if parameters.Length > 0 then

            // ensure we have a large enough models fields array
            let mutable length = renderer.RenderModelsFields.Length
            while parameters.Length * 16 > length do length <- length * 2
            if renderer.RenderModelsFields.Length < length then
                renderer.RenderModelsFields <- Array.zeroCreate<single> length

            // ensure we have a large enough texCoordsOffsets fields array
            let mutable length = renderer.RenderTexCoordsOffsetsFields.Length
            while parameters.Length * 4 > length do length <- length * 2
            if renderer.RenderTexCoordsOffsetsFields.Length < length then
                renderer.RenderTexCoordsOffsetsFields <- Array.zeroCreate<single> length

            // ensure we have a large enough abledos fields array
            let mutable length = renderer.RenderAlbedosFields.Length
            while parameters.Length * 4 > length do length <- length * 2
            if renderer.RenderAlbedosFields.Length < length then
                renderer.RenderAlbedosFields <- Array.zeroCreate<single> length

            // ensure we have a large enough materials fields array
            let mutable length = renderer.RenderMaterialsFields.Length
            while parameters.Length * 4 > length do length <- length * 2
            if renderer.RenderMaterialsFields.Length < length then
                renderer.RenderMaterialsFields <- Array.zeroCreate<single> length

            // blit parameters to field arrays
            for i in 0 .. dec parameters.Length do
                let struct (model, texCoordsOffset, renderMaterial) = parameters.[i]
                model.ToArray (renderer.RenderModelsFields, i * 16)
                renderer.RenderTexCoordsOffsetsFields.[i * 4] <- texCoordsOffset.Min.X
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 1] <- texCoordsOffset.Min.Y
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
                let (albedo, metalness, roughness, emission, ambientOcclusion) =
                    ((match renderMaterial.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterial.Albedo),
                     (match renderMaterial.MetalnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterial.Metalness),
                     (match renderMaterial.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterial.Roughness),
                     (match renderMaterial.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterial.Emission),
                     (match renderMaterial.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterial.AmbientOcclusion))
                renderer.RenderAlbedosFields.[i * 4] <- albedo.R
                renderer.RenderAlbedosFields.[i * 4 + 1] <- albedo.G
                renderer.RenderAlbedosFields.[i * 4 + 2] <- albedo.B
                renderer.RenderAlbedosFields.[i * 4 + 3] <- albedo.A
                renderer.RenderMaterialsFields.[i * 4] <- metalness
                renderer.RenderMaterialsFields.[i * 4 + 1] <- roughness
                renderer.RenderMaterialsFields.[i * 4 + 2] <- emission
                renderer.RenderMaterialsFields.[i * 4 + 3] <- ambientOcclusion

            // draw surfaces
            OpenGL.PhysicallyBased.DrawPhysicallyBasedSurfaces
                (eyeCenter, parameters.Length, renderer.RenderModelsFields, renderer.RenderTexCoordsOffsetsFields, renderer.RenderAlbedosFields, renderer.RenderMaterialsFields, viewArray, projectionArray,
                 blending, irradianceMap, environmentFilterMap, brdfTexture, lightOrigins, lightColors, lightBrightnesses, lightIntensities,
                 surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader)

    static member inline private makeBillboardMaterial (renderMaterial : RenderMaterial) albedoImage metalnessImage roughnessImage emissionImage ambientOcclusionImage normalImage minFilterOpt magFilterOpt renderer =
        let (albedoMetadata, albedoTexture) =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize albedoImage) renderer with
            | ValueSome (TextureAsset (_, textureMetadata, texture)) -> (textureMetadata, texture)
            | _ -> (OpenGL.Texture.TextureMetadata.empty, renderer.RenderPhysicallyBasedMaterial.AlbedoTexture)
        let metalnessTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize metalnessImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.MetalnessTexture
        let roughnessTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize roughnessImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.RoughnessTexture
        let emissionTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize emissionImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.EmissionTexture
        let ambientOcclusionTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize ambientOcclusionImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.AmbientOcclusionTexture
        let normalTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize normalImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.NormalTexture
        let billboardMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { Albedo = ValueOption.defaultValue Color.White renderMaterial.AlbedoOpt
              AlbedoMetadata = albedoMetadata
              AlbedoTexture = albedoTexture
              Metalness = ValueOption.defaultValue 1.0f renderMaterial.MetalnessOpt
              MetalnessTexture = metalnessTexture
              Roughness = ValueOption.defaultValue 1.0f renderMaterial.RoughnessOpt
              RoughnessTexture = roughnessTexture
              Emission = ValueOption.defaultValue 1.0f renderMaterial.EmissionOpt
              EmissionTexture = emissionTexture
              AmbientOcclusion = ValueOption.defaultValue 1.0f renderMaterial.AmbientOcclusionOpt
              AmbientOcclusionTexture = ambientOcclusionTexture
              NormalTexture = normalTexture
              TextureMinFilterOpt = minFilterOpt
              TextureMagFilterOpt = magFilterOpt
              TwoSided = false }
        billboardMaterial

    /// Make a GlRenderer3d.
    static member make window config =

        // initialize context if directed
        if config.ShouldInitializeContext then

            // create SDL-OpenGL context if needed
            match window with
            | SglWindow window -> OpenGL.Hl.CreateSglContext window.SglWindow |> ignore<nativeint>
            | WfglWindow _ -> () // TODO: see if we can make current the GL context here so that threaded OpenGL works in Gaia.
            OpenGL.Hl.Assert ()

            // listen to debug messages
            OpenGL.Hl.AttachDebugMessageCallback ()

        // create sky box shader
        let skyBoxShader = OpenGL.SkyBox.CreateSkyBoxShader Constants.Paths.SkyBoxShaderFilePath
        OpenGL.Hl.Assert ()

        // create irradiance shader
        let irradianceShader = OpenGL.SkyBox.CreateSkyBoxShader Constants.Paths.IrradianceShaderFilePath
        OpenGL.Hl.Assert ()

        // create environment filter shader
        let environmentFilterShader = OpenGL.SkyBox.CreateEnvironmentFilterShader Constants.Paths.EnvironmentFilterShaderFilePath
        OpenGL.Hl.Assert ()

        // create forward shader
        let forwardShader = OpenGL.PhysicallyBased.CreatePhysicallyBasedShader Constants.Paths.PhysicallyBasedForwardShaderFilePath
        OpenGL.Hl.Assert ()

        // create deferred shaders
        let (deferredShader, deferred2Shader) =
            OpenGL.PhysicallyBased.CreatePhysicallyBasedDeferredShaders
                (Constants.Paths.PhysicallyBasedDeferredShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferred2ShaderFilePath)
        OpenGL.Hl.Assert ()

        // crete environment filter framebuffer
        let irradianceFramebuffer = OpenGL.Gl.GenFramebuffer ()
        let irradianceRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, irradianceFramebuffer)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, irradianceRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent24, Constants.Render.IrradianceMapResolution, Constants.Render.IrradianceMapResolution)
        OpenGL.Hl.Assert ()

        // crete environment filter framebuffer
        let environmentFilterFramebuffer = OpenGL.Gl.GenFramebuffer ()
        let environmentFilterRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, environmentFilterFramebuffer)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent24, Constants.Render.EnvironmentFilterResolution, Constants.Render.EnvironmentFilterResolution)
        OpenGL.Hl.Assert ()

        // create geometry framebuffer
        let geometryFramebuffer =
            match OpenGL.Framebuffer.TryCreateGeometryFramebuffer () with
            | Right geometryFramebuffer -> geometryFramebuffer
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create white sky box cube map
        let skyBoxMap =
            match 
                OpenGL.CubeMap.TryCreateCubeMap
                    ("Assets/Default/Image9.bmp",
                     "Assets/Default/Image9.bmp",
                     "Assets/Default/Image9.bmp",
                     "Assets/Default/Image9.bmp",
                     "Assets/Default/Image9.bmp",
                     "Assets/Default/Image9.bmp") with
            | Right cubeMap -> cubeMap
            | Left error -> failwith error
        OpenGL.Hl.Assert ()

        // create sky box geometry
        let skyBoxGeometry = OpenGL.SkyBox.CreateSkyBoxGeometry true
        OpenGL.Hl.Assert ()

        // create billboard geometry
        let billboardGeometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedBillboard true
        OpenGL.Hl.Assert ()

        // create physically-based quad
        let physicallyBasedQuad = OpenGL.PhysicallyBased.CreatePhysicallyBasedQuad true
        OpenGL.Hl.Assert ()

        // create sky box surface
        let skyBoxSurface = OpenGL.SkyBox.SkyBoxSurface.make skyBoxMap skyBoxGeometry
        OpenGL.Hl.Assert ()

        // create default irradiance map
        let irradianceMap = GlRenderer3d.createIrradianceMap Constants.Render.Viewport 0u irradianceRenderbuffer irradianceFramebuffer irradianceShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create default environment filter map
        let environmentFilterMap = GlRenderer3d.createEnvironmentFilterMap Constants.Render.Viewport 0u environmentFilterRenderbuffer environmentFilterFramebuffer environmentFilterShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create brdf texture
        let brdfTexture =
            match OpenGL.Texture.TryCreateTextureUnfiltered (Constants.Paths.BrdfTextureFilePath) with
            | Right (_, texture) -> texture
            | Left error -> failwith ("Could not load BRDF texture due to: " + error)

        // get albedo metadata and texture
        let (albedoMetadata, albedoTexture) = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAlbedo.png") |> Either.getRight

        // create default physically-based material
        let physicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { Albedo = Color.White
              AlbedoMetadata = albedoMetadata
              AlbedoTexture = albedoTexture
              Metalness = 1.0f
              MetalnessTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialMetalness.png") |> Either.getRight |> snd
              Roughness = 1.0f
              RoughnessTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialRoughness.png") |> Either.getRight |> snd
              Emission = 1.0f
              EmissionTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialEmission.png") |> Either.getRight |> snd
              AmbientOcclusion = 1.0f
              AmbientOcclusionTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAmbientOcclusion.png") |> Either.getRight |> snd
              NormalTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialNormal.png") |> Either.getRight |> snd
              TextureMinFilterOpt = ValueNone
              TextureMagFilterOpt = ValueNone
              TwoSided = false }

        // create render tasks
        let renderTasks =
            { RenderSkyBoxes = SegmentedList.make ()
              RenderLights = SegmentedList.make ()
              RenderSurfacesDeferredAbsolute = dictPlus HashIdentity.Structural []
              RenderSurfacesDeferredRelative = dictPlus HashIdentity.Structural []
              RenderSurfacesForwardAbsolute = SegmentedList.make ()
              RenderSurfacesForwardRelative = SegmentedList.make ()
              RenderSurfacesForwardAbsoluteSorted = SegmentedList.make ()
              RenderSurfacesForwardRelativeSorted = SegmentedList.make () }

        // make renderer
        let renderer =
            { RenderWindow = window
              RenderAssimp = new Assimp.AssimpContext ()
              RenderSkyBoxShader = skyBoxShader
              RenderIrradianceShader = irradianceShader
              RenderEnvironmentFilterShader = environmentFilterShader
              RenderPhysicallyBasedForwardShader = forwardShader
              RenderPhysicallyBasedDeferredShader = deferredShader
              RenderPhysicallyBasedDeferred2Shader = deferred2Shader
              RenderIrradianceFramebuffer = (irradianceRenderbuffer, irradianceFramebuffer)
              RenderEnvironmentFilterFramebuffer = (environmentFilterRenderbuffer, environmentFilterFramebuffer)
              RenderGeometryFramebuffer = geometryFramebuffer
              RenderSkyBoxGeometry = skyBoxGeometry
              RenderBillboardGeometry = billboardGeometry
              RenderPhysicallyBasedQuad = physicallyBasedQuad
              RenderIrradianceMap = irradianceMap
              RenderEnvironmentFilterMap = environmentFilterMap
              RenderBrdfTexture = brdfTexture
              RenderPhysicallyBasedMaterial = physicallyBasedMaterial
              RenderModelsFields = Array.zeroCreate<single> (16 * Constants.Render.GeometryBatchPrealloc)
              RenderTexCoordsOffsetsFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              RenderAlbedosFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              RenderMaterialsFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              RenderUserDefinedStaticModelFields = [||]
              RenderTasks = renderTasks
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderMessages = List ()
              RenderShouldBeginFrame = config.ShouldBeginFrame
              RenderShouldEndFrame = config.ShouldEndFrame }

        // fin
        renderer

    interface Renderer3d with

        member renderer.PhysicallyBasedShader =
            renderer.RenderPhysicallyBasedForwardShader

        member renderer.Render eyeCenter eyeRotation windowSize renderMessages =

            // begin frame
            let viewportOffset = Constants.Render.ViewportOffset windowSize
            if renderer.RenderShouldBeginFrame then
                OpenGL.Hl.BeginFrame viewportOffset
                OpenGL.Hl.Assert ()

            // compute view and projection
            let eyeTarget = eyeCenter + Vector3.Transform (v3Forward, eyeRotation)
            let viewAbsolute = m4Identity
            let viewAbsoluteArray = viewAbsolute.ToArray ()
            let viewRelative = Matrix4x4.CreateLookAt (eyeCenter, eyeTarget, v3Up)
            let viewRelativeArray = viewRelative.ToArray ()
            let viewSkyBox = Matrix4x4.CreateFromQuaternion (Quaternion.Inverse eyeRotation)
            let viewSkyBoxArray = viewSkyBox.ToArray ()
            let viewport = Constants.Render.Viewport
            let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // categorize messages
            // TODO: consider implementing some exception safety for the stateful operations in this function.
            let userDefinedStaticModelsToDestroy = SegmentedList.make ()
            let postPasses = hashSetPlus<RenderPassMessage3d> HashIdentity.Structural []
            for message in renderMessages do
                match message with
                | CreateUserDefinedStaticModel (surfaceDescriptors, bounds, assetTag) ->
                    GlRenderer3d.tryCreateUserDefinedStaticModel surfaceDescriptors bounds assetTag renderer
                | DestroyUserDefinedStaticModel staticModel ->
                    SegmentedList.add staticModel userDefinedStaticModelsToDestroy
                | RenderSkyBox cubeMap ->
                    SegmentedList.add cubeMap renderer.RenderTasks.RenderSkyBoxes
                | RenderLight3d (position, color, brightness, intensity, _) ->
                    let light = { SortableLightOrigin = position; SortableLightColor = color; SortableLightBrightness = brightness; SortableLightIntensity = intensity; SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderer.RenderTasks.RenderLights
                | RenderBillboard (absolute, modelMatrix, insetOpt, renderMaterial, albedoImage, metalnessImage, roughnessImage, emissionImage, ambientOcclusionImage, normalImage, minFilterOpt, magFilterOpt, renderType) ->
                    let billboardMaterial = GlRenderer3d.makeBillboardMaterial renderMaterial albedoImage metalnessImage roughnessImage emissionImage ambientOcclusionImage normalImage minFilterOpt magFilterOpt renderer
                    let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3 (v3 -0.5f 0.0f -0.5f) v3One, billboardMaterial, renderer.RenderBillboardGeometry)
                    GlRenderer3d.categorizeBillboardSurface (absolute, eyeRotation, modelMatrix, insetOpt, billboardMaterial.AlbedoMetadata, renderMaterial, renderType, billboardSurface, renderer)
                | RenderBillboards (absolute, billboards, renderMaterial, albedoImage, metalnessImage, roughnessImage, emissionImage, ambientOcclusionImage, normalImage, minFilterOpt, magFilterOpt, renderType) ->
                    let billboardMaterial = GlRenderer3d.makeBillboardMaterial renderMaterial albedoImage metalnessImage roughnessImage emissionImage ambientOcclusionImage normalImage minFilterOpt magFilterOpt renderer
                    let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3 (v3 -0.5f -0.5f -0.5f) v3One, billboardMaterial, renderer.RenderBillboardGeometry)
                    for (modelMatrix, insetOpt) in billboards do
                        GlRenderer3d.categorizeBillboardSurface (absolute, eyeRotation, modelMatrix, insetOpt, billboardMaterial.AlbedoMetadata, renderMaterial, renderType, billboardSurface, renderer)
                | RenderBillboardParticles (absolute, renderMaterial, albedoImage, metalnessImage, roughnessImage, emissionImage, ambientOcclusionImage, normalImage, minFilterOpt, magFilterOpt, renderType, particles) ->
                    let billboardMaterial = GlRenderer3d.makeBillboardMaterial renderMaterial albedoImage metalnessImage roughnessImage emissionImage ambientOcclusionImage normalImage minFilterOpt magFilterOpt renderer
                    for particle in particles do
                        let billboardMatrix =
                            Matrix4x4.CreateFromTrs
                                (particle.Transform.Center,
                                 particle.Transform.Rotation,
                                 particle.Transform.Size * particle.Transform.Scale)
                        let billboardMaterial = { billboardMaterial with Albedo = billboardMaterial.Albedo * particle.Color; Emission = particle.Emission.R }
                        let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3Zero, billboardMaterial, renderer.RenderBillboardGeometry)
                        GlRenderer3d.categorizeBillboardSurface (absolute, eyeRotation, billboardMatrix, particle.InsetOpt, billboardMaterial.AlbedoMetadata, renderMaterial, renderType, billboardSurface, renderer)
                | RenderStaticModelSurface (absolute, modelMatrix, insetOpt, renderMaterial, renderType, staticModel, surfaceIndex) ->
                    GlRenderer3d.categorizeStaticModelSurfaceByIndex (absolute, &modelMatrix, insetOpt, &renderMaterial, renderType, staticModel, surfaceIndex, renderer)
                | RenderStaticModel (absolute, modelMatrix, insetOpt, renderMaterial, renderType, staticModel) ->
                    GlRenderer3d.categorizeStaticModel (absolute, &modelMatrix, insetOpt, &renderMaterial, renderType, staticModel, renderer)
                | RenderStaticModels (absolute, parameters, renderType, staticModel) ->
                    for (modelMatrix, insetOpt, renderMaterial) in parameters do
                        GlRenderer3d.categorizeStaticModel (absolute, &modelMatrix, insetOpt, &renderMaterial, renderType, staticModel, renderer)
                | RenderCachedStaticModel d ->
                    GlRenderer3d.categorizeStaticModel (d.CachedStaticModelAbsolute, &d.CachedStaticModelAffineMatrix, d.CachedStaticModelInsetOpt, &d.CachedStaticModelRenderMaterial, d.CachedStaticModelRenderType, d.CachedStaticModel, renderer)
                | RenderUserDefinedStaticModel (absolute, modelMatrix, insetOpt, renderMaterial, renderType, surfaceDescriptors, bounds) ->
                    let assetTag = asset Assets.Default.PackageName Gen.name // TODO: see if we should instead use a specialized package for temporary assets like these.
                    GlRenderer3d.tryCreateUserDefinedStaticModel surfaceDescriptors bounds assetTag renderer
                    GlRenderer3d.categorizeStaticModel (absolute, &modelMatrix, insetOpt, &renderMaterial, renderType, assetTag, renderer)
                    SegmentedList.add assetTag userDefinedStaticModelsToDestroy
                | RenderPostPass3d postPass ->
                    postPasses.Add postPass |> ignore<bool>
                | LoadRenderPackage3d hintPackageUse ->
                    GlRenderer3d.handleLoadRenderPackage hintPackageUse renderer
                | UnloadRenderPackage3d hintPackageDisuse ->
                    GlRenderer3d.handleUnloadRenderPackage hintPackageDisuse renderer
                | ReloadRenderAssets3d ->
                    GlRenderer3d.handleReloadRenderAssets renderer

            // sort absolute forward surfaces
            let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyeCenter renderer.RenderTasks.RenderSurfacesForwardAbsolute
            SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsolute

            // sort relative forward surfaces
            let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyeCenter renderer.RenderTasks.RenderSurfacesForwardRelative
            SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardRelativeSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelative

            // setup geometry buffer
            let (positionTexture, albedoTexture, materialTexture, normalTexture, geometryFramebuffer) = renderer.RenderGeometryFramebuffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
            OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
            OpenGL.Gl.Scissor (viewportOffset.Bounds.Min.X, viewportOffset.Bounds.Min.Y, viewportOffset.Bounds.Size.X, viewportOffset.Bounds.Size.Y)
            OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest
            OpenGL.Hl.Assert ()

            // attempt to locate last sky box
            let skyBoxOpt =
                match Seq.tryLast renderer.RenderTasks.RenderSkyBoxes with
                | Some cubeMap ->
                    match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize cubeMap) renderer with
                    | ValueSome asset ->
                        match asset with
                        | CubeMapAsset (_, cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef) -> Some (cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef)
                        | _ -> Log.debug "Could not utilize sky box due to mismatched cube map asset."; None
                    | ValueNone -> Log.debug "Could not utilize sky box due to non-existent cube map asset."; None
                | None -> None

            // retrieve an irradiance map, preferably from the sky box
            let (irradianceMap, environmentFilterMap) =
                match skyBoxOpt with
                | Some (cubeMap, irradianceAndEnviconmentMapsOptRef) ->
                    if Option.isNone irradianceAndEnviconmentMapsOptRef.Value then
                        let irradianceMap =
                            GlRenderer3d.createIrradianceMap
                                viewportOffset
                                geometryFramebuffer
                                (fst renderer.RenderIrradianceFramebuffer)
                                (snd renderer.RenderIrradianceFramebuffer)
                                renderer.RenderIrradianceShader
                                (OpenGL.SkyBox.SkyBoxSurface.make cubeMap renderer.RenderSkyBoxGeometry)
                        let environmentFilterMap =
                            GlRenderer3d.createEnvironmentFilterMap
                                viewportOffset
                                geometryFramebuffer
                                (fst renderer.RenderEnvironmentFilterFramebuffer)
                                (snd renderer.RenderEnvironmentFilterFramebuffer)
                                renderer.RenderEnvironmentFilterShader
                                (OpenGL.SkyBox.SkyBoxSurface.make cubeMap renderer.RenderSkyBoxGeometry)
                        let result = (irradianceMap, environmentFilterMap)
                        irradianceAndEnviconmentMapsOptRef.Value <- Some result
                        result
                    else Option.get irradianceAndEnviconmentMapsOptRef.Value
                | None -> (renderer.RenderIrradianceMap, renderer.RenderEnvironmentFilterMap)

            // sort lights for deferred relative to eye center
            let (lightOrigins, lightColors, lightBrightnesses, lightIntensities) =
                SortableLight.sortLightsIntoArrays eyeCenter renderer.RenderTasks.RenderLights

            // deferred render surfaces w/ absolute transforms
            for entry in renderer.RenderTasks.RenderSurfacesDeferredAbsolute do
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyeCenter
                    viewAbsoluteArray
                    projectionArray
                    entry.Value
                    false
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightOrigins
                    lightColors
                    lightBrightnesses
                    lightIntensities
                    entry.Key
                    renderer.RenderPhysicallyBasedDeferredShader
                    renderer
                OpenGL.Hl.Assert ()

            // deferred render surfaces w/ relative transforms
            for entry in renderer.RenderTasks.RenderSurfacesDeferredRelative do
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyeCenter
                    viewRelativeArray
                    projectionArray
                    entry.Value
                    false
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightOrigins
                    lightColors
                    lightBrightnesses
                    lightIntensities
                    entry.Key
                    renderer.RenderPhysicallyBasedDeferredShader
                    renderer
                OpenGL.Hl.Assert ()

            // copy depths from geometry framebuffer to main framebuffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, geometryFramebuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, 0u)
            OpenGL.Gl.BlitFramebuffer
                (0, 0, Constants.Render.ResolutionX, Constants.Render.ResolutionY,
                 0, 0, Constants.Render.ResolutionX, Constants.Render.ResolutionY,
                 OpenGL.ClearBufferMask.DepthBufferBit,
                 OpenGL.BlitFramebufferFilter.Nearest)
            OpenGL.Hl.Assert ()

            // switch to main framebuffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)
            OpenGL.Hl.Assert ()

            // deferred render lighting quad
            OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferred2Surface
                (eyeCenter, positionTexture, albedoTexture, materialTexture, normalTexture,
                 irradianceMap, environmentFilterMap, renderer.RenderBrdfTexture, lightOrigins, lightColors, lightBrightnesses, lightIntensities,
                 renderer.RenderPhysicallyBasedQuad, renderer.RenderPhysicallyBasedDeferred2Shader)
            OpenGL.Hl.Assert ()

            // attempt to render sky box
            match skyBoxOpt with
            | Some (cubeMap, _) ->
                OpenGL.SkyBox.DrawSkyBox (viewSkyBoxArray, projectionArray, cubeMap, renderer.RenderSkyBoxGeometry, renderer.RenderSkyBoxShader)
                OpenGL.Hl.Assert ()
            | None -> ()

            // forward render surfaces w/ absolute transforms
            for (model, texCoordsOffset, renderMaterial, surface) in renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted do
                let (lightOrigins, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderer.RenderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyeCenter
                    viewAbsoluteArray
                    projectionArray
                    (SegmentedList.singleton (model, texCoordsOffset, renderMaterial))
                    true
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightColors
                    lightOrigins
                    lightBrightnesses
                    lightIntensities
                    surface
                    renderer.RenderPhysicallyBasedForwardShader
                    renderer
                OpenGL.Hl.Assert ()

            // forward render surfaces w/ relative transforms
            for (model, texCoordsOffset, renderMaterial, surface) in renderer.RenderTasks.RenderSurfacesForwardRelativeSorted do
                let (lightOrigins, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderer.RenderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyeCenter
                    viewRelativeArray
                    projectionArray
                    (SegmentedList.singleton (model, texCoordsOffset, renderMaterial))
                    true
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightOrigins
                    lightColors
                    lightBrightnesses
                    lightIntensities
                    surface
                    renderer.RenderPhysicallyBasedForwardShader
                    renderer
                OpenGL.Hl.Assert ()

            // render post-passes
            let passParameters =
                { EyeCenter = eyeCenter
                  EyeRotation = eyeRotation
                  ViewAbsolute = viewAbsolute
                  ViewRelative = viewRelative
                  ViewSkyBox = viewSkyBox
                  Viewport = viewport
                  Projection = projection
                  RenderTasks = renderer.RenderTasks
                  Renderer3d = renderer }
            for pass in postPasses do
                pass.RenderPassParameters3d passParameters
                OpenGL.Hl.Assert ()

            // clear render tasks
            SegmentedList.clear renderer.RenderTasks.RenderLights
            SegmentedList.clear renderer.RenderTasks.RenderSkyBoxes
            renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Clear ()
            renderer.RenderTasks.RenderSurfacesDeferredRelative.Clear ()
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelativeSorted

            // destroy user-defined static models
            for staticModel in userDefinedStaticModelsToDestroy do
                GlRenderer3d.tryDestroyUserDefinedStaticModel staticModel renderer

            // end frame
            if renderer.RenderShouldEndFrame then
                OpenGL.Hl.EndFrame ()
                OpenGL.Hl.Assert ()

        member renderer.Swap () =
            match renderer.RenderWindow with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
            | WfglWindow window -> window.WfglSwapWindow ()

        member renderer.CleanUp () =
            let renderPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            for renderPackage in renderPackages do
                OpenGL.Texture.DeleteTexturesMemoized renderPackage.PackageState.TextureMemo
                OpenGL.CubeMap.DeleteCubeMapsMemoized renderPackage.PackageState.CubeMapMemo
            renderer.RenderPackages.Clear ()
            renderer.RenderAssimp.Dispose ()