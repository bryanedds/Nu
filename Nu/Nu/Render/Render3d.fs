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
// TODO: add TwoSidedOpt as render message parameter.                                   //
// TODO: account for Blend in billboards (at least alpha, overwrite, and additive)      //
// TODO: account for Flip in billboards.                                                //
// TODO: optimize billboard rendering with some sort of batch renderer.                 //
// TODO: introduce records for RenderTask cases.                                        //
// TODO: make sure you're destroying ALL rendering resources at end, incl. light maps!  //
//////////////////////////////////////////////////////////////////////////////////////////

/// Material properties for surfaces.
type [<StructuralEquality; NoComparison; SymbolicExpansion; Struct>] MaterialProperties =
    { AlbedoOpt : Color option
      MetallicOpt : single option
      RoughnessOpt : single option
      AmbientOcclusionOpt : single option
      EmissionOpt : single option
      HeightOpt : single option
      InvertRoughnessOpt : bool option }

/// Describes a static model surface.
and [<NoEquality; NoComparison>] SurfaceDescriptor =
    { Positions : Vector3 array
      TexCoordses : Vector2 array
      Normals : Vector3 array
      Indices : int array
      AffineMatrix : Matrix4x4
      Bounds : Box3
      MaterialProperties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties
      AlbedoImage : Image AssetTag
      MetallicImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      TextureMinFilterOpt : OpenGL.TextureMinFilter option
      TextureMagFilterOpt : OpenGL.TextureMagFilter option
      TwoSided : bool }

/// Describes billboard-based particles.
type [<NoEquality; NoComparison>] BillboardParticlesDescriptor =
    { Absolute : bool
      MaterialProperties : MaterialProperties
      AlbedoImage : Image AssetTag
      MetallicImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      MinFilterOpt : OpenGL.TextureMinFilter option
      MagFilterOpt : OpenGL.TextureMagFilter option
      RenderType : RenderType
      Particles : Particle SegmentedArray }

/// A collection of render tasks in a pass.
and [<ReferenceEquality>] RenderTasks =
    { RenderSkyBoxes : (Color * single * Color * single * CubeMap AssetTag) SegmentedList
      RenderLightProbes : SegmentedDictionary<uint64, struct (Vector3 * bool)>
      RenderLights : SortableLight SegmentedList
      RenderSurfacesDeferredAbsolute : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Box2 * MaterialProperties) SegmentedList>
      RenderSurfacesDeferredRelative : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Box2 * MaterialProperties) SegmentedList>
      RenderSurfacesForwardAbsolute : struct (single * single * Matrix4x4 * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardRelative : struct (single * single * Matrix4x4 * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardAbsoluteSorted : struct (Matrix4x4 * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardRelativeSorted : struct (Matrix4x4 * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList }

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
and [<NoEquality; NoComparison>] CachedStaticModelMessage =
    { mutable CachedStaticModelAbsolute : bool
      mutable CachedStaticModelMatrix : Matrix4x4
      mutable CachedStaticModelInsetOpt : Box2 voption
      mutable CachedStaticModelMaterialProperties : MaterialProperties
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModel : StaticModel AssetTag }

and [<ReferenceEquality>] CreateUserDefinedStaticModel =
    { SurfaceDescriptors : SurfaceDescriptor array
      Bounds : Box3
      StaticModel : StaticModel AssetTag }

and [<ReferenceEquality>] DestroyUserDefinedStaticModel =
    { StaticModel : StaticModel AssetTag }

and [<ReferenceEquality>] RenderLightProbe3d =
    { LightProbeId : uint64
      Origin : Vector3
      Stale : bool }

and [<ReferenceEquality>] RenderSkyBox =
    { AmbientColor : Color
      AmbientBrightness : single
      CubeMapColor : Color
      CubeMapBrightness : single
      CubeMap : CubeMap AssetTag }

and [<ReferenceEquality>] RenderLight3d =
    { Origin : Vector3
      Direction : Vector3
      Color : Color
      Brightness : single
      AttenuationLinear : single
      AttenuationQuadratic : single
      LightType : LightType }

and [<ReferenceEquality>] RenderBillboard =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      AlbedoImage : Image AssetTag
      MetallicImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      MinFilterOpt : OpenGL.TextureMinFilter option
      MagFilterOpt : OpenGL.TextureMagFilter option
      RenderType : RenderType }

and [<ReferenceEquality>] RenderBillboards =
    { Absolute : bool
      Billboards : (Matrix4x4 * Box2 option) SegmentedList
      MaterialProperties : MaterialProperties
      AlbedoImage : Image AssetTag
      MetallicImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      MinFilterOpt : OpenGL.TextureMinFilter option
      MagFilterOpt : OpenGL.TextureMagFilter option
      RenderType : RenderType }

and [<ReferenceEquality>] RenderBillboardParticles =
    { Absolute : bool
      MaterialProperties : MaterialProperties
      AlbedoImage : Image AssetTag
      MetallicImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      MinFilterOpt : OpenGL.TextureMinFilter option
      MagFilterOpt : OpenGL.TextureMagFilter option
      RenderType : RenderType
      Particles : Particle SegmentedArray }

and [<ReferenceEquality>] RenderStaticModelSurface =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      RenderType : RenderType
      StaticModel : StaticModel AssetTag
      SurfaceIndex : int }

and [<ReferenceEquality>] RenderStaticModel =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      RenderType : RenderType
      StaticModel : StaticModel AssetTag }

and [<ReferenceEquality>] RenderStaticModels =
    { Absolute : bool
      StaticModels : (Matrix4x4 * Box2 option * MaterialProperties) SegmentedList
      RenderType : RenderType
      StaticModel : StaticModel AssetTag }

and [<ReferenceEquality>] RenderUserDefinedStaticModel =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      RenderType : RenderType
      SurfaceDescriptors : SurfaceDescriptor array
      Bounds : Box3 }

/// A message to the 3d renderer.
and [<ReferenceEquality>] RenderMessage3d =
    | CreateUserDefinedStaticModel of CreateUserDefinedStaticModel
    | DestroyUserDefinedStaticModel of DestroyUserDefinedStaticModel
    | RenderSkyBox of RenderSkyBox
    | RenderLightProbe3d of RenderLightProbe3d
    | RenderLight3d of RenderLight3d
    | RenderBillboard of RenderBillboard
    | RenderBillboards of RenderBillboards
    | RenderBillboardParticles of RenderBillboardParticles
    | RenderStaticModelSurface of RenderStaticModelSurface
    | RenderStaticModel of RenderStaticModel
    | RenderStaticModels of RenderStaticModels
    | RenderCachedStaticModel of CachedStaticModelMessage
    | RenderUserDefinedStaticModel of RenderUserDefinedStaticModel
    | RenderPostPass3d of RenderPassMessage3d
    | LoadRenderPackage3d of string
    | UnloadRenderPackage3d of string
    | ReloadRenderAssets3d

/// A sortable light.
/// OPTIMIZATION: mutable field for caching distance squared.
and [<ReferenceEquality>] SortableLight =
    { SortableLightOrigin : Vector3
      SortableLightDirection : Vector3
      SortableLightColor : Color
      SortableLightBrightness : single
      SortableLightAttenuationLinear : single
      SortableLightAttenuationQuadratic : single
      SortableLightDirectional : int
      SortableLightConeInner : single
      SortableLightConeOuter : single
      mutable SortableLightDistanceSquared : single }

    /// Sort lights into array for uploading to OpenGL.
    /// TODO: consider getting rid of allocation here.
    static member sortLightsIntoArrays lightsMax position lights =
        let lightOrigins = Array.zeroCreate<single> (lightsMax * 3)
        let lightDirections = Array.zeroCreate<single> (lightsMax * 3)
        let lightColors = Array.zeroCreate<single> (lightsMax * 4)
        let lightBrightnesses = Array.zeroCreate<single> lightsMax
        let lightAttenuationLinears = Array.zeroCreate<single> lightsMax
        let lightAttenuationQuadratics = Array.zeroCreate<single> lightsMax
        let lightDirectionals = Array.zeroCreate<int> lightsMax
        let lightConeInners = Array.zeroCreate<single> lightsMax
        let lightConeOuters = Array.zeroCreate<single> lightsMax
        for light in lights do
            light.SortableLightDistanceSquared <- (light.SortableLightOrigin - position).MagnitudeSquared
        let lightsSorted = lights |> Seq.toArray |> Array.sortBy (fun light -> light.SortableLightDistanceSquared)
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let p = i * 3
                let c = i * 4
                let b = i
                let n = i
                let light = lightsSorted.[i]
                lightOrigins.[p] <- light.SortableLightOrigin.X
                lightOrigins.[p+1] <- light.SortableLightOrigin.Y
                lightOrigins.[p+2] <- light.SortableLightOrigin.Z
                lightDirections.[p] <- light.SortableLightDirection.X
                lightDirections.[p+1] <- light.SortableLightDirection.Y
                lightDirections.[p+2] <- light.SortableLightDirection.Z
                lightColors.[c] <- light.SortableLightColor.R
                lightColors.[c+1] <- light.SortableLightColor.G
                lightColors.[c+2] <- light.SortableLightColor.B
                lightColors.[c+3] <- light.SortableLightColor.A
                lightBrightnesses.[b] <- light.SortableLightBrightness
                lightAttenuationLinears.[n] <- light.SortableLightAttenuationLinear
                lightAttenuationQuadratics.[n] <- light.SortableLightAttenuationQuadratic
                lightDirectionals.[n] <- light.SortableLightDirectional
                lightConeInners.[n] <- light.SortableLightConeInner
                lightConeOuters.[n] <- light.SortableLightConeOuter
        (lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters)

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
          RenderSkyBoxShader : OpenGL.SkyBox.SkyBoxShader
          RenderIrradianceShader : OpenGL.CubeMap.CubeMapShader
          RenderEnvironmentFilterShader : OpenGL.LightMap.EnvironmentFilterShader
          RenderPhysicallyBasedForwardShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          RenderPhysicallyBasedDeferredShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          RenderPhysicallyBasedDeferred2Shader : OpenGL.PhysicallyBased.PhysicallyBasedDeferred2Shader
          RenderReflectionRenderbuffer : uint
          RenderReflectionFramebuffer : uint
          RenderIrradianceRenderbuffer : uint
          RenderIrradianceFramebuffer : uint
          RenderEnvironmentFilterRenderbuffer : uint
          RenderEnvironmentFilterFramebuffer : uint
          RenderGeometryBuffers : uint * uint * uint * uint * uint * uint
          RenderCubeMapGeometry : OpenGL.CubeMap.CubeMapGeometry
          RenderBillboardGeometry : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          RenderPhysicallyBasedQuad : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          RenderCubeMap : uint
          RenderIrradianceMap : uint
          RenderEnvironmentFilterMap : uint
          RenderBrdfTexture : uint
          RenderPhysicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial
          RenderLightMaps : Dictionary<uint64, OpenGL.LightMap.LightMap>
          mutable RenderModelsFields : single array
          mutable RenderTexCoordsOffsetsFields : single array
          mutable RenderAlbedosFields : single array
          mutable PhysicallyBasedMaterialsFields : single array
          mutable PhysicallyBasedHeightsFields : single array
          mutable PhysicallyBasedInvertRoughnessesFields : int array
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
        use assimp = new Assimp.AssimpContext ()
        match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (true, asset.FilePath, renderer.RenderPhysicallyBasedMaterial, packageState.TextureMemo, assimp) with
        | Right staticModel -> Some staticModel
        | Left error -> Log.debug ("Could not load static model '" + asset.FilePath + "' due to: " + error); None

    static member private tryLoadRenderAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match Path.GetExtension(asset.FilePath).ToLowerInvariant() with
        | ".bmp" | ".png" | ".tga" | ".jpg" | ".jpeg" | ".tif" | ".tiff" ->
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
                                match Path.GetExtension(asset.FilePath).ToLowerInvariant() with
                                | ".fbx" | ".obj" ->
                                    renderPackage.Assets.Remove asset.AssetTag.AssetName |> ignore<bool>
                                    OpenGL.PhysicallyBased.DestroyPhysicallyBasedStaticModel staticModel
                                    OpenGL.Hl.Assert ()
                                    match GlRenderer3d.tryLoadStaticModelAsset renderPackage.PackageState asset renderer with
                                    | Some staticModel -> renderPackage.Assets.Add (asset.AssetTag.AssetName, StaticModelAsset (userDefined, staticModel))
                                    | None -> ()
                                | _ -> ()
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

    static member private tryGetRenderAsset (assetTag : obj AssetTag) renderer =
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
            for (surfaceDescriptor : SurfaceDescriptor) in surfaceDescriptors do

                // get albedo metadata and texture
                let (albedoMetadata, albedoTexture) =
                    match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.AlbedoImage) renderer with
                    | ValueSome (TextureAsset (_, textureMetadata, texture)) -> (textureMetadata, texture)
                    | _ -> (renderer.RenderPhysicallyBasedMaterial.AlbedoMetadata, renderer.RenderPhysicallyBasedMaterial.AlbedoTexture)

                // make material properties
                let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
                    { Albedo = surfaceDescriptor.MaterialProperties.Albedo
                      Metallic = surfaceDescriptor.MaterialProperties.Metallic
                      Roughness = surfaceDescriptor.MaterialProperties.Roughness
                      AmbientOcclusion = surfaceDescriptor.MaterialProperties.AmbientOcclusion
                      Emission = surfaceDescriptor.MaterialProperties.Emission
                      Height = surfaceDescriptor.MaterialProperties.Height
                      InvertRoughness = surfaceDescriptor.MaterialProperties.InvertRoughness }

                // make material
                let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
                    { MaterialProperties = properties
                      AlbedoMetadata = albedoMetadata
                      AlbedoTexture = albedoTexture
                      MetallicTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.MetallicImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.MetallicTexture
                      RoughnessTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.RoughnessImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.RoughnessTexture
                      AmbientOcclusionTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.AmbientOcclusionImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.AmbientOcclusionTexture
                      EmissionTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.EmissionImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.EmissionTexture
                      NormalTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.NormalImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.NormalTexture
                      HeightTexture = match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize surfaceDescriptor.HeightImage) renderer with ValueSome (TextureAsset (_, _, texture)) -> texture | _ -> renderer.RenderPhysicallyBasedMaterial.HeightTexture
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
         affineMatrix : Matrix4x4,
         insetOpt : Box2 option,
         albedoMetadata : OpenGL.Texture.TextureMetadata,
         orientUp,
         properties,
         renderType,
         billboardSurface,
         renderer) =
        let texCoordsOffset =
            match insetOpt with
            | Some inset ->
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | None -> box2 v2Zero v2One // shouldn't we still be using borders?
        let billboardRotation =
            if orientUp then
                let eyeForward = (Vector3.Transform (v3Forward, eyeRotation)).WithY 0.0f
                let billboardAngle = if Vector3.Dot (eyeForward, v3Right) >= 0.0f then -eyeForward.AngleBetween v3Forward else eyeForward.AngleBetween v3Forward
                Matrix4x4.CreateFromQuaternion (Quaternion.CreateFromAxisAngle (v3Up, billboardAngle))
            else Matrix4x4.CreateFromQuaternion -eyeRotation
        let mutable affineRotation = affineMatrix
        affineRotation.Translation <- v3Zero
        let mutable billboardMatrix = affineMatrix * billboardRotation
        billboardMatrix.Translation <- affineMatrix.Translation
        match renderType with
        | DeferredRenderType ->
            if absolute then
                match renderer.RenderTasks.RenderSurfacesDeferredAbsolute.TryGetValue billboardSurface with
                | (true, renderTasks) -> SegmentedList.add struct (billboardMatrix, texCoordsOffset, properties) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Add (billboardSurface, SegmentedList.singleton (billboardMatrix, texCoordsOffset, properties))
            else
                match renderer.RenderTasks.RenderSurfacesDeferredRelative.TryGetValue billboardSurface with
                | (true, renderTasks) -> SegmentedList.add struct (billboardMatrix, texCoordsOffset, properties) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredRelative.Add (billboardSurface, SegmentedList.singleton (billboardMatrix, texCoordsOffset, properties))
        | ForwardRenderType (subsort, sort) ->
            if absolute
            then SegmentedList.add struct (subsort, sort, billboardMatrix, texCoordsOffset, properties, billboardSurface) renderer.RenderTasks.RenderSurfacesForwardAbsolute
            else SegmentedList.add struct (subsort, sort, billboardMatrix, texCoordsOffset, properties, billboardSurface) renderer.RenderTasks.RenderSurfacesForwardRelative

    static member private categorizeStaticModelSurface
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 option,
         properties : MaterialProperties inref,
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
            | Some inset ->
                let albedoMetadata = surface.SurfaceMaterial.AlbedoMetadata
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | None -> box2 v2Zero v2Zero
        match renderType with
        | DeferredRenderType ->
            if modelAbsolute then
                match renderer.RenderTasks.RenderSurfacesDeferredAbsolute.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, texCoordsOffset, properties) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Add (surface, SegmentedList.singleton (surfaceMatrix, texCoordsOffset, properties))
            else
                match renderer.RenderTasks.RenderSurfacesDeferredRelative.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, texCoordsOffset, properties) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredRelative.Add (surface, SegmentedList.singleton (surfaceMatrix, texCoordsOffset, properties))
        | ForwardRenderType (subsort, sort) ->
            if modelAbsolute
            then SegmentedList.add struct (subsort, sort, surfaceMatrix, texCoordsOffset, properties, surface) renderer.RenderTasks.RenderSurfacesForwardAbsolute
            else SegmentedList.add struct (subsort, sort, surfaceMatrix, texCoordsOffset, properties, surface) renderer.RenderTasks.RenderSurfacesForwardRelative

    static member private categorizeStaticModelSurfaceByIndex
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 option,
         properties : MaterialProperties inref,
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
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, insetOpt, &properties, renderType, true, surface, renderer)
            | _ -> Log.trace "Cannot render static model surface with a non-model asset."
        | _ -> Log.info ("Cannot render static model surface due to unloadable assets for '" + scstring staticModel + "'.")

    static member private categorizeStaticModel
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         insetOpt : Box2 option,
         properties : MaterialProperties inref,
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
                          SortableLightDirection = Vector3.Transform (v3Forward, lightMatrix.Rotation)
                          SortableLightColor = light.LightColor
                          SortableLightBrightness = light.LightBrightness
                          SortableLightAttenuationLinear = light.LightAttenuationLinear
                          SortableLightAttenuationQuadratic = light.LightAttenuationQuadratic
                          SortableLightDirectional = match light.PhysicallyBasedLightType with DirectionalLight -> 1 | _ -> 0
                          SortableLightConeInner = match light.PhysicallyBasedLightType with SpotLight (coneInner, _) -> coneInner | _ -> single (2.0 * Math.PI)
                          SortableLightConeOuter = match light.PhysicallyBasedLightType with SpotLight (_, coneOuter) -> coneOuter | _ -> single (2.0 * Math.PI)
                          SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderer.RenderTasks.RenderLights
                for surface in modelAsset.Surfaces do
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, insetOpt, &properties, renderType, false, surface, renderer)
            | _ -> Log.trace "Cannot render static model with a non-model asset."
        | _ -> Log.info ("Cannot render static model due to unloadable assets for '" + scstring staticModel + "'.")

    /// Create a reflection map.
    static member private createReflectionMap (currentViewport : Viewport) currentRenderbuffer currentFramebuffer origin renderbuffer framebuffer renderer =
        OpenGL.LightMap.CreateReflectionMap
            (GlRenderer3d.renderInternal renderer,
             currentViewport,
             currentRenderbuffer,
             currentFramebuffer,
             Constants.Render.ReflectionMapResolution,
             origin,
             renderbuffer,
             framebuffer)

    /// Create an irradiance map.
    static member private createIrradianceMap currentViewport currentRenderbuffer currentFramebuffer renderbuffer framebuffer shader skyBoxSurface =
        OpenGL.LightMap.CreateIrradianceMap
            (currentViewport,
             currentRenderbuffer,
             currentFramebuffer,
             Constants.Render.IrradianceMapResolution,
             renderbuffer,
             framebuffer,
             shader,
             skyBoxSurface)

    /// Create an environment filter map.
    static member private createEnvironmentFilterMap currentViewport currentFramebuffer currentRenderbuffer renderbuffer framebuffer shader skyBoxSurface =
        OpenGL.LightMap.CreateEnvironmentFilterMap
            (currentViewport,
             currentRenderbuffer,
             currentFramebuffer,
             Constants.Render.EnvironmentFilterResolution,
             renderbuffer,
             framebuffer,
             shader,
             skyBoxSurface)

    static member private getLastSkyBoxOpt renderer =
        match Seq.tryLast renderer.RenderTasks.RenderSkyBoxes with
        | Some (lightAmbientColor, lightAmbientBrightness, cubeMapColor, cubeMapBrightness, cubeMapAsset) ->
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize cubeMapAsset) renderer with
            | ValueSome asset ->
                match asset with
                | CubeMapAsset (_, cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef) ->
                    let cubeMapOpt = Some (cubeMapColor, cubeMapBrightness, cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef)
                    (lightAmbientColor, lightAmbientBrightness, cubeMapOpt)
                | _ ->
                    Log.debug "Could not utilize sky box due to mismatched cube map asset."
                    (lightAmbientColor, lightAmbientBrightness, None)
            | ValueNone ->
                Log.debug "Could not utilize sky box due to non-existent cube map asset."
                (lightAmbientColor, lightAmbientBrightness, None)
        | None -> (Color.White, 1.0f, None)

    static member private getLightMapFallback viewport geometryRenderbuffer geometryFramebuffer skyBoxOpt renderer =
        match skyBoxOpt with
        | Some (_, _, cubeMap, irradianceAndEnvironmentMapsOptRef : _ ref) ->
            if Option.isNone irradianceAndEnvironmentMapsOptRef.Value then
                let irradianceMap =
                    GlRenderer3d.createIrradianceMap
                        viewport
                        geometryRenderbuffer
                        geometryFramebuffer
                        renderer.RenderIrradianceRenderbuffer
                        renderer.RenderIrradianceFramebuffer
                        renderer.RenderIrradianceShader
                        (OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.RenderCubeMapGeometry)
                let environmentFilterMap =
                    GlRenderer3d.createEnvironmentFilterMap
                        viewport
                        geometryRenderbuffer
                        geometryFramebuffer
                        renderer.RenderEnvironmentFilterRenderbuffer
                        renderer.RenderEnvironmentFilterFramebuffer
                        renderer.RenderEnvironmentFilterShader
                        (OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.RenderCubeMapGeometry)
                irradianceAndEnvironmentMapsOptRef.Value <- Some (irradianceMap, environmentFilterMap)
                OpenGL.LightMap.CreateLightMap v3Zero cubeMap irradianceMap environmentFilterMap
            else
                let (irradianceMap, environmentFilterMap) = Option.get irradianceAndEnvironmentMapsOptRef.Value
                OpenGL.LightMap.CreateLightMap v3Zero cubeMap irradianceMap environmentFilterMap
        | None ->
            OpenGL.LightMap.CreateLightMap v3Zero renderer.RenderCubeMap renderer.RenderIrradianceMap renderer.RenderEnvironmentFilterMap

    static member private sortLightMaps eyeCenter lightMaps =
        lightMaps |>
        Seq.map (fun (lightMap : OpenGL.LightMap.LightMap) -> (lightMap, (lightMap.Origin - eyeCenter).MagnitudeSquared)) |>
        Seq.toArray |> // TODO: use a preallocated array to avoid allocating on the LOH.
        Array.sortByDescending snd |>
        Array.map fst

    static member private sortSurfaces eyeCenter (surfaces : struct (single * single * Matrix4x4 * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList) =
        surfaces |>
        Seq.map (fun struct (subsort, sort, model, texCoordsOffset, properties, surface) -> struct (subsort, sort, model, texCoordsOffset, properties, surface, (model.Translation - eyeCenter).MagnitudeSquared)) |>
        Seq.toArray |> // TODO: use a preallocated array to avoid allocating on the LOH.
        Array.sortByDescending (fun struct (subsort, sort, _, _, _, _, distanceSquared) -> struct (sort, distanceSquared, subsort)) |>
        Array.map (fun struct (_, _, model, texCoordsOffset, propertiesOpt, surface, _) -> struct (model, texCoordsOffset, propertiesOpt, surface))

    static member private renderPhysicallyBasedSurfaces
        viewArray
        projectionArray
        eyeCenter
        (parameters : struct (Matrix4x4 * Box2 * MaterialProperties) SegmentedList)
        blending
        lightAmbientColor
        lightAmbientBrightness
        irradianceMap
        environmentFilterMap
        brdfTexture
        lightOrigins
        lightDirections
        lightColors
        lightBrightnesses
        lightAttenuationLinears
        lightAttenuationQuadratics
        lightDirectionals
        lightConeInners
        lightConeOuters
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
            let mutable length = renderer.PhysicallyBasedMaterialsFields.Length
            while parameters.Length * 4 > length do length <- length * 2
            if renderer.PhysicallyBasedMaterialsFields.Length < length then
                renderer.PhysicallyBasedMaterialsFields <- Array.zeroCreate<single> length

            // ensure we have a large enough heights fields array
            let mutable length = renderer.PhysicallyBasedHeightsFields.Length
            while parameters.Length > length do length <- length * 2
            if renderer.PhysicallyBasedHeightsFields.Length < length then
                renderer.PhysicallyBasedHeightsFields <- Array.zeroCreate<single> length

            // ensure we have a large enough invert roughnesses fields array
            let mutable length = renderer.PhysicallyBasedInvertRoughnessesFields.Length
            while parameters.Length > length do length <- length * 2
            if renderer.PhysicallyBasedInvertRoughnessesFields.Length < length then
                renderer.PhysicallyBasedInvertRoughnessesFields <- Array.zeroCreate<int> length

            // blit parameters to field arrays
            for i in 0 .. dec parameters.Length do
                let struct (model, texCoordsOffset, properties) = parameters.[i]
                model.ToArray (renderer.RenderModelsFields, i * 16)
                renderer.RenderTexCoordsOffsetsFields.[i * 4] <- texCoordsOffset.Min.X
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 1] <- texCoordsOffset.Min.Y
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
                renderer.RenderTexCoordsOffsetsFields.[i * 4 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
                let (albedo, metallic, roughness, ambientOcclusion, emission, height, invertRoughness) =
                    ((match properties.AlbedoOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.Albedo),
                     (match properties.MetallicOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.Metallic),
                     (match properties.RoughnessOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.Roughness),
                     (match properties.AmbientOcclusionOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.AmbientOcclusion),
                     (match properties.EmissionOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.Emission),
                     (match properties.HeightOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.Height),
                     (match properties.InvertRoughnessOpt with Some value -> value | None -> surface.SurfaceMaterial.MaterialProperties.InvertRoughness))
                renderer.RenderAlbedosFields.[i * 4] <- albedo.R
                renderer.RenderAlbedosFields.[i * 4 + 1] <- albedo.G
                renderer.RenderAlbedosFields.[i * 4 + 2] <- albedo.B
                renderer.RenderAlbedosFields.[i * 4 + 3] <- albedo.A
                renderer.PhysicallyBasedMaterialsFields.[i * 4] <- metallic
                renderer.PhysicallyBasedMaterialsFields.[i * 4 + 1] <- ambientOcclusion
                renderer.PhysicallyBasedMaterialsFields.[i * 4 + 2] <- roughness
                renderer.PhysicallyBasedMaterialsFields.[i * 4 + 3] <- emission
                renderer.PhysicallyBasedHeightsFields.[i] <- surface.SurfaceMaterial.AlbedoMetadata.TextureTexelHeight * height
                renderer.PhysicallyBasedInvertRoughnessesFields.[i] <- if invertRoughness then 1 else 0

            // draw surfaces
            OpenGL.PhysicallyBased.DrawPhysicallyBasedSurfaces
                (viewArray, projectionArray, eyeCenter, parameters.Length, renderer.RenderModelsFields, renderer.RenderTexCoordsOffsetsFields, renderer.RenderAlbedosFields, renderer.PhysicallyBasedMaterialsFields, renderer.PhysicallyBasedHeightsFields, renderer.PhysicallyBasedInvertRoughnessesFields,
                 blending, lightAmbientColor, lightAmbientBrightness, irradianceMap, environmentFilterMap, brdfTexture,
                 lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters,
                 surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader)

    static member inline private makeBillboardMaterial (properties : MaterialProperties) albedoImage metallicImage roughnessImage ambientOcclusionImage emissionImage normalImage heightImage minFilterOpt magFilterOpt renderer =
        let (albedoMetadata, albedoTexture) =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize albedoImage) renderer with
            | ValueSome (TextureAsset (_, textureMetadata, texture)) -> (textureMetadata, texture)
            | _ -> (OpenGL.Texture.TextureMetadata.empty, renderer.RenderPhysicallyBasedMaterial.AlbedoTexture)
        let metallicTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize metallicImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.MetallicTexture
        let roughnessTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize roughnessImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.RoughnessTexture
        let ambientOcclusionTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize ambientOcclusionImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize emissionImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.EmissionTexture
        let normalTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize normalImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.NormalTexture
        let heightTexture =
            match GlRenderer3d.tryGetRenderAsset (AssetTag.generalize heightImage) renderer with
            | ValueSome (TextureAsset (_, _, texture)) -> texture
            | _ -> renderer.RenderPhysicallyBasedMaterial.HeightTexture
        let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
            { Albedo = Option.defaultValue Constants.Render.AlbedoDefault properties.AlbedoOpt
              Metallic = Option.defaultValue Constants.Render.MetallicDefault properties.MetallicOpt
              Roughness = Option.defaultValue Constants.Render.RoughnessDefault properties.RoughnessOpt
              AmbientOcclusion = Option.defaultValue Constants.Render.AmbientOcclusionDefault properties.AmbientOcclusionOpt
              Emission = Option.defaultValue Constants.Render.EmissionDefault properties.EmissionOpt
              Height = Option.defaultValue Constants.Render.HeightDefault properties.HeightOpt
              InvertRoughness = false }
        let billboardMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { MaterialProperties = properties
              AlbedoMetadata = albedoMetadata
              AlbedoTexture = albedoTexture
              MetallicTexture = metallicTexture
              RoughnessTexture = roughnessTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TextureMinFilterOpt = minFilterOpt
              TextureMagFilterOpt = magFilterOpt
              TwoSided = true }
        billboardMaterial

    static member private renderInternal
        renderer
        (topLevelRender : bool)
        (eyeCenter : Vector3)
        (viewAbsolute : Matrix4x4)
        (viewRelative : Matrix4x4)
        (viewSkyBox : Matrix4x4)
        (projection : Matrix4x4)
        (viewport : Viewport)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // compute matrix arrays
        let viewAbsoluteArray = viewAbsolute.ToArray ()
        let viewRelativeArray = viewRelative.ToArray ()
        let viewSkyBoxArray = viewSkyBox.ToArray ()
        let projectionArray = projection.ToArray ()

        // setup geometry buffer
        let (positionTexture, albedoTexture, materialTexture, normalAndDepthTexture, geometryRenderbuffer, geometryFramebuffer) = renderer.RenderGeometryBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, geometryRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
        OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
        OpenGL.Gl.Scissor (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Size.X, viewport.Bounds.Size.Y)
        OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest
        OpenGL.Hl.Assert ()

        // get sky box elements
        let (lightAmbientColor, lightAmbientBrightness, skyBoxOpt) = GlRenderer3d.getLastSkyBoxOpt renderer
        let lightAmbientColor = [|lightAmbientColor.R; lightAmbientColor.G; lightAmbientColor.B|]

        // collect light maps, rendering those that don't yet have an entry
        let lightMaps = List ()
        for lightProbeKvp in renderer.RenderTasks.RenderLightProbes do
            let lightProbeId = lightProbeKvp.Key
            let struct (lightProbeOrigin, lightProbeStale) = lightProbeKvp.Value
            match renderer.RenderLightMaps.TryGetValue lightProbeId with
            | (true, value) when not lightProbeStale -> lightMaps.Add value
            | (found, valueOpt) ->

                // render light map if in top level render
                if topLevelRender then

                    // destroy light map if already exists
                    if found then
                        OpenGL.LightMap.DestroyLightMap valueOpt
                        renderer.RenderLightMaps.Remove lightProbeId |> ignore<bool>

                    // create reflection map
                    let reflectionMap =
                        GlRenderer3d.createReflectionMap
                            viewport
                            renderbuffer
                            framebuffer
                            lightProbeOrigin
                            renderer.RenderReflectionRenderbuffer
                            renderer.RenderReflectionFramebuffer
                            renderer

                    // create irradiance map
                    let irradianceMap =
                        GlRenderer3d.createIrradianceMap
                            viewport
                            renderbuffer
                            framebuffer
                            renderer.RenderIrradianceRenderbuffer
                            renderer.RenderIrradianceFramebuffer
                            renderer.RenderIrradianceShader
                            (OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.RenderCubeMapGeometry)

                    // create env filter map
                    let environmentFilterMap =
                        GlRenderer3d.createEnvironmentFilterMap
                            viewport
                            renderbuffer
                            framebuffer
                            renderer.RenderEnvironmentFilterRenderbuffer
                            renderer.RenderEnvironmentFilterFramebuffer
                            renderer.RenderEnvironmentFilterShader
                            (OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.RenderCubeMapGeometry)

                    // construct light map
                    let lightMap = OpenGL.LightMap.CreateLightMap lightProbeOrigin reflectionMap irradianceMap environmentFilterMap

                    // add light map
                    renderer.RenderLightMaps.Add (lightProbeId, lightMap)

        // destroy light maps that aren't tasked to render
        for lightMapKvp in renderer.RenderLightMaps do
            if not (SegmentedDictionary.containsKey lightMapKvp.Key renderer.RenderTasks.RenderLightProbes) then
                if topLevelRender then
                    OpenGL.LightMap.DestroyLightMap lightMapKvp.Value
                    renderer.RenderLightMaps.Remove lightMapKvp.Key |> ignore<bool>

        // sort light maps relative to eye center
        let lightMapsSorted = GlRenderer3d.sortLightMaps eyeCenter lightMaps

        // sort lights for deferred relative to eye center
        let (lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters) =
            SortableLight.sortLightsIntoArrays Constants.Render.DeferredLightsMax eyeCenter renderer.RenderTasks.RenderLights

        // sort absolute forward surfaces
        let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyeCenter renderer.RenderTasks.RenderSurfacesForwardAbsolute
        SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
        SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsolute

        // sort relative forward surfaces
        let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyeCenter renderer.RenderTasks.RenderSurfacesForwardRelative
        SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardRelativeSorted
        SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelative

        // get light mapping elements
        let lightMapFallback = GlRenderer3d.getLightMapFallback viewport geometryRenderbuffer geometryFramebuffer skyBoxOpt renderer
        let lightMap = Seq.headOrDefault lightMapsSorted lightMapFallback

        // deferred render surfaces w/ absolute transforms if in top level render
        if topLevelRender then
            for entry in renderer.RenderTasks.RenderSurfacesDeferredAbsolute do
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    viewAbsoluteArray
                    projectionArray
                    eyeCenter
                    entry.Value
                    false
                    lightAmbientColor
                    lightAmbientBrightness
                    lightMap.IrradianceMap
                    lightMap.EnvironmentFilterMap
                    renderer.RenderBrdfTexture
                    lightOrigins
                    lightDirections
                    lightColors
                    lightBrightnesses
                    lightAttenuationLinears
                    lightAttenuationQuadratics
                    lightDirectionals
                    lightConeInners
                    lightConeOuters
                    entry.Key
                    renderer.RenderPhysicallyBasedDeferredShader
                    renderer
                OpenGL.Hl.Assert ()

        // deferred render surfaces w/ relative transforms
        for entry in renderer.RenderTasks.RenderSurfacesDeferredRelative do
            GlRenderer3d.renderPhysicallyBasedSurfaces
                viewRelativeArray
                projectionArray
                eyeCenter
                entry.Value
                false
                lightAmbientColor
                lightAmbientBrightness
                lightMap.IrradianceMap
                lightMap.EnvironmentFilterMap
                renderer.RenderBrdfTexture
                lightOrigins
                lightDirections
                lightColors
                lightBrightnesses
                lightAttenuationLinears
                lightAttenuationQuadratics
                lightDirectionals
                lightConeInners
                lightConeOuters
                entry.Key
                renderer.RenderPhysicallyBasedDeferredShader
                renderer
            OpenGL.Hl.Assert ()

        // copy depths from geometry framebuffer to output framebuffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, geometryFramebuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, framebuffer)
        OpenGL.Gl.BlitFramebuffer
            (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Size.X, viewport.Bounds.Size.Y,
             viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Size.X, viewport.Bounds.Size.Y,
             OpenGL.ClearBufferMask.DepthBufferBit,
             OpenGL.BlitFramebufferFilter.Nearest)
        OpenGL.Hl.Assert ()

        // switch to output framebuffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Hl.Assert ()

        // deferred render lighting quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferred2Surface
            (viewRelativeArray, projectionArray, eyeCenter, lightAmbientColor, lightAmbientBrightness, positionTexture, albedoTexture, materialTexture, normalAndDepthTexture, lightMap.IrradianceMap, lightMap.EnvironmentFilterMap, renderer.RenderBrdfTexture,
             lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters,
             renderer.RenderPhysicallyBasedQuad, renderer.RenderPhysicallyBasedDeferred2Shader)
        OpenGL.Hl.Assert ()

        // attempt to render sky box
        match skyBoxOpt with
        | Some (cubeMapColor, cubeMapBrightness, cubeMap, _) ->
            let cubeMapColor = [|cubeMapColor.R; cubeMapColor.G; cubeMapColor.B|]
            OpenGL.SkyBox.DrawSkyBox (viewSkyBoxArray, projectionArray, cubeMapColor, cubeMapBrightness, cubeMap, renderer.RenderCubeMapGeometry, renderer.RenderSkyBoxShader)
            OpenGL.Hl.Assert ()
        | None -> ()

        // forward render surfaces w/ absolute transforms if in top level render
        if topLevelRender then
            for (model, texCoordsOffset, properties, surface) in renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted do
                let (lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters) =
                    SortableLight.sortLightsIntoArrays Constants.Render.ForwardLightsMax model.Translation renderer.RenderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    viewAbsoluteArray
                    projectionArray
                    eyeCenter
                    (SegmentedList.singleton (model, texCoordsOffset, properties))
                    true
                    lightAmbientColor
                    lightAmbientBrightness
                    lightMap.IrradianceMap
                    lightMap.EnvironmentFilterMap
                    renderer.RenderBrdfTexture
                    lightOrigins
                    lightDirections
                    lightColors
                    lightBrightnesses
                    lightAttenuationLinears
                    lightAttenuationQuadratics
                    lightDirectionals
                    lightConeInners
                    lightConeOuters
                    surface
                    renderer.RenderPhysicallyBasedForwardShader
                    renderer
                OpenGL.Hl.Assert ()

        // forward render surfaces w/ relative transforms
        for (model, texCoordsOffset, properties, surface) in renderer.RenderTasks.RenderSurfacesForwardRelativeSorted do
            let (lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightDirectionals, lightConeInners, lightConeOuters) =
                SortableLight.sortLightsIntoArrays Constants.Render.ForwardLightsMax model.Translation renderer.RenderTasks.RenderLights
            GlRenderer3d.renderPhysicallyBasedSurfaces
                viewRelativeArray
                projectionArray
                eyeCenter
                (SegmentedList.singleton (model, texCoordsOffset, properties))
                true
                lightAmbientColor
                lightAmbientBrightness
                lightMap.IrradianceMap
                lightMap.EnvironmentFilterMap
                renderer.RenderBrdfTexture
                lightOrigins
                lightDirections
                lightColors
                lightBrightnesses
                lightAttenuationLinears
                lightAttenuationQuadratics
                lightDirectionals
                lightConeInners
                lightConeOuters
                surface
                renderer.RenderPhysicallyBasedForwardShader
                renderer
            OpenGL.Hl.Assert ()

    static member render eyeCenter (eyeRotation : Quaternion) windowSize renderbuffer framebuffer renderMessages renderer =

        // categorize messages
        // TODO: consider implementing some exception safety for the stateful operations in this function.
        let userDefinedStaticModelsToDestroy = SegmentedList.make ()
        let postPasses = hashSetPlus<RenderPassMessage3d> HashIdentity.Structural []
        for message in renderMessages do
            match message with
            | CreateUserDefinedStaticModel cudsm ->
                GlRenderer3d.tryCreateUserDefinedStaticModel cudsm.SurfaceDescriptors cudsm.Bounds cudsm.StaticModel renderer
            | DestroyUserDefinedStaticModel dudsm ->
                SegmentedList.add dudsm.StaticModel userDefinedStaticModelsToDestroy
            | RenderSkyBox rsb ->
                SegmentedList.add (rsb.AmbientColor, rsb.AmbientBrightness, rsb.CubeMapColor, rsb.CubeMapBrightness, rsb.CubeMap) renderer.RenderTasks.RenderSkyBoxes
            | RenderLightProbe3d lp ->
                if SegmentedDictionary.containsKey lp.LightProbeId renderer.RenderTasks.RenderLightProbes then
                    Log.debugOnce ("Multiple light probe messages coming in with the same id of '" + string lp.LightProbeId + "'.")
                    SegmentedDictionary.remove lp.LightProbeId renderer.RenderTasks.RenderLightProbes |> ignore<bool>
                SegmentedDictionary.add lp.LightProbeId struct (lp.Origin, lp.Stale) renderer.RenderTasks.RenderLightProbes
            | RenderLight3d rl3 ->
                let light =
                    { SortableLightOrigin = rl3.Origin
                      SortableLightDirection = rl3.Direction
                      SortableLightColor = rl3.Color
                      SortableLightBrightness = rl3.Brightness
                      SortableLightAttenuationLinear = rl3.AttenuationLinear
                      SortableLightAttenuationQuadratic = rl3.AttenuationQuadratic
                      SortableLightDirectional = match rl3.LightType with DirectionalLight -> 1 | _ -> 0
                      SortableLightConeInner = match rl3.LightType with SpotLight (coneInner, _) -> coneInner | _ -> single (2.0 * Math.PI)
                      SortableLightConeOuter = match rl3.LightType with SpotLight (_, coneOuter) -> coneOuter | _ -> single (2.0 * Math.PI)
                      SortableLightDistanceSquared = Single.MaxValue }
                SegmentedList.add light renderer.RenderTasks.RenderLights
            | RenderBillboard rb ->
                let billboardMaterial = GlRenderer3d.makeBillboardMaterial rb.MaterialProperties rb.AlbedoImage rb.MetallicImage rb.RoughnessImage rb.AmbientOcclusionImage rb.EmissionImage rb.NormalImage rb.HeightImage rb.MinFilterOpt rb.MagFilterOpt renderer
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3 (v3 -0.5f 0.5f -0.5f) v3One, billboardMaterial, renderer.RenderBillboardGeometry)
                GlRenderer3d.categorizeBillboardSurface (rb.Absolute, eyeRotation, rb.ModelMatrix, rb.InsetOpt, billboardMaterial.AlbedoMetadata, true, rb.MaterialProperties, rb.RenderType, billboardSurface, renderer)
            | RenderBillboards rbs ->
                let billboardMaterial = GlRenderer3d.makeBillboardMaterial rbs.MaterialProperties rbs.AlbedoImage rbs.MetallicImage rbs.RoughnessImage rbs.AmbientOcclusionImage rbs.EmissionImage rbs.NormalImage rbs.HeightImage rbs.MinFilterOpt rbs.MagFilterOpt renderer
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3 (v3 -0.5f -0.5f -0.5f) v3One, billboardMaterial, renderer.RenderBillboardGeometry)
                for (modelMatrix, insetOpt) in rbs.Billboards do
                    GlRenderer3d.categorizeBillboardSurface (rbs.Absolute, eyeRotation, modelMatrix, insetOpt, billboardMaterial.AlbedoMetadata, true, rbs.MaterialProperties, rbs.RenderType, billboardSurface, renderer)
            | RenderBillboardParticles rbps ->
                let billboardMaterial = GlRenderer3d.makeBillboardMaterial rbps.MaterialProperties rbps.AlbedoImage rbps.MetallicImage rbps.RoughnessImage rbps.AmbientOcclusionImage rbps.EmissionImage rbps.NormalImage rbps.HeightImage rbps.MinFilterOpt rbps.MagFilterOpt renderer
                for particle in rbps.Particles do
                    let billboardMatrix =
                        Matrix4x4.CreateFromTrs
                            (particle.Transform.Center,
                             particle.Transform.Rotation,
                             particle.Transform.Size * particle.Transform.Scale)
                    let billboardMaterialProperties = { billboardMaterial.MaterialProperties with Albedo = billboardMaterial.MaterialProperties.Albedo * particle.Color; Emission = particle.Emission.R }
                    let billboardMaterial = { billboardMaterial with MaterialProperties = billboardMaterialProperties }
                    let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], m4Identity, box3Zero, billboardMaterial, renderer.RenderBillboardGeometry)
                    GlRenderer3d.categorizeBillboardSurface (rbps.Absolute, eyeRotation, billboardMatrix, Option.ofValueOption particle.InsetOpt, billboardMaterial.AlbedoMetadata, false, rbps.MaterialProperties, rbps.RenderType, billboardSurface, renderer)
            | RenderStaticModelSurface rsms ->
                GlRenderer3d.categorizeStaticModelSurfaceByIndex (rsms.Absolute, &rsms.ModelMatrix, rsms.InsetOpt, &rsms.MaterialProperties, rsms.RenderType, rsms.StaticModel, rsms.SurfaceIndex, renderer)
            | RenderStaticModel rsm ->
                GlRenderer3d.categorizeStaticModel (rsm.Absolute, &rsm.ModelMatrix, rsm.InsetOpt, &rsm.MaterialProperties, rsm.RenderType, rsm.StaticModel, renderer)
            | RenderStaticModels rsms ->
                for (modelMatrix, insetOpt, properties) in rsms.StaticModels do
                    GlRenderer3d.categorizeStaticModel (rsms.Absolute, &modelMatrix, insetOpt, &properties, rsms.RenderType, rsms.StaticModel, renderer)
            | RenderCachedStaticModel d ->
                GlRenderer3d.categorizeStaticModel (d.CachedStaticModelAbsolute, &d.CachedStaticModelMatrix, Option.ofValueOption d.CachedStaticModelInsetOpt, &d.CachedStaticModelMaterialProperties, d.CachedStaticModelRenderType, d.CachedStaticModel, renderer)
            | RenderUserDefinedStaticModel renderUdsm ->
                let assetTag = asset Assets.Default.PackageName Gen.name // TODO: see if we should instead use a specialized package for temporary assets like these.
                GlRenderer3d.tryCreateUserDefinedStaticModel renderUdsm.SurfaceDescriptors renderUdsm.Bounds assetTag renderer
                GlRenderer3d.categorizeStaticModel (renderUdsm.Absolute, &renderUdsm.ModelMatrix, renderUdsm.InsetOpt, &renderUdsm.MaterialProperties, renderUdsm.RenderType, assetTag, renderer)
                SegmentedList.add assetTag userDefinedStaticModelsToDestroy
            | RenderPostPass3d postPass ->
                postPasses.Add postPass |> ignore<bool>
            | LoadRenderPackage3d hintPackageUse ->
                GlRenderer3d.handleLoadRenderPackage hintPackageUse renderer
            | UnloadRenderPackage3d hintPackageDisuse ->
                GlRenderer3d.handleUnloadRenderPackage hintPackageDisuse renderer
            | ReloadRenderAssets3d ->
                GlRenderer3d.handleReloadRenderAssets renderer

        // compute the viewport with the given offset
        let viewportOffset = Constants.Render.ViewportOffset windowSize

        // compute view and projection
        let eyeTarget = eyeCenter + Vector3.Transform (v3Forward, eyeRotation)
        let viewAbsolute = m4Identity
        let viewRelative = Matrix4x4.CreateLookAt (eyeCenter, eyeTarget, v3Up)
        let viewSkyBox = Matrix4x4.CreateFromQuaternion (Quaternion.Inverse eyeRotation)
        let viewport = Constants.Render.Viewport
        let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
        OpenGL.Hl.Assert ()

        // top-level render
        GlRenderer3d.renderInternal renderer true eyeCenter viewAbsolute viewRelative viewSkyBox projection viewportOffset renderbuffer framebuffer
        
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
        SegmentedList.clear renderer.RenderTasks.RenderSkyBoxes
        SegmentedDictionary.clear renderer.RenderTasks.RenderLightProbes
        SegmentedList.clear renderer.RenderTasks.RenderLights
        renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Clear ()
        renderer.RenderTasks.RenderSurfacesDeferredRelative.Clear ()
        SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
        SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelativeSorted

        // destroy user-defined static models
        for staticModel in userDefinedStaticModelsToDestroy do
            GlRenderer3d.tryDestroyUserDefinedStaticModel staticModel renderer

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
        let irradianceShader = OpenGL.CubeMap.CreateCubeMapShader Constants.Paths.IrradianceShaderFilePath
        OpenGL.Hl.Assert ()

        // create environment filter shader
        let environmentFilterShader = OpenGL.LightMap.CreateEnvironmentFilterShader Constants.Paths.EnvironmentFilterShaderFilePath
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

        // create reflection map renderbuffer
        let reflectionMapRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, reflectionMapRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.Depth24Stencil8, Constants.Render.ReflectionMapResolution, Constants.Render.ReflectionMapResolution)
        OpenGL.Hl.Assert ()

        // create reflection map framebuffer
        let reflectionMapFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, reflectionMapFramebuffer)
        //OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthStencilAttachment, OpenGL.RenderbufferTarget.Renderbuffer, reflectionMapRenderbuffer)
        Log.debugIf (fun () -> OpenGL.Gl.CheckFramebufferStatus OpenGL.FramebufferTarget.Framebuffer <> OpenGL.FramebufferStatus.FramebufferComplete) "Reflection map framebuffer is incomplete!"
        OpenGL.Hl.Assert ()

        // create irradiance renderbuffer
        let irradianceRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, irradianceRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent24, Constants.Render.IrradianceMapResolution, Constants.Render.IrradianceMapResolution)
        OpenGL.Hl.Assert ()

        // create irradiance framebuffer
        let irradianceFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, irradianceFramebuffer)
        //OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthAttachment, OpenGL.RenderbufferTarget.Renderbuffer, irradianceRenderbuffer)
        Log.debugIf (fun () -> OpenGL.Gl.CheckFramebufferStatus OpenGL.FramebufferTarget.Framebuffer <> OpenGL.FramebufferStatus.FramebufferComplete) "Irradiance framebuffer is incomplete!"
        OpenGL.Hl.Assert ()

        // create environment filter renderbuffer
        let environmentFilterRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent24, Constants.Render.EnvironmentFilterResolution, Constants.Render.EnvironmentFilterResolution)
        OpenGL.Hl.Assert ()

        // create environment filter framebuffer
        let environmentFilterFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, environmentFilterFramebuffer)
        OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthAttachment, OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        Log.debugIf (fun () -> OpenGL.Gl.CheckFramebufferStatus OpenGL.FramebufferTarget.Framebuffer <> OpenGL.FramebufferStatus.FramebufferComplete) "Environment filter framebuffer is incomplete!"
        OpenGL.Hl.Assert ()

        // create geometry buffers
        let geometryBuffers =
            match OpenGL.Framebuffer.TryCreateGeometryBuffers () with
            | Right geometryBuffers -> geometryBuffers
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

        // create cube map geometry
        let cubeMapGeometry = OpenGL.CubeMap.CreateCubeMapGeometry true
        OpenGL.Hl.Assert ()

        // create billboard geometry
        let billboardGeometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedBillboard true
        OpenGL.Hl.Assert ()

        // create physically-based quad
        let physicallyBasedQuad = OpenGL.PhysicallyBased.CreatePhysicallyBasedQuad true
        OpenGL.Hl.Assert ()

        // create sky box surface
        let skyBoxSurface = OpenGL.CubeMap.CubeMapSurface.make skyBoxMap cubeMapGeometry
        OpenGL.Hl.Assert ()

        // create default irradiance map
        let irradianceMap = GlRenderer3d.createIrradianceMap Constants.Render.Viewport 0u 0u irradianceRenderbuffer irradianceFramebuffer irradianceShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create default environment filter map
        let environmentFilterMap = GlRenderer3d.createEnvironmentFilterMap Constants.Render.Viewport 0u 0u environmentFilterRenderbuffer environmentFilterFramebuffer environmentFilterShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create brdf texture
        let brdfTexture =
            match OpenGL.Texture.TryCreateTextureUnfiltered (Constants.Paths.BrdfTextureFilePath) with
            | Right (_, texture) -> texture
            | Left error -> failwith ("Could not load BRDF texture due to: " + error)

        // get albedo metadata and texture
        let (albedoMetadata, albedoTexture) = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAlbedo.png") |> Either.getRight

        // create default physically-based material properties
        let physicallyBasedMaterialProperties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
            { Albedo = Constants.Render.AlbedoDefault
              Metallic = Constants.Render.MetallicDefault
              Roughness = Constants.Render.RoughnessDefault
              AmbientOcclusion = Constants.Render.AmbientOcclusionDefault
              Emission = Constants.Render.EmissionDefault
              Height = Constants.Render.HeightDefault
              InvertRoughness = false }

        // create default physically-based material
        let physicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { MaterialProperties = physicallyBasedMaterialProperties
              AlbedoMetadata = albedoMetadata
              AlbedoTexture = albedoTexture
              MetallicTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialMetallic.png") |> Either.getRight |> snd
              RoughnessTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialRoughness.png") |> Either.getRight |> snd
              AmbientOcclusionTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAmbientOcclusion.png") |> Either.getRight |> snd
              EmissionTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialEmission.png") |> Either.getRight |> snd
              NormalTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialNormal.png") |> Either.getRight |> snd
              HeightTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialHeight.png") |> Either.getRight |> snd
              TextureMinFilterOpt = None
              TextureMagFilterOpt = None
              TwoSided = false }

        // create render tasks
        let renderTasks =
            { RenderSkyBoxes = SegmentedList.make ()
              RenderLightProbes = SegmentedDictionary.make HashIdentity.Structural
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
              RenderSkyBoxShader = skyBoxShader
              RenderIrradianceShader = irradianceShader
              RenderEnvironmentFilterShader = environmentFilterShader
              RenderPhysicallyBasedForwardShader = forwardShader
              RenderPhysicallyBasedDeferredShader = deferredShader
              RenderPhysicallyBasedDeferred2Shader = deferred2Shader
              RenderReflectionRenderbuffer = reflectionMapRenderbuffer
              RenderReflectionFramebuffer = reflectionMapFramebuffer
              RenderIrradianceRenderbuffer = irradianceRenderbuffer
              RenderIrradianceFramebuffer = irradianceFramebuffer
              RenderEnvironmentFilterRenderbuffer = environmentFilterRenderbuffer
              RenderEnvironmentFilterFramebuffer = environmentFilterFramebuffer
              RenderGeometryBuffers = geometryBuffers
              RenderCubeMapGeometry = cubeMapGeometry
              RenderBillboardGeometry = billboardGeometry
              RenderPhysicallyBasedQuad = physicallyBasedQuad
              RenderCubeMap = skyBoxSurface.CubeMap
              RenderIrradianceMap = irradianceMap
              RenderEnvironmentFilterMap = environmentFilterMap
              RenderBrdfTexture = brdfTexture
              RenderPhysicallyBasedMaterial = physicallyBasedMaterial
              RenderLightMaps = dictPlus HashIdentity.Structural []
              RenderModelsFields = Array.zeroCreate<single> (16 * Constants.Render.GeometryBatchPrealloc)
              RenderTexCoordsOffsetsFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              RenderAlbedosFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              PhysicallyBasedMaterialsFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              PhysicallyBasedHeightsFields = Array.zeroCreate<single> Constants.Render.GeometryBatchPrealloc
              PhysicallyBasedInvertRoughnessesFields = Array.zeroCreate<int> Constants.Render.GeometryBatchPrealloc
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

            // render only if there are messages
            if renderMessages.Count > 0 then
                GlRenderer3d.render eyeCenter eyeRotation windowSize 0u 0u renderMessages renderer

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