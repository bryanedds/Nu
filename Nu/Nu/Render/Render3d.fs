// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open SDL2
open Prime
open Nu

/////////////////////////////////////////////////////////////////////////
// TODO: 3D: introduce records for a bunch of the tuples in this file! //
/////////////////////////////////////////////////////////////////////////

/// The type of rendering used on a surface.
type [<NoEquality; NoComparison; Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType of single

/// Materials used for rendering models.
and [<StructuralEquality; NoComparison; Struct>] RenderMaterial =
    { AlbedoOpt : Color option
      MetalnessOpt : single option
      RoughnessOpt : single option
      AmbientOcclusionOpt : single option }

/// A collection of render tasks in a pass.
and [<NoEquality; NoComparison>] RenderTasks =
    { RenderSurfacesDeferredAbsolute : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * RenderMaterial) SegmentedList>
      RenderSurfacesDeferredRelative : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * RenderMaterial) SegmentedList>
      mutable RenderSurfacesForwardAbsolute : struct (single * Matrix4x4 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      mutable RenderSurfacesForwardRelative : struct (single * Matrix4x4 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      mutable RenderSurfacesForwardAbsoluteSorted : struct (Matrix4x4 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      mutable RenderSurfacesForwardRelativeSorted : struct (Matrix4x4 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      mutable RenderSkyBoxes : CubeMap AssetTag SegmentedList
      mutable RenderLights : SortableLight SegmentedList }

/// The parameters for completing a render pass.
and [<NoEquality; NoComparison>] RenderPassParameters3d =
    { EyePosition : Vector3
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
            | _ -> -1
    override this.Equals (that : obj) =
        match that with
        | :? RenderPassMessage3d as that -> this.RenderPassOrder = that.RenderPassOrder
        | _ -> false
    override this.GetHashCode () = hash this.RenderPassOrder

/// An internally cached static model used to avoid GC promotion of static model messages.
and [<NoEquality; NoComparison>] CachedStaticModelMessage =
    { mutable CachedStaticModelAbsolute : bool
      mutable CachedStaticModelMatrix : Matrix4x4
      mutable CachedStaticModelRenderMaterial : RenderMaterial
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModel : StaticModel AssetTag }

/// Describes a user-defined static model surface.
and [<NoEquality; NoComparison>] StaticModelSurfaceDescriptor =
    { Positions : Vector3 array
      TexCoordses : Vector2 array
      Normals : Vector3 array
      Indices : int array
      Transform : Matrix4x4
      Bounds : Box3
      Albedo : Color
      AlbedoImage : Image AssetTag
      Metalness : single
      MetalnessImage : Image AssetTag
      Roughness : single
      RoughnessImage : Image AssetTag
      AmbientOcclusion : single
      AmbientOcclusionImage : Image AssetTag
      NormalImage : Image AssetTag
      TwoSided : bool }

/// A message to the 3d renderer.
and [<NoEquality; NoComparison>] RenderMessage3d =
    | RenderStaticModelSurfaceMessage of bool * Matrix4x4 * RenderMaterial * RenderType * StaticModel AssetTag * int
    | RenderStaticModelMessage of bool * Matrix4x4 * RenderMaterial * RenderType * StaticModel AssetTag
    | RenderStaticModelsMessage of bool * (Matrix4x4 * RenderMaterial) array * RenderType * StaticModel AssetTag
    | RenderCachedStaticModelMessage of CachedStaticModelMessage
    | RenderSkyBoxMessage of CubeMap AssetTag
    | RenderLightMessage3d of Vector3 * Color * single * single * LightType
    | RenderPostPassMessage3d of RenderPassMessage3d
    | SetImageMinFilter of OpenGL.TextureMinFilter * Image AssetTag
    | SetImageMagFilter of OpenGL.TextureMagFilter * Image AssetTag
    | CreateStaticModelMessage of StaticModelSurfaceDescriptor array * Box3 * StaticModel AssetTag
    | HintRenderPackageUseMessage3d of string
    | HintRenderPackageDisuseMessage3d of string
    | ReloadRenderAssetsMessage3d

/// A sortable light.
and [<NoEquality; NoComparison>] SortableLight =
    { SortableLightPosition : Vector3
      SortableLightColor : Color
      SortableLightBrightness : single
      SortableLightIntensity : single
      mutable SortableLightDistanceSquared : single }

    /// Sort lights into array for uploading to OpenGL.
    /// TODO: 3D: consider getting rid of allocation here.
    static member sortLightsIntoArrays position lights =
        let lightPositions = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax * 3)
        let lightColors = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax * 4)
        let lightBrightnesses = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax)
        let lightIntensities = Array.zeroCreate<single> (Constants.Render.ShaderLightsMax)
        for light in lights do
            light.SortableLightDistanceSquared <- (light.SortableLightPosition - position).MagnitudeSquared
        let lightsSorted = lights |> Seq.toArray |> Array.sortBy (fun light -> light.SortableLightDistanceSquared)
        for i in 0 .. dec Constants.Render.ShaderLightsMax do
            if i < lightsSorted.Length then
                let p = i * 3
                let c = i * 4
                let b = i
                let n = i
                let light = lightsSorted.[i]
                lightPositions.[p] <- light.SortableLightPosition.X
                lightPositions.[p+1] <- light.SortableLightPosition.Y
                lightPositions.[p+2] <- light.SortableLightPosition.Z
                lightBrightnesses.[b] <- light.SortableLightBrightness
                lightIntensities.[n] <- light.SortableLightIntensity
                lightColors.[c] <- light.SortableLightColor.R
                lightColors.[c+1] <- light.SortableLightColor.G
                lightColors.[c+2] <- light.SortableLightColor.B
                lightColors.[c+3] <- light.SortableLightColor.A
        (lightPositions, lightColors, lightBrightnesses, lightIntensities)

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
type [<ReferenceEquality; NoComparison>] MockRenderer3d =
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
type [<NoEquality; NoComparison>] private GlPackageState3d =
    { TextureMemo : OpenGL.Texture.TextureMemo
      CubeMapMemo : OpenGL.CubeMap.CubeMapMemo }

/// The OpenGL implementation of Renderer3d.
type [<ReferenceEquality; NoComparison>] GlRenderer3d =
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
          RenderPhysicallyBasedQuad : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          RenderIrradianceMap : uint
          RenderEnvironmentFilterMap : uint
          RenderBrdfTexture : uint
          RenderPhysicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial
          mutable RenderModelsFields : single array
          mutable RenderAlbedosFields : single array
          mutable RenderMaterialsFields : single array
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

    static member private tryLoadRenderAsset packageState (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp"
        | ".png"
        | ".tif" ->
            match OpenGL.Texture.TryCreateTextureMemoizedFiltered (asset.FilePath, packageState.TextureMemo) with
            | Right texture ->
                Some (asset.AssetTag.AssetName, TextureAsset texture)
            | Left error ->
                Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
                None
        | ".cbm" ->
            match File.ReadAllLines asset.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = Path.GetDirectoryName asset.FilePath
                let faceRightFilePath = dirPath + "/" + faceRightFilePath |> fun str -> str.Trim ()
                let faceLeftFilePath = dirPath + "/" + faceLeftFilePath |> fun str -> str.Trim ()
                let faceTopFilePath = dirPath + "/" + faceTopFilePath |> fun str -> str.Trim ()
                let faceBottomFilePath = dirPath + "/" + faceBottomFilePath |> fun str -> str.Trim ()
                let faceBackFilePath = dirPath + "/" + faceBackFilePath |> fun str -> str.Trim ()
                let faceFrontFilePath = dirPath + "/" + faceFrontFilePath |> fun str -> str.Trim ()
                match OpenGL.CubeMap.TryCreateCubeMapMemoized (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, packageState.CubeMapMemo) with
                | Right cubeMap -> Some (asset.AssetTag.AssetName, CubeMapAsset (cubeMap, ref None))
                | Left error -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to: " + error); None
            | _ -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line."); None
        | ".fbx" ->
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (true, UnitCentimeters, asset.FilePath, renderer.RenderPhysicallyBasedMaterial, packageState.TextureMemo, renderer.RenderAssimp) with
            | Right model -> Some (asset.AssetTag.AssetName, StaticModelAsset model)
            | Left error -> Log.debug ("Could not load static model '" + asset.FilePath + "' due to: " + error); None
        | ".obj" ->
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (true, UnitMeters, asset.FilePath, renderer.RenderPhysicallyBasedMaterial, packageState.TextureMemo, renderer.RenderAssimp) with
            | Right model -> Some (asset.AssetTag.AssetName, StaticModelAsset model)
            | Left error -> Log.debug ("Could not load static model '" + asset.FilePath + "' due to: " + error); None
        | _ -> None

    static member private tryLoadRenderPackage packageName renderer =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Render3d) packageName assetGraph with
            | Right assets ->
                let renderPackage =
                    match Dictionary.tryFind packageName renderer.RenderPackages with
                    | Some renderPackage -> renderPackage
                    | None ->
                        let renderPackageState = { TextureMemo = OpenGL.Texture.TextureMemo.make (); CubeMapMemo = OpenGL.CubeMap.CubeMapMemo.make () }
                        let renderPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = renderPackageState }
                        renderer.RenderPackages.Assign (packageName, renderPackage)
                        renderPackage
                let renderAssetOpts = List.map (fun asset -> GlRenderer3d.tryLoadRenderAsset renderPackage.PackageState asset renderer) assets
                let renderAssets = List.definitize renderAssetOpts
                for (key, value) in renderAssets do
                    renderPackage.Assets.Assign (key, value)
            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member tryFindRenderAsset (assetTag : obj AssetTag) renderer =
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
                GlRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, package) ->
                    renderer.RenderPackageCachedOpt <- (assetTag.PackageName, package.Assets)
                    match package.Assets.TryGetValue assetTag.AssetName with
                    | (true, asset) ->
                        renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                        ValueSome asset
                    | (false, _) -> ValueNone
                | (false, _) -> ValueNone

    static member private handleCreateStaticModelMessage surfaceDescriptors bounds assetTag renderer =

        // create surfaces
        let surfaces = List ()
        for surfaceDescriptor in surfaceDescriptors do

            // create material
            let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
                { Albedo = surfaceDescriptor.Albedo
                  AlbedoTexture = ValueOption.defaultValue 0u (match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize surfaceDescriptor.AlbedoImage) renderer with ValueSome (TextureAsset (_, texture)) -> ValueSome texture | _ -> ValueNone)
                  Metalness = surfaceDescriptor.Metalness
                  MetalnessTexture = ValueOption.defaultValue 0u (match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize surfaceDescriptor.MetalnessImage) renderer with ValueSome (TextureAsset (_, texture)) -> ValueSome texture | _ -> ValueNone)
                  Roughness = surfaceDescriptor.Roughness
                  RoughnessTexture = ValueOption.defaultValue 0u (match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize surfaceDescriptor.RoughnessImage) renderer with ValueSome (TextureAsset (_, texture)) -> ValueSome texture | _ -> ValueNone)
                  AmbientOcclusion = surfaceDescriptor.AmbientOcclusion
                  AmbientOcclusionTexture = ValueOption.defaultValue 0u (match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize surfaceDescriptor.AmbientOcclusionImage) renderer with ValueSome (TextureAsset (_, texture)) -> ValueSome texture | _ -> ValueNone)
                  NormalTexture = ValueOption.defaultValue 0u (match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize surfaceDescriptor.NormalImage) renderer with ValueSome (TextureAsset (_, texture)) -> ValueSome texture | _ -> ValueNone)
                  TwoSided = surfaceDescriptor.TwoSided }

            // create vertex data, truncating it when required
            let vertexCount = surfaceDescriptor.Positions.Length
            let mutable vertexData = Array.zeroCreate (vertexCount * 8)
            let mutable i = 0
            try
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
                vertexData <- Array.take i vertexData
                Log.debug "Vertex data truncated due to an inequal count among surface descriptor Positions, TexCoordses, and Normals."

            // crete geometry
            let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedGeometry (true, vertexData, surfaceDescriptor.Indices, surfaceDescriptor.Bounds)

            // create surface
            let surface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface ([||], surfaceDescriptor.Transform, surfaceDescriptor.Bounds, material, geometry)
            surfaces.Add surface

        // create static model
        let surfaces = Seq.toArray surfaces
        let hierarchy = TreeNode (Array.map OpenGL.PhysicallyBased.PhysicallyBasedSurface surfaces)
        let staticModel : OpenGL.PhysicallyBased.PhysicallyBasedStaticModel =
            { Bounds = bounds
              Lights = [||]
              Surfaces = surfaces
              PhysicallyBasedStaticHierarchy = hierarchy }

        // add static model to appropriate render package
        match renderer.RenderPackages.TryGetValue assetTag.PackageName with
        | (true, package) ->
            package.Assets.Add (assetTag.AssetName, StaticModelAsset staticModel)
        | (false, _) ->
            let packageState = { TextureMemo = OpenGL.Texture.TextureMemo.make (); CubeMapMemo = OpenGL.CubeMap.CubeMapMemo.make () }
            let package = { Assets = Dictionary.singleton StringComparer.Ordinal assetTag.AssetName (StaticModelAsset staticModel); PackageState = packageState }
            renderer.RenderPackages.Add (assetTag.PackageName, package)

    static member private handleHintRenderPackage3dUse hintPackageName renderer =
        GlRenderer3d.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackage3dDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some package ->
            OpenGL.Texture.DeleteTexturesMemoized package.PackageState.TextureMemo
            OpenGL.CubeMap.DeleteCubeMapsMemoized package.PackageState.CubeMapMemo
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRender3dAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            GlRenderer3d.tryLoadRenderPackage packageName renderer

    static member inline private categorizeStaticModelSurface
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         renderMaterial : RenderMaterial inref,
         renderType : RenderType,
         ignoreSurfaceMatrix,
         surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         renderer) =
        let surfaceMatrix =
            if ignoreSurfaceMatrix || surface.SurfaceMatrixIsIdentity
            then modelMatrix
            else surface.SurfaceMatrix * modelMatrix
        match renderType with
        | DeferredRenderType ->
            if modelAbsolute then
                match renderer.RenderTasks.RenderSurfacesDeferredAbsolute.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, renderMaterial) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Add (surface, SegmentedList.singleton (surfaceMatrix, renderMaterial))
            else
                match renderer.RenderTasks.RenderSurfacesDeferredRelative.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add struct (surfaceMatrix, renderMaterial) renderTasks
                | (false, _) -> renderer.RenderTasks.RenderSurfacesDeferredRelative.Add (surface, SegmentedList.singleton (surfaceMatrix, renderMaterial))
        | ForwardRenderType subsort ->
            if modelAbsolute
            then SegmentedList.add struct (subsort, surfaceMatrix, renderMaterial, surface) renderer.RenderTasks.RenderSurfacesForwardAbsolute
            else SegmentedList.add struct (subsort, surfaceMatrix, renderMaterial, surface) renderer.RenderTasks.RenderSurfacesForwardRelative

    static member inline private categorizeStaticModelSurfaceByIndex
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         renderMaterial : RenderMaterial inref,
         renderType : RenderType,
         staticModel : StaticModel AssetTag,
         surfaceIndex,
         renderer) =
        match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize staticModel) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset modelAsset ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces.[surfaceIndex]
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, &renderMaterial, renderType, true, surface, renderer)
            | _ -> Log.trace "Cannot render static model surface with a non-model asset."
        | _ -> Log.info ("Cannot render static model surface due to unloadable assets for '" + scstring staticModel + "'.")

    static member private categorizeStaticModel
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         renderMaterial : RenderMaterial inref,
         renderType,
         staticModel : StaticModel AssetTag,
         renderer) =
        match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize staticModel) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset modelAsset ->
                for light in modelAsset.Lights do
                    let lightMatrix = light.LightMatrix * modelMatrix
                    let light =
                        { SortableLightPosition = lightMatrix.Translation
                          SortableLightColor = light.LightColor
                          SortableLightBrightness = light.LightBrightness
                          SortableLightIntensity = light.LightIntensity
                          SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderer.RenderTasks.RenderLights
                for surface in modelAsset.Surfaces do
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, &renderMaterial, renderType, false, surface, renderer)
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

    static member private sortSurfaces eyePosition (surfaces : struct (single * Matrix4x4 * RenderMaterial * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList) =
        surfaces |>
        Seq.map (fun struct (subsort, model, renderMaterial, surface) -> struct (subsort, model, renderMaterial, surface, (model.Translation - eyePosition).MagnitudeSquared)) |>
        Seq.toArray |> // TODO: 3D: use a preallocated array to avoid allocating on the LOH.
        Array.sortByDescending (fun struct (subsort, _, _, _, distanceSquared) -> struct (distanceSquared, subsort)) |>
        Array.map (fun struct (_, model, renderMaterialOpt, surface, _) -> struct (model, renderMaterialOpt, surface))

    static member private renderPhysicallyBasedSurfaces
        eyePosition
        viewArray
        projectionArray
        (parameters : struct (Matrix4x4 * RenderMaterial) SegmentedList)
        blending
        irradianceMap
        environmentFilterMap
        brdfTexture
        lightPositions
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

            // ensure we have a large enough abledos fields array
            let mutable length = renderer.RenderAlbedosFields.Length
            while parameters.Length * 4 > length do length <- length * 2
            if renderer.RenderAlbedosFields.Length < length then
                renderer.RenderAlbedosFields <- Array.zeroCreate<single> length

            // ensure we have a large enough materials fields array
            let mutable length = renderer.RenderMaterialsFields.Length
            while parameters.Length * 3 > length do length <- length * 2
            if renderer.RenderMaterialsFields.Length < length then
                renderer.RenderMaterialsFields <- Array.zeroCreate<single> length

            // blit parameters to field arrays
            for i in 0 .. dec parameters.Length do
                let struct (model, renderMaterial) = parameters.[i]
                model.ToArray (renderer.RenderModelsFields, i * 16)
                let (albedo, metalness, roughness, ambientOcclusion) =
                    ((match renderMaterial.AlbedoOpt with Some value -> value | None -> surface.PhysicallyBasedMaterial.Albedo),
                     (match renderMaterial.MetalnessOpt with Some value -> value | None -> surface.PhysicallyBasedMaterial.Metalness),
                     (match renderMaterial.RoughnessOpt with Some value -> value | None -> surface.PhysicallyBasedMaterial.Roughness),
                     (match renderMaterial.AmbientOcclusionOpt with Some value -> value | None -> surface.PhysicallyBasedMaterial.AmbientOcclusion))
                renderer.RenderAlbedosFields.[i * 4] <- albedo.R
                renderer.RenderAlbedosFields.[i * 4 + 1] <- albedo.G
                renderer.RenderAlbedosFields.[i * 4 + 2] <- albedo.B
                renderer.RenderAlbedosFields.[i * 4 + 3] <- albedo.A
                renderer.RenderMaterialsFields.[i * 3] <- metalness
                renderer.RenderMaterialsFields.[i * 3 + 1] <- roughness
                renderer.RenderMaterialsFields.[i * 3 + 2] <- ambientOcclusion

            // draw surfaces
            OpenGL.PhysicallyBased.DrawPhysicallyBasedSurfaces
                (eyePosition, parameters.Length, renderer.RenderModelsFields, renderer.RenderAlbedosFields, renderer.RenderMaterialsFields, viewArray, projectionArray,
                 blending, irradianceMap, environmentFilterMap, brdfTexture, lightPositions, lightColors, lightBrightnesses, lightIntensities,
                 surface.PhysicallyBasedMaterial, surface.PhysicallyBasedGeometry, shader)

    /// Make a GlRenderer3d.
    static member make window config =

        // initialize context if directed
        if config.ShouldInitializeContext then

            // create SDL-OpenGL context if needed
            match window with
            | SglWindow window -> OpenGL.Hl.CreateSglContext window.SglWindow |> ignore<nativeint>
            | WfglWindow _ -> () // TODO: 3D: see if we can make current the GL context here so that threaded OpenGL works in Gaia.
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
            match OpenGL.Texture.TryCreateTextureUnfiltered Constants.Paths.BrdfTextureFilePath with
            | Right (_, texture) -> texture
            | Left error -> failwith ("Could not load BRDF texture due to: " + error)

        // create default physically-based material
        let physicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { Albedo = Color.White
              AlbedoTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAlbedo.png") |> Either.getRight |> snd
              Metalness = 1.0f
              MetalnessTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialMetalness.png") |> Either.getRight |> snd
              Roughness = 1.0f
              RoughnessTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialRoughness.png") |> Either.getRight |> snd
              AmbientOcclusion = 1.0f
              AmbientOcclusionTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialAmbientOcclusion.png") |> Either.getRight |> snd
              NormalTexture = OpenGL.Texture.TryCreateTextureFiltered ("Assets/Default/MaterialNormal.png") |> Either.getRight |> snd
              TwoSided = false }

        // create render tasks
        let renderTasks =
            { RenderSurfacesDeferredAbsolute = dictPlus HashIdentity.Structural []
              RenderSurfacesDeferredRelative = dictPlus HashIdentity.Structural []
              RenderSurfacesForwardAbsolute = SegmentedList.make ()
              RenderSurfacesForwardRelative = SegmentedList.make ()
              RenderSurfacesForwardAbsoluteSorted = SegmentedList.make ()
              RenderSurfacesForwardRelativeSorted = SegmentedList.make ()
              RenderSkyBoxes = SegmentedList.make ()
              RenderLights = SegmentedList.make () }

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
              RenderPhysicallyBasedQuad = physicallyBasedQuad
              RenderIrradianceMap = irradianceMap
              RenderEnvironmentFilterMap = environmentFilterMap
              RenderBrdfTexture = brdfTexture
              RenderPhysicallyBasedMaterial = physicallyBasedMaterial
              RenderModelsFields = Array.zeroCreate<single> (16 * Constants.Render.GeometryBatchPrealloc)
              RenderAlbedosFields = Array.zeroCreate<single> (4 * Constants.Render.GeometryBatchPrealloc)
              RenderMaterialsFields = Array.zeroCreate<single> (3 * Constants.Render.GeometryBatchPrealloc)
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

        member renderer.Render eyePosition eyeRotation windowSize renderMessages =

            // begin frame
            let viewportOffset = Constants.Render.ViewportOffset windowSize
            if renderer.RenderShouldBeginFrame then
                OpenGL.Hl.BeginFrame viewportOffset
                OpenGL.Hl.Assert ()

            // compute view and projection
            let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
            let viewAbsolute = m4Identity
            let viewAbsoluteArray = viewAbsolute.ToArray ()
            let viewRelative = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
            let viewRelativeArray = viewRelative.ToArray ()
            let viewSkyBox = Matrix4x4.CreateFromQuaternion (Quaternion.Inverse eyeRotation)
            let viewSkyBoxArray = viewSkyBox.ToArray ()
            let viewport = Constants.Render.Viewport
            let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // categorize messages
            let postPasses = hashSetPlus<RenderPassMessage3d> HashIdentity.Structural []
            for message in renderMessages do
                match message with
                | RenderStaticModelSurfaceMessage (absolute, modelMatrix, renderMaterial, renderType, staticModel, surfaceIndex) ->
                    GlRenderer3d.categorizeStaticModelSurfaceByIndex (absolute, &modelMatrix, &renderMaterial, renderType, staticModel, surfaceIndex, renderer)
                | RenderStaticModelMessage (absolute, modelMatrix, renderMaterial, renderType, staticModel) ->
                    GlRenderer3d.categorizeStaticModel (absolute, &modelMatrix, &renderMaterial, renderType, staticModel, renderer)
                | RenderStaticModelsMessage (absolute, parameters, renderType, staticModel) ->
                    for (modelMatrix, renderMaterial) in parameters do
                        GlRenderer3d.categorizeStaticModel (absolute, &modelMatrix, &renderMaterial, renderType, staticModel, renderer)
                | RenderCachedStaticModelMessage d ->
                    GlRenderer3d.categorizeStaticModel (d.CachedStaticModelAbsolute, &d.CachedStaticModelMatrix, &d.CachedStaticModelRenderMaterial, d.CachedStaticModelRenderType, d.CachedStaticModel, renderer)
                | RenderSkyBoxMessage cubeMap ->
                    SegmentedList.add cubeMap renderer.RenderTasks.RenderSkyBoxes
                | RenderLightMessage3d (position, color, brightness, intensity, _) ->
                    let light = { SortableLightPosition = position; SortableLightColor = color; SortableLightBrightness = brightness; SortableLightIntensity = intensity; SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderer.RenderTasks.RenderLights
                | RenderPostPassMessage3d postPass ->
                    postPasses.Add postPass |> ignore<bool>
                | SetImageMinFilter (minFilter, image) ->
                    match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize image) renderer with
                    | ValueSome (TextureAsset (_, texture)) ->
                        OpenGL.Texture.SetMinFilter (minFilter, texture)
                        OpenGL.Hl.Assert ()
                    | _ -> Log.debug ("Could not set min filter for non-texture or missing asset '" + scstring image + "'")
                | SetImageMagFilter (magFilter, image) ->
                    match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize image) renderer with
                    | ValueSome (TextureAsset (_, texture)) ->
                        OpenGL.Texture.SetMagFilter (magFilter, texture)
                        OpenGL.Hl.Assert ()
                    | _ -> Log.debug ("Could not set mag filter for non-texture or missing asset '" + scstring image + "'")
                | CreateStaticModelMessage (surfaceDescriptors, bounds, assetTag) ->
                    GlRenderer3d.handleCreateStaticModelMessage surfaceDescriptors bounds assetTag renderer
                | HintRenderPackageUseMessage3d hintPackageUse ->
                    GlRenderer3d.handleHintRenderPackage3dUse hintPackageUse renderer
                | HintRenderPackageDisuseMessage3d hintPackageDisuse ->
                    GlRenderer3d.handleHintRenderPackage3dDisuse hintPackageDisuse renderer
                | ReloadRenderAssetsMessage3d ->
                    () // TODO: 3D: implement asset reloading.

            // sort absolute forward surfaces
            let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyePosition renderer.RenderTasks.RenderSurfacesForwardAbsolute
            SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsolute

            // sort relative forward surfaces
            let forwardSurfacesSorted = GlRenderer3d.sortSurfaces eyePosition renderer.RenderTasks.RenderSurfacesForwardRelative
            SegmentedList.addMany forwardSurfacesSorted renderer.RenderTasks.RenderSurfacesForwardRelativeSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelative

            // setup geometry buffer
            let (positionTexture, albedoTexture, materialTexture, normalTexture, geometryFramebuffer) = renderer.RenderGeometryFramebuffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
            OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
            OpenGL.Gl.Scissor (viewportOffset.Bounds.Position.X, viewportOffset.Bounds.Position.Y, viewportOffset.Bounds.Size.X, viewportOffset.Bounds.Size.Y)
            OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest
            OpenGL.Hl.Assert ()

            // attempt to locate last sky box
            let skyBoxOpt =
                match Seq.tryLast renderer.RenderTasks.RenderSkyBoxes with
                | Some cubeMap ->
                    match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize cubeMap) renderer with
                    | ValueSome asset ->
                        match asset with
                        | CubeMapAsset (cubeMap, cubeMapIrradianceOptRef) -> Some (cubeMap, cubeMapIrradianceOptRef)
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
                        irradianceAndEnviconmentMapsOptRef := Some result
                        result
                    else Option.get irradianceAndEnviconmentMapsOptRef.Value
                | None -> (renderer.RenderIrradianceMap, renderer.RenderEnvironmentFilterMap)

            // sort lights for deferred relative to eye position
            let (lightPositions, lightColors, lightBrightnesses, lightIntensities) =
                SortableLight.sortLightsIntoArrays eyePosition renderer.RenderTasks.RenderLights

            // render deferred pass w/ absolute-transformed surfaces
            for entry in renderer.RenderTasks.RenderSurfacesDeferredAbsolute do
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewAbsoluteArray
                    projectionArray
                    entry.Value
                    false
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightPositions
                    lightColors
                    lightBrightnesses
                    lightIntensities
                    entry.Key
                    renderer.RenderPhysicallyBasedDeferredShader
                    renderer
                OpenGL.Hl.Assert ()

            // render deferred pass w/ relative-transformed surfaces
            for entry in renderer.RenderTasks.RenderSurfacesDeferredRelative do
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewRelativeArray
                    projectionArray
                    entry.Value
                    false
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightPositions
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

            // render deferred lighting quad
            OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferred2Surface
                (eyePosition, positionTexture, albedoTexture, materialTexture, normalTexture,
                 irradianceMap, environmentFilterMap, renderer.RenderBrdfTexture, lightPositions, lightColors, lightBrightnesses, lightIntensities,
                 renderer.RenderPhysicallyBasedQuad, renderer.RenderPhysicallyBasedDeferred2Shader)
            OpenGL.Hl.Assert ()

            // attempt to render sky box
            match skyBoxOpt with
            | Some (cubeMap, _) ->
                OpenGL.SkyBox.DrawSkyBox (viewSkyBoxArray, projectionArray, cubeMap, renderer.RenderSkyBoxGeometry, renderer.RenderSkyBoxShader)
                OpenGL.Hl.Assert ()
            | None -> ()

            // render forward pass w/ absolute-transformed surfaces
            for (model, renderMaterial, surface) in renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted do
                let (lightPositions, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderer.RenderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewAbsoluteArray
                    projectionArray
                    (SegmentedList.singleton (model, renderMaterial))
                    true
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightColors
                    lightPositions
                    lightBrightnesses
                    lightIntensities
                    surface
                    renderer.RenderPhysicallyBasedForwardShader
                    renderer
                OpenGL.Hl.Assert ()

            // render forward pass w/ relative-transformed surfaces
            for (model, renderMaterial, surface) in renderer.RenderTasks.RenderSurfacesForwardRelativeSorted do
                let (lightPositions, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderer.RenderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewRelativeArray
                    projectionArray
                    (SegmentedList.singleton (model, renderMaterial))
                    true
                    irradianceMap
                    environmentFilterMap
                    renderer.RenderBrdfTexture
                    lightPositions
                    lightColors
                    lightBrightnesses
                    lightIntensities
                    surface
                    renderer.RenderPhysicallyBasedForwardShader
                    renderer
                OpenGL.Hl.Assert ()

            // render post-passes
            let passParameters =
                { EyePosition = eyePosition
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
            renderer.RenderTasks.RenderSurfacesDeferredAbsolute.Clear ()
            renderer.RenderTasks.RenderSurfacesDeferredRelative.Clear ()
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardAbsoluteSorted
            SegmentedList.clear renderer.RenderTasks.RenderSurfacesForwardRelativeSorted
            SegmentedList.clear renderer.RenderTasks.RenderSkyBoxes
            SegmentedList.clear renderer.RenderTasks.RenderLights

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