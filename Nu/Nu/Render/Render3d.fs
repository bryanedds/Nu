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

// 3d rendering notes:
// I may borrow some sky dome code from here or related - https://github.com/shff/opengl_sky/blob/master/main.mm
// There appears to be a bias constant that can be used with VSMs to fix up light leaks, so consider that.

/// Describe the type of projection matrix to construct.
/// TODO: 3D: expose this elsewhere.
type [<Struct>] ProjectionType =
    | Enclosed
    | Unenclosed
    | Afatecs // TODO: 3D: can we think of a better name here?

/// The type of rendering used on a surface.
and [<NoEquality; NoComparison; Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType

/// A collection of render tasks in a pass.
and [<NoEquality; NoComparison>] RenderTasks =
    { RenderSurfacesDeferredAbsolute : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, Matrix4x4 SegmentedList>
      RenderSurfacesDeferredRelative : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, Matrix4x4 SegmentedList>
      RenderSurfacesForwardAbsolute : struct (Matrix4x4 * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSurfacesForwardRelative : struct (Matrix4x4 * OpenGL.PhysicallyBased.PhysicallyBasedSurface) SegmentedList
      RenderSkyBoxes : CubeMap AssetTag SegmentedList
      RenderLights : SortableLight SegmentedList }

/// Describes a 3d render pass.
and [<CustomEquality; CustomComparison>] RenderPassDescriptor3d =
    { RenderPassOrder : int64
      RenderPass3d : Matrix4x4 * Matrix4x4 * Matrix4x4 * Matrix4x4 * RenderTasks * Renderer3d -> unit }
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

/// Describes an internally cached static model used to avoid GC promotion of static model descriptors.
and [<NoEquality; NoComparison>] CachedStaticModelDescriptor =
    { mutable CachedStaticModelAbsolute : bool
      mutable CachedStaticModelMatrix : Matrix4x4
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModel : StaticModel AssetTag }

/// A message to the 3d renderer.
and [<NoEquality; NoComparison>] RenderMessage3d =
    | RenderStaticModelSurfaceDescriptor of bool * Matrix4x4 * RenderType * StaticModel AssetTag * int
    | RenderStaticModelDescriptor of bool * Matrix4x4 * RenderType * StaticModel AssetTag
    | RenderStaticModelsDescriptor of bool * Matrix4x4 array * RenderType * StaticModel AssetTag
    | RenderCachedStaticModelDescriptor of CachedStaticModelDescriptor
    | RenderSkyBoxDescriptor of CubeMap AssetTag
    | RenderLightDescriptor of Vector3 * Color * single * single * LightType
    | RenderPostPassDescriptor3d of RenderPassDescriptor3d
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
          mutable RenderModelsFields : single array
          RenderPackages : RenderAsset Packages
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          RenderMessages : RenderMessage3d List
          RenderShouldBeginFrame : bool
          RenderShouldEndFrame : bool }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset renderAsset renderer =
        GlRenderer3d.invalidateCaches renderer
        match renderAsset with
        | TextureAsset (_, texture) -> OpenGL.Texture.DeleteTexture texture
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp"
        | ".png" ->
            match OpenGL.Texture.TryCreateTexture2dUnfiltered asset.FilePath with
            | Right texture ->
                Some (asset.AssetTag.AssetName, TextureAsset texture)
            | Left error ->
                Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
                None
        | ".cbm" ->
            match File.ReadAllLines asset.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = Path.GetDirectoryName asset.FilePath
                let faceRightFilePath = Path.Combine (dirPath, faceRightFilePath) |> fun str -> str.Trim ()
                let faceLeftFilePath = Path.Combine (dirPath, faceLeftFilePath) |> fun str -> str.Trim ()
                let faceTopFilePath = Path.Combine (dirPath, faceTopFilePath) |> fun str -> str.Trim ()
                let faceBottomFilePath = Path.Combine (dirPath, faceBottomFilePath) |> fun str -> str.Trim ()
                let faceBackFilePath = Path.Combine (dirPath, faceBackFilePath) |> fun str -> str.Trim ()
                let faceFrontFilePath = Path.Combine (dirPath, faceFrontFilePath) |> fun str -> str.Trim ()
                match OpenGL.Texture.TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
                | Right cubeMap -> Some (asset.AssetTag.AssetName, CubeMapAsset (cubeMap, ref None))
                | Left error -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to: " + error); None
            | _ -> Log.debug ("Could not load cube map '" + asset.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line."); None
        | ".obj" ->
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (true, asset.FilePath, renderer.RenderAssimp) with
            | Right model -> Some (asset.AssetTag.AssetName, StaticModelAsset model)
            | Left error -> Log.debug ("Could not load static model '" + asset.FilePath + "' due to: " + error); None
        | _ -> None

    static member private tryLoadRenderPackage packageName renderer =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Render) packageName assetGraph with
            | Right assets ->
                let renderAssetOpts = List.map (fun asset -> GlRenderer3d.tryLoadRenderAsset asset renderer) assets
                let renderAssets = List.definitize renderAssetOpts
                match Dictionary.tryFind packageName renderer.RenderPackages with
                | Some renderAssetDict ->
                    for (key, value) in renderAssets do renderAssetDict.Assign (key, value)
                    renderer.RenderPackages.Assign (packageName, renderAssetDict)
                | None ->
                    let renderAssetDict = dictPlus StringComparer.Ordinal renderAssets
                    renderer.RenderPackages.Assign (packageName, renderAssetDict)
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
            | Some assets ->
                renderer.RenderPackageCachedOpt <- (assetTag.PackageName, assets)
                match assets.TryGetValue assetTag.AssetName with
                | (true, asset) ->
                    renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
            | None ->
                Log.info ("Loading render package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                GlRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, assets) ->
                    renderer.RenderPackageCachedOpt <- (assetTag.PackageName, assets)
                    match assets.TryGetValue assetTag.AssetName with
                    | (true, asset) ->
                        renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                        ValueSome asset
                    | (false, _) -> ValueNone
                | (false, _) -> ValueNone

    static member private handleHintRenderPackageUse hintPackageName renderer =
        GlRenderer3d.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackageDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some assets ->
            for asset in assets do GlRenderer3d.freeRenderAsset asset.Value renderer
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            GlRenderer3d.tryLoadRenderPackage packageName renderer

    static member private categorizeStaticModelSurface
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         modelRenderType : RenderType,
         surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         renderTasks) =
        let surfaceMatrix =
            if not surface.SurfaceMatrixIsIdentity
            then modelMatrix * surface.SurfaceMatrix
            else modelMatrix
        match modelRenderType with
        | DeferredRenderType ->
            if modelAbsolute then
                match renderTasks.RenderSurfacesDeferredAbsolute.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add surfaceMatrix renderTasks
                | (false, _) -> renderTasks.RenderSurfacesDeferredAbsolute.Add (surface, SegmentedList.singleton surfaceMatrix)
            else
                match renderTasks.RenderSurfacesDeferredRelative.TryGetValue surface with
                | (true, renderTasks) -> SegmentedList.add surfaceMatrix renderTasks
                | (false, _) -> renderTasks.RenderSurfacesDeferredRelative.Add (surface, SegmentedList.singleton surfaceMatrix)
        | ForwardRenderType ->
            if modelAbsolute
            then SegmentedList.add struct (surfaceMatrix, surface) renderTasks.RenderSurfacesForwardAbsolute
            else SegmentedList.add struct (surfaceMatrix, surface) renderTasks.RenderSurfacesForwardRelative

    static member private categorizeStaticModelSurfaceByIndex
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         modelRenderType : RenderType,
         modelAssetTag : StaticModel AssetTag,
         surfaceIndex,
         renderTasks,
         renderer) =
        match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize modelAssetTag) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset modelAsset ->
                GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, modelRenderType, modelAsset.Surfaces.[surfaceIndex], renderTasks)
            | _ -> Log.trace "Cannot render static model surface with a non-model asset."
        | _ -> Log.info ("Cannot render static model surface due to unloadable assets for '" + scstring modelAssetTag + "'.")

    static member private categorizeStaticModel
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         modelRenderType,
         modelAssetTag : StaticModel AssetTag,
         renderTasks,
         renderer) =
        match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize modelAssetTag) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset modelAsset ->
                for surface in modelAsset.Surfaces do
                    GlRenderer3d.categorizeStaticModelSurface (modelAbsolute, &modelMatrix, modelRenderType, surface, renderTasks)
            | _ -> Log.trace "Cannot render static model with a non-model asset."
        | _ -> Log.info ("Cannot render static model due to unloadable assets for '" + scstring modelAssetTag + "'.")

    /// Compute the 3d projection matrix.
    /// TODO: 3D: expose this elsewhere.
    static member computeProjection projectionType =
        let farPlaneDistance =
            match projectionType with
            | Enclosed -> Constants.Render.FarPlaneDistanceEnclosed
            | Unenclosed -> Constants.Render.FarPlaneDistanceUnenclosed
            | Afatecs -> Constants.Render.FarPlaneDistanceAfatecs
        Matrix4x4.CreatePerspectiveFieldOfView
            (Constants.Render.FieldOfView,
             Constants.Render.AspectRatio,
             Constants.Render.NearPlaneDistance,
             farPlaneDistance)

    /// Compute the 3d view frustum.
    /// TODO: 3D: expose this elsewhere.
    static member computeFrustum enclosed eyePosition (eyeRotation : Quaternion) =
        let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
        let view = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
        let projection = GlRenderer3d.computeProjection enclosed
        let viewProjection = view * projection
        Frustum viewProjection

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

    static member inline private renderPhysicallyBasedSurfaces
        eyePosition
        viewArray
        projectionArray
        (models : Matrix4x4 SegmentedList)
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

        // ensure there are models to render
        if models.Length > 0 then

            // ensure we have a large enough field array
            let mutable length = renderer.RenderModelsFields.Length
            while models.Length * 16 * 16 > length do length <- length * 2
            if renderer.RenderModelsFields.Length < length then
                renderer.RenderModelsFields <- Array.zeroCreate<single> length

            // blit models to field array
            for i in 0 .. dec models.Length do
                models.[i].ToArray (renderer.RenderModelsFields, i * 16)

            // draw surfaces
            OpenGL.PhysicallyBased.DrawPhysicallyBasedSurfaces
                (eyePosition, renderer.RenderModelsFields, models.Length, viewArray, projectionArray,
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

        // create sky box cube map
        let skyBoxMap =
            match 
                OpenGL.Texture.TryCreateCubeMap
                    ("Assets/Default/SkyBoxRight.png",
                     "Assets/Default/SkyBoxLeft.png",
                     "Assets/Default/SkyBoxTop.png",
                     "Assets/Default/SkyBoxBottom.png",
                     "Assets/Default/SkyBoxBack.png",
                     "Assets/Default/SkyBoxFront.png") with
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
        let irradianceMap = GlRenderer3d.createIrradianceMap box2iZero 0u irradianceRenderbuffer irradianceFramebuffer irradianceShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create default environment filter map
        let environmentFilterMap = GlRenderer3d.createEnvironmentFilterMap box2iZero 0u environmentFilterRenderbuffer environmentFilterFramebuffer environmentFilterShader skyBoxSurface
        OpenGL.Hl.Assert ()

        // create brdf texture
        let brdfTexture =
            match OpenGL.Texture.TryCreateTexture2dLinear Constants.Paths.BrdfTextureFilePath with
            | Right (_, texture) -> texture
            | Left error -> failwith ("Could not load Brdf texture due to: " + error)

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
              RenderModelsFields = Array.zeroCreate<single> (16 * 1024)
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
            let viewSkyBox = Matrix4x4.CreateFromQuaternion (Quaternion.Inverse eyeRotation)
            let viewSkyBoxArray = viewSkyBox.ToArray ()
            let viewRelative = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
            let viewRelativeArray = viewRelative.ToArray ()
            let projection = GlRenderer3d.computeProjection Afatecs
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // categorize messages
            let renderTasks =
                { RenderSurfacesDeferredAbsolute = dictPlus HashIdentity.Structural []
                  RenderSurfacesDeferredRelative = dictPlus HashIdentity.Structural []
                  RenderSurfacesForwardAbsolute = SegmentedList.make ()
                  RenderSurfacesForwardRelative = SegmentedList.make ()
                  RenderSkyBoxes = SegmentedList.make ()
                  RenderLights = SegmentedList.make () }
            let postPasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            for message in renderMessages do
                match message with
                | RenderStaticModelSurfaceDescriptor (modelAbsolute, modelMatrix, modelRenderType, modelAssetTag, modelSurfaceIndex) ->
                    GlRenderer3d.categorizeStaticModelSurfaceByIndex (modelAbsolute, &modelMatrix, modelRenderType, modelAssetTag, modelSurfaceIndex, renderTasks, renderer)
                | RenderStaticModelDescriptor (modelAbsolute, modelMatrix, modelRenderType, modelAssetTag) ->
                    GlRenderer3d.categorizeStaticModel (modelAbsolute, &modelMatrix, modelRenderType, modelAssetTag, renderTasks, renderer)
                | RenderStaticModelsDescriptor (modelAbsolute, modelMatrices, modelRenderType, modelAssetTag) ->
                    for modelMatrix in modelMatrices do
                        GlRenderer3d.categorizeStaticModel (modelAbsolute, &modelMatrix, modelRenderType, modelAssetTag, renderTasks, renderer)
                | RenderCachedStaticModelDescriptor d ->
                    GlRenderer3d.categorizeStaticModel (d.CachedStaticModelAbsolute, &d.CachedStaticModelMatrix, d.CachedStaticModelRenderType, d.CachedStaticModel, renderTasks, renderer)
                | RenderSkyBoxDescriptor cubeMap ->
                    SegmentedList.add cubeMap renderTasks.RenderSkyBoxes
                | RenderLightDescriptor (position, color, brightness, intensity, _) ->
                    let light = { SortableLightPosition = position; SortableLightColor = color; SortableLightBrightness = brightness; SortableLightIntensity = intensity; SortableLightDistanceSquared = Single.MaxValue }
                    SegmentedList.add light renderTasks.RenderLights
                | RenderPostPassDescriptor3d postPass ->
                    postPasses.Add postPass |> ignore<bool> // TODO: 3D: implement post-pass handling.

            // sort absolute forward surfaces
            // TODO: 3D: use persistent buffers to elide allocation.
            // TODO: 3D: extract sorting function from this mess.
            let renderTasks =
                { renderTasks with
                    RenderSurfacesForwardAbsolute =
                        renderTasks.RenderSurfacesForwardAbsolute |>
                        Seq.map (fun struct (model, surface) -> struct (model, surface, (model.Translation - eyePosition).MagnitudeSquared)) |>
                        Seq.toArray |>
                        Array.sortByDescending (fun struct (_, _, distanceSquared) -> distanceSquared) |>
                        Array.map (fun struct (model, surface, _) -> struct (model, surface)) |>
                        SegmentedList.ofSeq;
                    RenderSurfacesForwardRelative =
                        renderTasks.RenderSurfacesForwardRelative |>
                        Seq.map (fun struct (model, surface) -> struct (model, surface, (model.Translation - eyePosition).MagnitudeSquared)) |>
                        Seq.toArray |>
                        Array.sortByDescending (fun struct (_, _, distanceSquared) -> distanceSquared) |>
                        Array.map (fun struct (model, surface, _) -> struct (model, surface)) |>
                        SegmentedList.ofSeq }

            // setup geometry buffer
            let (positionTexture, normalTexture, albedoTexture, materialTexture, geometryFramebuffer) = renderer.RenderGeometryFramebuffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
            OpenGL.Gl.DepthMask true
            OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
            OpenGL.Gl.Scissor (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
            OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest
            OpenGL.Gl.DepthMask false
            OpenGL.Hl.Assert ()

            // attempt to locate last sky box
            let skyBoxOpt =
                match Seq.tryLast renderTasks.RenderSkyBoxes with
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
                SortableLight.sortLightsIntoArrays eyePosition renderTasks.RenderLights

            // render deferred pass w/ absolute-transformed surfaces
            for entry in renderTasks.RenderSurfacesDeferredAbsolute do
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
            for entry in renderTasks.RenderSurfacesDeferredRelative do
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
                (eyePosition, positionTexture, normalTexture, albedoTexture, materialTexture,
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
            for (model, surface) in renderTasks.RenderSurfacesForwardAbsolute do
                let (lightPositions, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewAbsoluteArray
                    projectionArray
                    (SegmentedList.singleton model)
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
            for (model, surface) in renderTasks.RenderSurfacesForwardRelative do
                let (lightPositions, lightColors, lightBrightnesses, lightIntensities) =
                    SortableLight.sortLightsIntoArrays model.Translation renderTasks.RenderLights
                GlRenderer3d.renderPhysicallyBasedSurfaces
                    eyePosition
                    viewRelativeArray
                    projectionArray
                    (SegmentedList.singleton model)
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

            // render pre-passes
            for pass in postPasses do
                pass.RenderPass3d (viewAbsolute, viewSkyBox, viewRelative, projection, renderTasks, renderer :> Renderer3d)
                OpenGL.Hl.Assert ()

            // end frame
            if renderer.RenderShouldEndFrame then
                OpenGL.Hl.EndFrame ()
                OpenGL.Hl.Assert ()

        member renderer.Swap () =
            match renderer.RenderWindow with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
            | WfglWindow window -> window.WfglSwapWindow ()

        member renderer.CleanUp () =
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderAssetPackages |> Seq.collect (Seq.map (fun entry -> entry.Value))
            for renderAsset in renderAssets do GlRenderer3d.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()
            renderer.RenderAssimp.Dispose ()