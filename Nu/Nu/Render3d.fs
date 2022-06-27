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

/// A callback for one-off forward rendering of a surface.
/// TODO: 3D: consider turning this into a delegate for byref params.
type ForwardRenderCallback =
    Vector3 * Matrix4x4 * Matrix4x4 * Matrix4x4 * Matrix4x4 * OpenGL.Hl.PhysicallyBasedSurface * Vector3 array * Color array -> Renderer3d -> unit

/// The type of rendering used on a surface.
and [<StructuralEquality; NoComparison; Struct>] RenderType =
    | RenderDeferred
    | RenderForward of obj voption

    static member makeForwardCallback (callback : ForwardRenderCallback)  =
        RenderForward (ValueSome (callback :> obj))

    static member tryGetCallback renderType =
        match renderType with
        | RenderDeferred -> ValueNone
        | RenderForward callbackObjOpt ->
            match callbackObjOpt with
            | ValueSome (:? ForwardRenderCallback as callback) -> ValueSome callback
            | _ -> ValueNone

/// A collection of render surfaces in a pass.
and [<NoEquality; NoComparison>] RenderSurfaces =
    { RenderSurfacesDeferredAbsolute : Dictionary<OpenGL.Hl.PhysicallyBasedSurface, Matrix4x4 SegmentedList>
      RenderSurfacesDeferredRelative : Dictionary<OpenGL.Hl.PhysicallyBasedSurface, Matrix4x4 SegmentedList>
      RenderSurfacesForwardAbsolute : struct (Matrix4x4 * OpenGL.Hl.PhysicallyBasedSurface * ForwardRenderCallback voption) SegmentedList
      RenderSurfacesForwardRelative : struct (Matrix4x4 * OpenGL.Hl.PhysicallyBasedSurface * ForwardRenderCallback voption) SegmentedList }

/// Describes a 3d render pass.
and [<CustomEquality; CustomComparison>] RenderPassDescriptor3d =
    { RenderPassOrder : int64
      RenderPass3d : RenderSurfaces * Matrix4x4 * Matrix4x4 * Matrix4x4 * Renderer3d -> unit }
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
    | RenderStaticModelDescriptor of bool * Matrix4x4 * RenderType * StaticModel AssetTag
    | RenderStaticModelsDescriptor of bool * Matrix4x4 array * RenderType * StaticModel AssetTag
    | RenderCachedStaticModelDescriptor of CachedStaticModelDescriptor
    | RenderPrePassDescriptor3d of RenderPassDescriptor3d
    | RenderPostPassDescriptor3d of RenderPassDescriptor3d
    | HintRenderPackageUseMessage3d of string
    | HintRenderPackageDisuseMessage3d of string
    | ReloadRenderAssetsMessage3d

/// The 3d renderer. Represents the 3d rendering system in Nu generally.
and Renderer3d =
    inherit Renderer
    /// The physically-based shader.
    abstract PhysicallyBasedShader : OpenGL.Hl.PhysicallyBasedShader
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
          RenderPhysicallyBasedShader : OpenGL.Hl.PhysicallyBasedShader
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
        | TextureAsset (_, texture) -> OpenGL.Hl.DeleteTexture texture
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset (asset : obj Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp"
        | ".png" ->
            match OpenGL.Hl.TryCreateSpriteTexture asset.FilePath with
            | Right texture ->
                Some (asset.AssetTag.AssetName, TextureAsset texture)
            | Left error ->
                Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
                None
        | ".obj" ->
            match OpenGL.Hl.TryCreatePhysicallyBasedStaticModel (true, asset.FilePath, renderer.RenderAssimp) with
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

    static member private categorizeStaticModel
        (modelAbsolute,
         modelMatrix : Matrix4x4 inref,
         modelRenderType : RenderType,
         modelAssetTag,
         surfacesDeferredAbsolute : Dictionary<_, _>,
         surfacesDeferredRelative : Dictionary<_, _>,
         renderer) =
        match GlRenderer3d.tryFindRenderAsset (AssetTag.generalize modelAssetTag) renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset modelAsset ->
                for surface in modelAsset.Surfaces do
                    match modelRenderType with
                    | RenderDeferred ->
                        if modelAbsolute then
                            match surfacesDeferredAbsolute.TryGetValue surface with
                            | (true, surfaces) -> SegmentedList.add modelMatrix surfaces
                            | (false, _) -> surfacesDeferredAbsolute.Add (surface, SegmentedList.singleton modelMatrix)
                        else
                            match surfacesDeferredRelative.TryGetValue surface with
                            | (true, surfaces) -> SegmentedList.add modelMatrix surfaces
                            | (false, _) -> surfacesDeferredRelative.Add (surface, SegmentedList.singleton modelMatrix)
                    | RenderForward _ ->
                        failwithnie ()
            | _ -> Log.trace "Cannot render static model with a non-model asset."
        | _ -> Log.info ("Descriptor failed to render due to unloadable assets for '" + scstring modelAssetTag + "'.")

    /// Compute the 3d projection matrix.
    /// TODO: 3D: expose this elsewhere.
    static member computeProjection () =
        Matrix4x4.CreatePerspectiveFieldOfView
            (Constants.Render.FieldOfView,
             Constants.Render.AspectRatio,
             Constants.Render.NearPlaneDistance,
             Constants.Render.FarPlaneDistance)

    /// Compute the 3d view frustum.
    /// TODO: 3D: expose this elsewhere.
    static member computeFrustum eyePosition (eyeRotation : Quaternion) =
        let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
        let view = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
        let projection = GlRenderer3d.computeProjection ()
        let viewProjection = view * projection
        Frustum viewProjection

    /// Get the physically-based shader.
    static member getPhysicallyBasedShader renderer =
        renderer.RenderPhysicallyBasedShader

    static member inline private renderSurfacesDeferred
        eyePosition
        viewArray
        projectionArray
        (models : Matrix4x4 SegmentedList)
        (surface : OpenGL.Hl.PhysicallyBasedSurface)
        lightPositions
        lightColors
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
            OpenGL.Hl.DrawPhysicallyBasedSurfaces
                (eyePosition, renderer.RenderModelsFields, models.Length, viewArray, projectionArray,
                 surface.AlbedoTexture, surface.MetalnessTexture, surface.RoughnessTexture, surface.NormalTexture, surface.AmbientOcclusionTexture,
                 lightPositions, lightColors, surface.PhysicallyBasedGeometry, renderer.RenderPhysicallyBasedShader)

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

        // create one-off resources
        let physicallyBasedShader = OpenGL.Hl.CreatePhysicallyBasedShader Constants.Paths.PhysicallyBasedShaderFilePath
        OpenGL.Hl.Assert ()

        // make renderer
        let renderer =
            { RenderWindow = window
              RenderAssimp = new Assimp.AssimpContext ()
              RenderPhysicallyBasedShader = physicallyBasedShader
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
            renderer.RenderPhysicallyBasedShader

        member renderer.Render eyePosition eyeRotation windowSize renderMessages =

            // begin frame
            if renderer.RenderShouldBeginFrame then
                OpenGL.Hl.BeginFrame (Constants.Render.ViewportOffset windowSize)
                OpenGL.Hl.Assert ()

            // compute view and projection
            let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
            let viewAbsolute = m4Identity
            let viewAbsoluteArray = viewAbsolute.ToArray ()
            let viewRelative = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
            let viewRelativeArray = viewRelative.ToArray ()
            let projection = GlRenderer3d.computeProjection ()
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // just use constant lights for now
            let lightPositions = [|-5.0f; 2.0f; 5.0f; 5.0f; 2.0f; 5.0f; -5.0f; 2.0f; -5.0f; 5.0f; 2.0f; -5.0f|]
            let lightColors = [|100.0f; 0.0f; 100.0f; 0.0f; 100.0f; 0.0f; 100.0f; 100.0f; 0.0f; 100.0f; 0.0f; 100.0f|]

            // categorize messages
            let prePasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let postPasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let surfacesDeferredAbsolute = dictPlus<OpenGL.Hl.PhysicallyBasedSurface, Matrix4x4 SegmentedList> HashIdentity.Structural []
            let surfacesDeferredRelative = dictPlus<OpenGL.Hl.PhysicallyBasedSurface, Matrix4x4 SegmentedList> HashIdentity.Structural []
            for message in renderMessages do
                match message with
                | RenderStaticModelDescriptor (modelAbsolute, modelMatrix, modelRenderType, modelAssetTag) ->
                    GlRenderer3d.categorizeStaticModel
                        (modelAbsolute,
                         &modelMatrix,
                         modelRenderType,
                         modelAssetTag,
                         surfacesDeferredAbsolute,
                         surfacesDeferredRelative,
                         renderer)
                | RenderStaticModelsDescriptor (modelAbsolute, modelMatrices, modelRenderType, modelAssetTag) ->
                    for modelMatrix in modelMatrices do
                        GlRenderer3d.categorizeStaticModel
                            (modelAbsolute,
                             &modelMatrix,
                             modelRenderType,
                             modelAssetTag,
                             surfacesDeferredAbsolute,
                             surfacesDeferredRelative,
                             renderer)
                | RenderCachedStaticModelDescriptor descriptor ->
                    GlRenderer3d.categorizeStaticModel
                        (descriptor.CachedStaticModelAbsolute,
                         &descriptor.CachedStaticModelMatrix,
                         descriptor.CachedStaticModelRenderType,
                         descriptor.CachedStaticModel,
                         surfacesDeferredAbsolute,
                         surfacesDeferredRelative,
                         renderer)
                | RenderPrePassDescriptor3d prePass ->
                    prePasses.Add prePass |> ignore<bool>
                | RenderPostPassDescriptor3d postPass ->
                    postPasses.Add postPass |> ignore<bool>

            // make render surfaces
            let surfaces =
                { RenderSurfacesDeferredAbsolute = surfacesDeferredAbsolute
                  RenderSurfacesDeferredRelative = surfacesDeferredRelative
                  RenderSurfacesForwardAbsolute = SegmentedList.make ()
                  RenderSurfacesForwardRelative = SegmentedList.make () }

            // render pre-passes
            for pass in prePasses do
                pass.RenderPass3d (surfaces, viewAbsolute, viewRelative, projection, renderer :> Renderer3d)

            // render main pass
            for entry in surfaces.RenderSurfacesDeferredRelative do
                GlRenderer3d.renderSurfacesDeferred
                    eyePosition
                    viewRelativeArray
                    projectionArray
                    entry.Value
                    entry.Key
                    lightPositions
                    lightColors
                    renderer

            // render pre-passes
            for pass in postPasses do
                pass.RenderPass3d (surfaces, viewAbsolute, viewRelative, projection, renderer :> Renderer3d)
                
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