// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Threading
open System.Threading.Tasks
open SDL2
open Prime
open Nu

// 3d rendering notes:
// I may borrow some sky dome code from here or related - https://github.com/shff/opengl_sky/blob/master/main.mm
// There appears to be a bias constant that can be used with VSMs to fix up light leaks, so consider that.

/// A collection of render surfaces in a pass.
type [<NoEquality; NoComparison>] RenderSurfaces =
    { RenderSurfacesOpaque : Dictionary<OpenGL.Hl.PhysicallyBasedMaterial, Matrix4x4 SegmentedList>
      RenderSurfacesTransparent : struct (OpenGL.Hl.PhysicallyBasedMaterial * Matrix4x4) SegmentedList }

/// Describes a 3d render pass.
type [<CustomEquality; CustomComparison>] RenderPassDescriptor3d =
    { RenderPassOrder : int64
      RenderPass3d : RenderSurfaces * Matrix4x4 * Matrix4x4 * Renderer3d -> unit }
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
    | RenderSurfaceDescriptor of OpenGL.Hl.PhysicallyBasedMaterial * Matrix4x4
    | RenderSurfacesDescriptor of OpenGL.Hl.PhysicallyBasedMaterial * Matrix4x4 array
    | RenderCallbackDescriptor3d of (Matrix4x4 * Matrix4x4 * Vector3 * Vector3 * Vector3 * Renderer3d -> unit)
    | RenderPrePassDescriptor3d of RenderPassDescriptor3d
    | RenderPostPassDescriptor3d of RenderPassDescriptor3d

/// The 3d renderer. Represents the 3d rendering system in Nu generally.
and Renderer3d =
    inherit Renderer
    /// The physically-based shader.
    abstract PhysicallyBasedShader : OpenGL.Hl.PhysicallyBasedShader
    /// Render a frame of the game.
    abstract Render : Vector3 -> Matrix4x4 -> Vector2i -> RenderMessage3d SegmentedList -> unit
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
          RenderModelRow0Buffer : uint
          RenderModelRow1Buffer : uint
          RenderModelRow2Buffer : uint
          RenderModelRow3Buffer : uint
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
        | ".fbx"
        | ".obj" ->
            match OpenGL.Hl.TryCreatePhysicallyBasedModel (asset.FilePath, renderer.RenderAssimp) with
            | Right model -> Some (asset.AssetTag.AssetName, ModelAsset model)
            | Left error -> Log.debug ("Could not load model '" + asset.FilePath + "' due to: " + error); None
        | extension -> Log.debug ("Could not load render asset '" + scstring asset + "' due to unknown extension '" + extension + "'."); None

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

    /// Compute the 3d projection matrix.
    static member computeProjection () =
        Matrix4x4.CreatePerspectiveFieldOfView
            (Constants.Render.FieldOfView,
             Constants.Render.AspectRatio,
             Constants.Render.NearPlaneDistance,
             Constants.Render.FarPlaneDistance)

    /// Get the physically-based shader.
    static member getPhysicallyBasedShader renderer =
        renderer.RenderPhysicallyBasedShader

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
              RenderModelRow0Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow1Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow2Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow3Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelsFields = Array.zeroCreate<single> (16 * 1024)
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderMessages = List ()
              RenderShouldBeginFrame = config.ShouldBeginFrame
              RenderShouldEndFrame = config.ShouldEndFrame }

        let filePath = "Assets/Default/test/gun.fbx"
        let modelOpt =
            match OpenGL.Hl.TryCreatePhysicallyBasedModel (filePath, renderer.RenderAssimp) with
            | Right model -> Some (ModelAsset model)
            | Left error -> Log.debug ("Could not load model '" + filePath + "' due to: " + error); None

        // fin
        renderer

    interface Renderer3d with

        member renderer.PhysicallyBasedShader =
            renderer.RenderPhysicallyBasedShader

        member renderer.Render eyePosition eyeRotation windowSize renderMessages =

            // begin frame
            if renderer.RenderShouldBeginFrame then
                OpenGL.Hl.BeginFrame (Constants.Render.ViewportOffset windowSize)

            // compute view and projection
            let mutable eyePosition = eyePosition
            let mutable view = eyeRotation
            view.M41 <- eyePosition.X
            view.M42 <- eyePosition.Y
            view.M43 <- eyePosition.Z
            let viewArray = view.ToArray ()
            let projection = GlRenderer3d.computeProjection ()
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // just use constant lights for now
            let lightPositions = [|-50.0f; 0.0f; 50.0f; 0.0f; 50.0f; 0.0f; 50.0f; 0.0f; 50.0f; -50.0f; 50.0f; 50.0f|]
            let lightColors = [|100.0f; 0.0f; 100.0f; 0.0f; 100.0f; 0.0f; 100.0f; 100.0f; 0.0f; 100.0f; 0.0f; 100.0f|]

            // categorize messages
            let prePasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let postPasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let surfaces = dictPlus<OpenGL.Hl.PhysicallyBasedMaterial, Matrix4x4 SegmentedList> HashIdentity.Structural []
            for message in renderMessages do
                match message with
                | RenderSurfaceDescriptor (surface, model) ->
                    match surfaces.TryGetValue surface with
                    | (true, surfaces) -> SegmentedList.add model surfaces
                    | (false, _) -> surfaces.Add (surface, SegmentedList.singleton model)
                | RenderSurfacesDescriptor (surface, models) ->
                    for model in models do
                        match surfaces.TryGetValue surface with
                        | (true, surfaces) -> SegmentedList.add model surfaces
                        | (false, _) -> surfaces.Add (surface, SegmentedList.singleton model)
                | RenderCallbackDescriptor3d _ -> ()
                | RenderPrePassDescriptor3d prePass -> prePasses.Add prePass |> ignore<bool>
                | RenderPostPassDescriptor3d postPass -> postPasses.Add postPass |> ignore<bool>

            // make render surfaces
            let surfaces =
                { RenderSurfacesOpaque = surfaces
                  RenderSurfacesTransparent = SegmentedList.make () }

            // render pre-passes
            for pass in prePasses do
                pass.RenderPass3d (surfaces, view, projection, renderer :> Renderer3d)

            // render main pass
            for entry in surfaces.RenderSurfacesOpaque do
                let material = entry.Key
                let models = entry.Value
                if models.Length > 0 then

                    // ensure we have a large enough field array
                    if models.Length * 16 * 16 > renderer.RenderModelsFields.Length then
                        renderer.RenderModelsFields <- Array.zeroCreate<single> (renderer.RenderModelsFields.Length * 2)

                    // blit models to field array
                    for i in 0 .. dec models.Length do
                        models.[i].ToArray (renderer.RenderModelsFields, i * 16)

                    // draw surfaces
                    OpenGL.Hl.DrawSurfaces
                        (eyePosition, renderer.RenderModelsFields, models.Length, viewArray, projectionArray,
                         material.AlbedoTexture, material.MetalnessTexture, material.RoughnessTexture, material.NormalTexture, material.AmbientOcclusionTexture,
                         lightPositions, lightColors,
                         renderer.RenderModelRow0Buffer, renderer.RenderModelRow1Buffer, renderer.RenderModelRow2Buffer, renderer.RenderModelRow3Buffer,
                         renderer.RenderPhysicallyBasedShader, material.Geometry)

            // render pre-passes
            for pass in postPasses do
                pass.RenderPass3d (surfaces, view, projection, renderer :> Renderer3d)
                
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