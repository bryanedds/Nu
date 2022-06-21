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

type [<CustomEquality; NoComparison; Struct>] RenderMaterial =
    { mutable HashCode : int
      Model : Matrix4x4
      Bounds : Box3
      Transparent : bool
      AlbedoTexture : uint
      MetalnessTexture : uint
      RoughnessTexture : uint
      NormalTexture : uint
      AmbientOcclusionTexture : uint
      LightPositions : Vector3 array
      LightColors : Vector3 array
      Surface : OpenGL.Hl.PhysicallyBasedSurface }

    static member inline hash material =
        hash material.Transparent ^^^
        hash material.Surface ^^^
        hash material.AlbedoTexture * hash material.MetalnessTexture * hash material.RoughnessTexture * hash material.NormalTexture * hash material.AmbientOcclusionTexture ^^^
        hash material.LightPositions ^^^
        hash material.LightColors

    static member inline create model bounds transparent albedoTexture metalnessTexture roughnessTexture normalTexture ambientOcclusionTexture lightPositions lightColors surface =
        let mutable result =
            { HashCode = 0
              Model = model
              Bounds = bounds
              Transparent = transparent
              Surface = surface
              AlbedoTexture = albedoTexture
              MetalnessTexture = metalnessTexture
              RoughnessTexture = roughnessTexture
              NormalTexture = normalTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              LightPositions = lightPositions
              LightColors = lightColors }
        result.HashCode <- RenderMaterial.hash result
        result

    static member inline equals left right =
        left.HashCode = right.HashCode &&
        left.Transparent = right.Transparent &&
        left.Surface.IndexBuffer = right.Surface.IndexBuffer &&
        left.Surface.VertexBuffer = right.Surface.VertexBuffer &&
        left.Surface.PhysicallyBasedVao = right.Surface.PhysicallyBasedVao &&
        left.AlbedoTexture = right.AlbedoTexture &&
        left.MetalnessTexture = right.MetalnessTexture &&
        left.RoughnessTexture = right.RoughnessTexture &&
        left.NormalTexture = right.NormalTexture &&
        left.AmbientOcclusionTexture = right.AmbientOcclusionTexture &&
        left.LightPositions = right.LightPositions &&
        left.LightColors = right.LightColors

    member this.Equals that =
        RenderMaterial.equals this that

    override this.Equals (thatObj : obj) =
        match thatObj with
        | :? RenderMaterial as that -> RenderMaterial.equals this that
        | _ -> false

    override this.GetHashCode () =
        RenderMaterial.hash this

/// A collection of render materials in a pass.
type [<NoEquality; NoComparison>] RenderMaterials =
    { RenderMaterialsOpaque : Dictionary<RenderMaterial, RenderMaterial SegmentedList>
      RenderMaterialsTransparent : RenderMaterial SegmentedList }

/// Describes a 3d render pass.
type [<CustomEquality; CustomComparison>] RenderPassDescriptor3d =
    { RenderPassOrder : int64
      RenderPass3d : RenderMaterials * Matrix4x4 * Matrix4x4 * Renderer3d -> unit }
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
    | RenderMaterialDescriptor of RenderMaterial
    | RenderMaterialsDescriptor of RenderMaterial array
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
        member renderer.Render _ _ _ _ = ()
        member renderer.Swap () = ()
        member renderer.CleanUp () = ()

    static member make () =
        { MockRenderer3d = () }

/// The OpenGL implementation of Renderer3d.
type [<ReferenceEquality; NoComparison>] GlRenderer3d =
    private
        { RenderWindow : Window
          RenderPhysicallyBasedShader : OpenGL.Hl.PhysicallyBasedShader
          RenderModelRow0Buffer : uint
          RenderModelRow1Buffer : uint
          RenderModelRow2Buffer : uint
          RenderModelRow3Buffer : uint
          RenderPackages : RenderAsset Packages
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          RenderMessages : RenderMessage3d List }

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
        | ".ttf" ->
            let fileFirstName = Path.GetFileNameWithoutExtension asset.FilePath
            let fileFirstNameLength = String.length fileFirstName
            if fileFirstNameLength >= 3 then
                let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                match Int32.TryParse fontSizeText with
                | (true, fontSize) ->
                    let fontOpt = SDL_ttf.TTF_OpenFont (asset.FilePath, fontSize)
                    if fontOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, FontAsset (fontSize, fontOpt))
                    else Log.debug ("Could not load font due to unparsable font size in file name '" + asset.FilePath + "'."); None
                | (false, _) -> Log.debug ("Could not load font due to file name being too short: '" + asset.FilePath + "'."); None
            else Log.debug ("Could not load font '" + asset.FilePath + "'."); None
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
    static member make attachDebugMessageCallback window =

        // create SDL-OpenGL context if needed
        match window with
        | SglWindow window -> OpenGL.Hl.CreateSglContext window.SglWindow |> ignore<nativeint>
        | WfglWindow _ -> () // TODO: 3D: see if we can make current the GL context here so that threaded OpenGL works in Gaia.
        OpenGL.Hl.Assert ()

        // listen to debug messages
        if attachDebugMessageCallback then
            OpenGL.Hl.AttachDebugMessageCallback ()

        // create one-off resources
        let physicallyBasedShader = OpenGL.Hl.CreatePhysicallyBasedShader Constants.Paths.PhysicallyBasedShaderFilePath
        OpenGL.Hl.Assert ()

        // make renderer
        let renderer =
            { RenderWindow = window
              RenderPhysicallyBasedShader = physicallyBasedShader
              RenderModelRow0Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow1Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow2Buffer = OpenGL.Gl.GenBuffer ()
              RenderModelRow3Buffer = OpenGL.Gl.GenBuffer ()
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderMessages = List () }

        // fin
        renderer

    interface Renderer3d with

        member renderer.PhysicallyBasedShader =
            renderer.RenderPhysicallyBasedShader

        member renderer.Render eyePosition eyeRotation windowSize renderMessages =

            // begin frame
            OpenGL.Hl.BeginFrame (Constants.Render.ViewportOffset windowSize)
            let mutable eyePosition = eyePosition
            let mutable view = eyeRotation
            view.M41 <- eyePosition.X
            view.M42 <- eyePosition.Y
            view.M43 <- eyePosition.Z
            let viewArray = view.ToArray ()
            let projection = GlRenderer3d.computeProjection ()
            let projectionArray = projection.ToArray ()
            OpenGL.Hl.Assert ()

            // categorize messages
            let prePasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let postPasses = hashSetPlus<RenderPassDescriptor3d> HashIdentity.Structural []
            let materials = dictPlus<RenderMaterial, RenderMaterial SegmentedList> HashIdentity.Structural []
            for message in renderer.RenderMessages do
                match message with
                | RenderMaterialDescriptor material ->
                    match materials.TryGetValue material with
                    | (true, materials) -> SegmentedList.add material materials
                    | (false, _) -> materials.Add (material, SegmentedList.singleton material)
                | RenderMaterialsDescriptor materials' ->
                    for material in materials' do
                        match materials.TryGetValue material with
                        | (true, materials) -> SegmentedList.add material materials
                        | (false, _) -> materials.Add (material, SegmentedList.singleton material)
                | RenderCallbackDescriptor3d _ -> ()
                | RenderPrePassDescriptor3d prePass -> prePasses.Add prePass |> ignore<bool>
                | RenderPostPassDescriptor3d postPass -> postPasses.Add postPass |> ignore<bool>

            // make render materials
            let materials =
                { RenderMaterialsOpaque = materials
                  RenderMaterialsTransparent = SegmentedList.make () }

            // render pre-passes
            for pass in prePasses do
                pass.RenderPass3d (materials, view, projection, renderer :> Renderer3d)

            // render main pass
            for entry in materials.RenderMaterialsOpaque do
                let material0 = entry.Key
                let materials = entry.Value
                if materials.Length > 0 then
                    OpenGL.Hl.DrawSurfaces
                        (material0.Surface, &eyePosition, (), viewArray, projectionArray,
                         material0.AlbedoTexture, material0.MetalnessTexture, material0.RoughnessTexture, material0.NormalTexture, material0.AmbientOcclusionTexture,
                         [||], [||],
                         renderer.RenderModelRow0Buffer, renderer.RenderModelRow1Buffer, renderer.RenderModelRow2Buffer, renderer.RenderModelRow3Buffer,
                         renderer.RenderPhysicallyBasedShader)

            // render pre-passes
            for pass in postPasses do
                pass.RenderPass3d (materials, view, projection, renderer :> Renderer3d)

            // end frame
            OpenGL.Hl.EndFrame ()
            OpenGL.Hl.Assert ()

        member renderer.Swap () =
            match renderer.RenderWindow with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
            | WfglWindow window -> window.WfglSwapWindow ()

        member renderer.CleanUp () =
            OpenGL.SpriteBatch.DestroyEnv renderer.RenderSpriteBatchEnv
            OpenGL.Hl.Assert ()
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderAssetPackages |> Seq.collect (Seq.map (fun entry -> entry.Value))
            for renderAsset in renderAssets do GlRenderer2d.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()