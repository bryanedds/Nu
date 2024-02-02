// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open SDL2
open TiledSharp
open Prime

/// A mutable sprite value.
type [<Struct>] SpriteValue =
    { mutable Transform : Transform
      mutable InsetOpt : Box2 ValueOption
      mutable Image : Image AssetTag
      mutable Color : Color
      mutable Blend : Blend
      mutable Emission : Color
      mutable Flip : Flip }

/// A mutable text value.
type [<Struct>] TextValue =
    { mutable Transform : Transform
      mutable Text : string
      mutable Font : Font AssetTag
      mutable FontSizing : int option
      mutable FontStyling : FontStyle Set
      mutable Color : Color
      mutable Justification : Justification }

/// Describes how to render a sprite to a rendering subsystem.
type SpriteDescriptor =
    { mutable Transform : Transform
      InsetOpt : Box2 ValueOption
      Image : Image AssetTag
      Color : Color
      Blend : Blend
      Emission : Color
      Flip : Flip }

/// Describes how to render multiple sprites to a rendering subsystem.
type [<NoEquality; NoComparison>] SpritesDescriptor =
    { Sprites : SpriteValue SArray }

/// Describes how to render multiple sprite descriptors to a rendering subsystem.
type [<NoEquality; NoComparison>] SpriteDescriptors =
    { SpriteDescriptors : SpriteDescriptor SList }

/// Describes an internally cached sprite used to avoid GC promotion of sprite descriptors.
type CachedSpriteDescriptor =
    { mutable CachedSprite : SpriteValue }

/// Describes how to render tile map tiles to the rendering system.
type [<NoEquality; NoComparison>] TilesDescriptor =
    { mutable Transform : Transform
      Color : Color
      Emission : Color
      MapSize : Vector2i
      Tiles : TmxLayerTile SList
      TileSourceSize : Vector2i
      TileSize : Vector2
      TileAssets : (TmxTileset * Image AssetTag) array }

/// Describes sprite-based particles.
type [<NoEquality; NoComparison>] SpriteParticlesDescriptor =
    { Absolute : bool
      Elevation : single
      Horizon : single
      Blend : Blend
      Image : Image AssetTag
      Particles : Particle SArray }

/// Describes how to render text to a rendering subsystem.
type TextDescriptor =
    { mutable Transform : Transform
      Text : string
      Font : Font AssetTag
      FontSizing : int option
      FontStyling : FontStyle Set
      Color : Color
      Justification : Justification }

/// Describes a 2d rendering operation.
type [<ReferenceEquality>] RenderOperation2d =
    | RenderSprite of SpriteDescriptor
    | RenderSprites of SpritesDescriptor
    | RenderSpriteDescriptors of SpriteDescriptors
    | RenderSpriteParticles of SpriteParticlesDescriptor
    | RenderCachedSprite of CachedSpriteDescriptor
    | RenderText of TextDescriptor
    | RenderTiles of TilesDescriptor
    | RenderCallback2d of (Vector2 * Vector2 * Renderer2d -> unit)

/// Describes a layered rendering operation to a 2d rendering subsystem.
/// NOTE: mutation is used only for internal sprite descriptor caching.
and [<ReferenceEquality>] LayeredOperation2d =
    { mutable Elevation : single
      mutable Horizon : single
      mutable AssetTag : AssetTag
      mutable RenderOperation2d : RenderOperation2d }

/// A message to a 2d rendering subsystem.
and [<ReferenceEquality>] RenderMessage2d =
    | LayeredOperation2d of LayeredOperation2d
    | LoadRenderPackage2d of string
    | UnloadRenderPackage2d of string
    | ReloadRenderAssets2d

/// The 2d renderer. Represents a 2d rendering subsystem in Nu generally.
and Renderer2d =
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> Vector2i -> RenderMessage2d List -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of Renderer2d.
type [<ReferenceEquality>] StubRenderer2d =
    private
        { StubRenderer2d : unit }

    interface Renderer2d with
        member renderer.Render _ _ _ _ = ()
        member renderer.CleanUp () = ()

    static member make () =
        { StubRenderer2d = () }

/// Compares layered 2d operations.
type LayeredOperation2dComparer () =
    interface IComparer<LayeredOperation2d> with
        member this.Compare (left, right) =
            if left.Elevation < right.Elevation then -1
            elif left.Elevation > right.Elevation then 1
            elif left.Horizon > right.Horizon then -1
            elif left.Horizon < right.Horizon then 1
            else
                let assetNameCompare = strCmp left.AssetTag.AssetName right.AssetTag.AssetName
                if assetNameCompare <> 0 then assetNameCompare
                else strCmp left.AssetTag.PackageName right.AssetTag.PackageName

/// The internally used package state for the 2d OpenGL renderer.
type [<ReferenceEquality>] private GlPackageState2d =
    { TextureMemo : OpenGL.Texture.TextureMemo
      CubeMapMemo : OpenGL.CubeMap.CubeMapMemo
      AssimpSceneMemo : OpenGL.Assimp.AssimpSceneMemo }

/// The OpenGL implementation of Renderer2d.
type [<ReferenceEquality>] GlRenderer2d =
    private
        { Window : Window
          SpriteShader : int * int * int * int * uint // TODO: release these resources on clean-up.
          SpriteQuad : uint * uint * uint // TODO: release these resources on clean-up.
          TextQuad : uint * uint * uint // TODO: release these resources on clean-up.
          SpriteBatchEnv : OpenGL.SpriteBatch.SpriteBatchEnv
          RenderPackages : Packages<RenderAsset, GlPackageState2d>
          mutable RenderPackageCachedOpt : string * Package<RenderAsset, GlPackageState2d> // OPTIMIZATION: nullable for speed.
          mutable RenderAssetCachedOpt : AssetTag * RenderAsset
          mutable ReloadAssetsRequested : bool
          LayeredOperations : LayeredOperation2d List }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset renderAsset renderer =
        GlRenderer2d.invalidateCaches renderer
        match renderAsset with
        | RawAsset -> ()
        | TextureAsset (_, texture) -> OpenGL.Texture.DestroyTexture texture
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font
        | CubeMapAsset _ -> ()
        | StaticModelAsset _ -> ()
        | AnimatedModelAsset _ -> ()

    static member private tryLoadRenderAsset packageState (asset : Asset) renderer =
        GlRenderer2d.invalidateCaches renderer
        match PathF.GetExtensionLower asset.FilePath with
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff"| ".dds" ->
            match OpenGL.Texture.TryCreateTextureUnfilteredMemoized (asset.FilePath, packageState.TextureMemo) with
            | Right (textureMetadata, texture) ->
                Some (TextureAsset (textureMetadata, texture))
            | Left error ->
                Log.infoOnce ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
                None
        | ".ttf" ->
            let fileFirstName = PathF.GetFileNameWithoutExtension asset.FilePath
            let fileFirstNameLength = String.length fileFirstName
            let fontSizeDefault =
                if fileFirstNameLength >= 3 then
                    let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                    match Int32.TryParse fontSizeText with
                    | (true, fontSize) -> fontSize
                    | (false, _) -> Constants.Render.FontSizeDefault
                else Constants.Render.FontSizeDefault
            let fontSize = fontSizeDefault * Constants.Render.VirtualScalar
            let fontOpt = SDL_ttf.TTF_OpenFont (asset.FilePath, fontSize)
            if fontOpt <> IntPtr.Zero
            then Some (FontAsset (fontSizeDefault, fontOpt))
            else Log.info ("Could not load font due to '" + SDL_ttf.TTF_GetError () + "'."); None
        | _ -> None

    static member private tryLoadRenderPackage packageName renderer =

        // attempt to make new asset graph and load its assets
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Render2d) packageName assetGraph with
            | Right assetsCollected ->

                // find or create render package
                let renderPackage =
                    match Dictionary.tryFind packageName renderer.RenderPackages with
                    | Some renderPackage -> renderPackage
                    | None ->
                        let renderPackageState = { TextureMemo = OpenGL.Texture.TextureMemo.make (); CubeMapMemo = OpenGL.CubeMap.CubeMapMemo.make (); AssimpSceneMemo = OpenGL.Assimp.AssimpSceneMemo.make () }
                        let renderPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = renderPackageState }
                        renderer.RenderPackages.[packageName] <- renderPackage
                        renderPackage

                // categorize existing assets based on the required action
                let assetsExisting = renderPackage.Assets
                let assetsToFree = Dictionary ()
                let assetsToKeep = Dictionary ()
                for assetEntry in assetsExisting do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, filePath, renderAsset) = assetEntry.Value
                    let lastWriteTime' =
                        try DateTimeOffset (File.GetLastWriteTime filePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    if lastWriteTime < lastWriteTime'
                    then assetsToFree.Add (filePath, renderAsset)
                    else assetsToKeep.Add (assetName, (lastWriteTime, filePath, renderAsset))

                // free assets, including memo entries
                for assetEntry in assetsToFree do
                    let filePath = assetEntry.Key
                    let renderAsset = assetEntry.Value
                    match renderAsset with
                    | RawAsset -> ()
                    | TextureAsset _ -> renderPackage.PackageState.TextureMemo.Textures.Remove filePath |> ignore<bool>
                    | FontAsset _ -> ()
                    | CubeMapAsset (cubeMapKey, _, _) -> renderPackage.PackageState.CubeMapMemo.CubeMaps.Remove cubeMapKey |> ignore<bool>
                    | StaticModelAsset _ | AnimatedModelAsset _ -> renderPackage.PackageState.AssimpSceneMemo.AssimpScenes.Remove filePath |> ignore<bool>
                    GlRenderer2d.freeRenderAsset renderAsset renderer

                // categorize assets to load
                let assetsToLoad = HashSet ()
                for asset in assetsCollected do
                    if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                        assetsToLoad.Add asset |> ignore<bool>

                // memoize assets in parallel
                AssetMemo.memoizeParallel
                    true assetsToLoad renderPackage.PackageState.TextureMemo renderPackage.PackageState.CubeMapMemo renderPackage.PackageState.AssimpSceneMemo

                // load assets
                let assetsLoaded = Dictionary ()
                for asset in assetsToLoad do
                    match GlRenderer2d.tryLoadRenderAsset renderPackage.PackageState asset renderer with
                    | Some renderAsset ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                        assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, renderAsset)
                    | None -> ()

                // insert assets into package
                for assetEntry in Seq.append assetsToKeep assetsLoaded do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, filePath, renderAsset) = assetEntry.Value
                    renderPackage.Assets.[assetName] <- (lastWriteTime, filePath, renderAsset)

            // handle error cases
            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member tryGetRenderAsset (assetTag : AssetTag) renderer =
        if  renderer.RenderAssetCachedOpt :> obj |> notNull &&
            assetEq assetTag (fst renderer.RenderAssetCachedOpt) then
            ValueSome (snd renderer.RenderAssetCachedOpt)
        elif
            renderer.RenderPackageCachedOpt :> obj |> notNull &&
            fst renderer.RenderPackageCachedOpt = assetTag.PackageName then
            let package = snd renderer.RenderPackageCachedOpt
            match package.Assets.TryGetValue assetTag.AssetName with
            | (true, (_, _, asset)) ->
                renderer.RenderAssetCachedOpt <- (assetTag, asset)
                ValueSome asset
            | (false, _) -> ValueNone
        else
            match Dictionary.tryFind assetTag.PackageName renderer.RenderPackages with
            | Some package ->
                renderer.RenderPackageCachedOpt <- (assetTag.PackageName, package)
                match package.Assets.TryGetValue assetTag.AssetName with
                | (true, (_, _, asset)) ->
                    renderer.RenderAssetCachedOpt <- (assetTag, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
            | None ->
                Log.info ("Loading Render2d package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                GlRenderer2d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, package) ->
                    renderer.RenderPackageCachedOpt <- (assetTag.PackageName, package)
                    match package.Assets.TryGetValue assetTag.AssetName with
                    | (true, (_, _, asset)) ->
                        renderer.RenderAssetCachedOpt <- (assetTag, asset)
                        ValueSome asset
                    | (false, _) -> ValueNone
                | (false, _) -> ValueNone

    static member private handleLoadRenderPackage hintPackageName renderer =
        GlRenderer2d.tryLoadRenderPackage hintPackageName renderer

    static member private handleUnloadRenderPackage hintPackageName renderer =
        GlRenderer2d.invalidateCaches renderer
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some package ->
            for asset in package.Assets do GlRenderer2d.freeRenderAsset (__c asset.Value) renderer
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        GlRenderer2d.invalidateCaches renderer
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        for packageName in packageNames do
            GlRenderer2d.tryLoadRenderPackage packageName renderer

    static member private handleRenderMessage renderMessage renderer =
        match renderMessage with
        | LayeredOperation2d operation -> renderer.LayeredOperations.Add operation
        | LoadRenderPackage2d hintPackageUse -> GlRenderer2d.handleLoadRenderPackage hintPackageUse renderer
        | UnloadRenderPackage2d hintPackageDisuse -> GlRenderer2d.handleUnloadRenderPackage hintPackageDisuse renderer
        | ReloadRenderAssets2d -> renderer.ReloadAssetsRequested <- true

    static member private handleRenderMessages renderMessages renderer =
        for renderMessage in renderMessages do
            GlRenderer2d.handleRenderMessage renderMessage renderer

    static member private sortLayeredOperations renderer =
        renderer.LayeredOperations.Sort (LayeredOperation2dComparer ())

    /// Get the sprite shader created by OpenGL.Hl.CreateSpriteShader.
    static member getSpriteShader renderer =
        renderer.SpriteShader

    /// Get the sprite quad created by OpenGL.Hl.CreateSpriteQuad.
    static member getSpriteQuad renderer =
        renderer.SpriteQuad

    /// Get the text quad created by OpenGL.Hl.CreateSpriteQuad.
    static member getTextQuad renderer =
        renderer.TextQuad

    static member
#if !DEBUG
        inline
#endif
        private batchSprite
        absolute
        min
        size
        pivot
        rotation
        (insetOpt : Box2 voption)
        (textureMetadata : OpenGL.Texture.TextureMetadata)
        (texture : OpenGL.Texture.Texture)
        (color : Color)
        blend
        (emission : Color)
        flip
        renderer =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            let texelWidth = textureMetadata.TextureTexelWidth
            let texelHeight = textureMetadata.TextureTexelHeight
            let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
            let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
            match insetOpt with
            | ValueSome inset ->
                let mx = inset.Min.X * texelWidth + borderWidth
                let my = (inset.Min.Y + inset.Size.Y) * texelHeight - borderHeight
                let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                Box2 (mx, my, sx, sy)
            | ValueNone ->
                let mx = borderWidth
                let my = 1.0f - borderHeight
                let sx = 1.0f - borderWidth * 2.0f
                let sy = -1.0f + borderHeight * 2.0f
                Box2 (mx, my, sx, sy)

        // compute a flipping flags
        let struct (flipH, flipV) =
            match flip with
            | FlipNone -> struct (false, false)
            | FlipH -> struct (true, false)
            | FlipV -> struct (false, true)
            | FlipHV -> struct (true, true)

        // compute tex coords
        let texCoords =
            box2
                (v2
                    (if flipH then texCoordsUnflipped.Min.X + texCoordsUnflipped.Size.X else texCoordsUnflipped.Min.X)
                    (if flipV then texCoordsUnflipped.Min.Y + texCoordsUnflipped.Size.Y else texCoordsUnflipped.Min.Y))
                (v2
                    (if flipH then -texCoordsUnflipped.Size.X else texCoordsUnflipped.Size.X)
                    (if flipV then -texCoordsUnflipped.Size.Y else texCoordsUnflipped.Size.Y))

        // compute blending instructions
        let struct (bfs, bfd, beq) =
            match blend with
            | Transparent -> struct (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha, OpenGL.BlendEquationMode.FuncAdd)
            | Additive -> struct (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.One, OpenGL.BlendEquationMode.FuncAdd)
            | Overwrite -> struct (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero, OpenGL.BlendEquationMode.FuncAdd)

        // attempt to draw normal sprite
        if color.A <> 0.0f then
            OpenGL.SpriteBatch.SubmitSpriteBatchSprite (absolute, min, size, pivot, rotation, &texCoords, &color, bfs, bfd, beq, texture, renderer.SpriteBatchEnv)

        // attempt to draw emission sprite
        if emission.A <> 0.0f then
            OpenGL.SpriteBatch.SubmitSpriteBatchSprite (absolute, min, size, pivot, rotation, &texCoords, &emission, OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.One, OpenGL.BlendEquationMode.FuncAdd, texture, renderer.SpriteBatchEnv)

    /// Render sprite.
    static member renderSprite
        (transform : Transform byref,
         insetOpt : Box2 ValueOption inref,
         image : Image AssetTag,
         color : Color inref,
         blend : Blend,
         emission : Color inref,
         flip : Flip,
         renderer) =
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let min = perimeter.Min.V2 * Constants.Render.VirtualScalar2
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let pivot = transform.PerimeterPivot.V2 * Constants.Render.VirtualScalar2
        let rotation = -transform.Angles.Z
        match GlRenderer2d.tryGetRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset (textureMetadata, texture) ->
                GlRenderer2d.batchSprite absolute min size pivot rotation insetOpt textureMetadata texture color blend emission flip renderer
            | _ -> Log.infoOnce ("Cannot render sprite with a non-texture asset for '" + scstring image + "'.")
        | _ -> Log.infoOnce ("Sprite failed to render due to unloadable asset for '" + scstring image + "'.")

    /// Render sprite particles.
    static member renderSpriteParticles (blend : Blend, image : Image AssetTag, particles : Particle SArray, renderer) =
        match GlRenderer2d.tryGetRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset (textureMetadata, texture) ->
                let mutable index = 0
                while index < particles.Length do
                    let particle = &particles.[index]
                    let transform = &particle.Transform
                    let absolute = transform.Absolute
                    let perimeter = transform.Perimeter
                    let min = perimeter.Min.V2 * Constants.Render.VirtualScalar2
                    let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
                    let pivot = transform.PerimeterPivot.V2 * Constants.Render.VirtualScalar2
                    let rotation = -transform.Angles.Z
                    let color = &particle.Color
                    let emission = &particle.Emission
                    let flip = particle.Flip
                    let insetOpt = &particle.InsetOpt
                    GlRenderer2d.batchSprite absolute min size pivot rotation insetOpt textureMetadata texture color blend emission flip renderer
                    index <- inc index
            | _ -> Log.infoOnce ("Cannot render sprite particle with a non-texture asset for '" + scstring image + "'.")
        | _ -> Log.infoOnce ("Sprite particles failed to render due to unloadable asset for '" + scstring image + "'.")

    /// Render tiles.
    static member renderTiles
        (transform : Transform byref,
         color : Color inref,
         emission : Color inref,
         mapSize : Vector2i,
         tiles : TmxLayerTile SList,
         tileSourceSize : Vector2i,
         tileSize : Vector2,
         tileAssets : (TmxTileset * Image AssetTag) array,
         eyeCenter : Vector2,
         eyeSize : Vector2,
         renderer) =

        // gather context for rendering tiles
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let min = perimeter.Min.V2 * Constants.Render.VirtualScalar2
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let eyeCenter = eyeCenter * Constants.Render.VirtualScalar2
        let eyeSize = eyeSize * Constants.Render.VirtualScalar2
        let tileSize = tileSize * Constants.Render.VirtualScalar2
        let tilePivot = tileSize * 0.5f // just rotate around center
        let (allFound, tileSetTextures) =
            tileAssets |>
            Array.map (fun (tileSet, tileSetImage) ->
                match GlRenderer2d.tryGetRenderAsset tileSetImage renderer with
                | ValueSome (TextureAsset (tileSetTexture, tileSetTextureMetadata)) -> Some (tileSet, tileSetImage, tileSetTexture, tileSetTextureMetadata)
                | ValueSome _ -> None
                | ValueNone -> None) |>
            Array.definitizePlus

        // render only when all needed textures are found
        if allFound then

            // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
            let tilesLength = tiles.Length
            let mutable tileIndex = 0
            while tileIndex < tilesLength do

                // gather context for rendering tile
                let tile = tiles.[tileIndex]
                if tile.Gid <> 0 then // not the empty tile
                    let mapRun = mapSize.X
                    let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
                    let tileMin =
                        v2
                            (min.X + tileSize.X * single i)
                            (min.Y - tileSize.Y - tileSize.Y * single j + size.Y)
                    let tileBounds = box2 tileMin tileSize
                    let viewBounds = box2 (eyeCenter - eyeSize * 0.5f) eyeSize
                    if tileBounds.Intersects viewBounds then
        
                        // compute tile flip
                        let flip =
                            match (tile.HorizontalFlip, tile.VerticalFlip) with
                            | (false, false) -> FlipNone
                            | (true, false) -> FlipH
                            | (false, true) -> FlipV
                            | (true, true) -> FlipHV
        
                        // attempt to compute tile set texture
                        let mutable tileOffset = 1 // gid 0 is the empty tile
                        let mutable tileSetIndex = 0
                        let mutable tileSetWidth = 0
                        let mutable tileSetTextureOpt = None
                        for (set, _, textureMetadata, texture) in tileSetTextures do
                            let tileCountOpt = set.TileCount
                            let tileCount = if tileCountOpt.HasValue then tileCountOpt.Value else 0
                            if  tile.Gid >= set.FirstGid && tile.Gid < set.FirstGid + tileCount ||
                                not tileCountOpt.HasValue then // HACK: when tile count is missing, assume we've found the tile...?
                                tileSetWidth <- let width = set.Image.Width in width.Value
#if DEBUG
                                if tileSetWidth % tileSourceSize.X <> 0 then Log.infoOnce ("Tile set '" + set.Name + "' width is not evenly divided by tile width.")
#endif
                                tileSetTextureOpt <- Some (textureMetadata, texture)
                            if Option.isNone tileSetTextureOpt then
                                tileSetIndex <- inc tileSetIndex
                                tileOffset <- tileOffset + tileCount
        
                        // attempt to render tile
                        match tileSetTextureOpt with
                        | Some (textureMetadata, texture) ->
                            let tileId = tile.Gid - tileOffset
                            let tileIdPosition = tileId * tileSourceSize.X
                            let tileSourcePosition = v2 (single (tileIdPosition % tileSetWidth)) (single (tileIdPosition / tileSetWidth * tileSourceSize.Y))
                            let inset = box2 tileSourcePosition (v2 (single tileSourceSize.X) (single tileSourceSize.Y))
                            GlRenderer2d.batchSprite absolute tileMin tileSize tilePivot 0.0f (ValueSome inset) textureMetadata texture color Transparent emission flip renderer
                        | None -> ()

                // fin
                tileIndex <- inc tileIndex
        else Log.infoOnce ("TileLayerDescriptor failed due to unloadable or non-texture assets for one or more of '" + scstring tileAssets + "'.")

    /// Render text.
    static member renderText
        (transform : Transform byref,
         text : string,
         font : Font AssetTag,
         fontSizing : int option,
         fontStyling : FontStyle Set,
         color : Color inref,
         justification : Justification,
         eyeCenter : Vector2,
         eyeSize : Vector2,
         renderer : GlRenderer2d) =

        // render only when color isn't fully transparent because SDL_TTF doesn't handle zero alpha text as expected.
        let color = color // copy to local for proprety access
        if color.A8 <> 0uy then
            let transform = transform // copy to local to make visible from lambda
            flip OpenGL.SpriteBatch.InterruptSpriteBatchFrame renderer.SpriteBatchEnv $ fun () ->

                // gather context for rendering text
                let mutable transform = transform
                let absolute = transform.Absolute
                let perimeter = transform.Perimeter
                let position = perimeter.Min.V2 * Constants.Render.VirtualScalar2
                let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
                let viewport = Constants.Render.Viewport
                let viewProjection = viewport.ViewProjection2d (absolute, eyeCenter, eyeSize)
                match GlRenderer2d.tryGetRenderAsset font renderer with
                | ValueSome renderAsset ->
                    match renderAsset with
                    | FontAsset (fontSizeDefault, font) ->

                        // gather rendering resources
                        // NOTE: the resource implications (throughput and fragmentation?) of creating and destroying a
                        // surface and texture one or more times a frame must be understood!
                        let (offset, textSurface, textSurfacePtr) =

                            // create sdl color
                            let mutable colorSdl = SDL.SDL_Color ()
                            colorSdl.r <- color.R8
                            colorSdl.g <- color.G8
                            colorSdl.b <- color.B8
                            colorSdl.a <- color.A8

                            // attempt to configure sdl font size
                            let fontSize =
                                match fontSizing with
                                | Some fontSize -> fontSize * Constants.Render.VirtualScalar
                                | None -> fontSizeDefault * Constants.Render.VirtualScalar
                            let errorCode = SDL_ttf.TTF_SetFontSize (font, fontSize)
                            if errorCode <> 0 then
                                let error = SDL_ttf.TTF_GetError ()
                                Log.infoOnce ("Failed to set font size for font '" + scstring font + "' due to: " + error)
                                SDL_ttf.TTF_SetFontSize (font, fontSizeDefault * Constants.Render.VirtualScalar) |> ignore<int>

                            // configure sdl font style
                            let styleSdl =
                                if fontStyling.Count > 0 then // OPTIMIZATION: avoid set queries where possible.
                                    (if fontStyling.Contains Bold then SDL_ttf.TTF_STYLE_BOLD else 0) |||
                                    (if fontStyling.Contains Italic then SDL_ttf.TTF_STYLE_ITALIC else 0) |||
                                    (if fontStyling.Contains Underline then SDL_ttf.TTF_STYLE_UNDERLINE else 0) |||
                                    (if fontStyling.Contains Strikethrough then SDL_ttf.TTF_STYLE_STRIKETHROUGH else 0)
                                else 0
                            SDL_ttf.TTF_SetFontStyle (font, styleSdl)

                            // render text to surface
                            match justification with
                            | Unjustified wrapped ->
                                let textSurfacePtr =
                                    if wrapped
                                    then SDL_ttf.TTF_RenderUNICODE_Blended_Wrapped (font, text, colorSdl, uint32 size.X)
                                    else SDL_ttf.TTF_RenderUNICODE_Blended (font, text, colorSdl)
                                let textSurface = Marshal.PtrToStructure<SDL.SDL_Surface> textSurfacePtr
                                let textSurfaceHeight = single textSurface.h
                                let offsetY = size.Y - textSurfaceHeight
                                (v2 0.0f offsetY, textSurface, textSurfacePtr)
                            | Justified (h, v) ->
                                let mutable width = 0
                                let mutable height = 0
                                SDL_ttf.TTF_SizeUNICODE (font, text, &width, &height) |> ignore
                                let textSurfacePtr = SDL_ttf.TTF_RenderUNICODE_Blended (font, text, colorSdl)
                                let textSurface = Marshal.PtrToStructure<SDL.SDL_Surface> textSurfacePtr
                                let offsetX =
                                    match h with
                                    | JustifyLeft -> 0.0f
                                    | JustifyCenter -> (size.X - single width) * 0.5f
                                    | JustifyRight -> size.X - single width
                                let offsetY =
                                    match v with
                                    | JustifyTop -> 0.0f
                                    | JustifyMiddle -> (size.Y - single height) * 0.5f
                                    | JustifyBottom -> size.Y - single height
                                let offset = v2 offsetX offsetY
                                (offset, textSurface, textSurfacePtr)

                        // render only when a valid surface was created
                        if textSurfacePtr <> IntPtr.Zero then

                            // construct mvp matrix
                            let textSurfaceWidth = textSurface.pitch / 4 // NOTE: textSurface.w may be an innacurate representation of texture width in SDL2_ttf versions beyond v2.0.15 because... I don't know why.
                            let textSurfaceHeight = textSurface.h
                            let translation = (position + offset).V3
                            let scale = v3 (single textSurfaceWidth) (single textSurfaceHeight) 1.0f
                            let modelTranslation = Matrix4x4.CreateTranslation translation
                            let modelScale = Matrix4x4.CreateScale scale
                            let modelMatrix = modelScale * modelTranslation
                            let modelViewProjection = modelMatrix * viewProjection

                            // upload texture data
                            let textTextureId = OpenGL.Gl.GenTexture ()
                            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, textTextureId)
                            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, Constants.OpenGL.UncompressedTextureFormat, textSurfaceWidth, textSurfaceHeight, 0, OpenGL.PixelFormat.Bgra, OpenGL.PixelType.UnsignedByte, textSurface.pixels)
                            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
                            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
                            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
                            OpenGL.Hl.Assert ()

                            // make texture drawable
                            let textTexture = OpenGL.Texture.CreateTextureFromId textTextureId
                            OpenGL.Hl.Assert ()

                            // draw text sprite
                            // NOTE: we allocate an array here, too.
                            let (vertices, indices, vao) = renderer.TextQuad
                            let (modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader) = renderer.SpriteShader
                            OpenGL.Sprite.DrawSprite (vertices, indices, vao, modelViewProjection.ToArray (), ValueNone, Color.White, FlipNone, textSurfaceWidth, textSurfaceHeight, textTexture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader)
                            OpenGL.Hl.Assert ()

                            // destroy texture
                            SDL.SDL_FreeSurface textSurfacePtr
                            OpenGL.Texture.DestroyTexture textTexture
                            OpenGL.Hl.Assert ()

                    // fin
                    | _ -> Log.infoOnce ("Cannot render text with a non-font asset for '" + scstring font + "'.")
                | _ -> Log.infoOnce ("TextDescriptor failed due to unloadable asset for '" + scstring font + "'.")
            OpenGL.Hl.Assert ()

    static member
#if !DEBUG
        inline
#endif
        private renderCallback callback eyeCenter eyeSize renderer =
        flip OpenGL.SpriteBatch.InterruptSpriteBatchFrame renderer.SpriteBatchEnv $ fun () -> callback (eyeCenter, eyeSize, renderer)
        OpenGL.Hl.Assert ()

    static member private renderDescriptor descriptor eyeCenter eyeSize renderer =
        match descriptor with
        | RenderSprite descriptor ->
            GlRenderer2d.renderSprite (&descriptor.Transform, &descriptor.InsetOpt, descriptor.Image, &descriptor.Color, descriptor.Blend, &descriptor.Emission, descriptor.Flip, renderer)
        | RenderSprites descriptor ->
            let sprites = descriptor.Sprites
            for index in 0 .. sprites.Length - 1 do
                let sprite = &sprites.[index]
                GlRenderer2d.renderSprite (&sprite.Transform, &sprite.InsetOpt, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip, renderer)
        | RenderSpriteDescriptors descriptor ->
            let sprites = descriptor.SpriteDescriptors
            for index in 0 .. sprites.Length - 1 do
                let sprite = sprites.[index]
                GlRenderer2d.renderSprite (&sprite.Transform, &sprite.InsetOpt, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip, renderer)
        | RenderSpriteParticles descriptor ->
            GlRenderer2d.renderSpriteParticles (descriptor.Blend, descriptor.Image, descriptor.Particles, renderer)
        | RenderCachedSprite descriptor ->
            GlRenderer2d.renderSprite (&descriptor.CachedSprite.Transform, &descriptor.CachedSprite.InsetOpt, descriptor.CachedSprite.Image, &descriptor.CachedSprite.Color, descriptor.CachedSprite.Blend, &descriptor.CachedSprite.Emission, descriptor.CachedSprite.Flip, renderer)
        | RenderText descriptor ->
            GlRenderer2d.renderText
                (&descriptor.Transform, descriptor.Text, descriptor.Font, descriptor.FontSizing, descriptor.FontStyling, &descriptor.Color, descriptor.Justification, eyeCenter, eyeSize, renderer)
        | RenderTiles descriptor ->
            GlRenderer2d.renderTiles
                (&descriptor.Transform, &descriptor.Color, &descriptor.Emission,
                 descriptor.MapSize, descriptor.Tiles, descriptor.TileSourceSize, descriptor.TileSize, descriptor.TileAssets,
                 eyeCenter, eyeSize, renderer)
        | RenderCallback2d callback ->
            GlRenderer2d.renderCallback callback eyeCenter eyeSize renderer

    static member private renderLayeredOperations eyeCenter eyeSize renderer =
        for operation in renderer.LayeredOperations do
            GlRenderer2d.renderDescriptor operation.RenderOperation2d eyeCenter eyeSize renderer

    static member private render eyeCenter eyeSize renderMessages renderer =

        // begin sprite batch frame
        let viewport = Constants.Render.Viewport
        let viewProjectionAbsolute = viewport.ViewProjection2d (true, eyeCenter, eyeSize)
        let viewProjectionRelative = viewport.ViewProjection2d (false, eyeCenter, eyeSize)
        OpenGL.SpriteBatch.BeginSpriteBatchFrame (&viewProjectionAbsolute, &viewProjectionRelative, renderer.SpriteBatchEnv)
        OpenGL.Hl.Assert ()

        // render frame
        GlRenderer2d.handleRenderMessages renderMessages renderer
        GlRenderer2d.sortLayeredOperations renderer
        GlRenderer2d.renderLayeredOperations eyeCenter eyeSize renderer
        renderer.LayeredOperations.Clear ()

        // end sprite batch frame
        OpenGL.SpriteBatch.EndSpriteBatchFrame renderer.SpriteBatchEnv
        OpenGL.Hl.Assert ()

        // reload render assets upon request
        if renderer.ReloadAssetsRequested then
            GlRenderer2d.handleReloadRenderAssets renderer
            OpenGL.Hl.Assert ()
            renderer.ReloadAssetsRequested <- false

    /// Make a GlRenderer2d.
    static member make window =

        // create one-off sprite and text resources
        let spriteShader = OpenGL.Sprite.CreateSpriteShader Constants.Paths.SpriteShaderFilePath
        let spriteQuad = OpenGL.Sprite.CreateSpriteQuad false
        let textQuad = OpenGL.Sprite.CreateSpriteQuad true
        OpenGL.Hl.Assert ()

        // create sprite batch env
        let spriteBatchEnv = OpenGL.SpriteBatch.CreateSpriteBatchEnv Constants.Paths.SpriteBatchShaderFilePath
        OpenGL.Hl.Assert ()

        // make renderer
        let renderer =
            { Window = window
              SpriteShader = spriteShader
              SpriteQuad = spriteQuad
              TextQuad = textQuad
              SpriteBatchEnv = spriteBatchEnv
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              ReloadAssetsRequested = false
              LayeredOperations = List () }

        // fin
        renderer

    interface Renderer2d with

        member renderer.Render eyeCenter eyeSize _ renderMessages =
            if renderMessages.Count > 0 then
                GlRenderer2d.render eyeCenter eyeSize renderMessages renderer

        member renderer.CleanUp () =
            OpenGL.SpriteBatch.DestroySpriteBatchEnv renderer.SpriteBatchEnv
            OpenGL.Hl.Assert ()
            let renderPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, renderAsset) in renderAssets do GlRenderer2d.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()