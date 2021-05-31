// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Linq
open System.Numerics
open System.IO
open SDL2
open TiledSharp
open Prime
open Nu

/// An image. Currently just used as a phantom type.
type Image = private { __ : unit }

/// A font. Currently just used as a phantom type.
type Font = private { __ : unit }

/// A tile map. Currently just used as a phantom type.
type TileMap = private { __ : unit }

/// The flipness of a sprite.
type [<StructuralEquality; NoComparison; Struct>] Flip =
    | FlipNone
    | FlipH
    | FlipV
    | FlipHV

    /// Convert to a flip value recognized by SDL.
    static member toSdlFlip flip =
        match flip with
        | FlipHV -> SDL.GPU_FlipEnum.GPU_FLIP_HORIZONTAL ||| SDL.GPU_FlipEnum.GPU_FLIP_VERTICAL
        | FlipH -> SDL.GPU_FlipEnum.GPU_FLIP_HORIZONTAL
        | FlipV -> SDL.GPU_FlipEnum.GPU_FLIP_VERTICAL
        | FlipNone -> SDL.GPU_FlipEnum.GPU_FLIP_NONE

/// The blend more of a sprite.
type [<StructuralEquality; NoComparison; Struct>] Blend =
    | Transparent
    | Additive
    | Modulate
    | Overwrite

    /// Convert to a blend mode value recognized by SDL.
    static member toSdlBlendMode flip =
        match flip with
        | Transparent -> SDL.GPU_BlendPresetEnum.GPU_BLEND_NORMAL
        | Additive -> SDL.GPU_BlendPresetEnum.GPU_BLEND_ADD
        | Modulate -> SDL.GPU_BlendPresetEnum.GPU_BLEND_MOD_ALPHA
        | Overwrite -> SDL.GPU_BlendPresetEnum.GPU_BLEND_SET

/// Horizontal justification.
type [<StructuralEquality; NoComparison; Struct>] JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

/// Vertical justification.
type [<StructuralEquality; NoComparison; Struct>] JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

/// Justification (such as for text alignement).
type [<StructuralEquality; NoComparison>] Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

/// A mutable sprite value.
type [<NoEquality; NoComparison; Struct>] Sprite =
    { mutable Transform : Transform
      mutable Absolute : bool
      mutable Offset : Vector2
      mutable Inset : Vector4 // OPTIMIZATION: elides optionality to avoid pointer indirection; v4Zero is full texture.
      mutable Image : Image AssetTag
      mutable Color : Color
      mutable Blend : Blend
      mutable Glow : Color
      mutable Flip : Flip }

/// A mutable particle value.
type [<NoEquality; NoComparison; Struct>] Particle =
    { mutable Transform : Transform
      mutable Absolute : bool
      mutable Offset : Vector2
      mutable Inset : Vector4 // OPTIMIZATION: elides optionality to avoid pointer indirection; v4Zero is full texture.
      mutable Color : Color
      mutable Glow : Color
      mutable Flip : Flip }

/// Describes how to render a sprite to the rendering system.
type [<NoEquality; NoComparison>] SpriteDescriptor =
    { Transform : Transform
      Absolute : bool
      Offset : Vector2
      InsetOpt : Vector4 option
      Image : Image AssetTag
      Color : Color
      Blend : Blend
      Glow : Color
      Flip : Flip }

/// Describes how to render multiple sprites to the rendering system.
type [<NoEquality; NoComparison>] SpritesDescriptor =
    { Sprites : Sprite array }

/// Describes how to render a tile map layer to the rendering system.
type [<NoEquality; NoComparison>] TileLayerDescriptor =
    { Transform : Transform
      Absolute : bool
      Color : Color
      Glow : Color
      MapSize : Vector2i
      Tiles : TmxLayerTile array
      TileSourceSize : Vector2i
      TileSize : Vector2
      TileAssets : (TmxTileset * Image AssetTag) array }

/// Describes how to render text to the rendering system.
type [<NoEquality; NoComparison>] TextDescriptor =
    { Transform : Transform
      Absolute : bool
      Text : string
      Font : Font AssetTag
      Color : Color
      Justification : Justification }

/// Describes particles.
type [<NoEquality; NoComparison>] ParticlesDescriptor =
    { Elevation : single
      PositionY : single
      Absolute : bool
      Blend : Blend
      Image : Image AssetTag
      Particles : Particle array }

/// Describes how to render something to the rendering system.
type [<NoEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor
    | SpritesDescriptor of SpritesDescriptor
    | TileLayerDescriptor of TileLayerDescriptor
    | TextDescriptor of TextDescriptor
    | ParticlesDescriptor of ParticlesDescriptor
    | RenderCallback of (Matrix3x3 * Matrix3x3 * Vector2 * Vector2 * Renderer -> unit)

/// A layered message to the rendering system.
and [<NoEquality; NoComparison>] RenderLayeredMessage =
    { Elevation : single
      PositionY : single
      AssetTag : obj AssetTag
      RenderDescriptor : RenderDescriptor }

/// A message to the rendering system.
and [<NoEquality; NoComparison>] RenderMessage =
    | RenderLayeredMessage of RenderLayeredMessage
    | HintRenderPackageUseMessage of string
    | HintRenderPackageDisuseMessage of string
    | ReloadRenderAssetsMessage
    //| ScreenFlashMessage of ...
    //| ScreenShakeMessage of ...

/// An asset that is used for rendering.
and [<NoEquality; NoComparison>] RenderAsset =
    | TextureAsset of nativeint
    | FontAsset of nativeint * int

/// The renderer. Represents the rendering system in Nu generally.
and Renderer =
    /// Pop all of the render messages that have been enqueued.
    abstract PopMessages : unit -> RenderMessage List
    /// Clear all of the render messages that have been enqueued.
    abstract ClearMessages : unit -> unit
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : RenderMessage -> unit
    /// Enqueue a layered message for rendering, bypassing EnqueueMessage for speed.
    abstract EnqueueLayeredMessage : RenderLayeredMessage -> unit
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> RenderMessage List -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> Renderer

type RenderLayeredMessageComparer () =
    interface IComparer<RenderLayeredMessage> with
        member this.Compare (left, right) =
            let elevationCompare = left.Elevation.CompareTo right.Elevation
            if elevationCompare <> 0 then elevationCompare else
            let positionYCompare = -(left.PositionY.CompareTo right.PositionY)
            if positionYCompare <> 0 then positionYCompare else
            let assetNameCompare = strCmp left.AssetTag.AssetName right.AssetTag.AssetName
            if assetNameCompare <> 0 then assetNameCompare else
            strCmp left.AssetTag.PackageName right.AssetTag.PackageName

/// The mock implementation of Renderer.
type [<ReferenceEquality; NoComparison>] MockRenderer =
    private
        { MockRenderer : unit }

    interface Renderer with
        member renderer.PopMessages () = List ()
        member renderer.ClearMessages () = ()
        member renderer.EnqueueMessage _ = ()
        member renderer.EnqueueLayeredMessage _ = ()
        member renderer.Render _ _ _ = ()
        member renderer.CleanUp () = renderer :> Renderer

    static member make () =
        { MockRenderer = () }

/// The SDL implementation of Renderer.
type [<ReferenceEquality; NoComparison>] SdlRenderer =
    private
        { RenderContext : nativeint
          RenderPackages : RenderAsset Packages
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          mutable RenderMessages : RenderMessage List
          RenderLayeredMessages : RenderLayeredMessage List }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset renderAsset renderer =
        SdlRenderer.invalidateCaches renderer
        match renderAsset with
        | TextureAsset texture -> SDL_gpu.GPU_FreeImage texture
        | FontAsset (font, _) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset (asset : obj Asset) renderContext renderer =
        SdlRenderer.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp"
        | ".png" ->
            let textureOpt = SDL_image.IMG_LoadTexture (renderContext, asset.FilePath)
            if textureOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, TextureAsset textureOpt)
            else
                let errorMsg = SDL.SDL_GetError ()
                Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
        | ".ttf" ->
            let fileFirstName = Path.GetFileNameWithoutExtension asset.FilePath
            let fileFirstNameLength = String.length fileFirstName
            if fileFirstNameLength >= 3 then
                let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                match Int32.TryParse fontSizeText with
                | (true, fontSize) ->
                    let fontOpt = SDL_ttf.TTF_OpenFont (asset.FilePath, fontSize)
                    if fontOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, FontAsset (fontOpt, fontSize))
                    else Log.debug ("Could not load font due to unparsable font size in file name '" + asset.FilePath + "'."); None
                | (false, _) -> Log.debug ("Could not load font due to file name being too short: '" + asset.FilePath + "'."); None
            else Log.debug ("Could not load font '" + asset.FilePath + "'."); None
        | extension -> Log.debug ("Could not load render asset '" + scstring asset + "' due to unknown extension '" + extension + "'."); None

    static member private tryLoadRenderPackage packageName renderer =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Render) packageName assetGraph with
            | Right assets ->
                let renderAssetOpts = List.map (fun asset -> SdlRenderer.tryLoadRenderAsset asset renderer.RenderContext renderer) assets
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
                SdlRenderer.tryLoadRenderPackage assetTag.PackageName renderer
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
        SdlRenderer.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackageDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some assets ->
            for asset in assets do SdlRenderer.freeRenderAsset asset.Value renderer
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            SdlRenderer.tryLoadRenderPackage packageName renderer

    static member private handleRenderMessage renderMessage renderer =
        match renderMessage with
        | RenderLayeredMessage message -> renderer.RenderLayeredMessages.Add message
        | HintRenderPackageUseMessage hintPackageUse -> SdlRenderer.handleHintRenderPackageUse hintPackageUse renderer
        | HintRenderPackageDisuseMessage hintPackageDisuse -> SdlRenderer.handleHintRenderPackageDisuse hintPackageDisuse renderer
        | ReloadRenderAssetsMessage -> SdlRenderer.handleReloadRenderAssets renderer

    static member private handleRenderMessages renderMessages renderer =
        for renderMessage in renderMessages do
            SdlRenderer.handleRenderMessage renderMessage renderer

    /// Render sprite.
    static member renderSprite
        (viewAbsolute : Matrix3x3 inref,
         viewRelative : Matrix3x3 inref,
         _ : Vector2,
         eyeSize : Vector2,
         transform : Transform inref,
         absolute : bool,
         offset : Vector2,
         inset : Vector4 inref,
         image : Image AssetTag,
         color : Color inref,
         blend : Blend,
         glow : Color inref,
         flip : Flip,
         renderer) =
        let view = if absolute then &viewAbsolute else &viewRelative
        let viewScale = Matrix3x3.ExtractScaleMatrix &view
        let position = transform.Position - Vector2.Multiply (offset, transform.Size)
        let positionView = Matrix3x3.Multiply (&position, &view)
        let sizeView = Matrix3x3.Multiply (&transform.Size, &viewScale)
        let image = AssetTag.generalize image
        let blend = Blend.toSdlBlendMode blend
        let flip = Flip.toSdlFlip flip
        match SdlRenderer.tryFindRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                let mutable (textureSizeX, textureSizeY) = (0, 0)
                SDL_gpu.GPU_QueryImageSize (texture, &textureSizeX, &textureSizeY)
                let mutable sourceRect = Unchecked.defaultof<SDL.GPU_Rect>
                if inset.X = 0.0f && inset.Y = 0.0f && inset.Z = 0.0f && inset.W = 0.0f then
                    sourceRect.x <- 0.0f
                    sourceRect.y <- 0.0f
                    sourceRect.w <- single textureSizeX
                    sourceRect.h <- single textureSizeY
                else
                    sourceRect.x <- inset.X
                    sourceRect.y <- inset.Y
                    sourceRect.w <- inset.Z
                    sourceRect.h <- inset.W
                let mutable destRect = Unchecked.defaultof<SDL.GPU_Rect>
                destRect.x <- (+positionView.X + eyeSize.X * 0.5f) * single Constants.Render.VirtualScalar
                destRect.y <- (-positionView.Y + eyeSize.Y * 0.5f) * single Constants.Render.VirtualScalar - (sizeView.Y * single Constants.Render.VirtualScalar) // negation for right-handedness
                destRect.w <- sizeView.X * single Constants.Render.VirtualScalar
                destRect.h <- sizeView.Y * single Constants.Render.VirtualScalar
                let rotation = -transform.Rotation * single Constants.Math.RadiansToDegrees // negation for right-handedness
                let rotationCenterX = sizeView.X * 0.5f * single Constants.Render.VirtualScalar
                let rotationCenterY = sizeView.Y * 0.5f * single Constants.Render.VirtualScalar
                if color.A <> byte 0 then
                    SDL_gpu.GPU_SetBlendMode (texture, blend)
                    SDL_gpu.GPU_SetRGBA (texture, uint color.R, uint color.G, uint color.B, uint color.A)
                    SDL_gpu.GPU_BlitRectX (texture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, flip)
                if glow.A <> byte 0 then
                    SDL_gpu.GPU_SetBlendMode (texture, SDL.GPU_BlendPresetEnum.GPU_BLEND_ADD)
                    SDL_gpu.GPU_SetRGBA (texture, uint glow.R, uint glow.G, uint glow.B, uint glow.A)
                    SDL_gpu.GPU_BlitRectX (texture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, flip)
            | _ -> Log.trace "Cannot render sprite with a non-texture asset."
        | _ -> Log.info ("SpriteDescriptor failed to render due to unloadable assets for '" + scstring image + "'.")

    /// Render tile layer.
    static member renderTileLayer
        (viewAbsolute : Matrix3x3 inref,
         viewRelative : Matrix3x3 inref,
         _ : Vector2,
         eyeSize : Vector2,
         transform : Transform inref,
         absolute : bool,
         color : Color inref,
         glow : Color inref,
         mapSize : Vector2i,
         tiles : TmxLayerTile array,
         tileSourceSize : Vector2i,
         tileSize : Vector2,
         tileAssets : (TmxTileset * Image AssetTag) array,
         renderer) =
        let view = if absolute then &viewAbsolute else &viewRelative
        let viewScale = Matrix3x3.ExtractScaleMatrix &view
        let positionView = Matrix3x3.Multiply (&transform.Position, &view)
        let sizeView = Matrix3x3.Multiply (&transform.Size, &viewScale)
        let tileRotation = transform.Rotation
        let (allFound, tileSetTextures) =
            tileAssets |>
            Array.map (fun (tileSet, tileSetImage) ->
                match SdlRenderer.tryFindRenderAsset (AssetTag.generalize tileSetImage) renderer with
                | ValueSome (TextureAsset tileSetTexture) -> Some (tileSet, tileSetImage, tileSetTexture)
                | ValueSome _ -> None
                | ValueNone -> None) |>
            Array.definitizePlus
        if allFound then
            // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
            let tilesLength = Array.length tiles
            let mutable tileIndex = 0
            while tileIndex < tilesLength do
                let tile = &tiles.[tileIndex] // iteration would be faster if TmxLayerTile were a struct...
                if tile.Gid <> 0 then // not the empty tile
                    let mapRun = mapSize.X
                    let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
                    let tilePosition =
                        v2
                            (positionView.X + tileSize.X * single i + eyeSize.X * 0.5f)
                            (-(positionView.Y - tileSize.Y * single j + sizeView.Y) + eyeSize.Y * 0.5f) // negation for right-handedness
                    let tileBounds = v4Bounds tilePosition tileSize
                    let viewBounds = v4Bounds Vector2.Zero eyeSize
                    if Math.isBoundsIntersectingBounds tileBounds viewBounds then
                        let tileFlip =
                            match (tile.HorizontalFlip, tile.VerticalFlip) with
                            | (false, false) -> SDL.GPU_FlipEnum.GPU_FLIP_NONE
                            | (true, false) -> SDL.GPU_FlipEnum.GPU_FLIP_HORIZONTAL
                            | (false, true) -> SDL.GPU_FlipEnum.GPU_FLIP_VERTICAL
                            | (true, true) -> SDL.GPU_FlipEnum.GPU_FLIP_HORIZONTAL ||| SDL.GPU_FlipEnum.GPU_FLIP_VERTICAL
                        let mutable tileOffset = 1 // gid 0 is the empty tile
                        let mutable tileSetIndex = 0
                        let mutable tileSetWidth = 0
                        let mutable tileSetTexture = nativeint 0
                        for (set, _, texture) in tileSetTextures do
                            let tileCountOpt = set.TileCount
                            let tileCount = if tileCountOpt.HasValue then tileCountOpt.Value else 0
                            if  tile.Gid >= set.FirstGid && tile.Gid < set.FirstGid + tileCount ||
                                not tileCountOpt.HasValue then // HACK: when tile count is missing, assume we've found the tile...?
                                tileSetWidth <- let tileSetWidthOpt = set.Image.Width in tileSetWidthOpt.Value
                                tileSetTexture <- texture
                            if  tileSetTexture = nativeint 0 then
                                tileSetIndex <- inc tileSetIndex
                                tileOffset <- tileOffset + tileCount
                        let tileId = tile.Gid - tileOffset
                        let tileIdPosition = tileId * tileSourceSize.X
                        let tileSourcePosition =
                            v2
                                (single (tileIdPosition % tileSetWidth))
                                (single (tileIdPosition / tileSetWidth * tileSourceSize.Y))
                        let mutable sourceRect = Unchecked.defaultof<SDL.GPU_Rect>
                        sourceRect.x <- tileSourcePosition.X
                        sourceRect.y <- tileSourcePosition.Y
                        sourceRect.w <- single tileSourceSize.X
                        sourceRect.h <- single tileSourceSize.Y
                        let mutable destRect = Unchecked.defaultof<SDL.GPU_Rect>
                        destRect.x <- tilePosition.X * single Constants.Render.VirtualScalar
                        destRect.y <- tilePosition.Y * single Constants.Render.VirtualScalar
                        destRect.w <- tileSize.X * single Constants.Render.VirtualScalar
                        destRect.h <- tileSize.Y * single Constants.Render.VirtualScalar
                        let rotation = -tileRotation * single Constants.Math.RadiansToDegrees // negation for right-handedness
                        let rotationCenterX = tileSize.X * 0.5f * single Constants.Render.VirtualScalar
                        let rotationCenterY = tileSize.Y * 0.5f * single Constants.Render.VirtualScalar
                        if color.A <> byte 0 then
                            SDL_gpu.GPU_SetBlendMode (tileSetTexture, SDL.GPU_BlendPresetEnum.GPU_BLEND_NORMAL)
                            SDL_gpu.GPU_SetRGBA (tileSetTexture, uint color.R, uint color.G, uint color.B, uint color.A)
                            SDL_gpu.GPU_BlitRectX (tileSetTexture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, tileFlip)
                        if glow.A <> byte 0 then
                            SDL_gpu.GPU_SetBlendMode (tileSetTexture, SDL.GPU_BlendPresetEnum.GPU_BLEND_ADD)
                            SDL_gpu.GPU_SetRGBA (tileSetTexture, uint glow.R, uint glow.G, uint glow.B, uint glow.A)
                            SDL_gpu.GPU_BlitRectX (tileSetTexture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, tileFlip)
                tileIndex <- inc tileIndex
        else Log.info ("TileLayerDescriptor failed due to unloadable or non-texture assets for one or more of '" + scstring tileAssets + "'.")

    /// Render text.
    static member renderText
        (viewAbsolute : Matrix3x3 inref,
         viewRelative : Matrix3x3 inref,
         _ : Vector2,
         eyeSize : Vector2,
         transform : Transform inref,
         absolute : bool,
         text : string,
         font : Font AssetTag,
         color : Color inref,
         justification : Justification,
         renderer) =
        let view = if absolute then &viewAbsolute else &viewRelative
        let viewScale = Matrix3x3.ExtractScaleMatrix &view
        let positionView = Matrix3x3.Multiply (&transform.Position, &view)
        let sizeView = Matrix3x3.Multiply (&transform.Size, &viewScale)
        let font = AssetTag.generalize font
        match SdlRenderer.tryFindRenderAsset font renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | FontAsset (font, _) ->
                let mutable renderColor = SDL.SDL_Color ()
                renderColor.r <- color.R
                renderColor.g <- color.G
                renderColor.b <- color.B
                renderColor.a <- color.A
                // NOTE: the resource implications (perf and vram fragmentation?) of creating and destroying a
                // texture one or more times a frame must be understood! Although, maybe it all happens in software
                // and vram fragmentation would not be a concern in the first place... perf could still be, however.
                //
                // TODO: P1: it seems like the API intends for us to cache these surfaces across frames. We should
                // probably accomodate that for performance reasons.
                let (offset, textSurface) =
                    match justification with
                    | Unjustified wrapped ->
                        let textSurface =
                            if wrapped
                            then SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 sizeView.X)
                            else SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
                        (Vector2.Zero, textSurface)
                    | Justified (h, v) ->
                        let textSurface = SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
                        let mutable width = 0
                        let mutable height = 0
                        SDL_ttf.TTF_SizeText (font, text, &width, &height) |> ignore
                        let offsetX =
                            match h with
                            | JustifyLeft -> 0.0f
                            | JustifyCenter -> (sizeView.X - single width) * 0.5f
                            | JustifyRight -> sizeView.X - single width
                        let offsetY =
                            match v with
                            | JustifyTop -> 0.0f
                            | JustifyMiddle -> (sizeView.Y - single height) * 0.5f
                            | JustifyBottom -> sizeView.Y - single height
                        (v2 offsetX offsetY, textSurface)
                if textSurface <> IntPtr.Zero then
                    let textTexture = SDL_gpu.GPU_CopyImageFromSurface textSurface
                    let mutable (textureSizeX, textureSizeY) = (0, 0)
                    SDL_gpu.GPU_QueryImageSize (textTexture, &textureSizeX, &textureSizeY)
                    let mutable sourceRect = Unchecked.defaultof<SDL.GPU_Rect>
                    sourceRect.x <- 0.0f
                    sourceRect.y <- 0.0f
                    sourceRect.w <- single textureSizeX
                    sourceRect.h <- single textureSizeY
                    let mutable destRect = Unchecked.defaultof<SDL.GPU_Rect>
                    destRect.x <- (+positionView.X + offset.X + eyeSize.X * 0.5f) * single Constants.Render.VirtualScalar
                    destRect.y <- (-positionView.Y + offset.Y + eyeSize.Y * 0.5f) * single Constants.Render.VirtualScalar - (sizeView.Y * single Constants.Render.VirtualScalar) // negation for right-handedness
                    destRect.w <- single textureSizeX * single Constants.Render.VirtualScalar
                    destRect.h <- single textureSizeY * single Constants.Render.VirtualScalar
                    if textTexture <> IntPtr.Zero then SDL_gpu.GPU_BlitRect (textTexture, &sourceRect, renderer.RenderContext, &destRect)
                    SDL_gpu.GPU_FreeImage textTexture
                    SDL.SDL_FreeSurface textSurface
            | _ -> Log.debug "Cannot render text with a non-font asset."
        | _ -> Log.info ("TextDescriptor failed due to unloadable assets for '" + scstring font + "'.")

    /// Render particles.
    static member renderParticles
        (viewAbsolute : Matrix3x3 inref,
         viewRelative : Matrix3x3 inref,
         _ : Vector2,
         eyeSize : Vector2,
         _ : single,
         _ : single,
         absolute : bool,
         blend : Blend,
         image : Image AssetTag,
         particles : Particle array,
         renderer) =
        let view = if absolute then &viewAbsolute else &viewRelative
        let positionOffset = -(v2Zero * view)
        let blend = Blend.toSdlBlendMode blend
        let image = AssetTag.generalize image
        match SdlRenderer.tryFindRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                let mutable (textureSizeX, textureSizeY) = (0, 0)
                SDL_gpu.GPU_QueryImageSize (texture, &textureSizeX, &textureSizeY)
                let mutable sourceRect = Unchecked.defaultof<SDL.GPU_Rect>
                let mutable destRect = Unchecked.defaultof<SDL.GPU_Rect>
                let mutable index = 0
                while index < particles.Length do
                    let particle = &particles.[index]
                    let transform = &particle.Transform
                    let position = transform.Position - Vector2.Multiply (particle.Offset, transform.Size)
                    let positionView = position + positionOffset
                    let sizeView = transform.Size * Matrix3x3.CreateScale (Vector3 (view.Row0.X, view.Row1.Y, view.Row2.Z))
                    let color = &particle.Color
                    let glow = &particle.Glow
                    let flip = Flip.toSdlFlip particle.Flip
                    let inset = particle.Inset
                    if inset.X = 0.0f && inset.Y = 0.0f && inset.Z = 0.0f && inset.W = 0.0f then
                        sourceRect.x <- 0.0f
                        sourceRect.y <- 0.0f
                        sourceRect.w <- single textureSizeX
                        sourceRect.h <- single textureSizeY
                    else
                        sourceRect.x <- inset.X
                        sourceRect.y <- inset.Y
                        sourceRect.w <- inset.Z
                        sourceRect.h <- inset.W
                    destRect.x <- (+positionView.X + eyeSize.X * 0.5f) * single Constants.Render.VirtualScalar
                    destRect.y <- (-positionView.Y + eyeSize.Y * 0.5f) * single Constants.Render.VirtualScalar - (sizeView.Y * single Constants.Render.VirtualScalar) // negation for right-handedness
                    destRect.w <- sizeView.X * single Constants.Render.VirtualScalar
                    destRect.h <- sizeView.Y * single Constants.Render.VirtualScalar
                    let rotation = -transform.Rotation * single Constants.Math.RadiansToDegrees // negation for right-handedness
                    let rotationCenterX = sizeView.X * 0.5f * single Constants.Render.VirtualScalar
                    let rotationCenterY = sizeView.Y * 0.5f * single Constants.Render.VirtualScalar
                    if color.A <> byte 0 then
                        SDL_gpu.GPU_SetBlendMode (texture, blend)
                        SDL_gpu.GPU_SetRGBA (texture, uint color.R, uint color.G, uint color.B, uint color.A)
                        SDL_gpu.GPU_BlitRectX (texture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, flip)
                    if glow.A <> byte 0 then
                        SDL_gpu.GPU_SetBlendMode (texture, SDL.GPU_BlendPresetEnum.GPU_BLEND_ADD)
                        SDL_gpu.GPU_SetRGBA (texture, uint glow.R, uint glow.G, uint glow.B, uint glow.A)
                        SDL_gpu.GPU_BlitRectX (texture, &sourceRect, renderer.RenderContext, &destRect, rotation, rotationCenterX, rotationCenterY, flip)
                    index <- inc index
            | _ -> Log.trace "Cannot render particle with a non-texture asset."
        | _ -> Log.info ("RenderDescriptors failed to render due to unloadable assets for '" + scstring image + "'.")

    static member private renderDescriptor
        (viewAbsolute : Matrix3x3 inref,
         viewRelative : Matrix3x3 inref,
         eyeCenter : Vector2,
         eyeSize : Vector2,
         descriptor,
         renderer) =
        match descriptor with
        | SpriteDescriptor descriptor ->
            let inset = match descriptor.InsetOpt with Some inset -> inset | None -> v4Zero
            SdlRenderer.renderSprite
                (&viewAbsolute, &viewRelative, eyeCenter, eyeSize,
                 &descriptor.Transform, descriptor.Absolute, descriptor.Offset, &inset, descriptor.Image, &descriptor.Color, descriptor.Blend, &descriptor.Glow, descriptor.Flip,
                 renderer)
        | SpritesDescriptor descriptor ->
            let sprites = descriptor.Sprites
            for index in 0 .. sprites.Length - 1 do
                let sprite = &sprites.[index]
                SdlRenderer.renderSprite
                    (&viewAbsolute, &viewRelative, eyeCenter, eyeSize,
                     &sprite.Transform, sprite.Absolute, sprite.Offset, &sprite.Inset, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Glow, sprite.Flip,
                     renderer)
        | TileLayerDescriptor descriptor ->
            SdlRenderer.renderTileLayer
                (&viewAbsolute, &viewRelative, eyeCenter, eyeSize,
                 &descriptor.Transform, descriptor.Absolute, &descriptor.Color, &descriptor.Glow, descriptor.MapSize, descriptor.Tiles, descriptor.TileSourceSize, descriptor.TileSize, descriptor.TileAssets,
                 renderer)
        | TextDescriptor descriptor ->
            SdlRenderer.renderText
                (&viewAbsolute, &viewRelative, eyeCenter, eyeSize,
                 &descriptor.Transform, descriptor.Absolute, descriptor.Text, descriptor.Font, &descriptor.Color, descriptor.Justification,
                 renderer)
        | ParticlesDescriptor descriptor ->
            SdlRenderer.renderParticles
                (&viewAbsolute, &viewRelative, eyeCenter, eyeSize,
                 descriptor.Elevation, descriptor.PositionY, descriptor.Absolute, descriptor.Blend, descriptor.Image, descriptor.Particles,
                 renderer)
        | RenderCallback callback ->
            callback (viewAbsolute, viewRelative, eyeCenter, eyeSize, renderer)

    static member private renderLayeredMessages eyeCenter eyeSize (messages : RenderLayeredMessage List) renderer =
        let renderContext = renderer.RenderContext
        SDL_gpu.GPU_SetBlendMode (renderContext, SDL.GPU_BlendPresetEnum.GPU_BLEND_NORMAL)
        let viewAbsolute = (Math.getViewAbsoluteI eyeCenter eyeSize).InvertedView ()
        let viewRelative = (Math.getViewRelativeI eyeCenter eyeSize).InvertedView ()
#if MULTITHREAD_RENDER_SORT
        let messages = messages.AsParallel().OrderBy(id, RenderLayeredMessageComparer ())
#else
        messages.Sort (RenderLayeredMessageComparer ())
#endif
        for message in messages do SdlRenderer.renderDescriptor (&viewAbsolute, &viewRelative, eyeCenter, eyeSize, message.RenderDescriptor, renderer)
        SDL_gpu.GPU_FlushBlitBuffer ()

    /// Get the render context.
    static member getRenderContext renderer =
        renderer.RenderContext

    /// Make a Renderer.
    static member make renderContext =
        let renderer =
            { RenderContext = renderContext
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderMessages = List ()
              RenderLayeredMessages = List () }
        renderer

    interface Renderer with

        member renderer.PopMessages () =
            let messages = renderer.RenderMessages
            renderer.RenderMessages <- List ()
            messages

        member renderer.ClearMessages () =
            renderer.RenderMessages <- List ()
            renderer.RenderLayeredMessages.Clear ()

        member renderer.EnqueueMessage renderMessage =
            renderer.RenderMessages.Add renderMessage

        member renderer.EnqueueLayeredMessage layeredMessage =
            renderer.RenderLayeredMessages.Add layeredMessage

        member renderer.Render eyeCenter eyeSize renderMessages =
            SdlRenderer.handleRenderMessages renderMessages renderer
            SdlRenderer.renderLayeredMessages eyeCenter eyeSize renderer.RenderLayeredMessages renderer
            renderer.RenderLayeredMessages.Clear ()

        member renderer.CleanUp () =
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderAssetPackages |> Seq.collect (Seq.map (fun entry -> entry.Value))
            for renderAsset in renderAssets do SdlRenderer.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()
            renderer :> Renderer

[<RequireQualifiedAccess>]
module Renderer =

    /// Clear all of the render messages that have been enqueued.
    let clearMessages (renderer : Renderer) =
        renderer.ClearMessages ()

    /// Enqueue a message from an external source.
    let enqueueMessage message (renderer : Renderer) =
        renderer.EnqueueMessage message

    /// Enqueue a layered message from an external source, bypassing enqueueMessage for speed.
    let enqueueLayeredMessage message (renderer : Renderer) =
        renderer.EnqueueLayeredMessage message

    /// Render a frame of the game.
    let render eyeCenter eyeSize renderMessages (renderer : Renderer) =
        renderer.Render eyeCenter eyeSize renderMessages

    /// Handle render clean up by freeing all loaded render assets.
    let cleanUp (renderer : Renderer) =
        renderer.CleanUp ()