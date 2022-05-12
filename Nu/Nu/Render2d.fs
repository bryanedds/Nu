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
open TiledSharp
open Prime
open Nu

/// Describes what to render.
/// TODO: see if we can make RenderCallback receive args by reference or something.
type [<NoEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor
    | SpritesDescriptor of SpritesDescriptor
    | TilesDescriptor of TilesDescriptor
    | TextDescriptor of TextDescriptor
    | ParticlesDescriptor of ParticlesDescriptor
    | RenderCallbackDescriptor2d of (bool * Vector2 * Vector2 * Vector2 * Renderer2d -> unit)

/// A layered message to the 2d rendering system.
and [<NoEquality; NoComparison>] RenderLayeredMessage2d =
    { Elevation : single
      Horizon : single
      AssetTag : obj AssetTag
      RenderDescriptor : RenderDescriptor }

/// Describes a 2d render pass.
and [<CustomEquality; CustomComparison>] RenderPassDescriptor2d =
    { RenderPassOrder : int64
      RenderPass2d : Matrix3x3 * Renderer2d -> unit } // TODO: check if an M33 accounts for camera zoom.
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? RenderPassDescriptor2d as that -> this.RenderPassOrder.CompareTo that.RenderPassOrder
            | _ -> -1
    override this.Equals (that : obj) =
        match that with
        | :? RenderPassDescriptor2d as that -> this.RenderPassOrder = that.RenderPassOrder
        | _ -> false
    override this.GetHashCode () = hash this.RenderPassOrder

/// A message to the 2d rendering system.
and [<NoEquality; NoComparison>] RenderMessage2d =
    | RenderLayeredMessage2d of RenderLayeredMessage2d
    | RenderUpdateMaterial2d of (Renderer -> unit)
    | RenderPrePassDescriptor2d of RenderPassDescriptor2d
    | RenderPostPassDescriptor2d of RenderPassDescriptor2d
    | HintRenderPackageUseMessage2d of string
    | HintRenderPackageDisuseMessage2d of string
    | ReloadRenderAssetsMessage2d

/// The 2d renderer. Represents the 2d rendering system in Nu generally.
and Renderer2d =
    inherit Renderer
    /// The sprite batch operational environment if it exists for this implementation.
    abstract SpriteBatchEnvOpt : OpenGL.SpriteBatch.Env option
    /// Pop all of the render messages that have been enqueued.
    abstract PopMessages : unit -> RenderMessage2d List
    /// Clear all of the render messages that have been enqueued.
    abstract ClearMessages : unit -> unit
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : RenderMessage2d -> unit
    /// Enqueue a layered message for rendering, bypassing EnqueueMessage for speed.
    abstract EnqueueLayeredMessage : RenderLayeredMessage2d -> unit
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> Vector2 -> RenderMessage2d List -> unit
    /// Swap a rendered frame of the game.
    abstract Swap : unit -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> Renderer2d

/// The mock implementation of Renderer.
type [<ReferenceEquality; NoComparison>] MockRenderer2d =
    private
        { MockRenderer2d : unit }

    interface Renderer2d with
        member renderer.SpriteBatchEnvOpt = None
        member renderer.PopMessages () = List ()
        member renderer.ClearMessages () = ()
        member renderer.EnqueueMessage _ = ()
        member renderer.EnqueueLayeredMessage _ = ()
        member renderer.Render _ _ _ _ = ()
        member renderer.Swap () = ()
        member renderer.CleanUp () = renderer :> Renderer2d

    static member make () =
        { MockRenderer2d = () }

/// Compares layered 2d render messages.
type RenderLayeredMessage2dComparer () =
    interface IComparer<RenderLayeredMessage2d> with
        member this.Compare (left, right) =
            if left.Elevation < right.Elevation then -1
            elif left.Elevation > right.Elevation then 1
            elif left.Horizon > right.Horizon then -1
            elif left.Horizon < right.Horizon then 1
            else
                let assetNameCompare = strCmp left.AssetTag.AssetName right.AssetTag.AssetName
                if assetNameCompare <> 0 then assetNameCompare
                else strCmp left.AssetTag.PackageName right.AssetTag.PackageName

/// The SDL implementation of Renderer.
type [<ReferenceEquality; NoComparison>] GlRenderer2d =
    private
        { RenderWindow : Window
          mutable RenderSpriteBatchEnv : OpenGL.SpriteBatch.Env
          RenderPackages : RenderAsset Packages
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          mutable RenderMessages : RenderMessage2d List
          RenderLayeredMessages : RenderLayeredMessage2d List }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset renderAsset renderer =
        GlRenderer2d.invalidateCaches renderer
        match renderAsset with
        | TextureAsset (_, texture) -> OpenGL.Hl.DeleteTexture texture
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset (asset : obj Asset) renderer =
        GlRenderer2d.invalidateCaches renderer
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
                let renderAssetOpts = List.map (fun asset -> GlRenderer2d.tryLoadRenderAsset asset renderer) assets
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
                GlRenderer2d.tryLoadRenderPackage assetTag.PackageName renderer
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
        GlRenderer2d.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackageDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some assets ->
            for asset in assets do GlRenderer2d.freeRenderAsset asset.Value renderer
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            GlRenderer2d.tryLoadRenderPackage packageName renderer

    static member private handleRenderMessage renderMessage renderer =
        match renderMessage with
        | RenderLayeredMessage2d message -> renderer.RenderLayeredMessages.Add message
        | HintRenderPackageUseMessage2d hintPackageUse -> GlRenderer2d.handleHintRenderPackageUse hintPackageUse renderer
        | HintRenderPackageDisuseMessage2d hintPackageDisuse -> GlRenderer2d.handleHintRenderPackageDisuse hintPackageDisuse renderer
        | ReloadRenderAssetsMessage2d -> GlRenderer2d.handleReloadRenderAssets renderer

    static member private handleRenderMessages renderMessages renderer =
        for renderMessage in renderMessages do
            GlRenderer2d.handleRenderMessage renderMessage renderer

    static member private sortRenderLayeredMessages renderer =
        renderer.RenderLayeredMessages.Sort (RenderLayeredMessage2dComparer ())

    static member inline private renderSpriteInline
        absolute position size pivot rotation (inset : Box2) textureMetadata texture (color : Color) blend (glow : Color) flip renderer =

        // compute coords
        // TODO: 3D: consider putting the texel multiplies in the shader.
        let coords =
            let texelWidth = textureMetadata.TextureTexelWidth
            let texelHeight = textureMetadata.TextureTexelHeight
            let texelBorderInset = Constants.Render.SpriteTexelEpsilon
            let texelBorderInsetTimes2 = Constants.Render.SpriteTexelEpsilonTimes2
            if not (inset.Equals box2Zero) then
                let px = single inset.Position.X * texelWidth + texelBorderInset
                let py = single (inset.Position.Y + inset.Size.Y) * texelHeight - texelBorderInset
                let sx = single inset.Size.X * texelWidth - texelBorderInsetTimes2
                let sy = single -inset.Size.Y * texelHeight + texelBorderInsetTimes2
                Box2 (px, py, sx, sy)
            else Box2 (0.0f, 1.0f, 1.0f, -1.0f)

        // compute blending instructions
        let (bfs, bfd, beq) =
            match blend with
            | Transparent -> (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha, OpenGL.BlendEquationMode.FuncAdd)
            | Additive -> (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.One, OpenGL.BlendEquationMode.FuncAdd)
            | Overwrite -> (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero, OpenGL.BlendEquationMode.FuncAdd)

        // attempt to draw normal sprite
        if color.A <> 0.0f then
            OpenGL.SpriteBatch.NextSprite
                (absolute, position, size, pivot, rotation, coords, color, flip, bfs, bfd, beq, texture, renderer.RenderSpriteBatchEnv)

        // attempt to draw glow sprite
        if glow.A <> 0.0f then
            OpenGL.SpriteBatch.NextSprite
                (absolute, position, size, pivot, rotation, coords, glow, flip, bfs, bfd, beq, texture, renderer.RenderSpriteBatchEnv)

    /// Render sprite.
    static member renderSprite
        (transform : Transform byref,
         inset : Box2 inref,
         image : Image AssetTag,
         color : Color inref,
         blend : Blend,
         glow : Color inref,
         flip : Flip,
         renderer) =
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let position = perimeter.Position.V2 * Constants.Render.VirtualScalar2
        let rotation = transform.Angles.X
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let pivot = transform.Offset.V2 * size
        let image = AssetTag.generalize image
        match GlRenderer2d.tryFindRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset (textureMetadata, texture) ->
                GlRenderer2d.renderSpriteInline absolute position size pivot rotation inset textureMetadata texture color blend glow flip renderer
            | _ -> Log.trace "Cannot render sprite with a non-texture asset."
        | _ -> Log.info ("SpriteDescriptor failed to render due to unloadable assets for '" + scstring image + "'.")

    /// Render tiles.
    static member renderTiles
        (transform : Transform byref,
         color : Color inref,
         glow : Color inref,
         mapSize : Vector2i,
         tiles : TmxLayerTile array,
         tileSourceSize : Vector2i,
         tileSize : Vector2,
         tileAssets : (TmxTileset * Image AssetTag) array,
         eyePosition : Vector2,
         eyeSize : Vector2,
         renderer) =
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let position = perimeter.Position.V2 * Constants.Render.VirtualScalar2
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let eyePosition = eyePosition * Constants.Render.VirtualScalar2
        let eyeSize = eyeSize * Constants.Render.VirtualScalar2
        let tileSize = tileSize * Constants.Render.VirtualScalar2
        let tilePivot = tileSize * 0.5f // just rotate around center
        let (allFound, tileSetTextures) =
            tileAssets |>
            Array.map (fun (tileSet, tileSetImage) ->
                match GlRenderer2d.tryFindRenderAsset (AssetTag.generalize tileSetImage) renderer with
                | ValueSome (TextureAsset (tileSetTexture, tileSetTextureMetadata)) -> Some (tileSet, tileSetImage, tileSetTexture, tileSetTextureMetadata)
                | ValueSome _ -> None
                | ValueNone -> None) |>
            Array.definitizePlus
        if allFound then
            // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
            let tilesLength = Array.length tiles
            let mutable tileIndex = 0
            while tileIndex < tilesLength do
                let tile = &tiles.[tileIndex]
                if tile.Gid <> 0 then // not the empty tile
                    let mapRun = mapSize.X
                    let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
                    let tilePosition =
                        v2
                            (position.X + tileSize.X * single i)
                            (position.Y - tileSize.Y - tileSize.Y * single j + size.Y)
                    let tileBounds = box2 tilePosition tileSize
                    let viewBounds = box2 (eyePosition - eyeSize * 0.5f) eyeSize
                    if Math.isBoundsIntersectingBounds2d tileBounds viewBounds then
        
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
                                tileSetWidth <- let tileSetWidthOpt = set.Image.Width in tileSetWidthOpt.Value
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
                            GlRenderer2d.renderSpriteInline absolute tilePosition tileSize tilePivot 0.0f inset textureMetadata texture color Transparent glow flip renderer
                        | None -> ()
        
                tileIndex <- inc tileIndex
        else Log.info ("TileLayerDescriptor failed due to unloadable or non-texture assets for one or more of '" + scstring tileAssets + "'.")

    /// Render text.
    static member renderText
        (viewAbsolute : Matrix3x3 byref,
         viewRelative : Matrix3x3 byref,
         _ : Vector2,
         eyeSize : Vector2,
         eyeMargin : Vector2,
         transform : Transform byref,
         text : string,
         font : Font AssetTag,
         color : Color inref,
         justification : Justification,
         renderer) =
        ()
        //let mutable color = color
        //let view = if transform.Absolute then &viewAbsolute else &viewRelative
        //let viewScale = Matrix3x3.ExtractScaleMatrix &view
        //let perimeter = transform.Perimeter
        //let position = perimeter.Position.V2
        //let positionView = Matrix3x3.Multiply (&position, &view)
        //let positionOffset = positionView + v2 eyeMargin.X -eyeMargin.Y
        //let size = perimeter.Size.V2
        //let sizeView = Matrix3x3.Multiply (&size, &viewScale)
        //let font = AssetTag.generalize font
        //match GlRenderer2d.tryFindRenderAsset font renderer with
        //| ValueSome renderAsset ->
        //    match renderAsset with
        //    | FontAsset (_, font) ->
        //        let mutable renderColor = SDL.SDL_Color ()
        //        renderColor.r <- color.R8
        //        renderColor.g <- color.G8
        //        renderColor.b <- color.B8
        //        renderColor.a <- color.A8
        //        // NOTE: the resource implications (perf and vram fragmentation?) of creating and destroying a
        //        // texture one or more times a frame must be understood! Although, maybe it all happens in software
        //        // and vram fragmentation would not be a concern in the first place... perf could still be, however.
        //        let (offset, textSurface) =
        //            match justification with
        //            | Unjustified wrapped ->
        //                let textSurface =
        //                    if wrapped
        //                    then SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 sizeView.X)
        //                    else SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
        //                (Vector2.Zero, textSurface)
        //            | Justified (h, v) ->
        //                let textSurface = SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
        //                let mutable width = 0
        //                let mutable height = 0
        //                SDL_ttf.TTF_SizeText (font, text, &width, &height) |> ignore
        //                let offsetX =
        //                    match h with
        //                    | JustifyLeft -> 0.0f
        //                    | JustifyCenter -> (sizeView.X - single width) * 0.5f
        //                    | JustifyRight -> sizeView.X - single width
        //                let offsetY =
        //                    match v with
        //                    | JustifyTop -> 0.0f
        //                    | JustifyMiddle -> (sizeView.Y - single height) * 0.5f
        //                    | JustifyBottom -> sizeView.Y - single height
        //                (v2 offsetX offsetY, textSurface)
        //        if textSurface <> IntPtr.Zero then
        //            let textTexture = SDL.SDL_CreateTextureFromSurface (renderer.RenderContext, textSurface)
        //            let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture textTexture
        //            let mutable sourceRect = SDL.SDL_Rect ()
        //            sourceRect.x <- 0
        //            sourceRect.y <- 0
        //            sourceRect.w <- textureSizeX
        //            sourceRect.h <- textureSizeY
        //            let mutable destRect = SDL.SDL_Rect ()
        //            destRect.x <- int (+positionOffset.X + offset.X + eyeSize.X * 0.5f) * Constants.Render.VirtualScalar
        //            destRect.y <- int (-positionOffset.Y + offset.Y + eyeSize.Y * 0.5f) * Constants.Render.VirtualScalar - (int sizeView.Y * Constants.Render.VirtualScalar) // negation for right-handedness
        //            destRect.w <- textureSizeX * Constants.Render.VirtualScalar
        //            destRect.h <- textureSizeY * Constants.Render.VirtualScalar
        //            if textTexture <> IntPtr.Zero then SDL.SDL_RenderCopy (renderer.RenderContext, textTexture, &sourceRect, &destRect) |> ignore
        //            SDL.SDL_DestroyTexture textTexture
        //            SDL.SDL_FreeSurface textSurface
        //    | _ -> Log.debug "Cannot render text with a non-font asset."
        //| _ -> Log.info ("TextDescriptor failed due to unloadable assets for '" + scstring font + "'.")

    /// Render particles.
    static member renderParticles
        (viewAbsolute : Matrix3x3 byref,
         viewRelative : Matrix3x3 byref,
         _ : Vector2,
         eyeSize : Vector2,
         eyeMargin : Vector2,
         _ : single,
         _ : single,
         absolute : bool,
         blend : Blend,
         image : Image AssetTag,
         particles : Particle array,
         renderer) =
        ()
        //let view = if absolute then &viewAbsolute else &viewRelative
        //let viewScale = Matrix3x3.ExtractScaleMatrix &view
        //let positionOffset = -(v2Zero * view) + v2 eyeMargin.X -eyeMargin.Y
        //let blend = Blend.toSdlBlendMode blend
        //let image = AssetTag.generalize image
        //match GlRenderer2d.tryFindRenderAsset image renderer with
        //| ValueSome renderAsset ->
        //    match renderAsset with
        //    | TextureAsset texture ->
        //        let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture texture
        //        let mutable sourceRect = SDL.SDL_Rect ()
        //        let mutable destRect = SDL.SDL_Rect ()
        //        let mutable index = 0
        //        while index < particles.Length do
        //            let particle = &particles.[index]
        //            let perimeter = particle.Transform.Perimeter
        //            let position = perimeter.Position.V2
        //            let positionView = position + positionOffset
        //            let size = perimeter.Size.V2
        //            let sizeView = Matrix3x3.Multiply (&size, &viewScale)
        //            let rotation = particle.Transform.Angles.X
        //            let color = &particle.Color
        //            let glow = &particle.Glow
        //            let flip = Flip.toSdlFlip particle.Flip
        //            let inset = particle.Inset
        //            if inset.IsEmpty then
        //                sourceRect.x <- 0
        //                sourceRect.y <- 0
        //                sourceRect.w <- textureSizeX
        //                sourceRect.h <- textureSizeY
        //            else
        //                sourceRect.x <- int inset.Position.X
        //                sourceRect.y <- int inset.Position.Y
        //                sourceRect.w <- int inset.Size.X
        //                sourceRect.h <- int inset.Size.Y
        //            destRect.x <- int (+positionView.X + eyeSize.X * 0.5f) * Constants.Render.VirtualScalar
        //            destRect.y <- int (-positionView.Y + eyeSize.Y * 0.5f) * Constants.Render.VirtualScalar - (int sizeView.Y * Constants.Render.VirtualScalar) // negation for right-handedness
        //            destRect.w <- int sizeView.X * Constants.Render.VirtualScalar
        //            destRect.h <- int sizeView.Y * Constants.Render.VirtualScalar
        //            let rotation = double (Math.radiansToDegrees rotation) // negation for right-handedness
        //            let mutable rotationCenter = SDL.SDL_Point ()
        //            rotationCenter.x <- int (sizeView.X * 0.5f) * Constants.Render.VirtualScalar
        //            rotationCenter.y <- int (sizeView.Y * 0.5f) * Constants.Render.VirtualScalar
        //            if color.A <> 0.0f then
        //                SDL.SDL_SetTextureBlendMode (texture, blend) |> ignore
        //                SDL.SDL_SetTextureColorMod (texture, color.R8, color.G8, color.B8) |> ignore
        //                SDL.SDL_SetTextureAlphaMod (texture, color.A8) |> ignore
        //                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, &sourceRect, &destRect, rotation, &rotationCenter, flip)
        //                if renderResult <> 0 then Log.info ("Render error - could not render texture for particle '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
        //            if glow.A <> 0.0f then
        //                SDL.SDL_SetTextureBlendMode (texture, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
        //                SDL.SDL_SetTextureColorMod (texture, glow.R8, glow.G8, glow.B8) |> ignore
        //                SDL.SDL_SetTextureAlphaMod (texture, glow.A8) |> ignore
        //                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, &sourceRect, &destRect, rotation, &rotationCenter, flip)
        //                if renderResult <> 0 then Log.info ("Render error - could not render texture for particle '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
        //            index <- inc index
        //    | _ -> Log.trace "Cannot render particle with a non-texture asset."
        //| _ -> Log.info ("RenderDescriptors failed to render due to unloadable assets for '" + scstring image + "'.")

    static member private renderDescriptor descriptor eyePosition eyeSize renderer =
        match descriptor with
        | SpriteDescriptor descriptor ->
            let inset = match descriptor.InsetOpt with Some inset -> inset | None -> box2Zero
            GlRenderer2d.renderSprite (&descriptor.Transform, &inset, descriptor.Image, &descriptor.Color, descriptor.Blend, &descriptor.Glow, descriptor.Flip, renderer)
        | SpritesDescriptor descriptor ->
            ()
            //let sprites = descriptor.Sprites
            //for index in 0 .. sprites.Length - 1 do
            //    let sprite = &sprites.[index]
            //    GlRenderer2d.renderSprite
            //        (&viewAbsolute, &viewRelative, eyePosition, eyeSize, eyeMargin,
            //         &sprite.Transform, &sprite.Inset, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Glow, sprite.Flip,
            //         renderer, spriteBatchEnvOpt)
        | TilesDescriptor descriptor ->
            GlRenderer2d.renderTiles
                (&descriptor.Transform, &descriptor.Color, &descriptor.Glow,
                 descriptor.MapSize, descriptor.Tiles, descriptor.TileSourceSize, descriptor.TileSize, descriptor.TileAssets,
                 eyePosition, eyeSize, renderer)
        | TextDescriptor descriptor ->
            ()
            //GlRenderer2d.renderText
            //    (&descriptor.Transform, descriptor.Text, descriptor.Font, &descriptor.Color, descriptor.Justification, renderer)
        | ParticlesDescriptor descriptor ->
            ()
            //GlRenderer2d.renderParticles
            //    (descriptor.Elevation, descriptor.Horizon, descriptor.Absolute, descriptor.Blend, descriptor.Image, descriptor.Particles, renderer)
        | RenderCallbackDescriptor2d callback ->
            ()
            //callback (viewAbsolute, viewRelative, eyePosition, eyeSize, eyeMargin, renderer)

    static member private renderLayeredMessages eyePosition eyeSize renderer =
        for message in renderer.RenderLayeredMessages do
            GlRenderer2d.renderDescriptor message.RenderDescriptor eyePosition eyeSize renderer

    static member addEyeMarginMessage (_ : Vector2) eyeSize eyeMargin renderer =
        let eyeMarginBounds = box2 (eyeSize * -0.5f - eyeMargin) (eyeSize + eyeMargin * 2.0f)
        let image = asset Assets.Default.PackageName Assets.Default.Image8Name
        let sprites =
            if eyeMargin <> v2Zero then
                let mutable transform = Transform.makeDefault v3Cartesian2d
                transform.Absolute <- true
                let sprite = { Transform = transform; Inset = box2Zero; Image = image; Color = Color.Black; Blend = Overwrite; Glow = Color.Zero; Flip = FlipNone }
                let mutable bottomMarginTransform = transform
                bottomMarginTransform.Position <- eyeMarginBounds.BottomLeft.V3
                bottomMarginTransform.Size <- v3 eyeMarginBounds.Size.X eyeMargin.Y 0.0f
                let bottomMargin = { sprite with Transform = bottomMarginTransform }
                let mutable leftMarginTransform = transform
                leftMarginTransform.Position <- eyeMarginBounds.BottomLeft.V3
                leftMarginTransform.Size <- v3 eyeMargin.X eyeMarginBounds.Size.Y 0.0f
                let leftMargin = { sprite with Transform = leftMarginTransform }
                let mutable topMarginTransform = transform
                topMarginTransform.Position <- eyeMarginBounds.TopLeft.V3 - v3 0.0f eyeMargin.Y 0.0f
                topMarginTransform.Size <- v3 eyeMarginBounds.Size.X eyeMargin.Y 0.0f
                let topMargin = { sprite with Transform = topMarginTransform }
                let mutable rightMarginTransform = transform
                rightMarginTransform.Position <- eyeMarginBounds.BottomRight.V3 - v3 eyeMargin.X 0.0f 0.0f
                rightMarginTransform.Size <- v3 eyeMargin.X eyeMarginBounds.Size.Y 0.0f
                let rightMargin = { sprite with Transform = rightMarginTransform }
                [|bottomMargin; leftMargin; topMargin; rightMargin|]
            else [||]
        let message = { Elevation = Single.MaxValue; AssetTag = AssetTag.generalize image; Horizon = 0.0f; RenderDescriptor = SpritesDescriptor { Sprites = sprites }}
        renderer.RenderLayeredMessages.Add message

    /// Make a Renderer.
    static member make window =

        // create SDL OpenGL context if needed
        match window with
        | SglWindow window -> OpenGL.Hl.CreateSgl410Context window.SglWindow |> ignore<nativeint>
        | WfglWindow window -> SDL.SDL_CreateWindowFrom window.WfglWindow |> ignore<nativeint>

        // create sprite batch env
        let spriteBatchEnv = OpenGL.SpriteBatch.CreateEnv ()

        // make renderer
        let renderer =
            { RenderWindow = window
              RenderSpriteBatchEnv = spriteBatchEnv
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderMessages = List ()
              RenderLayeredMessages = List () }

        // fin
        renderer

    interface Renderer2d with

        member renderer.SpriteBatchEnvOpt =
            Some renderer.RenderSpriteBatchEnv

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

        member renderer.Render eyePosition eyeSize eyeMargin renderMessages =

            // compute views
            let viewport = box2i v2iZero (v2i Constants.Render.ResolutionX Constants.Render.ResolutionY)
            let viewAbsolute = Matrix4x4.CreateTranslation (eyeSize * 0.5f * Constants.Render.VirtualScalar2 - eyeMargin * Constants.Render.VirtualScalar2).V3
            let viewRelative = Matrix4x4.CreateTranslation (-eyePosition * Constants.Render.VirtualScalar2 + eyeSize * 0.5f * Constants.Render.VirtualScalar2 - eyeMargin * Constants.Render.VirtualScalar2).V3

            // prepare frame
            OpenGL.Gl.Viewport (viewport.Position.X, viewport.Position.Y, viewport.Size.X, viewport.Size.Y)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)
            match Constants.Render.ScreenClearing with
            | ColorClear color -> OpenGL.Gl.ClearColor (color.R, color.G, color.B, color.A)
            | NoClear -> ()
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.SpriteBatch.BeginFrame (viewport, viewAbsolute, viewRelative, renderer.RenderSpriteBatchEnv)

            // render frame
            GlRenderer2d.handleRenderMessages renderMessages renderer
            GlRenderer2d.addEyeMarginMessage eyePosition eyeSize eyeMargin renderer // TODO: 3D: make sure margins are being rendered correctly.
            GlRenderer2d.sortRenderLayeredMessages renderer
            GlRenderer2d.renderLayeredMessages eyePosition eyeSize renderer
            renderer.RenderLayeredMessages.Clear ()

            // end frame
            OpenGL.SpriteBatch.EndFrame renderer.RenderSpriteBatchEnv

        member renderer.Swap () =
            match renderer.RenderWindow with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
            | WfglWindow window -> window.WfglSwapWindow ()

        member renderer.CleanUp () =
            OpenGL.SpriteBatch.DestroyEnv renderer.RenderSpriteBatchEnv
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderAssetPackages |> Seq.collect (Seq.map (fun entry -> entry.Value))
            for renderAsset in renderAssets do GlRenderer2d.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()
            renderer :> Renderer2d

/// A render thread for 2d rendering.
type RenderThread2d (createRenderer2d, window : Window) =

    let terminatedLock = obj ()
    let mutable terminated = false
    let submissionLock = obj ()
    let mutable submissionOpt = Option<Vector2 * Vector2 * Vector2 * RenderMessage2d List>.None
    let swapLock = obj ()
    let mutable swap = false

    member this.Terminated =
        lock terminatedLock (fun () -> terminated)

    member this.Start () =

        // run thread as task
        Task.Factory.StartNew (fun () ->

            // create renderer
            let renderer = createRenderer2d window : Renderer2d

            // loop until terminated
            while not this.Terminated do

                // loop until rendered or terminated
                let mutable rendered = false
                while not rendered && not this.Terminated do
                    rendered <-
                        lock submissionLock (fun () ->
                            match submissionOpt with
                            | Some (eyePosition, eyeSize, eyeMargin, messages) ->
                                renderer.Render eyePosition eyeSize eyeMargin messages
                                submissionOpt <- None
                                true
                            | None -> false)
                    if not rendered then Thread.Sleep 0
                    
                // loop until swapped or terminated
                let mutable swapped = false
                while not swapped && not this.Terminated do
                    swapped <- lock swapLock (fun () ->
                        if swap then
                            renderer.Swap ()
                            swap <- false
                            true
                        else false)
                    if not swapped then Thread.Sleep 0)

    member this.Submit eyePosition eyeSize eyeMargin messages =
        lock submissionLock (fun () ->
            while Option.isSome submissionOpt do Thread.Sleep 0
            submissionOpt <- Some ((eyePosition, eyeSize, eyeMargin, messages)))

    member this.Swap () =
        lock swapLock (fun () ->
            if swap then raise (InvalidOperationException "Redundant swap calls")
            swap <- true)

    member this.Terminate () =
        lock terminatedLock (fun () ->
            if terminated then raise (InvalidOperationException "Redundant terminate calls")
            terminated <- true)