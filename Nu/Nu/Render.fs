// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open OpenTK
open SDL2
open TiledSharp
open Prime
open Nu

/// Describes how to render a sprite to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] SpriteDescriptor =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Offset : Vector2
      ViewType : ViewType
      InsetOpt : Vector4 option
      Image : AssetTag
      Color : Vector4 }

/// Describes how to render a tile map to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] TileLayerDescriptor =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      ViewType : ViewType
      MapSize : Vector2i
      Tiles : TmxLayerTile List // OPTIMIZATION: using List for direct transfer from Tmx
      TileSourceSize : Vector2i
      TileSize : Vector2
      TileSet : TmxTileset
      TileSetImage : AssetTag }

/// Describes how to render text to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] TextDescriptor =
    { Position : Vector2
      Size : Vector2
      ViewType : ViewType
      Text : string
      Font : AssetTag
      Color : Vector4 }

/// Describes how to render a layered 'thing' to the rendering system.
/// TODO: P1: make this a struct when F# allows it.
type [<StructuralEquality; NoComparison>] LayeredDescriptor =
    | SpriteDescriptor of SpriteDescriptor
    | SpritesDescriptor of SpriteDescriptor array
    | TileLayerDescriptor of TileLayerDescriptor
    | TextDescriptor of TextDescriptor

/// Describes how to render a layerable 'thing' to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] LayerableDescriptor =
    { Depth : single
      PositionY : single
      LayeredDescriptor : LayeredDescriptor }

/// Describes how to render something to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] RenderDescriptor =
    | LayerableDescriptor of LayerableDescriptor

/// A message to the rendering system.
type [<StructuralEquality; NoComparison>] RenderMessage =
    | RenderDescriptorsMessage of RenderDescriptor array
    | HintRenderPackageUseMessage of string
    | HintRenderPackageDisuseMessage of string
    | ReloadRenderAssetsMessage
    //| ScreenFlashMessage of ...

/// An asset that is used for rendering.
type [<ReferenceEquality>] RenderAsset =
    | TextureAsset of nativeint
    | FontAsset of nativeint * int

/// The renderer. Represents the rendering system in Nu generally.
type IRenderer =
    /// Clear all of the render messages that have been enqueued.
    abstract ClearMessages : unit -> IRenderer
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : RenderMessage -> IRenderer
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> IRenderer
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> IRenderer

[<AutoOpen>]
module RendererModule =

    /// The primary implementation of IRenderer.
    type [<ReferenceEquality>] Renderer =
        private
            { RenderContext : nativeint
              RenderPackageMap : RenderAsset PackageMap
              RenderMessages : RenderMessage UList
              RenderDescriptors : RenderDescriptor array }

        static member private sortDescriptors (LayerableDescriptor left) (LayerableDescriptor right) =
            if left.Depth < right.Depth then -1
            elif left.Depth > right.Depth then 1
            else 0

        static member private freeRenderAsset renderAsset =
            match renderAsset with
            | TextureAsset texture -> SDL.SDL_DestroyTexture texture
            | FontAsset (font, _) -> SDL_ttf.TTF_CloseFont font

        static member private tryLoadRenderAsset2 renderContext (asset : Asset) =
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
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Render) packageName assetGraph with
                | Right assets ->
                    let renderAssetOpts = List.map (Renderer.tryLoadRenderAsset2 renderer.RenderContext) assets
                    let renderAssets = List.definitize renderAssetOpts
                    let renderAssetMapOpt = UMap.tryFindFast packageName renderer.RenderPackageMap
                    if FOption.isSome renderAssetMapOpt then
                        let renderAssetMap = FOption.get renderAssetMapOpt
                        let renderAssetMap = UMap.addMany renderAssets renderAssetMap
                        { renderer with RenderPackageMap = UMap.add packageName renderAssetMap renderer.RenderPackageMap }
                    else
                        let renderAssetMap = UMap.makeFromSeq Functional renderAssets
                        { renderer with RenderPackageMap = UMap.add packageName renderAssetMap renderer.RenderPackageMap }
                | Left failedAssetNames ->
                    Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
                    renderer
            | Left error ->
                Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)
                renderer

        static member private tryLoadRenderAsset (assetTag : AssetTag) renderer =
            let (assetMapOpt, renderer) =
                if UMap.containsKey assetTag.PackageName renderer.RenderPackageMap
                then (UMap.tryFindFast assetTag.PackageName renderer.RenderPackageMap, renderer)
                else
                    Log.info ("Loading render package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                    let renderer = Renderer.tryLoadRenderPackage assetTag.PackageName renderer
                    (UMap.tryFindFast assetTag.PackageName renderer.RenderPackageMap, renderer)
            (FOption.bind (fun assetMap -> UMap.tryFindFast assetTag.AssetName assetMap) assetMapOpt, renderer)

        static member private handleHintRenderPackageUse hintPackageName renderer =
            Renderer.tryLoadRenderPackage hintPackageName renderer

        static member private handleHintRenderPackageDisuse hintPackageName renderer =
            let assetsOpt = UMap.tryFindFast hintPackageName renderer.RenderPackageMap
            if FOption.isSome assetsOpt then
                let assets = FOption.get assetsOpt
                for (_, asset) in assets do Renderer.freeRenderAsset asset
                { renderer with RenderPackageMap = UMap.remove hintPackageName renderer.RenderPackageMap }
            else renderer

        static member private handleReloadRenderAssets renderer =
            let oldPackageMap = renderer.RenderPackageMap
            let oldPackageNames = oldPackageMap |> UMap.toSeq |> Seq.map fst |> Array.ofSeq
            let renderer = { renderer with RenderPackageMap = UMap.makeEmpty Functional }
            Array.fold
                (fun renderer packageName -> Renderer.tryLoadRenderPackage packageName renderer)
                renderer
                oldPackageNames

        static member private handleRenderMessage renderer renderMessage =
            match renderMessage with
            | RenderDescriptorsMessage renderDescriptors -> { renderer with RenderDescriptors = Array.append renderDescriptors renderer.RenderDescriptors }
            | HintRenderPackageUseMessage hintPackageUse -> Renderer.handleHintRenderPackageUse hintPackageUse renderer
            | HintRenderPackageDisuseMessage hintPackageDisuse -> Renderer.handleHintRenderPackageDisuse hintPackageDisuse renderer
            | ReloadRenderAssetsMessage -> Renderer.handleReloadRenderAssets renderer

        static member private handleRenderMessages renderMessages renderer =
            UList.fold Renderer.handleRenderMessage renderer renderMessages

        static member private renderSprite
            (viewAbsolute : Matrix3)
            (viewRelative : Matrix3)
            (_ : Vector2)
            (eyeSize : Vector2)
            (sprite : SpriteDescriptor)
            renderer =
            let view = match sprite.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
            let position = sprite.Position - Vector2.Multiply (sprite.Offset, sprite.Size)
            let positionView = position * view
            let sizeView = sprite.Size * view.ExtractScaleMatrix ()
            let color = sprite.Color
            let image = sprite.Image
            let (renderAssetOpt, renderer) = Renderer.tryLoadRenderAsset image renderer
            if FOption.isSome renderAssetOpt then
                let renderAsset = FOption.get renderAssetOpt
                match renderAsset with
                | TextureAsset texture ->
                    let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture texture
                    let mutable sourceRect = SDL.SDL_Rect ()
                    match sprite.InsetOpt with
                    | Some inset ->
                        sourceRect.x <- int inset.X
                        sourceRect.y <- int inset.Y
                        sourceRect.w <- int ^ inset.Z - inset.X
                        sourceRect.h <- int ^ inset.W - inset.Y
                    | None ->
                        sourceRect.x <- 0
                        sourceRect.y <- 0
                        sourceRect.w <- textureSizeX
                        sourceRect.h <- textureSizeY
                    let mutable destRect = SDL.SDL_Rect ()
                    destRect.x <- int ^ positionView.X + eyeSize.X * 0.5f
                    destRect.y <- int ^ -positionView.Y + eyeSize.Y * 0.5f - sizeView.Y // negation for right-handedness
                    destRect.w <- int sizeView.X
                    destRect.h <- int sizeView.Y
                    let rotation = double -sprite.Rotation * Constants.Math.RadiansToDegrees // negation for right-handedness
                    let mutable rotationCenter = SDL.SDL_Point ()
                    rotationCenter.x <- int ^ sizeView.X * 0.5f
                    rotationCenter.y <- int ^ sizeView.Y * 0.5f
                    SDL.SDL_SetTextureColorMod (texture, byte ^ 255.0f * color.X, byte ^ 255.0f * color.Y, byte ^ 255.0f * color.Z) |> ignore
                    SDL.SDL_SetTextureAlphaMod (texture, byte ^ 255.0f * color.W) |> ignore
                    let renderResult =
                        SDL.SDL_RenderCopyEx (
                            renderer.RenderContext,
                            texture,
                            ref sourceRect,
                            ref destRect,
                            rotation,
                            ref rotationCenter,
                            SDL.SDL_RendererFlip.SDL_FLIP_NONE)
                    if renderResult <> 0 then Log.info ("Render error - could not render texture for sprite '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
                    renderer
                | _ -> Log.trace "Cannot render sprite with a non-texture asset."; renderer
            else Log.info ("SpriteDescriptor failed to render due to unloadable assets for '" + scstring image + "'."); renderer

        static member private renderSprites viewAbsolute viewRelative eyeCenter eyeSize sprites renderer =
            Array.fold
                (fun renderer sprite -> Renderer.renderSprite viewAbsolute viewRelative eyeCenter eyeSize sprite renderer)
                renderer
                sprites

        static member private renderTileLayerDescriptor
            (viewAbsolute : Matrix3)
            (viewRelative : Matrix3)
            (_ : Vector2)
            (eyeSize : Vector2)
            (descriptor : TileLayerDescriptor)
            renderer =
            let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
            let positionView = descriptor.Position * view
            let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
            let tileRotation = descriptor.Rotation
            let mapSize = descriptor.MapSize
            let tiles = descriptor.Tiles
            let tileSourceSize = descriptor.TileSourceSize
            let tileSize = descriptor.TileSize
            let tileSet = descriptor.TileSet
            let tileSetImage = descriptor.TileSetImage
            let tileSetWidth = let tileSetWidthOpt = tileSet.Image.Width in tileSetWidthOpt.Value
            let (renderAssetOpt, renderer) = Renderer.tryLoadRenderAsset tileSetImage renderer
            if FOption.isSome renderAssetOpt then
                let renderAsset = FOption.get renderAssetOpt
                match renderAsset with
                | TextureAsset texture ->
                    // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
                    let refTileSourceRect = ref ^ SDL.SDL_Rect ()
                    let refTileDestRect = ref ^ SDL.SDL_Rect ()
                    let refTileRotationCenter = ref ^ SDL.SDL_Point ()
                    Seq.iteri
                        (fun n (tile : TmxLayerTile) ->
                            let mapRun = mapSize.X
                            let (i, j) = (n % mapRun, n / mapRun)
                            let tilePosition =
                                Vector2
                                    (positionView.X + tileSize.X * single i + eyeSize.X * 0.5f,
                                     -(positionView.Y - tileSize.Y * single j + sizeView.Y) + eyeSize.Y * 0.5f) // negation for right-handedness
                            let tileBounds = Math.makeBounds tilePosition tileSize
                            let viewBounds = Math.makeBounds Vector2.Zero eyeSize
                            if Math.isBoundsIntersectingBounds tileBounds viewBounds then
                                let gid = tile.Gid - tileSet.FirstGid
                                let gidPosition = gid * tileSourceSize.X
                                let tileSourcePosition =
                                    Vector2
                                        (single ^ gidPosition % tileSetWidth,
                                         single ^ gidPosition / tileSetWidth * tileSourceSize.Y)
                                let mutable sourceRect = SDL.SDL_Rect ()
                                sourceRect.x <- int tileSourcePosition.X
                                sourceRect.y <- int tileSourcePosition.Y
                                sourceRect.w <- tileSourceSize.X
                                sourceRect.h <- tileSourceSize.Y
                                let mutable destRect = SDL.SDL_Rect ()
                                destRect.x <- int tilePosition.X
                                destRect.y <- int tilePosition.Y
                                destRect.w <- int tileSize.X
                                destRect.h <- int tileSize.Y
                                let rotation = double -tileRotation * Constants.Math.RadiansToDegrees // negation for right-handedness
                                let mutable rotationCenter = SDL.SDL_Point ()
                                rotationCenter.x <- int ^ tileSize.X * 0.5f
                                rotationCenter.y <- int ^ tileSize.Y * 0.5f
                                refTileSourceRect := sourceRect
                                refTileDestRect := destRect
                                refTileRotationCenter := rotationCenter
                                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, refTileSourceRect, refTileDestRect, rotation, refTileRotationCenter, SDL.SDL_RendererFlip.SDL_FLIP_NONE) // TODO: implement tile flip
                                if renderResult <> 0 then Log.info ("Render error - could not render texture for tile '" + scstring descriptor + "' due to '" + SDL.SDL_GetError () + "."))
                        tiles
                    renderer
                | _ -> Log.debug "Cannot render tile with a non-texture asset."; renderer
            else Log.info ("TileLayerDescriptor failed due to unloadable assets for '" + scstring tileSetImage + "'."); renderer

        static member private renderTextDescriptor
            (viewAbsolute : Matrix3)
            (viewRelative : Matrix3)
            (_ : Vector2)
            (eyeSize : Vector2)
            (descriptor : TextDescriptor)
            renderer =
            let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
            let positionView = descriptor.Position * view
            let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
            let text = String.textualize descriptor.Text
            let color = descriptor.Color
            let font = descriptor.Font
            let (renderAssetOpt, renderer) = Renderer.tryLoadRenderAsset font renderer
            if FOption.isSome renderAssetOpt then
                let renderAsset = FOption.get renderAssetOpt
                match renderAsset with
                | FontAsset (font, _) ->
                    let mutable renderColor = SDL.SDL_Color ()
                    renderColor.r <- byte ^ color.X * 255.0f
                    renderColor.g <- byte ^ color.Y * 255.0f
                    renderColor.b <- byte ^ color.Z * 255.0f
                    renderColor.a <- byte ^ color.W * 255.0f
                    // NOTE: the resource implications (perf and vram fragmentation?) of creating and destroying a
                    // texture one or more times a frame must be understood! Although, maybe it all happens in software
                    // and vram frag would not be a concern in the first place... perf could still be, however.
                    let textSurface = SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 sizeView.X)
                    if textSurface <> IntPtr.Zero then
                        let textTexture = SDL.SDL_CreateTextureFromSurface (renderer.RenderContext, textSurface)
                        let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture textTexture
                        let mutable sourceRect = SDL.SDL_Rect ()
                        sourceRect.x <- 0
                        sourceRect.y <- 0
                        sourceRect.w <- textureSizeX
                        sourceRect.h <- textureSizeY
                        let mutable destRect = SDL.SDL_Rect ()
                        destRect.x <- int ^ positionView.X + eyeSize.X * 0.5f
                        destRect.y <- int ^ -positionView.Y + eyeSize.Y * 0.5f - single textureSizeY // negation for right-handedness
                        destRect.w <- textureSizeX
                        destRect.h <- textureSizeY
                        if textTexture <> IntPtr.Zero then SDL.SDL_RenderCopy (renderer.RenderContext, textTexture, ref sourceRect, ref destRect) |> ignore
                        SDL.SDL_DestroyTexture textTexture
                        SDL.SDL_FreeSurface textSurface
                    renderer
                | _ -> Log.debug "Cannot render text with a non-font asset."; renderer
            else Log.info ("TextDescriptor failed due to unloadable assets for '" + scstring font + "'."); renderer

        static member private renderLayerableDescriptor
            (viewAbsolute : Matrix3)
            (viewRelative : Matrix3)
            (eyeCenter : Vector2)
            (eyeSize : Vector2)
            renderer
            layerableDescriptor =
            match layerableDescriptor with
            | SpriteDescriptor sprite -> Renderer.renderSprite viewAbsolute viewRelative eyeCenter eyeSize sprite renderer
            | SpritesDescriptor sprites -> Renderer.renderSprites viewAbsolute viewRelative eyeCenter eyeSize sprites renderer
            | TileLayerDescriptor descriptor -> Renderer.renderTileLayerDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer
            | TextDescriptor descriptor -> Renderer.renderTextDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer

        static member private renderDescriptors eyeCenter eyeSize renderDescriptors renderer =
            let renderContext = renderer.RenderContext
            let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
            match targetResult with
            | 0 ->
                // OPTIMIZATION: uses arrays for speed
                SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
                let renderDescriptorsSorted =
                    renderDescriptors |>
                    Array.rev |>
                    Seq.sortWith Renderer.sortDescriptors |> // Seq.sort is stable, unlike Array.sort...
                    Array.ofSeq
                let layeredDescriptors = Array.map (fun (LayerableDescriptor descriptor) -> descriptor.LayeredDescriptor) renderDescriptorsSorted
                let viewAbsolute = Matrix3.InvertView ^ Math.getViewAbsoluteI eyeCenter eyeSize
                let viewRelative = Matrix3.InvertView ^ Math.getViewRelativeI eyeCenter eyeSize
                Array.fold (Renderer.renderLayerableDescriptor viewAbsolute viewRelative eyeCenter eyeSize) renderer layeredDescriptors
            | _ ->
                Log.trace ("Render error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")
                renderer

        /// Make a Renderer.
        static member make renderContext =
            let renderer =
                { RenderContext = renderContext
                  RenderPackageMap = UMap.makeEmpty Functional
                  RenderMessages = UList.makeEmpty Functional
                  RenderDescriptors = [||] }
            renderer

        interface IRenderer with

            member renderer.ClearMessages () =
                let renderer = { renderer with RenderMessages = UList.makeEmpty Functional }
                renderer :> IRenderer

            member renderer.EnqueueMessage renderMessage =
                let renderMessages = UList.add renderMessage renderer.RenderMessages
                let renderer = { renderer with RenderMessages = renderMessages }
                renderer :> IRenderer

            member renderer.Render eyeCenter eyeSize =
                let renderMessages = renderer.RenderMessages
                let renderer = { renderer with RenderMessages = UList.makeEmpty Functional }
                let renderer = Renderer.handleRenderMessages renderMessages renderer
                let renderDescriptors = renderer.RenderDescriptors
                let renderer = { renderer with RenderDescriptors = [||] }
                let renderer = Renderer.renderDescriptors eyeCenter eyeSize renderDescriptors renderer
                renderer :> IRenderer

            member renderer.CleanUp () =
                let renderAssetMaps = renderer.RenderPackageMap |> UMap.toSeq |> Seq.map snd
                let renderAssets = Seq.collect (UMap.toSeq >> Seq.map snd) renderAssetMaps
                for renderAsset in renderAssets do Renderer.freeRenderAsset renderAsset
                let renderer = { renderer with RenderPackageMap = UMap.makeEmpty Functional }
                renderer :> IRenderer

/// The primary implementation of IRenderer.
type Renderer = RendererModule.Renderer

/// The mock implementation of IRenderer.
type [<ReferenceEquality>] MockRenderer =
    private
        { MockRenderer : unit }

    interface IRenderer with
        member renderer.ClearMessages () = renderer :> IRenderer
        member renderer.EnqueueMessage _ = renderer :> IRenderer
        member renderer.Render _ _ = renderer :> IRenderer
        member renderer.CleanUp () = renderer :> IRenderer

    static member make () =
        { MockRenderer = () }