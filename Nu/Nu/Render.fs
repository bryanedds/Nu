// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open OpenTK
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

type JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

type JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

type Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

/// Describes how to render a sprite to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] SpriteDescriptor =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Offset : Vector2
      ViewType : ViewType
      InsetOpt : Vector4 option
      Image : Image AssetTag
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
      TileSetImage : Image AssetTag }

/// Describes how to render text to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] TextDescriptor =
    { Position : Vector2
      Size : Vector2
      ViewType : ViewType
      Text : string
      Font : Font AssetTag
      Color : Vector4
      Justification : Justification }

/// Describes how to render a layered 'thing' to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] LayeredDescriptor =
    | SpriteDescriptor of SpriteDescriptor : SpriteDescriptor
    | SpritesDescriptor of SpriteDescriptors : SpriteDescriptor array
    | TileLayerDescriptor of TileLayerDescriptor : TileLayerDescriptor
    | TextDescriptor of TextDescriptor : TextDescriptor

/// Describes how to render a layerable 'thing' to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] LayerableDescriptor =
    { Depth : single
      AssetTag : AssetTag
      PositionY : single
      LayeredDescriptor : LayeredDescriptor }

/// Describes how to render something to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] RenderDescriptor =
    | LayerableDescriptor of LayerableDescriptor

/// A message to the rendering system.
type [<Struct; StructuralEquality; NoComparison>] RenderMessage =
    | RenderDescriptorsMessage of renderDescriptorsMessage : RenderDescriptor array
    | HintRenderPackageUseMessage of hintRenderPackageUseMessage : string
    | HintRenderPackageDisuseMessage of hintRenderPackageDisuseMessage : string
    | ReloadRenderAssetsMessage
    //| ScreenFlashMessage of ...

/// An asset that is used for rendering.
type [<ReferenceEquality>] RenderAsset =
    | TextureAsset of nativeint
    | FontAsset of nativeint * int

/// The renderer. Represents the rendering system in Nu generally.
type Renderer =
    /// Pop all of the physics messages that have been enqueued.
    abstract PopMessages : unit -> RenderMessage List
    /// Clear all of the render messages that have been enqueued.
    abstract ClearMessages : unit -> unit
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : RenderMessage -> unit
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> RenderMessage List -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> Renderer

/// The mock implementation of Renderer.
type [<ReferenceEquality>] MockRenderer =
    private
        { MockRenderer : unit }

    interface Renderer with
        member renderer.PopMessages () = List ()
        member renderer.ClearMessages () = ()
        member renderer.EnqueueMessage _ = ()
        member renderer.Render _ _ _ = ()
        member renderer.CleanUp () = renderer :> Renderer

    static member make () =
        { MockRenderer = () }

/// The SDL implementation of Renderer.
type [<ReferenceEquality>] SdlRenderer =
    private
        { RenderContext : nativeint
          RenderPackages : RenderAsset Packages
          mutable RenderMessages : RenderMessage List
          RenderDescriptors : RenderDescriptor List }

    static member private sortDescriptors (LayerableDescriptor left) (LayerableDescriptor right) =
        let depthCompare = left.Depth.CompareTo right.Depth
        if depthCompare <> 0 then depthCompare else
        let positionYCompare = -(left.PositionY.CompareTo right.PositionY)
        if positionYCompare <> 0 then positionYCompare else
        let packageCompare = strCmp left.AssetTag.PackageName right.AssetTag.PackageName
        if packageCompare <> 0 then packageCompare else
        strCmp left.AssetTag.AssetName right.AssetTag.AssetName

    static member private freeRenderAsset renderAsset =
        match renderAsset with
        | TextureAsset texture -> SDL.SDL_DestroyTexture texture
        | FontAsset (font, _) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset2 renderContext (asset : obj Asset) =
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
                let renderAssetOpts = List.map (SdlRenderer.tryLoadRenderAsset2 renderer.RenderContext) assets
                let renderAssets = List.definitize renderAssetOpts
                match Dictionary.tryFind packageName renderer.RenderPackages with
                | Some renderAssetDict ->
                    for (key, value) in renderAssets do renderAssetDict.ForceAdd (key, value)
                    renderer.RenderPackages.ForceAdd (packageName, renderAssetDict)
                | None ->
                    let renderAssetDict = dictPlus renderAssets
                    renderer.RenderPackages.ForceAdd (packageName, renderAssetDict)
            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member private tryLoadRenderAsset (assetTag : obj AssetTag) renderer =
        let assetsOpt =
            if renderer.RenderPackages.ContainsKey assetTag.PackageName then
                Dictionary.tryFind assetTag.PackageName renderer.RenderPackages
            else
                Log.info ("Loading render package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                SdlRenderer.tryLoadRenderPackage assetTag.PackageName renderer
                Dictionary.tryFind assetTag.PackageName renderer.RenderPackages
        Option.bind (fun assets -> Dictionary.tryFind assetTag.AssetName assets) assetsOpt

    static member private handleHintRenderPackageUse hintPackageName renderer =
        SdlRenderer.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackageDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some assets ->
            for asset in assets do SdlRenderer.freeRenderAsset asset.Value
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            SdlRenderer.tryLoadRenderPackage packageName renderer

    static member private handleRenderMessage renderMessage renderer =
        match renderMessage with
        | RenderDescriptorsMessage renderDescriptors -> renderer.RenderDescriptors.AddRange renderDescriptors
        | HintRenderPackageUseMessage hintPackageUse -> SdlRenderer.handleHintRenderPackageUse hintPackageUse renderer
        | HintRenderPackageDisuseMessage hintPackageDisuse -> SdlRenderer.handleHintRenderPackageDisuse hintPackageDisuse renderer
        | ReloadRenderAssetsMessage -> SdlRenderer.handleReloadRenderAssets renderer

    static member private handleRenderMessages renderMessages renderer =
        for renderMessage in renderMessages do
            SdlRenderer.handleRenderMessage renderMessage renderer

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
        let image = AssetTag.generalize sprite.Image
        match SdlRenderer.tryLoadRenderAsset image renderer with
        | Some renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture texture
                let mutable sourceRect = SDL.SDL_Rect ()
                match sprite.InsetOpt with
                | Some inset ->
                    sourceRect.x <- int inset.X
                    sourceRect.y <- int inset.Y
                    sourceRect.w <- int (inset.Z - inset.X)
                    sourceRect.h <- int (inset.W - inset.Y)
                | None ->
                    sourceRect.x <- 0
                    sourceRect.y <- 0
                    sourceRect.w <- textureSizeX
                    sourceRect.h <- textureSizeY
                let mutable destRect = SDL.SDL_Rect ()
                destRect.x <- int (positionView.X + eyeSize.X * 0.5f)
                destRect.y <- int (-positionView.Y + eyeSize.Y * 0.5f - sizeView.Y) // negation for right-handedness
                destRect.w <- int sizeView.X
                destRect.h <- int sizeView.Y
                let rotation = double -sprite.Rotation * Constants.Math.RadiansToDegrees // negation for right-handedness
                let mutable rotationCenter = SDL.SDL_Point ()
                rotationCenter.x <- int (sizeView.X * 0.5f)
                rotationCenter.y <- int (sizeView.Y * 0.5f)
                SDL.SDL_SetTextureColorMod (texture, byte (255.0f * color.X), byte (255.0f * color.Y), byte (255.0f * color.Z)) |> ignore
                SDL.SDL_SetTextureAlphaMod (texture, byte (255.0f * color.W)) |> ignore
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
            | _ -> Log.trace "Cannot render sprite with a non-texture asset."
        | _ -> Log.info ("SpriteDescriptor failed to render due to unloadable assets for '" + scstring image + "'.")

    static member private renderSprites viewAbsolute viewRelative eyeCenter eyeSize sprites renderer =
        for sprite in sprites do
            SdlRenderer.renderSprite viewAbsolute viewRelative eyeCenter eyeSize sprite renderer

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
        let tileSetImage = AssetTag.generalize descriptor.TileSetImage
        let tileSetWidth = let tileSetWidthOpt = tileSet.Image.Width in tileSetWidthOpt.Value
        match SdlRenderer.tryLoadRenderAsset tileSetImage renderer with
        | Some renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
                let tileSourceRectRef = ref (SDL.SDL_Rect ())
                let tileDestRectRef = ref (SDL.SDL_Rect ())
                let tileRotationCenterRef = ref (SDL.SDL_Point ())
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
                                    (single (gidPosition % tileSetWidth),
                                        single (gidPosition / tileSetWidth * tileSourceSize.Y))
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
                            rotationCenter.x <- int (tileSize.X * 0.5f)
                            rotationCenter.y <- int (tileSize.Y * 0.5f)
                            tileSourceRectRef := sourceRect
                            tileDestRectRef := destRect
                            tileRotationCenterRef := rotationCenter
                            let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, tileSourceRectRef, tileDestRectRef, rotation, tileRotationCenterRef, SDL.SDL_RendererFlip.SDL_FLIP_NONE) // TODO: implement tile flip
                            if renderResult <> 0 then Log.info ("Render error - could not render texture for tile '" + scstring descriptor + "' due to '" + SDL.SDL_GetError () + "."))
                    tiles
            | _ -> Log.debug "Cannot render tile with a non-texture asset."
        | _ -> Log.info ("TileLayerDescriptor failed due to unloadable assets for '" + scstring tileSetImage + "'.")

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
        let font = AssetTag.generalize descriptor.Font
        match SdlRenderer.tryLoadRenderAsset font renderer with
        | Some renderAsset ->
            match renderAsset with
            | FontAsset (font, _) ->
                let mutable renderColor = SDL.SDL_Color ()
                renderColor.r <- byte (color.X * 255.0f)
                renderColor.g <- byte (color.Y * 255.0f)
                renderColor.b <- byte (color.Z * 255.0f)
                renderColor.a <- byte (color.W * 255.0f)
                // NOTE: the resource implications (perf and vram fragmentation?) of creating and destroying a
                // texture one or more times a frame must be understood! Although, maybe it all happens in software
                // and vram fragmentation would not be a concern in the first place... perf could still be, however.
                let (offset, textSurface) =
                    match descriptor.Justification with
                    | Unjustified wrapped ->
                        let textSurface =
                            if wrapped
                            then SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
                            else SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 sizeView.X)
                        (Vector2.Zero, textSurface)
                    | Justified (h, v) ->
                        let textSurface = SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
                        let (width, height) = (ref 0, ref 0)
                        SDL_ttf.TTF_SizeText (font, text, width, height) |> ignore
                        let offsetX =
                            match h with
                            | JustifyLeft -> 0.0f
                            | JustifyCenter -> (sizeView.X - single !width) * 0.5f
                            | JustifyRight -> sizeView.X - single !width
                        let offsetY =
                            match v with
                            | JustifyTop -> 0.0f
                            | JustifyMiddle -> (sizeView.Y - single !height) * 0.5f
                            | JustifyBottom -> sizeView.Y - single !height
                        (Vector2 (offsetX, offsetY), textSurface)
                if textSurface <> IntPtr.Zero then
                    let textTexture = SDL.SDL_CreateTextureFromSurface (renderer.RenderContext, textSurface)
                    let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture textTexture
                    let mutable sourceRect = SDL.SDL_Rect ()
                    sourceRect.x <- 0
                    sourceRect.y <- 0
                    sourceRect.w <- textureSizeX
                    sourceRect.h <- textureSizeY
                    let mutable destRect = SDL.SDL_Rect ()
                    destRect.x <- int (positionView.X + offset.X + eyeSize.X * 0.5f)
                    destRect.y <- int (-positionView.Y + offset.Y + eyeSize.Y * 0.5f - single sizeView.Y) // negation for right-handedness
                    destRect.w <- textureSizeX
                    destRect.h <- textureSizeY
                    if textTexture <> IntPtr.Zero then SDL.SDL_RenderCopy (renderer.RenderContext, textTexture, ref sourceRect, ref destRect) |> ignore
                    SDL.SDL_DestroyTexture textTexture
                    SDL.SDL_FreeSurface textSurface
            | _ -> Log.debug "Cannot render text with a non-font asset."
        | _ -> Log.info ("TextDescriptor failed due to unloadable assets for '" + scstring font + "'.")

    static member private renderLayerableDescriptor
        (viewAbsolute : Matrix3)
        (viewRelative : Matrix3)
        (eyeCenter : Vector2)
        (eyeSize : Vector2)
        layerableDescriptor
        renderer =
        match layerableDescriptor with
        | SpriteDescriptor sprite -> SdlRenderer.renderSprite viewAbsolute viewRelative eyeCenter eyeSize sprite renderer
        | SpritesDescriptor sprites -> SdlRenderer.renderSprites viewAbsolute viewRelative eyeCenter eyeSize sprites renderer
        | TileLayerDescriptor descriptor -> SdlRenderer.renderTileLayerDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer
        | TextDescriptor descriptor -> SdlRenderer.renderTextDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer

    static member private renderDescriptors eyeCenter eyeSize (renderDescriptors : RenderDescriptor List) renderer =
        let renderContext = renderer.RenderContext
        let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
        match targetResult with
        | 0 ->
            SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
            let viewAbsolute = (Math.getViewAbsoluteI eyeCenter eyeSize).InvertedView ()
            let viewRelative = (Math.getViewRelativeI eyeCenter eyeSize).InvertedView ()
            renderDescriptors.Sort SdlRenderer.sortDescriptors
            for renderDescriptor in renderDescriptors do
                let layeredDescriptor = match renderDescriptor with LayerableDescriptor layerableDescriptor -> layerableDescriptor.LayeredDescriptor
                SdlRenderer.renderLayerableDescriptor viewAbsolute viewRelative eyeCenter eyeSize layeredDescriptor renderer
        | _ ->
            Log.trace ("Render error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")

    /// Make a Renderer.
    static member make renderContext =
        let renderer =
            { RenderContext = renderContext
              RenderPackages = dictPlus []
              RenderMessages = List ()
              RenderDescriptors = List<RenderDescriptor> () }
        renderer

    interface Renderer with

        member renderer.PopMessages () =
            let messages = renderer.RenderMessages
            renderer.RenderMessages <- List ()
            messages

        member renderer.ClearMessages () =
            renderer.RenderMessages <- List ()

        member renderer.EnqueueMessage renderMessage =
            renderer.RenderMessages.Add renderMessage

        member renderer.Render eyeCenter eyeSize renderMessages =
            SdlRenderer.handleRenderMessages renderMessages renderer
            SdlRenderer.renderDescriptors eyeCenter eyeSize renderer.RenderDescriptors renderer
            renderer.RenderDescriptors.Clear ()

        member renderer.CleanUp () =
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = Seq.collect (Seq.map (fun (entry : KeyValuePair<_, _>) -> entry.Value)) renderAssetPackages
            for renderAsset in renderAssets do SdlRenderer.freeRenderAsset renderAsset
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

    /// Render a frame of the game.
    let render eyeCenter eyeSize renderMessages (renderer : Renderer) =
        renderer.Render eyeCenter eyeSize renderMessages

    /// Handle render clean up by freeing all loaded render assets.
    let cleanUp (renderer : Renderer) =
        renderer.CleanUp ()