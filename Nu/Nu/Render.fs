// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
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

type [<StructuralEquality; StructuralComparison>] JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

type [<StructuralEquality; StructuralComparison>] JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

type [<StructuralEquality; StructuralComparison>] Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

type [<StructuralEquality; StructuralComparison>] Flip =
    | FlipNone
    | FlipH
    | FlipV
    | FlipHV
    static member toSdlFlip flip =
        match flip with
        | FlipNone -> SDL.SDL_RendererFlip.SDL_FLIP_NONE
        | FlipH -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL
        | FlipV -> SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL
        | FlipHV -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL ||| SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL

/// Describes how to render a sprite to the rendering system.
type [<StructuralEquality; NoComparison>] SpriteDescriptor =
    { Transform : Transform
      Offset : Vector2
      InsetOpt : Vector4 option
      Image : Image AssetTag
      Color : Color
      Glow : Color
      Flip : Flip }

/// Describes how to render a tile map to the rendering system.
type [<StructuralEquality; NoComparison>] TileLayerDescriptor =
    { Transform : Transform
      MapSize : Vector2i
      Tiles : TmxLayerTile array
      TileSourceSize : Vector2i
      TileSize : Vector2
      TileSet : TmxTileset
      TileSetImage : Image AssetTag }

/// Describes how to render text to the rendering system.
type [<StructuralEquality; NoComparison>] TextDescriptor =
    { Transform : Transform
      Text : string
      Font : Font AssetTag
      Color : Color
      Justification : Justification }

/// Describes how to render something to the rendering system.
type [<StructuralEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor : SpriteDescriptor
    | SpritesDescriptor of SpriteDescriptors : SpriteDescriptor array
    | TileLayerDescriptor of TileLayerDescriptor : TileLayerDescriptor
    | TextDescriptor of TextDescriptor : TextDescriptor

/// Describes how to render a layered thing to the rendering system.
type [<StructuralEquality; NoComparison>] LayeredDescriptor =
    { Depth : single
      PositionY : single
      AssetTag : obj AssetTag
      RenderDescriptor : RenderDescriptor }

/// A message to the rendering system.
type [<StructuralEquality; NoComparison>] RenderMessage =
    | LayeredDescriptorMessage of LayeredDescriptor
    | LayeredDescriptorsMessage of LayeredDescriptor array
    | HintRenderPackageUseMessage of string
    | HintRenderPackageDisuseMessage of string
    | ReloadRenderAssetsMessage
    //| ScreenFlashMessage of ...
    //| ScreenShakeMessage of ...

/// An asset that is used for rendering.
type [<StructuralEquality; NoComparison>] RenderAsset =
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
type [<ReferenceEquality; NoComparison>] MockRenderer =
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
type [<ReferenceEquality; NoComparison>] SdlRenderer =
    private
        { RenderContext : nativeint
          RenderPackages : RenderAsset Packages
          mutable RenderMessages : RenderMessage List
          LayeredDescriptors : LayeredDescriptor List }

    static member private sortDescriptors (left : LayeredDescriptor) (right : LayeredDescriptor) =
        let depthCompare = left.Depth.CompareTo right.Depth
        if depthCompare <> 0 then depthCompare else
        let positionYCompare = -(left.PositionY.CompareTo right.PositionY)
        if positionYCompare <> 0 then positionYCompare else
        let assetNameCompare = strCmp left.AssetTag.AssetName right.AssetTag.AssetName
        if assetNameCompare <> 0 then assetNameCompare else
        strCmp left.AssetTag.PackageName right.AssetTag.PackageName

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
        | LayeredDescriptorMessage descriptor -> renderer.LayeredDescriptors.Add descriptor
        | LayeredDescriptorsMessage descriptors -> renderer.LayeredDescriptors.AddRange descriptors
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
        (descriptor : SpriteDescriptor)
        renderer =
        let mutable transform = descriptor.Transform
        let view = if transform.Absolute then viewAbsolute else viewRelative
        let position = transform.Position - Vector2.Multiply (descriptor.Offset, transform.Size)
        let positionView = position * view
        let sizeView = transform.Size * view.ExtractScaleMatrix ()
        let color = descriptor.Color
        let glow = descriptor.Glow
        let image = AssetTag.generalize descriptor.Image
        let flip = Flip.toSdlFlip descriptor.Flip
        match SdlRenderer.tryLoadRenderAsset image renderer with
        | Some renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture texture
                let mutable sourceRect = SDL.SDL_Rect ()
                match descriptor.InsetOpt with
                | Some inset ->
                    sourceRect.x <- int inset.X
                    sourceRect.y <- int inset.Y
                    sourceRect.w <- int inset.Z
                    sourceRect.h <- int inset.W
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
                let rotation = double -transform.Rotation * Constants.Math.RadiansToDegrees // negation for right-handedness
                let mutable rotationCenter = SDL.SDL_Point ()
                rotationCenter.x <- int (sizeView.X * 0.5f)
                rotationCenter.y <- int (sizeView.Y * 0.5f)
                SDL.SDL_SetTextureBlendMode (texture, SDL.SDL_BlendMode.SDL_BLENDMODE_BLEND) |> ignore
                SDL.SDL_SetTextureColorMod (texture, color.R, color.G, color.B) |> ignore
                SDL.SDL_SetTextureAlphaMod (texture, color.A) |> ignore
                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, ref sourceRect, ref destRect, rotation, ref rotationCenter, flip)
                if renderResult <> 0 then Log.info ("Render error - could not render texture for sprite '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
                if glow <> Color.Zero then
                    SDL.SDL_SetTextureBlendMode (texture, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
                    SDL.SDL_SetTextureColorMod (texture, glow.R, glow.G, glow.B) |> ignore
                    SDL.SDL_SetTextureAlphaMod (texture, glow.A) |> ignore
                    let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, ref sourceRect, ref destRect, rotation, ref rotationCenter, flip)
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
        let mutable transform = descriptor.Transform
        let view = if transform.Absolute then viewAbsolute else viewRelative
        let positionView = transform.Position * view
        let sizeView = transform.Size * view.ExtractScaleMatrix ()
        let tileRotation = transform.Rotation
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
                Array.iteri
                    (fun n (tile : TmxLayerTile) ->
                        let mapRun = mapSize.X
                        let (i, j) = (n % mapRun, n / mapRun)
                        let tilePosition =
                            Vector2
                                (positionView.X + tileSize.X * single i + eyeSize.X * 0.5f,
                                -(positionView.Y - tileSize.Y * single j + sizeView.Y) + eyeSize.Y * 0.5f) // negation for right-handedness
                        let tileBounds = v4Bounds tilePosition tileSize
                        let viewBounds = v4Bounds Vector2.Zero eyeSize
                        if Math.isBoundsIntersectingBounds tileBounds viewBounds then
                            let tileFlip =
                                match (tile.HorizontalFlip, tile.VerticalFlip) with
                                | (false, false) -> SDL.SDL_RendererFlip.SDL_FLIP_NONE
                                | (true, false) -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL
                                | (false, true) -> SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL
                                | (true, true) -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL ||| SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL
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
                            let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, tileSourceRectRef, tileDestRectRef, rotation, tileRotationCenterRef, tileFlip)
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
        let mutable transform = descriptor.Transform
        let view = if transform.Absolute then viewAbsolute else viewRelative
        let positionView = transform.Position * view
        let sizeView = transform.Size * view.ExtractScaleMatrix ()
        let text = String.textualize descriptor.Text
        let color = descriptor.Color
        let font = AssetTag.generalize descriptor.Font
        match SdlRenderer.tryLoadRenderAsset font renderer with
        | Some renderAsset ->
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
                let (offset, textSurface) =
                    match descriptor.Justification with
                    | Unjustified wrapped ->
                        let textSurface =
                            if wrapped
                            then SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 sizeView.X)
                            else SDL_ttf.TTF_RenderText_Blended (font, text, renderColor)
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
                        (v2 offsetX offsetY, textSurface)
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

    static member private renderDescriptor
        (viewAbsolute : Matrix3)
        (viewRelative : Matrix3)
        (eyeCenter : Vector2)
        (eyeSize : Vector2)
        descriptor
        renderer =
        match descriptor with
        | SpriteDescriptor sprite -> SdlRenderer.renderSprite viewAbsolute viewRelative eyeCenter eyeSize sprite renderer
        | SpritesDescriptor sprites -> SdlRenderer.renderSprites viewAbsolute viewRelative eyeCenter eyeSize sprites renderer
        | TileLayerDescriptor descriptor -> SdlRenderer.renderTileLayerDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer
        | TextDescriptor descriptor -> SdlRenderer.renderTextDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor renderer

    static member private renderLayeredDescriptors eyeCenter eyeSize (descriptors : LayeredDescriptor List) renderer =
        let renderContext = renderer.RenderContext
        let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
        match targetResult with
        | 0 ->
            SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
            let viewAbsolute = (Math.getViewAbsoluteI eyeCenter eyeSize).InvertedView ()
            let viewRelative = (Math.getViewRelativeI eyeCenter eyeSize).InvertedView ()
            descriptors.Sort SdlRenderer.sortDescriptors
            for descriptor in descriptors do
                SdlRenderer.renderDescriptor viewAbsolute viewRelative eyeCenter eyeSize descriptor.RenderDescriptor renderer
        | _ ->
            Log.trace ("Render error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")

    /// Make a Renderer.
    static member make renderContext =
        let renderer =
            { RenderContext = renderContext
              RenderPackages = dictPlus []
              RenderMessages = List ()
              LayeredDescriptors = List () }
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
            SdlRenderer.renderLayeredDescriptors eyeCenter eyeSize renderer.LayeredDescriptors renderer
            renderer.LayeredDescriptors.Clear ()

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