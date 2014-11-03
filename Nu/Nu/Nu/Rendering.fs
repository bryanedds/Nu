namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.ComponentModel
open OpenTK
open SDL2
open TiledSharp
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module RenderingModule =

    /// Describes an image asset.
    type
        [<StructuralEquality;
          NoComparison;
          TypeConverter (typeof<AlgebraicConverter>);
          XDefaultValue (DefaultImageValue)>]
        Image =
        { ImageAssetName : string
          PackageName : string }

    /// Describes how to render a sprite to the rendering system.
    type [<StructuralEquality; NoComparison>] SpriteDescriptor =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          ViewType : ViewType
          OptInset : Vector4 option
          Image : Image
          Color : Vector4 }

    /// Describes a tile map asset.
    type
        [<StructuralEquality;
          NoComparison;
          TypeConverter (typeof<AlgebraicConverter>);
          XDefaultValue (DefaultTileMapAssetValue)>]
        TileMapAsset =
        { TileMapAssetName : string
          PackageName : string }

    /// Describes how to render a tile map to the rendering system.
    type [<StructuralEquality; NoComparison>] TileLayerDescriptor =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          ViewType : ViewType
          MapSize : Vector2I
          Tiles : TmxLayerTile List
          TileSourceSize : Vector2I
          TileSize : Vector2
          TileSet : TmxTileset
          TileSetImage : Image }
    
    /// Describes a font asset.
    type
        [<StructuralEquality;
          NoComparison;
          TypeConverter (typeof<AlgebraicConverter>);
          XDefaultValue (DefaultFontValue)>]
        Font =
        { FontAssetName : string
          PackageName : string }

    /// Describes how to render text to the rendering system.
    type [<StructuralEquality; NoComparison>] TextDescriptor =
        { Position : Vector2
          Size : Vector2
          ViewType : ViewType
          Text : string
          Font : Font
          Color : Vector4 }

    /// Describes how to render a layered 'thing' to the rendering system.
    type [<StructuralEquality; NoComparison>] LayeredDescriptor =
        | SpriteDescriptor of SpriteDescriptor
        | TileLayerDescriptor of TileLayerDescriptor
        | TextDescriptor of TextDescriptor

    /// Describes how to render a layerable 'thing' to the rendering system.
    type [<StructuralEquality; NoComparison>] LayerableDescriptor =
        { Depth : single
          LayeredDescriptor : LayeredDescriptor }

    /// Describes how to render something to the rendering system.
    type [<StructuralEquality; NoComparison>] RenderDescriptor =
        | LayerableDescriptor of LayerableDescriptor

    /// Hint that a rendering asset package with the given name should be loaded. Should be used to
    /// avoid loading assets at inconvenient times (such as in the middle of game play!)
    type [<StructuralEquality; NoComparison>] HintRenderingPackageUseMessage =
        { PackageName : string }
        
    /// Hint that a rendering package should be unloaded since its assets will not be used again
    /// (or until specified via a HintRenderingPackageUseMessage).
    type [<StructuralEquality; NoComparison>] HintRenderingPackageDisuseMessage =
        { PackageName : string }

    /// A message to the rendering system.
    type [<StructuralEquality; NoComparison>] RenderMessage =
        | HintRenderingPackageUseMessage of HintRenderingPackageUseMessage
        | HintRenderingPackageDisuseMessage of HintRenderingPackageDisuseMessage
        | ReloadRenderingAssetsMessage
        //| ScreenFlashMessage of ...

    /// An asset that is used for rendering.
    type [<ReferenceEquality>] RenderAsset =
        | TextureAsset of nativeint
        | FontAsset of nativeint * int

    /// The renderer. Represents the rendering system in Nu generally.
    type IRenderer =

        /// Handle render exit by freeing all loaded render assets.
        abstract HandleRenderExit : unit -> IRenderer

        /// Render a frame of the game.
        abstract Render : Camera * RenderMessage rQueue * RenderDescriptor list -> IRenderer

    /// The primary implementation of IRenderer.
    type [<ReferenceEquality>] Renderer =
        private
            { RenderContext : nativeint
              RenderAssetMap : RenderAsset AssetMap
              AssetGraphFilePath : string }

        static member private freeRenderAsset renderAsset =
            match renderAsset with
            | TextureAsset texture -> SDL.SDL_DestroyTexture texture
            | FontAsset (font, _) -> SDL_ttf.TTF_CloseFont font

        static member private tryLoadRenderAsset2 renderContext (asset : Asset) =
            let extension = Path.GetExtension asset.FilePath
            match extension with
            | ".bmp"
            | ".png" ->
                let optTexture = SDL_image.IMG_LoadTexture (renderContext, asset.FilePath)
                if optTexture <> IntPtr.Zero then Some (asset.Name, TextureAsset optTexture)
                else
                    let errorMsg = SDL.SDL_GetError ()
                    trace <| "Could not load texture '" + asset.FilePath + "' due to '" + errorMsg + "'."
                    None
            | ".ttf" ->
                let fileFirstName = Path.GetFileNameWithoutExtension asset.FilePath
                let fileFirstNameLength = String.length fileFirstName
                if fileFirstNameLength >= 3 then
                    let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                    let fontSize = ref 0
                    if Int32.TryParse (fontSizeText, fontSize) then
                        let optFont = SDL_ttf.TTF_OpenFont (asset.FilePath, !fontSize)
                        if optFont <> IntPtr.Zero
                        then Some (asset.Name, FontAsset (optFont, !fontSize))
                        else trace <| "Could not load font due to unparsable font size in file name '" + asset.FilePath + "'."; None
                    else trace <| "Could not load font due to file name being too short: '" + asset.FilePath + "'."; None
                else trace <| "Could not load font '" + asset.FilePath + "'."; None
            | _ -> trace <| "Could not load render asset '" + tcstring asset + "' due to unknown extension '" + extension + "'."; None

        static member private tryLoadRenderPackage packageName renderer =
            let optAssets = Assets.tryLoadAssetsFromPackage true (Some RenderingAssociation) packageName renderer.AssetGraphFilePath
            match optAssets with
            | Right assets ->
                let optRenderAssets = List.map (Renderer.tryLoadRenderAsset2 renderer.RenderContext) assets
                let renderAssets = List.definitize optRenderAssets
                let optRenderAssetMap = Map.tryFind packageName renderer.RenderAssetMap
                match optRenderAssetMap with
                | Some renderAssetMap ->
                    let renderAssetMap = Map.addMany renderAssets renderAssetMap
                    { renderer with RenderAssetMap = Map.add packageName renderAssetMap renderer.RenderAssetMap }
                | None ->
                    let renderAssetMap = Map.ofSeq renderAssets
                    { renderer with RenderAssetMap = Map.add packageName renderAssetMap renderer.RenderAssetMap }
            | Left error ->
                note <| "Render package load failed due unloadable assets '" + error + "' for package '" + packageName + "'."
                renderer

        static member private tryLoadRenderAsset packageName assetName renderer =
            let optAssetMap = Map.tryFind packageName renderer.RenderAssetMap
            let (renderer, optAssetMap) =
                match optAssetMap with
                | Some _ -> (renderer, Map.tryFind packageName renderer.RenderAssetMap)
                | None ->
                    note <| "Loading render package '" + packageName + "' for asset '" + assetName + "' on the fly."
                    let renderer = Renderer.tryLoadRenderPackage packageName renderer
                    (renderer, Map.tryFind packageName renderer.RenderAssetMap)
            (renderer, Option.bind (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap)

        static member private handleHintRenderingPackageUse (hintPackageUse : HintRenderingPackageUseMessage) renderer =
            Renderer.tryLoadRenderPackage hintPackageUse.PackageName renderer
    
        static member private handleHintRenderingPackageDisuse (hintPackageDisuse : HintRenderingPackageDisuseMessage) renderer =
            let packageName = hintPackageDisuse.PackageName
            let optAssets = Map.tryFind packageName renderer.RenderAssetMap
            match optAssets with
            | Some assets ->
                for asset in Map.toValueList assets do Renderer.freeRenderAsset asset
                { renderer with RenderAssetMap = Map.remove packageName renderer.RenderAssetMap }
            | None -> renderer

        static member private handleReloadRenderingAssets renderer =
            let oldAssetMap = renderer.RenderAssetMap
            let renderer = { renderer with RenderAssetMap = Map.empty }
            List.fold
                (fun renderer packageName -> Renderer.tryLoadRenderPackage packageName renderer)
                renderer
                (Map.toKeyList oldAssetMap)

        static member private handleRenderMessage renderer renderMessage =
            match renderMessage with
            | HintRenderingPackageUseMessage hintPackageUse -> Renderer.handleHintRenderingPackageUse hintPackageUse renderer
            | HintRenderingPackageDisuseMessage hintPackageDisuse -> Renderer.handleHintRenderingPackageDisuse hintPackageDisuse renderer
            | ReloadRenderingAssetsMessage  -> Renderer.handleReloadRenderingAssets renderer

        static member private handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
            List.fold Renderer.handleRenderMessage renderer (List.rev renderMessages)

        static member private renderSpriteDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : SpriteDescriptor) renderer =
            let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
            let positionView = descriptor.Position * view
            let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
            let image = descriptor.Image
            let color = descriptor.Color
            let (renderer, optRenderAsset) = Renderer.tryLoadRenderAsset image.PackageName image.ImageAssetName renderer
            match optRenderAsset with
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    let textureFormat = ref 0u
                    let textureAccess = ref 0
                    let textureSizeX = ref 0
                    let textureSizeY = ref 0
                    ignore <| SDL.SDL_QueryTexture (texture, textureFormat, textureAccess, textureSizeX, textureSizeY)
                    let mutable sourceRect = SDL.SDL_Rect ()
                    match descriptor.OptInset with
                    | Some inset ->
                        sourceRect.x <- int inset.X
                        sourceRect.y <- int inset.Y
                        sourceRect.w <- int <| inset.Z - inset.X
                        sourceRect.h <- int <| inset.W - inset.Y
                    | None ->
                        sourceRect.x <- 0
                        sourceRect.y <- 0
                        sourceRect.w <- !textureSizeX
                        sourceRect.h <- !textureSizeY
                    let mutable destRect = SDL.SDL_Rect ()
                    destRect.x <- int <| positionView.X + camera.EyeSize.X * 0.5f
                    destRect.y <- int <| -positionView.Y + camera.EyeSize.Y * 0.5f - sizeView.Y // negation for right-handedness
                    destRect.w <- int sizeView.X
                    destRect.h <- int sizeView.Y
                    let rotation = double -descriptor.Rotation * RadiansToDegrees // negation for right-handedness
                    let mutable rotationCenter = SDL.SDL_Point ()
                    rotationCenter.x <- int <| sizeView.X * 0.5f
                    rotationCenter.y <- int <| sizeView.Y * 0.5f
                    ignore <| SDL.SDL_SetTextureColorMod (texture, byte <| 255.0f * color.X, byte <| 255.0f * color.Y, byte <| 255.0f * color.Z)
                    ignore <| SDL.SDL_SetTextureAlphaMod (texture, byte <| 255.0f * color.W)
                    let renderResult =
                        SDL.SDL_RenderCopyEx (
                            renderer.RenderContext,
                            texture,
                            ref sourceRect,
                            ref destRect,
                            rotation,
                            ref rotationCenter,
                            SDL.SDL_RendererFlip.SDL_FLIP_NONE)
                    if renderResult <> 0 then note <| "Rendering error - could not render texture for sprite '" + tcstring descriptor + "' due to '" + SDL.SDL_GetError () + "."
                    renderer
                | _ -> trace "Cannot render sprite with a non-texture asset."; renderer
            | None -> note <| "SpriteDescriptor failed to render due to unloadable assets for '" + tcstring image + "'."; renderer

        static member private renderTileLayerDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : TileLayerDescriptor) renderer =
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
            let optTileSetWidth = tileSet.Image.Width
            let tileSetWidth = optTileSetWidth.Value
            let (renderer, optRenderAsset) = Renderer.tryLoadRenderAsset tileSetImage.PackageName tileSetImage.ImageAssetName renderer
            match optRenderAsset with
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    // OPTIMIZATION: allocating refs in a tight-loop = problematic
                    let refTileSourceRect = ref <| SDL.SDL_Rect ()
                    let refTileDestRect = ref <| SDL.SDL_Rect ()
                    let refTileRotationCenter = ref <| SDL.SDL_Point ()
                    Seq.iteri
                        (fun n _ ->
                            let mapRun = mapSize.X
                            let (i, j) = (n % mapRun, n / mapRun)
                            let tilePosition =
                                Vector2 (
                                    positionView.X + tileSize.X * single i + camera.EyeSize.X * 0.5f,
                                    -(positionView.Y - tileSize.Y * single j + sizeView.Y) + camera.EyeSize.Y * 0.5f) // negation for right-handedness
                            if Math.isBoundsInBounds3 tilePosition tileSize <| Vector4 (0.0f, 0.0f, camera.EyeSize.X, camera.EyeSize.Y) then
                                let gid = tiles.[n].Gid - tileSet.FirstGid
                                let gidPosition = gid * tileSourceSize.X
                                let tileSourcePosition =
                                    Vector2 (
                                        single <| gidPosition % tileSetWidth,
                                        single <| gidPosition / tileSetWidth * tileSourceSize.Y)
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
                                let rotation = double -tileRotation * RadiansToDegrees // negation for right-handedness
                                let mutable rotationCenter = SDL.SDL_Point ()
                                rotationCenter.x <- int <| tileSize.X * 0.5f
                                rotationCenter.y <- int <| tileSize.Y * 0.5f
                                refTileSourceRect := sourceRect
                                refTileDestRect := destRect
                                refTileRotationCenter := rotationCenter
                                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, refTileSourceRect, refTileDestRect, rotation, refTileRotationCenter, SDL.SDL_RendererFlip.SDL_FLIP_NONE) // TODO: implement tile flip
                                if renderResult <> 0 then note <| "Rendering error - could not render texture for tile '" + tcstring descriptor + "' due to '" + SDL.SDL_GetError () + ".")
                        tiles
                    renderer
                | _ -> trace "Cannot render tile with a non-texture asset."; renderer
            | None -> note <| "TileLayerDescriptor failed due to unloadable assets for '" + tcstring tileSetImage + "'."; renderer
    
        static member private renderTextDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : TextDescriptor) renderer =
            let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
            let positionView = descriptor.Position * view
            let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
            let text = descriptor.Text
            let font = descriptor.Font
            let color = descriptor.Color
            let (renderer, optRenderAsset) = Renderer.tryLoadRenderAsset font.PackageName font.FontAssetName renderer
            match optRenderAsset with
            | Some renderAsset ->
                match renderAsset with
                | FontAsset (font, _) ->
                    let mutable renderColor = SDL.SDL_Color ()
                    renderColor.r <- byte <| color.X * 255.0f
                    renderColor.g <- byte <| color.Y * 255.0f
                    renderColor.b <- byte <| color.Z * 255.0f
                    renderColor.a <- byte <| color.W * 255.0f
                    // TODO: make the following code exception safe!
                    // TODO: the resource implications (perf and vram fragmentation?) of creating and destroying a
                    // texture one or more times a frame must be understood! Although, maybe it all happens in software
                    // and vram frag would not be a concern in the first place... perf could still be, however.
                    let textSizeX = int sizeView.X
                    let textSurface = SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, renderColor, uint32 textSizeX)
                    if textSurface <> IntPtr.Zero then
                        let textTexture = SDL.SDL_CreateTextureFromSurface (renderer.RenderContext, textSurface)
                        let textureFormat = ref 0u
                        let textureAccess = ref 0
                        let textureSizeX = ref 0
                        let textureSizeY = ref 0
                        ignore <| SDL.SDL_QueryTexture (textTexture, textureFormat, textureAccess, textureSizeX, textureSizeY)
                        let mutable sourceRect = SDL.SDL_Rect ()
                        sourceRect.x <- 0
                        sourceRect.y <- 0
                        sourceRect.w <- !textureSizeX
                        sourceRect.h <- !textureSizeY
                        let mutable destRect = SDL.SDL_Rect ()
                        destRect.x <- int <| positionView.X + camera.EyeSize.X * 0.5f
                        destRect.y <- int <| -positionView.Y + camera.EyeSize.Y * 0.5f - single !textureSizeY // negation for right-handedness
                        destRect.w <- !textureSizeX
                        destRect.h <- !textureSizeY
                        if textTexture <> IntPtr.Zero then ignore <| SDL.SDL_RenderCopy (renderer.RenderContext, textTexture, ref sourceRect, ref destRect)
                        SDL.SDL_DestroyTexture textTexture
                        SDL.SDL_FreeSurface textSurface
                    renderer
                | _ -> trace "Cannot render text with a non-font asset."; renderer
            | None -> note <| "TextDescriptor failed due to unloadable assets for '" + tcstring font + "'."; renderer

        static member private renderLayerableDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera renderer layerableDescriptor =
            match layerableDescriptor with
            | SpriteDescriptor descriptor -> Renderer.renderSpriteDescriptor viewAbsolute viewRelative camera descriptor renderer
            | TileLayerDescriptor descriptor -> Renderer.renderTileLayerDescriptor viewAbsolute viewRelative camera descriptor renderer
            | TextDescriptor descriptor -> Renderer.renderTextDescriptor viewAbsolute viewRelative camera descriptor renderer

        static member private renderDescriptors camera renderDescriptorsValue renderer =
            let renderContext = renderer.RenderContext
            let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
            match targetResult with
            | 0 ->
                ignore <| SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD)
                let renderDescriptorsSorted = List.sortBy (fun (LayerableDescriptor descriptor) -> descriptor.Depth) renderDescriptorsValue
                let layeredDescriptors = List.map (fun (LayerableDescriptor descriptor) -> descriptor.LayeredDescriptor) renderDescriptorsSorted
                let viewAbsolute = Matrix3.InvertView <| Camera.getViewAbsoluteI camera
                let viewRelative = Matrix3.InvertView <| Camera.getViewRelativeI camera
                List.fold (Renderer.renderLayerableDescriptor viewAbsolute viewRelative camera) renderer layeredDescriptors
            | _ ->
                trace <| "Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + "."
                renderer

        /// Make a Renderer.
        static member make renderContext assetGraphFilePath =
            let renderer =
                { RenderContext = renderContext
                  RenderAssetMap = Map.empty
                  AssetGraphFilePath = assetGraphFilePath }
            renderer :> IRenderer

        interface IRenderer with

            member renderer.HandleRenderExit () =
                let renderAssetMaps = Map.toValueSeq renderer.RenderAssetMap
                let renderAssets = Seq.collect Map.toValueSeq renderAssetMaps
                for renderAsset in renderAssets do Renderer.freeRenderAsset renderAsset
                let renderer = { renderer with RenderAssetMap = Map.empty }
                renderer :> IRenderer

            member renderer.Render (camera, renderMessages, renderDescriptorsValue) =
                let renderer = Renderer.handleRenderMessages renderMessages renderer
                let renderer = Renderer.renderDescriptors camera renderDescriptorsValue renderer
                renderer :> IRenderer

    /// The mock implementation of IRenderer.
    type [<ReferenceEquality>] MockRenderer =
        { MockRenderer : unit }
        interface IRenderer with
            member renderer.HandleRenderExit () = renderer :> IRenderer
            member renderer.Render (_, _, _) = renderer :> IRenderer