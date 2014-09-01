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
open Nu.NuConstants

[<AutoOpen>]
module RenderingModule =

    /// Depicts whether a view is purposed to render in relative or absolute space. For
    /// example, UI entities are rendered in absolute space since they remain still no matter
    /// where the camera moves, and vice versa for non-UI entities.
    type ViewType =
        | Absolute
        | Relative

    /// Describes an image asset.
    type [<StructuralEquality; NoComparison; XDefaultValue (DefaultImageValue)>] Image =
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
    type [<StructuralEquality; NoComparison; XDefaultValue (DefaultTileMapAssetValue)>] TileMapAsset =
        { TileMapAssetName : string
          PackageName : string }

    /// Describes how to render a tile map to the rendering system.
    type [<StructuralEquality; NoComparison>] TileLayerDescriptor =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          ViewType : ViewType
          MapSize : int * int // TODO: replace int * ints with Vector2I.
          Tiles : TmxLayerTile List
          TileSourceSize : int * int
          TileSize : Vector2
          TileSet : TmxTileset
          TileSetImage : Image }
    
    /// Describes a font asset.
    type [<StructuralEquality; NoComparison; XDefaultValue (DefaultFontValue)>] Font =
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

    /// The renderer. Represent the rendering system in Nu generally.
    /// NOTE: you should never access the RenderContext from outside the rednering system.
    /// TODO: consider making the Renderer an abstract data type to hide the RenderContext, or
    /// at least give said field a name that communicates its desired privacy.
    type [<ReferenceEquality>] Renderer =
        { RenderContext : nativeint
          RenderAssetMap : RenderAsset AssetMap
          AssetGraphFileName : string }

    /// Converts Image types.
    type ImageTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let s = obj :?> Image
            String.Format (culture, "{0};{1}", s.ImageAssetName, s.PackageName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Image> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Image> then obj
            else
                let args = (obj :?> string).Split ';'
                { ImageAssetName = args.[0]; PackageName = args.[1] } :> obj

    /// Converts TileMapAsset types.
    type TileMapAssetTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let s = obj :?> TileMapAsset
            String.Format (culture, "{0};{1}", s.TileMapAssetName, s.PackageName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Image> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<TileMapAsset> then obj
            else
                let args = (obj :?> string).Split ';'
                { TileMapAssetName = args.[0]; PackageName = args.[1] } :> obj

    /// Converts Font types.
    type FontTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let s = obj :?> Font
            String.Format (culture, "{0};{1}", s.FontAssetName, s.PackageName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Font> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Font> then obj
            else
                let args = (obj :?> string).Split ';'
                { FontAssetName = args.[0]; PackageName = args.[1] } :> obj

[<RequireQualifiedAccess>]
module Rendering =

    /// Initializes the type converters found in RenderingModule.
    let initTypeConverters () =
        assignTypeConverter<Image, ImageTypeConverter> ()
        assignTypeConverter<Font, FontTypeConverter> ()
        assignTypeConverter<TileMapAsset, TileMapAssetTypeConverter> ()

    let private freeRenderAsset renderAsset =
        match renderAsset with
        | TextureAsset texture -> SDL.SDL_DestroyTexture texture
        | FontAsset (font, _) -> SDL_ttf.TTF_CloseFont font

    let private tryLoadRenderAsset2 renderContext (asset : Asset) =
        let extension = Path.GetExtension asset.FileName
        match extension with
        | ".bmp"
        | ".png" ->
            let optTexture = SDL_image.IMG_LoadTexture (renderContext, asset.FileName)
            if optTexture <> IntPtr.Zero then Some (asset.Name, TextureAsset optTexture)
            else
                let errorMsg = SDL.SDL_GetError ()
                trace <| "Could not load texture '" + asset.FileName + "' due to '" + errorMsg + "'."
                None
        | ".ttf" ->
            let fileFirstName = Path.GetFileNameWithoutExtension asset.FileName
            let fileFirstNameLength = String.length fileFirstName
            if fileFirstNameLength >= 3 then
                let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                let fontSize = ref 0
                if Int32.TryParse (fontSizeText, fontSize) then
                    let optFont = SDL_ttf.TTF_OpenFont (asset.FileName, !fontSize)
                    if optFont <> IntPtr.Zero
                    then Some (asset.Name, FontAsset (optFont, !fontSize))
                    else trace <| "Could not load font due to unparsable font size in file name '" + asset.FileName + "'."; None
                else trace <| "Could not load font due to file name being too short: '" + asset.FileName + "'."; None
            else trace <| "Could not load font '" + asset.FileName + "'."; None
        | _ -> trace <| "Could not load render asset '" + string asset + "' due to unknown extension '" + extension + "'."; None

    let private tryLoadRenderPackage packageName renderer =
        let optAssets = Assets.tryLoadAssetsFromPackage (Some RenderingAssociation) packageName renderer.AssetGraphFileName
        match optAssets with
        | Right assets ->
            let optRenderAssets = List.map (tryLoadRenderAsset2 renderer.RenderContext) assets
            let renderAssets = List.definitize optRenderAssets
            let optRenderAssetMap = Map.tryFind packageName renderer.RenderAssetMap
            match optRenderAssetMap with
            | None ->
                let renderAssetMap = Map.ofSeq renderAssets
                { renderer with RenderAssetMap = Map.add packageName renderAssetMap renderer.RenderAssetMap }
            | Some renderAssetMap ->
                let renderAssetMap = Map.addMany renderAssets renderAssetMap
                { renderer with RenderAssetMap = Map.add packageName renderAssetMap renderer.RenderAssetMap }
        | Left error ->
            note <| "HintRenderingPackageUseMessage failed due unloadable assets '" + error + "' for '" + string (packageName, renderer.AssetGraphFileName) + "'."
            renderer

    let private tryLoadRenderAsset packageName assetName renderer =
        let optAssetMap = Map.tryFind packageName renderer.RenderAssetMap
        let (renderer, optAssetMap) =
            match optAssetMap with
            | None ->
                note <| "Loading render package '" + packageName + "' for asset '" + assetName + "' on the fly."
                let renderer = tryLoadRenderPackage packageName renderer
                (renderer, Map.tryFind packageName renderer.RenderAssetMap)
            | Some _ -> (renderer, Map.tryFind packageName renderer.RenderAssetMap)
        (renderer, Option.bind (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap)

    let private handleHintRenderingPackageUse (hintPackageUse : HintRenderingPackageUseMessage) renderer =
        tryLoadRenderPackage hintPackageUse.PackageName renderer
    
    let private handleHintRenderingPackageDisuse (hintPackageDisuse : HintRenderingPackageDisuseMessage) renderer =
        let packageName = hintPackageDisuse.PackageName
        let optAssets = Map.tryFind packageName renderer.RenderAssetMap
        match optAssets with
        | None -> renderer
        | Some assets ->
            for asset in Map.toValueList assets do freeRenderAsset asset
            { renderer with RenderAssetMap = Map.remove packageName renderer.RenderAssetMap }

    let private handleReloadRenderingAssets renderer =
        let oldAssetMap = renderer.RenderAssetMap
        let renderer = { renderer with RenderAssetMap = Map.empty }
        List.fold
            (fun renderer packageName -> tryLoadRenderPackage packageName renderer)
            renderer
            (Map.toKeyList oldAssetMap)

    let private handleRenderMessage renderer renderMessage =
        match renderMessage with
        | HintRenderingPackageUseMessage hintPackageUse -> handleHintRenderingPackageUse hintPackageUse renderer
        | HintRenderingPackageDisuseMessage hintPackageDisuse -> handleHintRenderingPackageDisuse hintPackageDisuse renderer
        | ReloadRenderingAssetsMessage  -> handleReloadRenderingAssets renderer

    let private handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
        List.fold handleRenderMessage renderer (List.rev renderMessages)

    let private renderSpriteDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : SpriteDescriptor) renderer =
        let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
        let positionView = descriptor.Position * view
        let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
        let image = descriptor.Image
        let color = descriptor.Color
        let (renderer, optRenderAsset) = tryLoadRenderAsset image.PackageName image.ImageAssetName renderer
        match optRenderAsset with
        | None ->
            debug <| "SpriteDescriptor failed due to unloadable assets for '" + string image + "'."
            renderer
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
                | None ->
                    sourceRect.x <- 0
                    sourceRect.y <- 0
                    sourceRect.w <- !textureSizeX
                    sourceRect.h <- !textureSizeY
                | Some inset ->
                    sourceRect.x <- int inset.X
                    sourceRect.y <- int inset.Y
                    sourceRect.w <- int <| inset.Z - inset.X
                    sourceRect.h <- int <| inset.W - inset.Y
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
                if renderResult <> 0 then debug <| "Rendering error - could not render texture for sprite '" + string descriptor + "' due to '" + SDL.SDL_GetError () + "."
                renderer
            | _ ->
                trace "Cannot render sprite with a non-texture asset."
                renderer

    let private renderTileLayerDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : TileLayerDescriptor) renderer =
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
        let (renderer, optRenderAsset) = tryLoadRenderAsset tileSetImage.PackageName tileSetImage.ImageAssetName renderer
        match optRenderAsset with
        | None ->
            debug <| "TileLayerDescriptor failed due to unloadable assets for '" + string tileSetImage + "'."
            renderer
        | Some renderAsset ->
            match renderAsset with
            | TextureAsset texture ->
                Seq.iteri
                    (fun n _ ->
                        let mapRun = fst mapSize
                        let (i, j) = (n % mapRun, n / mapRun)
                        let tilePosition =
                            Vector2 (
                                positionView.X + tileSize.X * single i + camera.EyeSize.X * 0.5f,
                                -(positionView.Y - tileSize.Y * single j + sizeView.Y) + camera.EyeSize.Y * 0.5f) // negation for right-handedness
                        if NuMath.isBoundsInBounds3 tilePosition tileSize <| Vector4 (0.0f, 0.0f, camera.EyeSize.X, camera.EyeSize.Y) then
                            let gid = tiles.[n].Gid - tileSet.FirstGid
                            let gidPosition = gid * fst tileSourceSize
                            let tileSourcePosition =
                                Vector2 (
                                    single <| gidPosition % tileSetWidth,
                                    single <| gidPosition / tileSetWidth * snd tileSourceSize)
                            let mutable sourceRect = SDL.SDL_Rect ()
                            sourceRect.x <- int tileSourcePosition.X
                            sourceRect.y <- int tileSourcePosition.Y
                            sourceRect.w <- fst tileSourceSize
                            sourceRect.h <- snd tileSourceSize
                            let mutable destRect = SDL.SDL_Rect ()
                            destRect.x <- int tilePosition.X
                            destRect.y <- int tilePosition.Y
                            destRect.w <- int tileSize.X
                            destRect.h <- int tileSize.Y
                            let rotation = double -tileRotation * RadiansToDegrees // negation for right-handedness
                            let mutable rotationCenter = SDL.SDL_Point ()
                            rotationCenter.x <- int <| tileSize.X * 0.5f
                            rotationCenter.y <- int <| tileSize.Y * 0.5f
                            let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, ref sourceRect, ref destRect, rotation, ref rotationCenter, SDL.SDL_RendererFlip.SDL_FLIP_NONE) // TODO: implement tile flip
                            if renderResult <> 0 then debug <| "Rendering error - could not render texture for tile '" + string descriptor + "' due to '" + SDL.SDL_GetError () + ".")
                    tiles
                renderer
            | _ ->
                trace "Cannot render tile with a non-texture asset."
                renderer
    
    let private renderTextDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera (descriptor : TextDescriptor) renderer =
        let view = match descriptor.ViewType with Absolute -> viewAbsolute | Relative -> viewRelative
        let positionView = descriptor.Position * view
        let sizeView = descriptor.Size * view.ExtractScaleMatrix ()
        let text = descriptor.Text
        let font = descriptor.Font
        let color = descriptor.Color
        let (renderer, optRenderAsset) = tryLoadRenderAsset font.PackageName font.FontAssetName renderer
        match optRenderAsset with
        | None ->
            debug <| "TextDescriptor failed due to unloadable assets for '" + string font + "'."
            renderer
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
            | _ ->
                trace "Cannot render text with a non-font asset."
                renderer

    let private renderLayerableDescriptor (viewAbsolute : Matrix3) (viewRelative : Matrix3) camera renderer layerableDescriptor =
        match layerableDescriptor with
        | SpriteDescriptor descriptor -> renderSpriteDescriptor viewAbsolute viewRelative camera descriptor renderer
        | TileLayerDescriptor descriptor -> renderTileLayerDescriptor viewAbsolute viewRelative camera descriptor renderer
        | TextDescriptor descriptor -> renderTextDescriptor viewAbsolute viewRelative camera descriptor renderer

    let private renderDescriptors camera renderDescriptorsValue renderer =
        let renderContext = renderer.RenderContext
        let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
        match targetResult with
        | 0 ->
            ignore <| SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD)
            let renderDescriptorsSorted = List.sortBy (fun (LayerableDescriptor descriptor) -> descriptor.Depth) renderDescriptorsValue
            let layeredDescriptors = List.map (fun (LayerableDescriptor descriptor) -> descriptor.LayeredDescriptor) renderDescriptorsSorted
            let viewAbsolute = Matrix3.InvertView <| Camera.getViewAbsoluteI camera
            let viewRelative = Matrix3.InvertView <| Camera.getViewRelativeI camera
            List.fold (renderLayerableDescriptor viewAbsolute viewRelative camera) renderer layeredDescriptors
        | _ ->
            trace <| "Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + "."
            renderer

    /// Handle render exit by freeing all loaded render assets.
    let handleRenderExit renderer =
        let renderAssetMaps = Map.toValueSeq renderer.RenderAssetMap
        let renderAssets = Seq.collect Map.toValueSeq renderAssetMaps
        for renderAsset in renderAssets do freeRenderAsset renderAsset
        { renderer with RenderAssetMap = Map.empty }

    /// Render a frame of the game.
    let render camera (renderMessages : RenderMessage rQueue) renderDescriptorsValue renderer =
        let renderer = handleRenderMessages renderMessages renderer
        renderDescriptors camera renderDescriptorsValue renderer

    /// Make a Renderer.
    let makeRenderer renderContext assetGraphFileName =
        { RenderContext = renderContext
          RenderAssetMap = Map.empty
          AssetGraphFileName = assetGraphFileName }