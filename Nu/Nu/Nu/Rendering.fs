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
open Nu.NuCore
open Nu.NuConstants
open Nu.Assets

[<AutoOpen>]
module RenderingModule =

    type [<StructuralEquality; NoComparison>] Sprite =
        { SpriteAssetName : Lun
          PackageName : Lun
          PackageFileName : string }

    type [<StructuralEquality; NoComparison>] SpriteDescriptor =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          Sprite : Sprite
          Color : Vector4 }

    type [<StructuralEquality; NoComparison>] TileMapAsset =
        { TileMapAssetName : Lun
          PackageName : Lun
          PackageFileName : string }

    type [<StructuralEquality; NoComparison>] TileLayerDescriptor =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          MapSize : Vector2
          Tiles : TmxLayerTile Collections.Generic.List
          TileSize : Vector2
          TileSet : TmxTileset
          TileSetSprite : Sprite }

    type [<StructuralEquality; NoComparison>] Font =
        { FontAssetName : Lun
          PackageName : Lun
          PackageFileName : string }

    type [<StructuralEquality; NoComparison>] TextDescriptor =
        { Text : string
          Position : Vector2
          Size : Vector2
          Font : Font
          Color : Vector4 }

    type [<StructuralEquality; NoComparison>] 'd LayeredDescriptor =
        { Descriptor : 'd
          Depth : single }

    type [<StructuralEquality; NoComparison>] LayerableDescriptor =
        | LayeredSpriteDescriptor of SpriteDescriptor LayeredDescriptor
        | LayeredTileLayerDescriptor of TileLayerDescriptor LayeredDescriptor
        | LayeredTextDescriptor of TextDescriptor LayeredDescriptor

    /// Describes a rendering asset.
    /// A serializable value type.
    type [<StructuralEquality; NoComparison>] RenderDescriptor =
        | LayerableDescriptor of LayerableDescriptor

    type [<StructuralEquality; NoComparison>] HintRenderingPackageUse =
        { FileName : string
          PackageName : string
          HRPU : unit }

    type [<StructuralEquality; NoComparison>] HintRenderingPackageDisuse =
        { FileName : string
          PackageName : string
          HRPD : unit }

    type [<StructuralEquality; NoComparison>] RenderMessage =
        | HintRenderingPackageUse of HintRenderingPackageUse
        | HintRenderingPackageDisuse of HintRenderingPackageDisuse
        | ScreenFlash // of ...

    type [<ReferenceEquality>] RenderAsset =
        | TextureAsset of nativeint
        | FontAsset of nativeint * int

    type [<ReferenceEquality>] Renderer =
        { RenderContext : nativeint
          RenderAssetMap : RenderAsset AssetMap }

    type SpriteTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj : obj, _) =
            let s = obj :?> Sprite
            String.Format (culture, "{0};{1};{2}", s.SpriteAssetName, s.PackageName, s.PackageFileName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Sprite> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Sprite> then obj
            else
                let args = (obj :?> string).Split ';'
                { SpriteAssetName = Lun.make args.[0]; PackageName = Lun.make args.[1]; PackageFileName = args.[2] } :> obj

    type FontTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj : obj, _) =
            let s = obj :?> Font
            String.Format (culture, "{0};{1};{2}", s.FontAssetName, s.PackageName, s.PackageFileName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Font> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Font> then obj
            else
                let args = (obj :?> string).Split ';'
                { FontAssetName = Lun.make args.[0]; PackageName = Lun.make args.[1]; PackageFileName = args.[2] } :> obj

    type TileMapAssetTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj : obj, _) =
            let s = obj :?> TileMapAsset
            String.Format (culture, "{0};{1};{2}", s.TileMapAssetName, s.PackageName, s.PackageFileName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Sprite> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<TileMapAsset> then obj
            else
                let args = (obj :?> string).Split ';'
                { TileMapAssetName = Lun.make args.[0]; PackageName = Lun.make args.[1]; PackageFileName = args.[2] } :> obj

module Rendering =

    let initRenderConverters () =
        assignTypeConverter<Sprite, SpriteTypeConverter> ()
        assignTypeConverter<Font, FontTypeConverter> ()
        assignTypeConverter<TileMapAsset, TileMapAssetTypeConverter> ()

    let private getLayerableDepth layerable =
        match layerable with
        | LayeredSpriteDescriptor descriptor -> descriptor.Depth
        | LayeredTileLayerDescriptor descriptor -> descriptor.Depth
        | LayeredTextDescriptor descriptor -> descriptor.Depth

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
            if optTexture <> IntPtr.Zero then Some (Lun.make asset.Name, TextureAsset optTexture)
            else
                trace <| "Could not load texture '" + asset.FileName + "'."
                None
        | ".ttf" ->
            let fileFirstName = Path.GetFileNameWithoutExtension asset.FileName
            let fileFirstNameLength = String.length fileFirstName
            if fileFirstNameLength >= 3 then
                let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                let fontSize = ref 0
                if Int32.TryParse (fontSizeText, fontSize) then
                    let optFont = SDL_ttf.TTF_OpenFont (asset.FileName, !fontSize)
                    if optFont <> IntPtr.Zero then Some (Lun.make asset.Name, FontAsset (optFont, !fontSize))
                    else trace <| "Could not load font due to unparsable font size in file name '" + asset.FileName + "'."; None
                else trace <| "Could not load font due to file name being too short: '" + asset.FileName + "'."; None
            else trace <| "Could not load font '" + asset.FileName + "'."; None
        | _ ->
            trace <| "Could not load render asset '" + str asset + "' due to unknown extension '" + extension + "'."
            None

    let private tryLoadRenderPackage packageName fileName renderer =
        let optAssets = tryLoadAssets "Rendering" packageName.LunStr fileName
        match optAssets with
        | Left error ->
            note <| "HintRenderingPackageUse failed due unloadable assets '" + error + "' for '" + str (packageName, fileName) + "'."
            renderer
        | Right assets ->
            let optRenderAssets = List.map (tryLoadRenderAsset2 renderer.RenderContext) assets
            let renderAssets = List.definitize optRenderAssets
            let optRenderAssetMap = Map.tryFind packageName renderer.RenderAssetMap
            match optRenderAssetMap with
            | None ->
                let renderAssetMap = Map.ofSeq renderAssets
                { renderer with RenderAssetMap = Map.add packageName renderAssetMap renderer.RenderAssetMap }
            | Some renderAssetMap ->
                let renderAssetMap' = Map.addMany renderAssets renderAssetMap
                { renderer with RenderAssetMap = Map.add packageName renderAssetMap' renderer.RenderAssetMap }

    let private tryLoadRenderAsset packageName packageFileName assetName renderer_ =
        let optAssetMap = Map.tryFind packageName renderer_.RenderAssetMap
        let (renderer_, optAssetMap_) =
            match optAssetMap with
            | None ->
                note <| "Loading render package '" + packageName.LunStr + "' for asset '" + assetName.LunStr + "' on the fly."
                let renderer_ = tryLoadRenderPackage packageName packageFileName renderer_
                (renderer_, Map.tryFind packageName renderer_.RenderAssetMap)
            | Some assetMap -> (renderer_, Map.tryFind packageName renderer_.RenderAssetMap)
        (renderer_, Option.bind (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap_)

    let private handleHintRenderingPackageUse (hintPackageUse : HintRenderingPackageUse) renderer =
        tryLoadRenderPackage (Lun.make hintPackageUse.PackageName) hintPackageUse.FileName renderer
    
    let private handleHintRenderingPackageDisuse (hintPackageDisuse : HintRenderingPackageDisuse) renderer =
        let packageNameLun = Lun.make hintPackageDisuse.PackageName
        let optAssets = Map.tryFind packageNameLun renderer.RenderAssetMap
        match optAssets with
        | None -> renderer
        | Some assets ->
            for asset in Map.toValueList assets do freeRenderAsset asset
            { renderer with RenderAssetMap = Map.remove packageNameLun renderer.RenderAssetMap }

    let private handleRenderMessage renderer renderMessage =
        match renderMessage with
        | HintRenderingPackageUse hintPackageUse -> handleHintRenderingPackageUse hintPackageUse renderer
        | HintRenderingPackageDisuse hintPackageDisuse -> handleHintRenderingPackageDisuse hintPackageDisuse renderer
        | ScreenFlash -> renderer // TODO: render screen flash for one frame

    let private handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
        List.fold handleRenderMessage renderer (List.rev renderMessages)

    // TODO: factor out with extraction when available from the IDE
    let private renderLayerableDescriptor renderer layerableDescriptor =
        match layerableDescriptor with
        | LayeredSpriteDescriptor lsd ->
            let spriteDescriptor = lsd.Descriptor
            let sprite = spriteDescriptor.Sprite
            let color = spriteDescriptor.Color
            let (renderer', optRenderAsset) = tryLoadRenderAsset sprite.PackageName sprite.PackageFileName sprite.SpriteAssetName renderer
            match optRenderAsset with
            | None ->
                note <| "LayeredSpriteDescriptor failed due to unloadable assets for '" + str sprite + "'."
                renderer'
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    let mutable textureFormat = ref 0u
                    let mutable textureAccess = ref 0
                    let mutable textureSizeX = ref 0
                    let mutable textureSizeY = ref 0
                    ignore <| SDL.SDL_QueryTexture (texture, textureFormat, textureAccess, textureSizeX, textureSizeY)
                    let mutable sourceRect = SDL.SDL_Rect ()
                    sourceRect.x <- 0
                    sourceRect.y <- 0
                    sourceRect.w <- !textureSizeX
                    sourceRect.h <- !textureSizeY
                    let mutable destRect = SDL.SDL_Rect ()
                    destRect.x <- int spriteDescriptor.Position.X
                    destRect.y <- int spriteDescriptor.Position.Y
                    destRect.w <- int spriteDescriptor.Size.X
                    destRect.h <- int spriteDescriptor.Size.Y
                    let mutable rotationCenter = SDL.SDL_Point ()
                    rotationCenter.x <- int <| spriteDescriptor.Size.X * 0.5f
                    rotationCenter.y <- int <| spriteDescriptor.Size.Y * 0.5f
                    ignore <| SDL.SDL_SetTextureColorMod (texture, byte <| 255.0f * color.X, byte <| 255.0f * color.Y, byte <| 255.0f * color.Z)
                    ignore <| SDL.SDL_SetTextureAlphaMod (texture, byte <| 255.0f * color.W)
                    let renderResult =
                        SDL.SDL_RenderCopyEx (
                            renderer'.RenderContext,
                            texture,
                            ref sourceRect,
                            ref destRect,
                            double spriteDescriptor.Rotation * RadiansToDegrees,
                            ref rotationCenter,
                            SDL.SDL_RendererFlip.SDL_FLIP_NONE)
                    if renderResult <> 0 then debug <| "Rendering error - could not render texture for sprite '" + str spriteDescriptor + "' due to '" + SDL.SDL_GetError () + "."
                    renderer'
                | _ ->
                    trace "Cannot render sprite with a non-texture asset."
                    renderer'
        | LayeredTileLayerDescriptor ltd ->
            let descriptor = ltd.Descriptor
            let tiles = descriptor.Tiles
            let tileSet = descriptor.TileSet
            let mapSize = descriptor.MapSize
            let tileSize = descriptor.TileSize
            let tileRotation = descriptor.Rotation
            let sprite = descriptor.TileSetSprite
            let optTileSetWidth = tileSet.Image.Width
            let tileSetWidth = optTileSetWidth.Value
            let (renderer', optRenderAsset) = tryLoadRenderAsset sprite.PackageName sprite.PackageFileName sprite.SpriteAssetName renderer
            match optRenderAsset with
            | None ->
                debug <| "LayeredTileLayerDescriptor failed due to unloadable assets for '" + str sprite + "'."
                renderer'
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    Seq.iteri
                        (fun n tile ->
                            let (i, j) = (n % (int mapSize.X), n / (int mapSize.Y))
                            let tilePosition = Vector2 (descriptor.Position.X + (tileSize.Y * single i), descriptor.Position.Y + (tileSize.Y * single j))
                            let gid = tiles.[n].Gid - tileSet.FirstGid
                            let gidPosition = gid * int tileSize.X
                            let tileSetPosition = Vector2 (single <| gidPosition % tileSetWidth, (single <| gidPosition / tileSetWidth) * tileSize.Y)
                            let mutable sourceRect = SDL.SDL_Rect ()
                            sourceRect.x <- int tileSetPosition.X
                            sourceRect.y <- int tileSetPosition.Y
                            sourceRect.w <- int tileSize.X
                            sourceRect.h <- int tileSize.Y
                            let mutable destRect = SDL.SDL_Rect ()
                            destRect.x <- int tilePosition.X
                            destRect.y <- int tilePosition.Y
                            destRect.w <- int tileSize.X
                            destRect.h <- int tileSize.Y
                            let mutable rotationCenter = SDL.SDL_Point ()
                            rotationCenter.x <- int <| tileSize.X * 0.5f
                            rotationCenter.y <- int <| tileSize.Y * 0.5f
                            let renderResult =
                                SDL.SDL_RenderCopyEx (
                                    renderer'.RenderContext,
                                    texture,
                                    ref sourceRect,
                                    ref destRect,
                                    double tileRotation * RadiansToDegrees,
                                    ref rotationCenter,
                                    SDL.SDL_RendererFlip.SDL_FLIP_NONE) // TODO: implement tile flip
                            if renderResult <> 0 then debug <| "Rendering error - could not render texture for tile '" + str descriptor + "' due to '" + SDL.SDL_GetError () + ".")
                        tiles
                    renderer'
                | _ ->
                    trace "Cannot render tile with a non-texture asset."
                    renderer'
        | LayeredTextDescriptor ltd ->
            let textDescriptor = ltd.Descriptor
            let font = textDescriptor.Font
            let (renderer', optRenderAsset) = tryLoadRenderAsset font.PackageName font.PackageFileName font.FontAssetName renderer
            match optRenderAsset with
            | None ->
                debug <| "LayeredTextDescriptor failed due to unloadable assets for '" + str font + "'."
                renderer'
            | Some renderAsset ->
                match renderAsset with
                | FontAsset (font, _) ->
                    let mutable color = SDL.SDL_Color ()
                    let textPositionX = int textDescriptor.Position.X
                    let textPositionY = int textDescriptor.Position.Y
                    let textSizeX = int textDescriptor.Size.X
                    let textSizeY = int textDescriptor.Size.Y
                    color.r <- byte <| textDescriptor.Color.X * 255.0f
                    color.g <- byte <| textDescriptor.Color.Y * 255.0f
                    color.b <- byte <| textDescriptor.Color.Z * 255.0f
                    color.a <- byte <| textDescriptor.Color.W * 255.0f
                    // NOTE: the following code is not exception safe!
                    // TODO: the resource implications (perf and vram fragmentation?) of creating and
                    // destroying a texture one or more times a frame must be understood!
                    let textSurface = SDL_ttf.TTF_RenderText_Blended_Wrapped (font, textDescriptor.Text, color, uint32 textSizeX)
                    if textSurface <> IntPtr.Zero then
                        let textTexture = SDL.SDL_CreateTextureFromSurface (renderer.RenderContext, textSurface)
                        let mutable textureFormat = ref 0u
                        let mutable textureAccess = ref 0
                        let mutable textureSizeX = ref 0
                        let mutable textureSizeY = ref 0
                        ignore <| SDL.SDL_QueryTexture (textTexture, textureFormat, textureAccess, textureSizeX, textureSizeY)
                        let mutable sourceRect = SDL.SDL_Rect ()
                        sourceRect.x <- 0
                        sourceRect.y <- 0
                        sourceRect.w <- !textureSizeX
                        sourceRect.h <- !textureSizeY
                        let mutable destRect = SDL.SDL_Rect ()
                        destRect.x <- textPositionX
                        destRect.y <- textPositionY
                        destRect.w <- !textureSizeX
                        destRect.h <- !textureSizeY
                        if textTexture <> IntPtr.Zero then ignore (SDL.SDL_RenderCopy (renderer.RenderContext, textTexture, ref sourceRect, ref destRect))
                        SDL.SDL_DestroyTexture textTexture
                        SDL.SDL_FreeSurface textSurface
                    renderer'
                | _ ->
                    trace "Cannot render text with a non-font asset."
                    renderer'

    let private renderDescriptors renderDescriptorsValue renderer =
        let renderContext = renderer.RenderContext
        let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
        match targetResult with
        | 0 ->
            ignore (SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD))
            let layerableDescriptors = Seq.map (fun (LayerableDescriptor descriptor) -> descriptor) renderDescriptorsValue
            //let (spriteDescriptors, renderDescriptorsValue_) = List.partitionPlus (fun descriptor -> match descriptor with SpriteDescriptor spriteDescriptor -> Some spriteDescriptor (*| _ -> None*)) renderDescriptorsValue
            let sortedDescriptors = Seq.sortBy getLayerableDepth layerableDescriptors
            Seq.fold renderLayerableDescriptor renderer sortedDescriptors
        | _ ->
            trace <| "Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + "."
            renderer

    let handleRenderExit renderer =
        let renderAssetMaps = Map.toValueSeq renderer.RenderAssetMap
        let renderAssets = Seq.collect Map.toValueSeq renderAssetMaps
        for renderAsset in renderAssets do freeRenderAsset renderAsset
        { renderer with RenderAssetMap = Map.empty }

    let render (renderMessages : RenderMessage rQueue) renderDescriptorsValue renderer =
        let renderer' = handleRenderMessages renderMessages renderer
        renderDescriptors renderDescriptorsValue renderer'

    let makeRenderer renderContext =
        { RenderContext = renderContext
          RenderAssetMap = Map.empty }