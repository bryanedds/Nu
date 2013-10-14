module Nu.Rendering
open System
open System.IO
open OpenTK
open SDL2
open Nu.Core
open Nu.Assets

type [<StructuralEquality; NoComparison>] Sprite =
    { SpriteAssetName : Lun
      PackageName : Lun }

type [<StructuralEquality; NoComparison>] SpriteDescriptor =
    { Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single
      Sprite : Sprite }

/// Describes a rendering asset.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor

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

type [<ReferenceEquality>] Renderer =
    private
        { RenderContext : nativeint
          RenderAssetMap : RenderAsset AssetMap }

let tryLoadRenderAsset renderContext (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".bmp"
    | ".png" ->
        let optTexture = SDL_image.IMG_LoadTexture (renderContext, asset.FileName)
        if optTexture = IntPtr.Zero then
            trace ("Could not load texture '" + asset.FileName + "'.")
            None
        else
            Some (Lun.make asset.Name, TextureAsset optTexture)
    | _ ->
        trace ("Could not load render asset '" + str asset + "' due to unknown extension '" + extension + "'.")
        None

let handleRenderMessage renderer renderMessage =
    match renderMessage with
    | HintRenderingPackageUse hintPackageUse ->
        let optAssets = Assets.tryLoadAssets "Rendering" hintPackageUse.PackageName hintPackageUse.FileName
        match optAssets with
        | Left error ->
            trace ("HintRenderingPackageUse failed due unloadable assets '" + error + "' for '" + str hintPackageUse + "'.")
            renderer
        | Right assets ->
            let optRenderAssets = List.map (tryLoadRenderAsset renderer.RenderContext) assets
            let renderAssets = List.definitize optRenderAssets
            let packageNameLun = Lun.make hintPackageUse.PackageName
            let optRenderAssetMap = Map.tryFind packageNameLun renderer.RenderAssetMap
            match optRenderAssetMap with
            | None ->
                let renderAssetMap = Map.ofSeq renderAssets
                { renderer with RenderAssetMap = Map.add packageNameLun renderAssetMap renderer.RenderAssetMap }
            | Some renderAssetMap ->
                let renderAssetMap2 = Map.addMany renderAssets renderAssetMap
                { renderer with RenderAssetMap = Map.add packageNameLun renderAssetMap2 renderer.RenderAssetMap }
    | HintRenderingPackageDisuse hintPackageDisuse ->
        let optAssets = Assets.tryLoadAssets "Rendering" hintPackageDisuse.PackageName hintPackageDisuse.FileName
        match optAssets with
        | Left error ->
            trace ("HintRenderingPackageDisuse failed due unloadable assets '" + error + "' for '" + str hintPackageDisuse + "'.")
            renderer
        | Right assets -> renderer // TODO: unload assets
    | ScreenFlash -> renderer // TODO: render screen flash for one frame

let handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
    List.fold handleRenderMessage renderer (List.rev renderMessages)

let doRender renderDescriptors renderer =
    let renderContext = renderer.RenderContext
    let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
    match targetResult with
    | 0 ->
        ignore (SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD))
        let spriteDescriptors = List.map (fun (SpriteDescriptor descriptor) -> descriptor) renderDescriptors
        //let (spriteDescriptors, renderDescriptors_) = List.partitionPlus (fun descriptor -> match descriptor with SpriteDescriptor spriteDescriptor -> Some spriteDescriptor (*| _ -> None*)) renderDescriptors
        let sortedSpriteDescriptors = Seq.sortBy (fun descriptor -> descriptor.Depth) spriteDescriptors
        for spriteDescriptor in sortedSpriteDescriptors do
            let optRenderAsset =
                Option.reduce
                    (fun assetMap -> Map.tryFind spriteDescriptor.Sprite.SpriteAssetName assetMap)
                    (Map.tryFind spriteDescriptor.Sprite.PackageName renderer.RenderAssetMap)
            match optRenderAsset with
            | None -> ()
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    let mutable sourceRect = SDL.SDL_Rect ()
                    sourceRect.x <- 0
                    sourceRect.y <- 0
                    sourceRect.w <- int spriteDescriptor.Size.X
                    sourceRect.h <- int spriteDescriptor.Size.Y
                    let mutable destRect = SDL.SDL_Rect ()
                    destRect.x <- int spriteDescriptor.Position.X
                    destRect.y <- int spriteDescriptor.Position.Y
                    destRect.w <- int spriteDescriptor.Size.X
                    destRect.h <- int spriteDescriptor.Size.Y
                    let mutable rotationCenter = SDL.SDL_Point ()
                    rotationCenter.x <- int (spriteDescriptor.Size.X * 0.5f)
                    rotationCenter.y <- int (spriteDescriptor.Size.Y * 0.5f)
                    let renderResult =
                        SDL.SDL_RenderCopyEx (
                            renderContext,
                            texture,
                            ref sourceRect,
                            ref destRect,
                            double spriteDescriptor.Rotation * 57.2957795,
                            ref rotationCenter,
                            SDL.SDL_RendererFlip.SDL_FLIP_NONE)
                    match renderResult with
                    | 0 -> ()
                    | _ -> debug ("Rendering error - could not render texture for sprite '" + str spriteDescriptor + "' due to '" + SDL.SDL_GetError () + ".")
                //| _ -> trace "Cannot sprite render with a non-texture asset."
    | _ -> trace ("Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")

let render (renderMessages : RenderMessage rQueue) renderDescriptors renderer =
    let renderer2 = handleRenderMessages renderMessages renderer
    doRender renderDescriptors renderer2
    renderer2

let handleRenderExit renderer =
    let renderAssetMaps = Map.toValueSeq renderer.RenderAssetMap
    let renderAssets = Seq.collect Map.toValueSeq renderAssetMaps
    for renderAsset in renderAssets do
        match renderAsset with
        | TextureAsset texture -> () // apparently there is no need to free textures in SDL
    { renderer with RenderAssetMap = Map.empty }

let makeRenderer renderContext =
    { RenderContext = renderContext; RenderAssetMap = Map.empty }