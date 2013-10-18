module Nu.Rendering
open System
open System.IO
open OpenTK
open SDL2
open Nu.Core
open Nu.Constants
open Nu.Assets

type [<StructuralEquality; NoComparison>] Sprite =
    { SpriteAssetName : Lun
      PackageName : Lun
      PackageFileName : string }

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

let tryLoadRenderAsset2 renderContext (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".bmp"
    | ".png" ->
        let optTexture = SDL_image.IMG_LoadTexture (renderContext, asset.FileName)
        if optTexture <> IntPtr.Zero then Some (Lun.make asset.Name, TextureAsset optTexture)
        else
            trace ("Could not load texture '" + asset.FileName + "'.")
            None
    | _ ->
        trace ("Could not load render asset '" + str asset + "' due to unknown extension '" + extension + "'.")
        None

let tryLoadRenderPackage packageName fileName renderer =
    let optAssets = Assets.tryLoadAssets "Rendering" packageName.LunStr fileName
    match optAssets with
    | Left error ->
        trace ("HintRenderingPackageUse failed due unloadable assets '" + error + "' for '" + str (packageName, fileName) + "'.")
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
            let renderAssetMap2 = Map.addMany renderAssets renderAssetMap
            { renderer with RenderAssetMap = Map.add packageName renderAssetMap2 renderer.RenderAssetMap }

let tryLoadRenderAsset packageName packageFileName assetName renderer =
    let optAssetMap = Map.tryFind packageName renderer.RenderAssetMap
    let (renderer_, optAssetMap_) =
        match optAssetMap with
        | None ->
            log ("Loading render package '" + packageName.LunStr + "' for asset '" + assetName.LunStr + "' on the fly.")
            let renderer_ = tryLoadRenderPackage packageName packageFileName renderer
            (renderer_, Map.tryFind packageName renderer_.RenderAssetMap)
        | Some assetMap -> (renderer, Map.tryFind packageName renderer.RenderAssetMap)
    (renderer_, Option.reduce (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap_)

let handleHintRenderingPackageUse (hintPackageUse : HintRenderingPackageUse) renderer =
    tryLoadRenderPackage (Lun.make hintPackageUse.PackageName) hintPackageUse.FileName renderer
    
let handleHintRenderingPackageDisuse (hintPackageDisuse : HintRenderingPackageDisuse) renderer =
    let packageNameLun = Lun.make hintPackageDisuse.PackageName
    let optAssets = Map.tryFind packageNameLun renderer.RenderAssetMap
    match optAssets with
    | None -> renderer
    | Some assets ->
        for asset in Map.toValueList assets do
            match asset with
            | TextureAsset textureAsset -> () // apparently there is no need to free textures in SDL
        { renderer with RenderAssetMap = Map.remove packageNameLun renderer.RenderAssetMap }

let handleRenderMessage renderer renderMessage =
    match renderMessage with
    | HintRenderingPackageUse hintPackageUse -> handleHintRenderingPackageUse hintPackageUse renderer
    | HintRenderingPackageDisuse hintPackageDisuse -> handleHintRenderingPackageDisuse hintPackageDisuse renderer
    | ScreenFlash -> renderer // TODO: render screen flash for one frame

let handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
    List.fold handleRenderMessage renderer (List.rev renderMessages)

let handleRenderExit renderer =
    let renderAssetMaps = Map.toValueSeq renderer.RenderAssetMap
    let renderAssets = Seq.collect Map.toValueSeq renderAssetMaps
    for renderAsset in renderAssets do
        match renderAsset with
        | TextureAsset texture -> () // apparently there is no need to free textures in SDL
    { renderer with RenderAssetMap = Map.empty }

let renderSpriteDescriptor renderer spriteDescriptor =
    let sprite = spriteDescriptor.Sprite
    let (renderer2, optRenderAsset) = tryLoadRenderAsset sprite.PackageName sprite.PackageFileName sprite.SpriteAssetName renderer
    match optRenderAsset with
    | None ->
        debug ("SpriteDescriptor failed due to unloadable assets for '" + str sprite + "'.")
        renderer2
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
                    renderer2.RenderContext,
                    texture,
                    ref sourceRect,
                    ref destRect,
                    double spriteDescriptor.Rotation * RadiansToDegrees,
                    ref rotationCenter,
                    SDL.SDL_RendererFlip.SDL_FLIP_NONE)
            if renderResult <> 0 then debug ("Rendering error - could not render texture for sprite '" + str spriteDescriptor + "' due to '" + SDL.SDL_GetError () + ".")
            renderer2
        //| _ -> trace "Cannot sprite render with a non-texture asset."

let renderDescriptors renderDescriptorsValue renderer =
    let renderContext = renderer.RenderContext
    let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
    match targetResult with
    | 0 ->
        ignore (SDL.SDL_SetRenderDrawBlendMode (renderContext, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD))
        let spriteDescriptors = List.map (fun (SpriteDescriptor descriptor) -> descriptor) renderDescriptorsValue
        //let (spriteDescriptors, renderDescriptorsValue_) = List.partitionPlus (fun descriptor -> match descriptor with SpriteDescriptor spriteDescriptor -> Some spriteDescriptor (*| _ -> None*)) renderDescriptorsValue
        let sortedSpriteDescriptors = Seq.sortBy (fun descriptor -> descriptor.Depth) spriteDescriptors
        Seq.fold renderSpriteDescriptor renderer sortedSpriteDescriptors
    | _ ->
        trace ("Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")
        renderer

let render (renderMessages : RenderMessage rQueue) renderDescriptorsValue renderer =
    let renderer2 = handleRenderMessages renderMessages renderer
    renderDescriptors renderDescriptorsValue renderer2

let makeRenderer renderContext =
    { RenderContext = renderContext; RenderAssetMap = Map.empty }