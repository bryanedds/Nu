module Nu.Rendering
open System
open System.IO
open OpenTK
open SDL2
open Nu.Assets

type [<StructuralEquality; NoComparison>] Sprite =
    { AssetName : Lun
      PackageName : Lun }

type [<StructuralEquality; NoComparison>] SpriteDescriptor =
    { Position : Vector2
      Size : Vector2
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
    { RenderContext : nativeint
      RenderAssetMap : RenderAsset AssetMap }

let tryLoadRenderAsset renderContext (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".bmp" ->
        // TODO: use withSdlResource here for exception safety!
        let optSurface = SDL.SDL_LoadBMP asset.FileName
        if optSurface = IntPtr.Zero then
            trace ("Could not load bitmap '" + asset.FileName + "'.")
            None
        else
            let result =
                let optTexture = SDL.SDL_CreateTextureFromSurface (renderContext, optSurface)
                if optTexture = IntPtr.Zero then
                    trace ("Could not create texture from surface for '" + asset.FileName + "'.")
                    None
                else
                    Some (Lun.make asset.Name, TextureAsset optTexture)
            SDL.SDL_FreeSurface optSurface
            result
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
            let optRenderAssetTrie = LunTrie.tryFind packageNameLun renderer.RenderAssetMap
            match optRenderAssetTrie with
            | None ->
                let renderAssetTrie = LunTrie.ofList renderAssets
                { renderer with RenderAssetMap = LunTrie.add packageNameLun renderAssetTrie renderer.RenderAssetMap }
            | Some renderAssetTrie ->
                let renderAssetTrie2 = LunTrie.addMany renderAssets renderAssetTrie
                { renderer with RenderAssetMap = LunTrie.add packageNameLun renderAssetTrie2 renderer.RenderAssetMap }
    | HintRenderingPackageDisuse hintPackageDisuse ->
        let optAssets = Assets.tryLoadAssets "Rendering" hintPackageDisuse.PackageName hintPackageDisuse.FileName
        match optAssets with
        | Left error ->
            trace ("HintRenderingPackageDisuse failed due unloadable assets '" + error + "' for '" + str hintPackageDisuse + "'.")
            renderer
        | Right assets -> renderer // TODO: unload assets
    | ScreenFlash -> renderer // TODO: render screen flash for one frame

let handleRenderMessages renderMessages renderer =
    List.fold handleRenderMessage renderer renderMessages

let doRender renderDescriptors renderer =
    let renderContext = renderer.RenderContext
    let targetResult = SDL.SDL_SetRenderTarget (renderContext, IntPtr.Zero)
    match targetResult with
    | 0 ->
        for descriptor in renderDescriptors do
            match descriptor with
            | SpriteDescriptor spriteDescriptor ->
                let optRenderAsset =
                    Option.reduce
                        (fun assetTrie -> LunTrie.tryFind spriteDescriptor.Sprite.AssetName assetTrie)
                        (LunTrie.tryFind spriteDescriptor.Sprite.PackageName renderer.RenderAssetMap)
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
                        let renderResult = SDL.SDL_RenderCopy (renderContext, texture, ref sourceRect, ref destRect)
                        match renderResult with
                        | 0 -> ()
                        | _ -> debug ("Rendering error - could not render texture for sprite '" + str descriptor + "' due to '" + SDL.SDL_GetError () + ".")
                    // | _ -> trace "Cannot sprite render with a non-texture asset."
    | _ -> trace ("Rendering error - could not set render target to display buffer due to '" + SDL.SDL_GetError () + ".")

let render renderMessages renderDescriptors renderer =
    let renderer2 = handleRenderMessages renderMessages renderer
    doRender renderDescriptors renderer2
    renderer2