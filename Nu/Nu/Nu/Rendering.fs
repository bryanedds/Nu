module Nu.Rendering
open System
open System.IO
open System.Runtime.InteropServices
open OpenTK
open OpenTK.Graphics.OpenGL
open SDL2
open Nu.Core
open Nu.Assets

type [<StructuralEquality; NoComparison>] Sprite =
    { SpriteAssetName : Lun
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
    | TextureAsset of int

type [<ReferenceEquality>] Renderer =
    private
        { RenderContext : nativeint
          RenderAssetMap : RenderAsset AssetMap }

let tryLoadRenderAsset (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".bmp" ->
        // TODO: use withSdlResource here for exception safety!
        let optSurface = SDL.SDL_LoadBMP asset.FileName
        if optSurface = IntPtr.Zero then
            trace ("Could not load bitmap '" + asset.FileName + "'.")
            None
        else
            let surface = Marshal.PtrToStructure (optSurface, typeof<SDL.SDL_Surface>) :?> SDL.SDL_Surface

            // Have OpenGL generate a texture object handle for us
            let texture = GL.GenTexture ()
 
            // Bind the texture object
            GL.BindTexture (TextureTarget.Texture2D, texture)
 
            // Set the texture's stretching properties
            GL.TexParameterI (TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, ref (int All.Nearest))
            GL.TexParameterI (TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, ref (int All.Nearest))
 
            // Edit the texture object's image data using the information SDL_Surface gives us
            GL.TexImage2D (
                TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba32ui, surface.w, surface.h, 0,
                PixelFormat.Rgba, PixelType.UnsignedByte, surface.pixels)

            SDL.SDL_FreeSurface optSurface

            Some (Lun.make asset.Name, TextureAsset texture)
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
            let optRenderAssets = List.map tryLoadRenderAsset assets
            let renderAssets = List.definitize optRenderAssets
            let packageNameLun = Lun.make hintPackageUse.PackageName
            let optRenderAssetTrie = LunTrie.tryFind packageNameLun renderer.RenderAssetMap
            match optRenderAssetTrie with
            | None ->
                let renderAssetTrie = LunTrie.ofSeq renderAssets
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

let handleRenderMessages (renderMessages : RenderMessage rQueue) renderer =
    List.fold handleRenderMessage renderer (List.rev renderMessages)

let doRender renderDescriptors renderer =
    for descriptor in renderDescriptors do
        match descriptor with
        | SpriteDescriptor spriteDescriptor ->
            let optRenderAsset =
                Option.reduce
                    (fun assetTrie -> LunTrie.tryFind spriteDescriptor.Sprite.SpriteAssetName assetTrie)
                    (LunTrie.tryFind spriteDescriptor.Sprite.PackageName renderer.RenderAssetMap)
            match optRenderAsset with
            | None -> ()
            | Some renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    (*let mutable sourceRect = SDL.SDL_Rect ()
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
                    | _ -> debug ("Rendering error - could not render texture for sprite '" + str descriptor + "' due to '" + SDL.SDL_GetError () + ".")*)
                    (*let top = spriteDescriptor.Position.Y
                    let bottom = spriteDescriptor.Position.Y + spriteDescriptor.Size.Y
                    let left = spriteDescriptor.Position.X
                    let right = spriteDescriptor.Position.X + spriteDescriptor.Size.X
                    // set up texture drawing
                    GL.BindTexture (TextureTarget.Texture2D, texture)
                    GL.Begin BeginMode.Quads
                    // bottom-left vertex (corner)
                    GL.TexCoord2 (0, 0);
                    GL.Vertex3 (bottom, left, 0.0f)
                    // bottom-right vertex (corner)
                    GL.TexCoord2 (1, 0)
                    GL.Vertex3 (bottom, right, 0.0f)
                    // top-right vertex (corner)
                    GL.TexCoord2 (1, 1)
                    GL.Vertex3 (top, right, 0.0f)
                    // top-left vertex (corner)
                    GL.TexCoord2 (0, 1)
                    GL.Vertex3 (top, left, 0.0f)
                    // fin
                    GL.End ()*)
                    ()
                // | _ -> trace "Cannot sprite render with a non-texture asset."

let render (renderMessages : RenderMessage rQueue) renderDescriptors renderer =
    let renderer2 = handleRenderMessages renderMessages renderer
    doRender renderDescriptors renderer2
    renderer2

let handleRenderExit renderer =
    let renderAssetTries = LunTrie.toValueSeq renderer.RenderAssetMap
    let renderAssets = Seq.collect LunTrie.toValueSeq renderAssetTries
    for renderAsset in renderAssets do
        match renderAsset with
        | TextureAsset texture -> () // apparently there is no need to free textures in SDL
    { renderer with RenderAssetMap = LunTrie.empty }

let makeRenderer renderContext =
    { RenderContext = renderContext; RenderAssetMap = LunTrie.empty }