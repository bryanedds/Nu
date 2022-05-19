// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open SDL2
open TiledSharp
open Prime
open Nu

/// Describes what to render.
/// TODO: 3D: make sure RenderCallbackDescriptor2d has all the parameters it needs, in a struct if necessary.
type [<NoEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor
    | SpritesDescriptor of SpritesDescriptor
    | TilesDescriptor of TilesDescriptor
    | TextDescriptor of TextDescriptor
    | ParticlesDescriptor of ParticlesDescriptor
    | RenderCallbackDescriptor2d of (Vector2 * Vector2 * Vector2 * Renderer2d -> unit)

/// A layered message to the 2d rendering system.
and [<NoEquality; NoComparison>] RenderLayeredMessage2d =
    { Elevation : single
      Horizon : single
      AssetTag : obj AssetTag
      RenderDescriptor : RenderDescriptor }

/// Describes a 2d render pass.
and [<CustomEquality; CustomComparison>] RenderPassDescriptor2d =
    { RenderPassOrder : int64
      RenderPass2d : Matrix3x3 * Renderer2d -> unit } // TODO: check if an M33 accounts for camera zoom.
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? RenderPassDescriptor2d as that -> this.RenderPassOrder.CompareTo that.RenderPassOrder
            | _ -> -1
    override this.Equals (that : obj) =
        match that with
        | :? RenderPassDescriptor2d as that -> this.RenderPassOrder = that.RenderPassOrder
        | _ -> false
    override this.GetHashCode () = hash this.RenderPassOrder

/// A message to the 2d rendering system.
and [<NoEquality; NoComparison>] RenderMessage2d =
    | RenderLayeredMessage2d of RenderLayeredMessage2d
    | RenderUpdateMaterial2d of (Renderer -> unit)
    | RenderPrePassDescriptor2d of RenderPassDescriptor2d
    | RenderPostPassDescriptor2d of RenderPassDescriptor2d
    | HintRenderPackageUseMessage2d of string
    | HintRenderPackageDisuseMessage2d of string
    | ReloadRenderAssetsMessage2d

/// The 2d renderer. Represents the 2d rendering system in Nu generally.
and Renderer2d =
    inherit Renderer
    /// The sprite batch operational environment if it exists for this implementation.
    abstract SpriteBatchEnvOpt : OpenGL.SpriteBatch.Env option
    /// Render a frame of the game.
    abstract Render : Vector2 -> Vector2 -> Vector2 -> RenderMessage2d List -> unit
    /// Swap a rendered frame of the game.
    abstract Swap : unit -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The mock implementation of Renderer.
type [<ReferenceEquality; NoComparison>] MockRenderer2d =
    private
        { MockRenderer2d : unit }

    interface Renderer2d with
        member renderer.SpriteBatchEnvOpt = None
        member renderer.Render _ _ _ _ = ()
        member renderer.Swap () = ()
        member renderer.CleanUp () = ()

    static member make () =
        { MockRenderer2d = () }

/// Compares layered 2d render messages.
type RenderLayeredMessage2dComparer () =
    interface IComparer<RenderLayeredMessage2d> with
        member this.Compare (left, right) =
            if left.Elevation < right.Elevation then -1
            elif left.Elevation > right.Elevation then 1
            elif left.Horizon > right.Horizon then -1
            elif left.Horizon < right.Horizon then 1
            else
                let assetNameCompare = strCmp left.AssetTag.AssetName right.AssetTag.AssetName
                if assetNameCompare <> 0 then assetNameCompare
                else strCmp left.AssetTag.PackageName right.AssetTag.PackageName

/// The SDL implementation of Renderer.
type [<ReferenceEquality; NoComparison>] GlRenderer2d =
    private
        { RenderWindow : Window
          RenderSpriteShader : int * int * int * uint
          RenderSpriteQuad : uint * uint * uint
          RenderTextQuad : uint * uint * uint
          mutable RenderSpriteBatchEnv : OpenGL.SpriteBatch.Env
          RenderPackages : RenderAsset Packages
          mutable RenderPackageCachedOpt : string * Dictionary<string, RenderAsset> // OPTIMIZATION: nullable for speed
          mutable RenderAssetCachedOpt : string * RenderAsset
          RenderLayeredMessages : RenderLayeredMessage2d List }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCachedOpt <- Unchecked.defaultof<_>

    static member private freeRenderAsset renderAsset renderer =
        GlRenderer2d.invalidateCaches renderer
        match renderAsset with
        | TextureAsset (_, texture) -> OpenGL.Hl.DeleteTexture texture
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font

    static member private tryLoadRenderAsset (asset : obj Asset) renderer =
        GlRenderer2d.invalidateCaches renderer
        match Path.GetExtension asset.FilePath with
        | ".bmp"
        | ".png" ->
            match OpenGL.Hl.TryCreateSpriteTexture asset.FilePath with
            | Right texture ->
                Some (asset.AssetTag.AssetName, TextureAsset texture)
            | Left error ->
                Log.debug ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
                None
        | ".ttf" ->
            let fileFirstName = Path.GetFileNameWithoutExtension asset.FilePath
            let fileFirstNameLength = String.length fileFirstName
            if fileFirstNameLength >= 3 then
                let fontSizeText = fileFirstName.Substring(fileFirstNameLength - 3, 3)
                match Int32.TryParse fontSizeText with
                | (true, fontSize) ->
                    let fontOpt = SDL_ttf.TTF_OpenFont (asset.FilePath, fontSize)
                    if fontOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, FontAsset (fontSize, fontOpt))
                    else Log.debug ("Could not load font due to unparsable font size in file name '" + asset.FilePath + "'."); None
                | (false, _) -> Log.debug ("Could not load font due to file name being too short: '" + asset.FilePath + "'."); None
            else Log.debug ("Could not load font '" + asset.FilePath + "'."); None
        | extension -> Log.debug ("Could not load render asset '" + scstring asset + "' due to unknown extension '" + extension + "'."); None

    static member private tryLoadRenderPackage packageName renderer =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Render) packageName assetGraph with
            | Right assets ->
                let renderAssetOpts = List.map (fun asset -> GlRenderer2d.tryLoadRenderAsset asset renderer) assets
                let renderAssets = List.definitize renderAssetOpts
                match Dictionary.tryFind packageName renderer.RenderPackages with
                | Some renderAssetDict ->
                    for (key, value) in renderAssets do renderAssetDict.Assign (key, value)
                    renderer.RenderPackages.Assign (packageName, renderAssetDict)
                | None ->
                    let renderAssetDict = dictPlus StringComparer.Ordinal renderAssets
                    renderer.RenderPackages.Assign (packageName, renderAssetDict)
            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member tryFindRenderAsset (assetTag : obj AssetTag) renderer =
        if  renderer.RenderPackageCachedOpt :> obj |> notNull &&
            fst renderer.RenderPackageCachedOpt = assetTag.PackageName then
            if  renderer.RenderAssetCachedOpt :> obj |> notNull &&
                fst renderer.RenderAssetCachedOpt = assetTag.AssetName then
                ValueSome (snd renderer.RenderAssetCachedOpt)
            else
                let assets = snd renderer.RenderPackageCachedOpt
                match assets.TryGetValue assetTag.AssetName with
                | (true, asset) ->
                    renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
        else
            match Dictionary.tryFind assetTag.PackageName renderer.RenderPackages with
            | Some assets ->
                renderer.RenderPackageCachedOpt <- (assetTag.PackageName, assets)
                match assets.TryGetValue assetTag.AssetName with
                | (true, asset) ->
                    renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                    ValueSome asset
                | (false, _) -> ValueNone
            | None ->
                Log.info ("Loading render package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                GlRenderer2d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, assets) ->
                    renderer.RenderPackageCachedOpt <- (assetTag.PackageName, assets)
                    match assets.TryGetValue assetTag.AssetName with
                    | (true, asset) ->
                        renderer.RenderAssetCachedOpt <- (assetTag.AssetName, asset)
                        ValueSome asset
                    | (false, _) -> ValueNone
                | (false, _) -> ValueNone

    static member private handleHintRenderPackageUse hintPackageName renderer =
        GlRenderer2d.tryLoadRenderPackage hintPackageName renderer

    static member private handleHintRenderPackageDisuse hintPackageName renderer =
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some assets ->
            for asset in assets do GlRenderer2d.freeRenderAsset asset.Value renderer
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        renderer.RenderPackages.Clear ()
        for packageName in packageNames do
            GlRenderer2d.tryLoadRenderPackage packageName renderer

    static member private handleRenderMessage renderMessage renderer =
        match renderMessage with
        | RenderLayeredMessage2d message -> renderer.RenderLayeredMessages.Add message
        | HintRenderPackageUseMessage2d hintPackageUse -> GlRenderer2d.handleHintRenderPackageUse hintPackageUse renderer
        | HintRenderPackageDisuseMessage2d hintPackageDisuse -> GlRenderer2d.handleHintRenderPackageDisuse hintPackageDisuse renderer
        | ReloadRenderAssetsMessage2d -> GlRenderer2d.handleReloadRenderAssets renderer

    static member private handleRenderMessages renderMessages renderer =
        for renderMessage in renderMessages do
            GlRenderer2d.handleRenderMessage renderMessage renderer

    static member private sortRenderLayeredMessages renderer =
        renderer.RenderLayeredMessages.Sort (RenderLayeredMessage2dComparer ())

    static member inline private batchSprite
        absolute position size pivot rotation (inset : Box2) textureMetadata texture (color : Color) blend (glow : Color) flip renderer =

        // compute tex coords
        // TODO: 3D: consider putting the texel multiplies in the shader.
        let texCoords =
            let texelWidth = textureMetadata.TextureTexelWidth
            let texelHeight = textureMetadata.TextureTexelHeight
            let texelBorderInset = Constants.Render.SpriteTexelEpsilon
            let texelBorderInsetTimes2 = Constants.Render.SpriteTexelEpsilonTimes2
            if not (inset.Equals box2Zero) then
                let px = single inset.Position.X * texelWidth + texelBorderInset
                let py = single (inset.Position.Y + inset.Size.Y) * texelHeight - texelBorderInset
                let sx = single inset.Size.X * texelWidth - texelBorderInsetTimes2
                let sy = single -inset.Size.Y * texelHeight + texelBorderInsetTimes2
                Box2 (px, py, sx, sy)
            else Box2 (0.0f, 1.0f, 1.0f, -1.0f)

        // compute blending instructions
        let (bfs, bfd, beq) =
            match blend with
            | Transparent -> (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha, OpenGL.BlendEquationMode.FuncAdd)
            | Additive -> (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.One, OpenGL.BlendEquationMode.FuncAdd)
            | Overwrite -> (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero, OpenGL.BlendEquationMode.FuncAdd)

        // attempt to draw normal sprite
        if color.A <> 0.0f then
            OpenGL.SpriteBatch.SubmitSprite (absolute, position, size, pivot, rotation, texCoords, color, flip, bfs, bfd, beq, texture, renderer.RenderSpriteBatchEnv)
            OpenGL.Hl.Assert ()

        // attempt to draw glow sprite
        if glow.A <> 0.0f then
            let (bfs, bfd, beq) = (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.One, OpenGL.BlendEquationMode.FuncAdd)
            OpenGL.SpriteBatch.SubmitSprite (absolute, position, size, pivot, rotation, texCoords, glow, flip, bfs, bfd, beq, texture, renderer.RenderSpriteBatchEnv)
            OpenGL.Hl.Assert ()

    /// Compute the 2d viewport.
    static member computeViewport (_ : GlRenderer2d) =
        box2i v2iZero (v2i Constants.Render.ResolutionX Constants.Render.ResolutionY)

    /// Compute the 2d absolute view matrix.
    static member computeViewAbsolute (_ : Vector2) (eyeSize : Vector2) eyeMargin (_ : GlRenderer2d) =
        let translation = eyeSize * 0.5f * Constants.Render.VirtualScalar2 - eyeMargin * Constants.Render.VirtualScalar2
        Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

    /// Compute the 2d relative view matrix.
    static member computeViewRelative (eyePosition : Vector2) (eyeSize : Vector2) eyeMargin (_ : GlRenderer2d) =
        let translation = -eyePosition * Constants.Render.VirtualScalar2 + eyeSize * 0.5f * Constants.Render.VirtualScalar2 - eyeMargin * Constants.Render.VirtualScalar2
        Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

    /// Compute the 2d projection matrix.
    static member computeProjection (viewport : Box2i) (_ : GlRenderer2d) =
        Matrix4x4.CreateOrthographicOffCenter
            (single (viewport.Position.X),
             single (viewport.Position.X + viewport.Size.X),
             single (viewport.Position.Y),
             single (viewport.Position.Y + viewport.Size.Y),
             -1.0f, 1.0f)

    /// Get the sprite shader created by OpenGL.Hl.CreateSpriteShader.
    static member getSpriteShader renderer =
        renderer.RenderSpriteShader

    /// Get the sprite quad created by OpenGL.Hl.CreateSpriteQuad.
    static member getSpriteQuad renderer =
        renderer.RenderSpriteQuad

    /// Get the text quad created by OpenGL.Hl.CreateSpriteQuad.
    static member getTextQuad renderer =
        renderer.RenderTextQuad

    /// Render sprite.
    static member renderSprite
        (transform : Transform byref,
         inset : Box2 inref,
         image : Image AssetTag,
         color : Color inref,
         blend : Blend,
         glow : Color inref,
         flip : Flip,
         renderer) =
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let position = perimeter.Position.V2 * Constants.Render.VirtualScalar2
        let rotation = transform.Angles.X
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let pivot = transform.Offset.V2 * size
        let image = AssetTag.generalize image
        match GlRenderer2d.tryFindRenderAsset image renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | TextureAsset (textureMetadata, texture) ->
                GlRenderer2d.batchSprite absolute position size pivot rotation inset textureMetadata texture color blend glow flip renderer
            | _ -> Log.trace "Cannot render sprite with a non-texture asset."
        | _ -> Log.info ("SpriteDescriptor failed to render due to unloadable assets for '" + scstring image + "'.")

    /// Render tiles.
    static member renderTiles
        (transform : Transform byref,
         color : Color inref,
         glow : Color inref,
         mapSize : Vector2i,
         tiles : TmxLayerTile array,
         tileSourceSize : Vector2i,
         tileSize : Vector2,
         tileAssets : (TmxTileset * Image AssetTag) array,
         eyePosition : Vector2,
         eyeSize : Vector2,
         renderer) =
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let position = perimeter.Position.V2 * Constants.Render.VirtualScalar2
        let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
        let eyePosition = eyePosition * Constants.Render.VirtualScalar2
        let eyeSize = eyeSize * Constants.Render.VirtualScalar2
        let tileSize = tileSize * Constants.Render.VirtualScalar2
        let tilePivot = tileSize * 0.5f // just rotate around center
        let (allFound, tileSetTextures) =
            tileAssets |>
            Array.map (fun (tileSet, tileSetImage) ->
                match GlRenderer2d.tryFindRenderAsset (AssetTag.generalize tileSetImage) renderer with
                | ValueSome (TextureAsset (tileSetTexture, tileSetTextureMetadata)) -> Some (tileSet, tileSetImage, tileSetTexture, tileSetTextureMetadata)
                | ValueSome _ -> None
                | ValueNone -> None) |>
            Array.definitizePlus
        if allFound then
            // OPTIMIZATION: allocating refs in a tight-loop is problematic, so pulled out here
            let tilesLength = Array.length tiles
            let mutable tileIndex = 0
            while tileIndex < tilesLength do
                let tile = &tiles.[tileIndex]
                if tile.Gid <> 0 then // not the empty tile
                    let mapRun = mapSize.X
                    let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
                    let tilePosition =
                        v2
                            (position.X + tileSize.X * single i)
                            (position.Y - tileSize.Y - tileSize.Y * single j + size.Y)
                    let tileBounds = box2 tilePosition tileSize
                    let viewBounds = box2 (eyePosition - eyeSize * 0.5f) eyeSize
                    if Math.isBoundsIntersectingBounds2d tileBounds viewBounds then
        
                        // compute tile flip
                        let flip =
                            match (tile.HorizontalFlip, tile.VerticalFlip) with
                            | (false, false) -> FlipNone
                            | (true, false) -> FlipH
                            | (false, true) -> FlipV
                            | (true, true) -> FlipHV
        
                        // attempt to compute tile set texture
                        let mutable tileOffset = 1 // gid 0 is the empty tile
                        let mutable tileSetIndex = 0
                        let mutable tileSetWidth = 0
                        let mutable tileSetTextureOpt = None
                        for (set, _, textureMetadata, texture) in tileSetTextures do
                            let tileCountOpt = set.TileCount
                            let tileCount = if tileCountOpt.HasValue then tileCountOpt.Value else 0
                            if  tile.Gid >= set.FirstGid && tile.Gid < set.FirstGid + tileCount ||
                                not tileCountOpt.HasValue then // HACK: when tile count is missing, assume we've found the tile...?
                                tileSetWidth <- let tileSetWidthOpt = set.Image.Width in tileSetWidthOpt.Value
                                tileSetTextureOpt <- Some (textureMetadata, texture)
                            if Option.isNone tileSetTextureOpt then
                                tileSetIndex <- inc tileSetIndex
                                tileOffset <- tileOffset + tileCount
        
                        // attempt to render tile
                        match tileSetTextureOpt with
                        | Some (textureMetadata, texture) ->
                            let tileId = tile.Gid - tileOffset
                            let tileIdPosition = tileId * tileSourceSize.X
                            let tileSourcePosition = v2 (single (tileIdPosition % tileSetWidth)) (single (tileIdPosition / tileSetWidth * tileSourceSize.Y))
                            let inset = box2 tileSourcePosition (v2 (single tileSourceSize.X) (single tileSourceSize.Y))
                            GlRenderer2d.batchSprite absolute tilePosition tileSize tilePivot 0.0f inset textureMetadata texture color Transparent glow flip renderer
                        | None -> ()
        
                tileIndex <- inc tileIndex
        else Log.info ("TileLayerDescriptor failed due to unloadable or non-texture assets for one or more of '" + scstring tileAssets + "'.")

    /// Render text.
    static member renderText
        (transform : Transform byref,
         text : string,
         font : Font AssetTag,
         color : Color inref,
         justification : Justification,
         eyePosition : Vector2,
         eyeSize : Vector2,
         eyeMargin : Vector2,
         renderer : GlRenderer2d) =
        let (transform, color) = (transform, color) // make visible from lambda
        flip OpenGL.SpriteBatch.InterruptFrame renderer.RenderSpriteBatchEnv $ fun () ->
            let mutable transform = transform
            let absolute = transform.Absolute
            let perimeter = transform.Perimeter
            let position = perimeter.Position.V2 * Constants.Render.VirtualScalar2
            let size = perimeter.Size.V2 * Constants.Render.VirtualScalar2
            let viewport = GlRenderer2d.computeViewport renderer
            let viewAbsolute = GlRenderer2d.computeViewAbsolute eyePosition eyeSize eyeMargin renderer
            let viewRelative = GlRenderer2d.computeViewRelative eyePosition eyeSize eyeMargin renderer
            let projection = GlRenderer2d.computeProjection viewport renderer
            let viewProjection = if absolute then viewAbsolute * projection else viewRelative * projection
            let font = AssetTag.generalize font
            match GlRenderer2d.tryFindRenderAsset font renderer with
            | ValueSome renderAsset ->
                match renderAsset with
                | FontAsset (_, font) ->
                
                    // 
                    // NOTE: the resource implications (throughput and fragmentation?) of creating and destroying a
                    // surface and texture one or more times a frame must be understood!
                    let (offset, textSurface) =

                        // create sdl color
                        let mutable colorSdl = SDL.SDL_Color ()
                        colorSdl.r <- color.R8
                        colorSdl.g <- color.G8
                        colorSdl.b <- color.B8
                        colorSdl.a <- color.A8

                        // get text metrics
                        let mutable width = 0
                        let mutable height = 0
                        SDL_ttf.TTF_SizeText (font, text, &width, &height) |> ignore
                        let width = single (width * Constants.Render.VirtualScalar)
                        let height = single (height * Constants.Render.VirtualScalar)

                        // justify
                        match justification with
                        | Unjustified wrapped ->
                            let textSurface =
                                if wrapped
                                then SDL_ttf.TTF_RenderText_Blended_Wrapped (font, text, colorSdl, uint32 size.X)
                                else SDL_ttf.TTF_RenderText_Blended (font, text, colorSdl)
                            (v2 0.0f (*size.Y - height*)0.0f, textSurface)
                        | Justified (h, v) ->
                            let textSurface = SDL_ttf.TTF_RenderText_Blended (font, text, colorSdl)
                            let offsetX =
                                match h with
                                | JustifyLeft -> 0.0f
                                | JustifyCenter -> (size.X - width) * 0.5f
                                | JustifyRight -> size.X - width
                            let offsetY =
                                match v with
                                | JustifyTop -> 0.0f
                                | JustifyMiddle -> (size.Y - height) * 0.5f
                                | JustifyBottom -> size.Y - height
                            let offset = v2 offsetX offsetY
                            (offset, textSurface)

                    if textSurface <> IntPtr.Zero then

                        // marshal surface and flip surface
                        let textSurface = Marshal.PtrToStructure<SDL.SDL_Surface> textSurface
                        let textSurfaceWidth = single (textSurface.w * Constants.Render.VirtualScalar)
                        let textSurfaceHeight = single (textSurface.h * Constants.Render.VirtualScalar)
                        OpenGL.Hl.FlipSurface &textSurface

                        // construct mvp matrix
                        let translation = (position + offset).V3
                        let scale = v3 textSurfaceWidth textSurfaceHeight 1.0f
                        let modelTranslation = Matrix4x4.CreateTranslation translation
                        let modelScale = Matrix4x4.CreateScale scale
                        let modelMatrix = modelScale * modelTranslation
                        let modelViewProjection = modelMatrix * viewProjection

                        // upload texture
                        let textTexture = OpenGL.Gl.GenTexture ()
                        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, textTexture)
                        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
                        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
                        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba8, textSurface.w, textSurface.h, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, textSurface.pixels)
                        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
                        OpenGL.Hl.Assert ()

                        // draw text sprite
                        let (indices, vertices, vao) = renderer.RenderTextQuad
                        let (colorUniform, modelViewProjectionUniform, texUniform, shader) = renderer.RenderSpriteShader
                        OpenGL.Hl.DrawSprite (indices, vertices, vao, Color.White, &modelViewProjection, textTexture, colorUniform, modelViewProjectionUniform, texUniform, shader)
                        OpenGL.Hl.Assert ()

                        // destroy texture
                        OpenGL.Gl.DeleteTextures textTexture
                        OpenGL.Hl.Assert ()

                    SDL.SDL_FreeSurface textSurface

                | _ -> Log.debug "Cannot render text with a non-font asset."
            | _ -> Log.info ("TextDescriptor failed due to unloadable assets for '" + scstring font + "'.")
        OpenGL.Hl.Assert ()

    /// Render particles.
    static member renderParticles
        (viewAbsolute : Matrix3x3 byref,
         viewRelative : Matrix3x3 byref,
         _ : Vector2,
         eyeSize : Vector2,
         eyeMargin : Vector2,
         _ : single,
         _ : single,
         absolute : bool,
         blend : Blend,
         image : Image AssetTag,
         particles : Particle array,
         renderer) =
        ()
        //let view = if absolute then &viewAbsolute else &viewRelative
        //let viewScale = Matrix3x3.ExtractScaleMatrix &view
        //let positionOffset = -(v2Zero * view) + v2 eyeMargin.X -eyeMargin.Y
        //let blend = Blend.toSdlBlendMode blend
        //let image = AssetTag.generalize image
        //match GlRenderer2d.tryFindRenderAsset image renderer with
        //| ValueSome renderAsset ->
        //    match renderAsset with
        //    | TextureAsset texture ->
        //        let (_, _, _, textureSizeX, textureSizeY) = SDL.SDL_QueryTexture texture
        //        let mutable sourceRect = SDL.SDL_Rect ()
        //        let mutable destRect = SDL.SDL_Rect ()
        //        let mutable index = 0
        //        while index < particles.Length do
        //            let particle = &particles.[index]
        //            let perimeter = particle.Transform.Perimeter
        //            let position = perimeter.Position.V2
        //            let positionView = position + positionOffset
        //            let size = perimeter.Size.V2
        //            let sizeView = Matrix3x3.Multiply (&size, &viewScale)
        //            let rotation = particle.Transform.Angles.X
        //            let color = &particle.Color
        //            let glow = &particle.Glow
        //            let flip = Flip.toSdlFlip particle.Flip
        //            let inset = particle.Inset
        //            if inset.IsEmpty then
        //                sourceRect.x <- 0
        //                sourceRect.y <- 0
        //                sourceRect.w <- textureSizeX
        //                sourceRect.h <- textureSizeY
        //            else
        //                sourceRect.x <- int inset.Position.X
        //                sourceRect.y <- int inset.Position.Y
        //                sourceRect.w <- int inset.Size.X
        //                sourceRect.h <- int inset.Size.Y
        //            destRect.x <- int (+positionView.X + eyeSize.X * 0.5f) * Constants.Render.VirtualScalar
        //            destRect.y <- int (-positionView.Y + eyeSize.Y * 0.5f) * Constants.Render.VirtualScalar - (int sizeView.Y * Constants.Render.VirtualScalar) // negation for right-handedness
        //            destRect.w <- int sizeView.X * Constants.Render.VirtualScalar
        //            destRect.h <- int sizeView.Y * Constants.Render.VirtualScalar
        //            let rotation = double (Math.radiansToDegrees rotation) // negation for right-handedness
        //            let mutable rotationCenter = SDL.SDL_Point ()
        //            rotationCenter.x <- int (sizeView.X * 0.5f) * Constants.Render.VirtualScalar
        //            rotationCenter.y <- int (sizeView.Y * 0.5f) * Constants.Render.VirtualScalar
        //            if color.A <> 0.0f then
        //                SDL.SDL_SetTextureBlendMode (texture, blend) |> ignore
        //                SDL.SDL_SetTextureColorMod (texture, color.R8, color.G8, color.B8) |> ignore
        //                SDL.SDL_SetTextureAlphaMod (texture, color.A8) |> ignore
        //                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, &sourceRect, &destRect, rotation, &rotationCenter, flip)
        //                if renderResult <> 0 then Log.info ("Render error - could not render texture for particle '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
        //            if glow.A <> 0.0f then
        //                SDL.SDL_SetTextureBlendMode (texture, SDL.SDL_BlendMode.SDL_BLENDMODE_ADD) |> ignore
        //                SDL.SDL_SetTextureColorMod (texture, glow.R8, glow.G8, glow.B8) |> ignore
        //                SDL.SDL_SetTextureAlphaMod (texture, glow.A8) |> ignore
        //                let renderResult = SDL.SDL_RenderCopyEx (renderer.RenderContext, texture, &sourceRect, &destRect, rotation, &rotationCenter, flip)
        //                if renderResult <> 0 then Log.info ("Render error - could not render texture for particle '" + scstring image + "' due to '" + SDL.SDL_GetError () + ".")
        //            index <- inc index
        //    | _ -> Log.trace "Cannot render particle with a non-texture asset."
        //| _ -> Log.info ("RenderDescriptors failed to render due to unloadable assets for '" + scstring image + "'.")

    static member inline private renderCallback callback eyePosition eyeSize eyeMargin renderer =
        flip OpenGL.SpriteBatch.InterruptFrame renderer.RenderSpriteBatchEnv $ fun () -> callback (eyePosition, eyeSize, eyeMargin, renderer)
        OpenGL.Hl.Assert ()

    static member private renderDescriptor descriptor eyePosition eyeSize eyeMargin renderer =
        match descriptor with
        | SpriteDescriptor descriptor ->
            let inset = match descriptor.InsetOpt with Some inset -> inset | None -> box2Zero
            GlRenderer2d.renderSprite (&descriptor.Transform, &inset, descriptor.Image, &descriptor.Color, descriptor.Blend, &descriptor.Glow, descriptor.Flip, renderer)
        | SpritesDescriptor descriptor ->
            let sprites = descriptor.Sprites
            for index in 0 .. sprites.Length - 1 do
                let sprite = &sprites.[index]
                GlRenderer2d.renderSprite (&sprite.Transform, &sprite.Inset, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Glow, sprite.Flip, renderer)
        | TilesDescriptor descriptor ->
            GlRenderer2d.renderTiles
                (&descriptor.Transform, &descriptor.Color, &descriptor.Glow,
                 descriptor.MapSize, descriptor.Tiles, descriptor.TileSourceSize, descriptor.TileSize, descriptor.TileAssets,
                 eyePosition, eyeSize, renderer)
        | TextDescriptor descriptor ->
            GlRenderer2d.renderText
                (&descriptor.Transform, descriptor.Text, descriptor.Font, &descriptor.Color, descriptor.Justification, eyePosition, eyeSize, v2Zero, renderer)
        | ParticlesDescriptor descriptor ->
            ()
            //GlRenderer2d.renderParticles
            //    (descriptor.Elevation, descriptor.Horizon, descriptor.Absolute, descriptor.Blend, descriptor.Image, descriptor.Particles, renderer)
        | RenderCallbackDescriptor2d callback ->
            GlRenderer2d.renderCallback callback eyePosition eyeSize eyeMargin renderer

    static member private renderLayeredMessages eyePosition eyeSize eyeMargin renderer =
        for message in renderer.RenderLayeredMessages do
            GlRenderer2d.renderDescriptor message.RenderDescriptor eyePosition eyeSize eyeMargin renderer

    static member addEyeMarginMessage (_ : Vector2) eyeSize eyeMargin renderer =
        let eyeMarginBounds = box2 (eyeSize * -0.5f - eyeMargin) (eyeSize + eyeMargin * 2.0f)
        let image = asset Assets.Default.PackageName Assets.Default.Image8Name
        let sprites =
            if eyeMargin <> v2Zero then
                let mutable transform = Transform.makeDefault v3Cartesian2d
                transform.Absolute <- true
                let sprite = { Transform = transform; Inset = box2Zero; Image = image; Color = Color.Black; Blend = Overwrite; Glow = Color.Zero; Flip = FlipNone }
                let mutable bottomMarginTransform = transform
                bottomMarginTransform.Position <- eyeMarginBounds.BottomLeft.V3
                bottomMarginTransform.Size <- v3 eyeMarginBounds.Size.X eyeMargin.Y 0.0f
                let bottomMargin = { sprite with Transform = bottomMarginTransform }
                let mutable leftMarginTransform = transform
                leftMarginTransform.Position <- eyeMarginBounds.BottomLeft.V3
                leftMarginTransform.Size <- v3 eyeMargin.X eyeMarginBounds.Size.Y 0.0f
                let leftMargin = { sprite with Transform = leftMarginTransform }
                let mutable topMarginTransform = transform
                topMarginTransform.Position <- eyeMarginBounds.TopLeft.V3 - v3 0.0f eyeMargin.Y 0.0f
                topMarginTransform.Size <- v3 eyeMarginBounds.Size.X eyeMargin.Y 0.0f
                let topMargin = { sprite with Transform = topMarginTransform }
                let mutable rightMarginTransform = transform
                rightMarginTransform.Position <- eyeMarginBounds.BottomRight.V3 - v3 eyeMargin.X 0.0f 0.0f
                rightMarginTransform.Size <- v3 eyeMargin.X eyeMarginBounds.Size.Y 0.0f
                let rightMargin = { sprite with Transform = rightMarginTransform }
                [|bottomMargin; leftMargin; topMargin; rightMargin|]
            else [||]
        let message = { Elevation = Single.MaxValue; AssetTag = AssetTag.generalize image; Horizon = 0.0f; RenderDescriptor = SpritesDescriptor { Sprites = sprites }}
        renderer.RenderLayeredMessages.Add message

    /// Make a Renderer.
    static member make window =

        // create SDL-OpenGL context if needed
        match window with
        | SglWindow window -> OpenGL.Hl.CreateSgl410Context window.SglWindow |> ignore<nativeint>
        | WfglWindow _ -> () // TODO: 3D: see if we can make current the GL context here so that threaded OpenGL works in Gaia.
        OpenGL.Hl.Assert ()

        // create one-off sprite and text resources
        let spriteShader = OpenGL.Hl.CreateSpriteShader ()
        let spriteQuad = OpenGL.Hl.CreateSpriteQuad false
        let textQuad = OpenGL.Hl.CreateSpriteQuad true
        OpenGL.Hl.Assert ()

        // create sprite batch env
        let spriteBatchEnv = OpenGL.SpriteBatch.CreateEnv ()
        OpenGL.Hl.Assert ()

        // make renderer
        let renderer =
            { RenderWindow = window
              RenderSpriteShader = spriteShader
              RenderSpriteQuad = spriteQuad
              RenderTextQuad = textQuad
              RenderSpriteBatchEnv = spriteBatchEnv
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCachedOpt = Unchecked.defaultof<_>
              RenderLayeredMessages = List () }

        // fin
        renderer

    interface Renderer2d with

        member renderer.SpriteBatchEnvOpt =
            Some renderer.RenderSpriteBatchEnv

        member renderer.Render eyePosition eyeSize eyeMargin renderMessages =

            // begin frame
            let viewport = GlRenderer2d.computeViewport renderer
            let projection = GlRenderer2d.computeProjection viewport renderer
            let viewProjectionAbsolute = GlRenderer2d.computeViewAbsolute eyePosition eyeSize eyeMargin renderer * projection
            let viewProjectionRelative = GlRenderer2d.computeViewRelative eyePosition eyeSize eyeMargin renderer * projection
            OpenGL.Hl.BeginFrame viewport
            OpenGL.SpriteBatch.BeginFrame (&viewProjectionAbsolute, &viewProjectionRelative, renderer.RenderSpriteBatchEnv)
            OpenGL.Hl.Assert ()

            // render frame
            GlRenderer2d.handleRenderMessages renderMessages renderer
            GlRenderer2d.addEyeMarginMessage eyePosition eyeSize eyeMargin renderer // TODO: 3D: make sure margins are being rendered correctly.
            GlRenderer2d.sortRenderLayeredMessages renderer
            GlRenderer2d.renderLayeredMessages eyePosition eyeSize eyeMargin renderer
            renderer.RenderLayeredMessages.Clear ()

            // end frame
            OpenGL.SpriteBatch.EndFrame renderer.RenderSpriteBatchEnv
            OpenGL.Hl.EndFrame ()
            OpenGL.Hl.Assert ()

        member renderer.Swap () =
            match renderer.RenderWindow with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow
            | WfglWindow window -> window.WfglSwapWindow ()

        member renderer.CleanUp () =
            OpenGL.SpriteBatch.DestroyEnv renderer.RenderSpriteBatchEnv
            OpenGL.Hl.Assert ()
            let renderAssetPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderAssetPackages |> Seq.collect (Seq.map (fun entry -> entry.Value))
            for renderAsset in renderAssets do GlRenderer2d.freeRenderAsset renderAsset renderer
            renderer.RenderPackages.Clear ()

/// A 2d renderer process that may or may not be threaded.
type RendererProcess2d =
    interface
        abstract Started : bool
        abstract Terminated : bool
        abstract Start : unit -> unit
        abstract EnqueueMessage : RenderMessage2d -> unit
        abstract ClearMessages : unit -> unit
        abstract SubmitMessages : Vector2 -> Vector2 -> Vector2 -> unit
        abstract Swap : unit -> unit
        abstract Terminate : unit -> unit
        end

/// A non-threaded 2d renderer.
type RendererInline2d (createRenderer2d) =

    let mutable started = false
    let mutable terminated = false
    let mutable messages = List ()
    let mutable rendererOpt = Option<Renderer2d>.None

    interface RendererProcess2d with

        member this.Started =
            started

        member this.Terminated =
            terminated

        member this.Start () =
            match rendererOpt with
            | Some _ -> raise (InvalidOperationException "Redundant Start calls.")
            | None ->
                rendererOpt <- Some (createRenderer2d ())
                started <- true

        member this.EnqueueMessage message =
            match rendererOpt with
            | Some _ -> messages.Add message
            | None -> raise (InvalidOperationException "Renderer is not yet or is no longer valid.")

        member this.ClearMessages () =
            messages <- List ()

        member this.SubmitMessages eyePosition eyeSize eyeMargin =
            match rendererOpt with
            | Some renderer ->
                renderer.Render eyePosition eyeSize eyeMargin messages
                messages.Clear ()
            | None -> raise (InvalidOperationException "Renderer is not yet or is no longer valid.")

        member this.Swap () =
            match rendererOpt with
            | Some renderer -> renderer.Swap ()
            | None -> raise (InvalidOperationException "Renderer is not yet or is no longer valid.")

        member this.Terminate () =
            match rendererOpt with
            | Some renderer ->
                renderer.CleanUp ()
                rendererOpt <- None
                terminated <- true
            | None -> raise (InvalidOperationException "Redundant Terminate calls.")

/// A threaded 2d renderer.
type RendererThread2d (createRenderer2d) =

    let mutable taskOpt = None
    let startedLock = obj ()
    let mutable started = false
    let terminatedLock = obj ()
    let mutable terminated = false
    let messagesLock = obj ()
    let mutable messages = List ()
    let submissionLock = obj ()
    let mutable submissionOpt = Option<Vector2 * Vector2 * Vector2>.None
    let swapLock = obj ()
    let mutable swap = false

    member private this.Started =
        (this :> RendererProcess2d).Started

    member private this.Terminated =
        (this :> RendererProcess2d).Terminated

    member private this.SubmissionOpt =
        lock submissionLock (fun () -> submissionOpt)

    member private this.Swapping =
        lock swapLock (fun () -> swap)

    member private this.Run () =

        // create renderer
        let renderer = createRenderer2d () : Renderer2d

        // mark as started
        lock startedLock (fun () -> started <- true)

        // loop until terminated
        while not this.Terminated do

            // loop until submission exists
            while Option.isNone this.SubmissionOpt && not this.Terminated do
                Thread.Yield () |> ignore<bool>

            // receive submission
            let (eyePosition, eyeSize, eyeMargin) =
                lock submissionLock (fun () ->
                    let submission = Option.get submissionOpt
                    submissionOpt <- None
                    submission)

            // receive messages
            let messagesReceived =
                lock messagesLock (fun () ->
                    let messagesReceived = messages
                    messages <- List ()
                    messagesReceived)

            // render
            renderer.Render eyePosition eyeSize eyeMargin messagesReceived

            // loop until swap is requested
            while not this.Swapping && not this.Terminated do
                Thread.Yield () |> ignore<bool>

            // swap
            renderer.Swap ()

            // complete swap request
            lock swapLock (fun () -> swap <- false)

        // clean up
        renderer.CleanUp ()

    interface RendererProcess2d with

        member this.Started =
            lock startedLock (fun () -> started)

        member this.Terminated =
            lock terminatedLock (fun () -> terminated)

        member this.Start () =
            if Option.isSome taskOpt then raise (InvalidOperationException "Render process already started.")
            let task = new Task ((fun () -> this.Run ()), TaskCreationOptions.LongRunning)
            taskOpt <- Some task
            task.Start ()
            while not this.Started do Thread.Yield () |> ignore<bool>

        member this.EnqueueMessage message =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            lock messagesLock (fun () -> messages.Add message)

        member this.ClearMessages () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            lock messagesLock (fun () -> messages <- List ())

        member this.SubmitMessages eyePosition eyeSize eyeMargin =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            lock submissionLock (fun () -> submissionOpt <- Some ((eyePosition, eyeSize, eyeMargin)))

        member this.Swap () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            match this.SubmissionOpt with
            | Some _ ->
                lock swapLock (fun () ->
                    if swap then raise (InvalidOperationException "Redundant Swap calls.")
                    swap <- true)
                while this.Swapping do Thread.Yield () |> ignore<bool>
            | None -> () // ignore invalid swap order

        member this.Terminate () =
            if Option.isNone taskOpt then raise (InvalidOperationException "Render process not yet started or already terminated.")
            let task = Option.get taskOpt
            lock terminatedLock (fun () ->
                if terminated then raise (InvalidOperationException "Redundant Terminate calls.")
                terminated <- true)
            task.Wait ()
            taskOpt <- None