// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<RequireQualifiedAccess>]
module CubeMap =

    /// Memoizes cube map loads.
    type [<NoEquality; NoComparison>] CubeMapMemo =
        private
            { CubeMaps : Dictionary<string * string * string * string * string * string, uint> }

        /// Make a cube map memoizer.
        static member make () =
            { CubeMaps = Dictionary HashIdentity.Structural }

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =

        // bind new cube map
        let cubeMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // load faces into cube map
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                match Texture2d.TryCreateImageData faceFilePath with
                | Some (metadata, imageData, disposer) ->
                    use d = disposer
                    Gl.TexImage2D (LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i), 0, InternalFormat.Rgba8, metadata.Texture2dWidth, metadata.Texture2dHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, imageData)
                    Hl.Assert ()
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
            Right cubeMap
        | Some error ->
            Texture2d.DeleteTexture2d cubeMap
            Left error

    /// Delete a cube map.
    let DeleteCubeMap (cubeMap : uint) =
        Gl.DeleteTextures cubeMap

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMapMemoized (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, cubeMapMemo) =

        // memoize cube map
        let cubeMapKey =
            (Path.Simplify faceRightFilePath,
             Path.Simplify faceLeftFilePath,
             Path.Simplify faceTopFilePath,
             Path.Simplify faceBottomFilePath,
             Path.Simplify faceBackFilePath,
             Path.Simplify faceFrontFilePath)
        match cubeMapMemo.CubeMaps.TryGetValue cubeMapKey with
        | (false, _) ->

            // attempt to create cube map
            match TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
            | Right cubeMap ->
                cubeMapMemo.CubeMaps.Add (cubeMapKey, cubeMap)
                Right cubeMap
            | Left error -> Left error

        // already exists
        | (true, cubeMap) -> Right cubeMap

    /// Delete memoized cube maps.
    let DeleteCubeMapsMemoized (cubeMapMemo) =
        for entry in cubeMapMemo.CubeMaps do
            DeleteCubeMap entry.Value
        cubeMapMemo.CubeMaps.Clear ()