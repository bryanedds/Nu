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

    /// The key identifying a cube map.
    /// TODO: 3D: consider keeping the original cube map face list file path in key so that its faces can be changed when recreated.
    type CubeMapMemoKey =
        string * string * string * string * string * string

    /// Memoizes cube map loads.
    type [<NoEquality; NoComparison>] CubeMapMemo =
        private
            { CubeMaps : Dictionary<CubeMapMemoKey, uint> }

        /// Make a cube map memoizer.
        static member make () =
            { CubeMaps = Dictionary HashIdentity.Structural }

    /// Attempt to create a cube map from 6 files.
    let private TryCreateCubeMapInternal (cubeMapOpt, faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =

        // bind new cube map
        let cubeMap = match cubeMapOpt with Some cubeMap -> cubeMap | None -> Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // load faces into cube map
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                match Texture.TryCreateImageData faceFilePath with
                | Some (metadata, imageData, disposer) ->
                    use d = disposer
                    Gl.TexImage2D (LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i), 0, InternalFormat.Rgba8, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, imageData)
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
            Texture.DeleteTexture cubeMap
            Left error

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =
        TryCreateCubeMapInternal (None, faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)

    /// Delete a cube map.
    let DeleteCubeMap (cubeMap : uint) =
        Gl.DeleteTextures cubeMap

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMapMemoized (cubeMapMemoKey, cubeMapMemo) =

        // deconstruct key
        let (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) = cubeMapMemoKey

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

    /// Recreate the memoized cube maps.
    let RecreateCubeMapsMemoized cubeMapMemo =
        for entry in cubeMapMemo.CubeMaps do
            let (f0, f1, f2, f3, f4, f5) = entry.Key
            let cubeMap = entry.Value
            TryCreateCubeMapInternal (Some cubeMap, f0, f1, f2, f3, f4, f5) |> ignore

    /// Delete a memoized cube map.
    let DeleteCubeMapMemoized cubeMapKey (cubeMapMemo : CubeMapMemo) =
        match cubeMapMemo.CubeMaps.TryGetValue cubeMapKey with
        | (true, cubeMap) ->
            DeleteCubeMap cubeMap
            cubeMapMemo.CubeMaps.Remove cubeMapKey |> ignore<bool>
        | (false, _) -> ()

    /// Delete memoized cube maps.
    let DeleteCubeMapsMemoized (cubeMapMemo) =
        for entry in cubeMapMemo.CubeMaps do
            DeleteCubeMap entry.Value
        cubeMapMemo.CubeMaps.Clear ()