// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Assets =

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Global =

        let [<Literal>] AssetGraphFilePath = Assets.Global.AssetGraphFilePath
        let [<Literal>] OverlayerFilePath = Assets.Global.OverlayerFilePath
        let [<Literal>] PreludeFilePath = Assets.Global.PreludeFilePath

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Default =

        let [<Uniform>] PackageName = Assets.Default.PackageName
        let [<Uniform>] Empty = asset<obj> PackageName Assets.Default.EmptyName
        let [<Uniform>] Image = asset<Image> PackageName Assets.Default.ImageName
        let [<Uniform>] Image2 = asset<Image> PackageName Assets.Default.Image2Name
        let [<Uniform>] Image3 = asset<Image> PackageName Assets.Default.Image3Name
        let [<Uniform>] Image4 = asset<Image> PackageName Assets.Default.Image4Name
        let [<Uniform>] Image5 = asset<Image> PackageName Assets.Default.Image5Name
        let [<Uniform>] Image6 = asset<Image> PackageName Assets.Default.Image6Name
        let [<Uniform>] Image7 = asset<Image> PackageName Assets.Default.Image7Name
        let [<Uniform>] Image8 = asset<Image> PackageName Assets.Default.Image8Name
        let [<Uniform>] Image9 = asset<Image> PackageName Assets.Default.Image9Name
        let [<Uniform>] Image10 = asset<Image> PackageName Assets.Default.Image10Name
        let [<Uniform>] Image11 = asset<Image> PackageName Assets.Default.Image11Name
        let [<Uniform>] Image12 = asset<Image> PackageName Assets.Default.Image12Name
        let [<Uniform>] ImageEmpty = asset<Image> PackageName Assets.Default.ImageEmptyName
        let [<Uniform>] SideViewCharacterIdleImage = asset<Image> PackageName Assets.Default.SideViewCharacterIdleImageName
        let [<Uniform>] SideViewCharacterJumpImage = asset<Image> PackageName Assets.Default.SideViewCharacterJumpImageName
        let [<Uniform>] SideViewCharacterWalkImage = asset<Image> PackageName Assets.Default.SideViewCharacterWalkImageName
        let [<Uniform>] HighlightImage = asset<Image> PackageName Assets.Default.HighlightImageName
        let [<Uniform>] Font = asset<Font> PackageName Assets.Default.FontName
        let [<Uniform>] TileMap = asset<TileMap> PackageName Assets.Default.TileMapName
        let [<Uniform>] TileMapEmpty = asset<TileMap> PackageName Assets.Default.TileMapEmptyName
        let [<Uniform>] MaterialAlbedo = asset<Image> PackageName Assets.Default.MaterialAlbedoName
        let [<Uniform>] MaterialMetalness = asset<Image> PackageName Assets.Default.MaterialMetalnessName
        let [<Uniform>] MaterialRoughness = asset<Image> PackageName Assets.Default.MaterialRoughnessName
        let [<Uniform>] MaterialAmbientOcclusion = asset<Image> PackageName Assets.Default.MaterialAmbientOcclusionName
        let [<Uniform>] MaterialNormal = asset<Image> PackageName Assets.Default.MaterialNormalName
        let [<Uniform>] SkyBoxMap = asset<CubeMap> PackageName Assets.Default.SkyBoxMapName
        let [<Uniform>] StaticModel = asset<StaticModel> PackageName Assets.Default.StaticModelName
        let [<Uniform>] HighlightModel = asset<StaticModel> PackageName Assets.Default.HighlightModelName
        let [<Uniform>] Sound = asset<Sound> PackageName Assets.Default.SoundName
        let [<Uniform>] Song = asset<Song> PackageName Assets.Default.SongName