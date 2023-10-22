// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

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
        let [<Uniform>] Image = asset<Image> PackageName Assets.Default.ImageName
        let [<Uniform>] ImageEmpty = asset<Image> PackageName Assets.Default.ImageEmptyName
        let [<Uniform>] Black = asset<Image> PackageName Assets.Default.BlackName
        let [<Uniform>] White = asset<Image> PackageName Assets.Default.WhiteName
        let [<Uniform>] ButtonUp = asset<Image> PackageName Assets.Default.ButtonUpName
        let [<Uniform>] ButtonDown = asset<Image> PackageName Assets.Default.ButtonDownName
        let [<Uniform>] Label = asset<Image> PackageName Assets.Default.LabelName
        let [<Uniform>] Border = asset<Image> PackageName Assets.Default.BorderName
        let [<Uniform>] Block = asset<Image> PackageName Assets.Default.BlockName
        let [<Uniform>] Box = asset<Image> PackageName Assets.Default.BoxName
        let [<Uniform>] NuSlide = asset<Image> PackageName Assets.Default.NuSlideName
        let [<Uniform>] SideViewCharacterIdleImage = asset<Image> PackageName Assets.Default.SideViewCharacterIdleImageName
        let [<Uniform>] SideViewCharacterJumpImage = asset<Image> PackageName Assets.Default.SideViewCharacterJumpImageName
        let [<Uniform>] SideViewCharacterWalkImage = asset<Image> PackageName Assets.Default.SideViewCharacterWalkImageName
        let [<Uniform>] HighlightImage = asset<Image> PackageName Assets.Default.HighlightImageName
        let [<Uniform>] HeightMap = asset<Image> PackageName Assets.Default.HeightMapName
        let [<Uniform>] HeightMapRaw = asset<Image> PackageName Assets.Default.HeightMapRawName
        let [<Uniform>] Font = asset<Font> PackageName Assets.Default.FontName
        let [<Uniform>] TileMap = asset<TileMap> PackageName Assets.Default.TileMapName
        let [<Uniform>] TileMapEmpty = asset<TileMap> PackageName Assets.Default.TileMapEmptyName
        let [<Uniform>] MaterialAlbedo = asset<Image> PackageName Assets.Default.MaterialAlbedoName
        let [<Uniform>] MaterialMetallic = asset<Image> PackageName Assets.Default.MaterialMetallicName
        let [<Uniform>] MaterialRoughness = asset<Image> PackageName Assets.Default.MaterialRoughnessName
        let [<Uniform>] MaterialAmbientOcclusion = asset<Image> PackageName Assets.Default.MaterialAmbientOcclusionName
        let [<Uniform>] MaterialEmission = asset<Image> PackageName Assets.Default.MaterialEmissionName
        let [<Uniform>] MaterialNormal = asset<Image> PackageName Assets.Default.MaterialNormalName
        let [<Uniform>] MaterialHeight = asset<Image> PackageName Assets.Default.MaterialHeightName
        let [<Uniform>] SkyBoxMap = asset<CubeMap> PackageName Assets.Default.SkyBoxMapName
        let [<Uniform>] StaticModel = asset<StaticModel> PackageName Assets.Default.StaticModelName
        let [<Uniform>] HighlightModel = asset<StaticModel> PackageName Assets.Default.HighlightModelName
        let [<Uniform>] LightbulbModel = asset<StaticModel> PackageName Assets.Default.LightbulbModelName
        let [<Uniform>] Sound = asset<Sound> PackageName Assets.Default.SoundName
        let [<Uniform>] Song = asset<Song> PackageName Assets.Default.SongName