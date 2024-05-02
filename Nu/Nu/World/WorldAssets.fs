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

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Default =

        let [<Uniform>] PackageName = Assets.Default.PackageName
        let [<Uniform>] Image = asset<Image> PackageName Assets.Default.ImageName
        let [<Uniform>] EmptyImage = asset<Image> PackageName Assets.Default.EmptyImageName
        let [<Uniform>] Black = asset<Image> PackageName Assets.Default.BlackName
        let [<Uniform>] White = asset<Image> PackageName Assets.Default.WhiteName
        let [<Uniform>] ButtonUp = asset<Image> PackageName Assets.Default.ButtonUpName
        let [<Uniform>] ButtonDown = asset<Image> PackageName Assets.Default.ButtonDownName
        let [<Uniform>] Label = asset<Image> PackageName Assets.Default.LabelName
        let [<Uniform>] Border = asset<Image> PackageName Assets.Default.BorderName
        let [<Uniform>] Panel = asset<Image> PackageName Assets.Default.PanelName
        let [<Uniform>] Ball = asset<Image> PackageName Assets.Default.BallName
        let [<Uniform>] Brick = asset<Image> PackageName Assets.Default.BrickName
        let [<Uniform>] StaticSprite = asset<Image> PackageName Assets.Default.StaticSpriteName
        let [<Uniform>] AnimatedSprite = asset<Image> PackageName Assets.Default.AnimatedSpriteName
        let [<Uniform>] NuSlide = asset<Image> PackageName Assets.Default.NuSlideName
        let [<Uniform>] Character2dIdle = asset<Image> PackageName Assets.Default.Character2dIdleName
        let [<Uniform>] Character2dJump = asset<Image> PackageName Assets.Default.Character2dJumpName
        let [<Uniform>] Character2dWalk = asset<Image> PackageName Assets.Default.Character2dWalkName
        let [<Uniform>] HighlightSprite = asset<Image> PackageName Assets.Default.HighlightSpriteName
        let [<Uniform>] HeightMap = asset<Raw> PackageName Assets.Default.HeightMapName
        let [<Uniform>] Font = asset<Font> PackageName Assets.Default.FontName
        let [<Uniform>] TileMap = asset<TileMap> PackageName Assets.Default.TileMapName
        let [<Uniform>] EmptyTileMap = asset<TileMap> PackageName Assets.Default.EmptyTileMapName
        let [<Uniform>] MaterialAlbedo = asset<Image> PackageName Assets.Default.MaterialAlbedoName
        let [<Uniform>] MaterialRoughness = asset<Image> PackageName Assets.Default.MaterialRoughnessName
        let [<Uniform>] MaterialMetallic = asset<Image> PackageName Assets.Default.MaterialMetallicName
        let [<Uniform>] MaterialAmbientOcclusion = asset<Image> PackageName Assets.Default.MaterialAmbientOcclusionName
        let [<Uniform>] MaterialEmission = asset<Image> PackageName Assets.Default.MaterialEmissionName
        let [<Uniform>] MaterialNormal = asset<Image> PackageName Assets.Default.MaterialNormalName
        let [<Uniform>] MaterialHeight = asset<Image> PackageName Assets.Default.MaterialHeightName
        let [<Uniform>] TerrainLayer0Albedo = asset<Image> PackageName Assets.Default.TerrainLayer0AlbedoName
        let [<Uniform>] TerrainLayer0Roughness = asset<Image> PackageName Assets.Default.TerrainLayer0RoughnessName
        let [<Uniform>] TerrainLayer0AmbientOcclusion = asset<Image> PackageName Assets.Default.TerrainLayer0AmbientOcclusionName
        let [<Uniform>] TerrainLayer0Normal = asset<Image> PackageName Assets.Default.TerrainLayer0NormalName
        let [<Uniform>] TerrainLayer0Height = asset<Image> PackageName Assets.Default.TerrainLayer0HeightName
        let [<Uniform>] TerrainLayer0Blend = asset<Image> PackageName Assets.Default.TerrainLayer0BlendName
        let [<Uniform>] TerrainLayer1Albedo = asset<Image> PackageName Assets.Default.TerrainLayer1AlbedoName
        let [<Uniform>] TerrainLayer1Roughness = asset<Image> PackageName Assets.Default.TerrainLayer1RoughnessName
        let [<Uniform>] TerrainLayer1AmbientOcclusion = asset<Image> PackageName Assets.Default.TerrainLayer1AmbientOcclusionName
        let [<Uniform>] TerrainLayer1Normal = asset<Image> PackageName Assets.Default.TerrainLayer1NormalName
        let [<Uniform>] TerrainLayer1Height = asset<Image> PackageName Assets.Default.TerrainLayer1HeightName
        let [<Uniform>] TerrainLayer1Blend = asset<Image> PackageName Assets.Default.TerrainLayer1BlendName
        let [<Uniform>] TerrainTint = asset<Image> PackageName Assets.Default.TerrainTintName
        let [<Uniform>] SkyBoxMap = asset<CubeMap> PackageName Assets.Default.SkyBoxMapName
        let [<Uniform>] StaticModel = asset<StaticModel> PackageName Assets.Default.StaticModelName
        let [<Uniform>] HighlightModel = asset<StaticModel> PackageName Assets.Default.HighlightModelName
        let [<Uniform>] LightbulbModel = asset<StaticModel> PackageName Assets.Default.LightbulbModelName
        let [<Uniform>] LightProbeModel = asset<StaticModel> PackageName Assets.Default.LightProbeModelName
        let [<Uniform>] AnimatedModel = asset<AnimatedModel> PackageName Assets.Default.AnimatedModelName
        let [<Uniform>] Sound = asset<Sound> PackageName Assets.Default.SoundName
        let [<Uniform>] Song = asset<Song> PackageName Assets.Default.SongName