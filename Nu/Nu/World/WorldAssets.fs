// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open Prime

/// The assets that come with the Nu Game Engine.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Assets =

    /// The global assets that come with the Nu Game Engine.
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Global =

        let [<Literal>] AssetGraphFilePath = Assets.Global.AssetGraphFilePath
        let [<Literal>] OverlayerFilePath = Assets.Global.OverlayerFilePath

    /// The default assets that come with the Nu Game Engine.
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Default =

        let [<Uniform>] PackageName = Assets.Default.PackageName
        let [<Uniform>] Image = asset<Image> PackageName Assets.Default.ImageName
        let [<Uniform>] EmptyImage = asset<Image> PackageName Assets.Default.EmptyImageName
        let [<Uniform>] Black = asset<Image> PackageName Assets.Default.BlackName
        let [<Uniform>] White = asset<Image> PackageName Assets.Default.WhiteName
        let [<Uniform>] StaticSprite = asset<Image> PackageName Assets.Default.StaticSpriteName
        let [<Uniform>] AnimatedSprite = asset<Image> PackageName Assets.Default.AnimatedSpriteName
        let [<Uniform>] Cursor = asset<Cursor> PackageName Assets.Default.CursorName
        let [<Uniform>] Block = asset<Image> PackageName Assets.Default.BlockName
        let [<Uniform>] Ball = asset<Image> PackageName Assets.Default.BallName
        let [<Uniform>] Brick = asset<Image> PackageName Assets.Default.BrickName
        let [<Uniform>] Paddle = asset<Image> PackageName Assets.Default.PaddleName
        let [<Uniform>] Fluid = asset<Image> PackageName Assets.Default.FluidName
        let [<Uniform>] Gas = asset<Image> PackageName Assets.Default.GasName
        let [<Uniform>] HighlightSprite = asset<Image> PackageName Assets.Default.HighlightSpriteName
        let [<Uniform>] Character2dIdle = asset<Image> PackageName Assets.Default.Character2dIdleName
        let [<Uniform>] Character2dJump = asset<Image> PackageName Assets.Default.Character2dJumpName
        let [<Uniform>] Character2dWalk = asset<Image> PackageName Assets.Default.Character2dWalkName
        let [<Uniform>] ButtonUp = asset<Image> PackageName Assets.Default.ButtonUpName
        let [<Uniform>] ButtonDown = asset<Image> PackageName Assets.Default.ButtonDownName
        let [<Uniform>] Label = asset<Image> PackageName Assets.Default.LabelName
        let [<Uniform>] Border = asset<Image> PackageName Assets.Default.BorderName
        let [<Uniform>] Panel = asset<Image> PackageName Assets.Default.PanelName
        let [<Uniform>] NuSlide = asset<Image> PackageName Assets.Default.NuSlideName
        let [<Uniform>] HeightMap = asset<Raw> PackageName Assets.Default.HeightMapName
        let [<Uniform>] Font = asset<Font> PackageName Assets.Default.FontName
        let [<Uniform>] TileMap = asset<TileMap> PackageName Assets.Default.TileMapName
        let [<Uniform>] EmptyTileMap = asset<TileMap> PackageName Assets.Default.EmptyTileMapName
        let [<Uniform>] Albedo = asset<Image> PackageName Assets.Default.AlbedoName
        let [<Uniform>] Roughness = asset<Image> PackageName Assets.Default.RoughnessName
        let [<Uniform>] Metallic = asset<Image> PackageName Assets.Default.MetallicName
        let [<Uniform>] AmbientOcclusion = asset<Image> PackageName Assets.Default.AmbientOcclusionName
        let [<Uniform>] Emission = asset<Image> PackageName Assets.Default.EmissionName
        let [<Uniform>] Normal = asset<Image> PackageName Assets.Default.NormalName
        let [<Uniform>] Subdermal = asset<Image> PackageName Assets.Default.SubdermalName
        let [<Uniform>] Fineness = asset<Image> PackageName Assets.Default.FinenessName
        let [<Uniform>] Scatter = asset<Image> PackageName Assets.Default.ScatterName
        let [<Uniform>] UserDefinedImage0 = asset<Image> PackageName Assets.Default.UserDefinedImage0Name
        let [<Uniform>] UserDefinedImage1 = asset<Image> PackageName Assets.Default.UserDefinedImage1Name
        let [<Uniform>] UserDefinedImage2 = asset<Image> PackageName Assets.Default.UserDefinedImage2Name
        let [<Uniform>] TerrainLayer0Albedo = asset<Image> PackageName Assets.Default.TerrainLayer0AlbedoName
        let [<Uniform>] TerrainLayer0Roughness = asset<Image> PackageName Assets.Default.TerrainLayer0RoughnessName
        let [<Uniform>] TerrainLayer0AmbientOcclusion = asset<Image> PackageName Assets.Default.TerrainLayer0AmbientOcclusionName
        let [<Uniform>] TerrainLayer0Normal = asset<Image> PackageName Assets.Default.TerrainLayer0NormalName
        let [<Uniform>] TerrainLayer0UserDefinedImage = asset<Image> PackageName Assets.Default.TerrainLayer0UserDefinedImageName
        let [<Uniform>] TerrainLayer0Blend = asset<Image> PackageName Assets.Default.TerrainLayer0BlendName
        let [<Uniform>] TerrainLayer1Albedo = asset<Image> PackageName Assets.Default.TerrainLayer1AlbedoName
        let [<Uniform>] TerrainLayer1Roughness = asset<Image> PackageName Assets.Default.TerrainLayer1RoughnessName
        let [<Uniform>] TerrainLayer1AmbientOcclusion = asset<Image> PackageName Assets.Default.TerrainLayer1AmbientOcclusionName
        let [<Uniform>] TerrainLayer1Normal = asset<Image> PackageName Assets.Default.TerrainLayer1NormalName
        let [<Uniform>] TerrainLayer1UserDefinedImage = asset<Image> PackageName Assets.Default.TerrainLayer1UserDefinedImageName
        let [<Uniform>] TerrainLayer1Blend = asset<Image> PackageName Assets.Default.TerrainLayer1BlendName
        let [<Uniform>] TerrainTint = asset<Image> PackageName Assets.Default.TerrainTintName
        let [<Uniform>] SkyBoxMap = asset<CubeMap> PackageName Assets.Default.SkyBoxMapName
        let [<Uniform>] StaticModel = asset<StaticModel> PackageName Assets.Default.StaticModelName
        let [<Uniform>] BallModel = asset<StaticModel> PackageName Assets.Default.BallModelName
        let [<Uniform>] HighlightModel = asset<StaticModel> PackageName Assets.Default.HighlightModelName
        let [<Uniform>] LightbulbModel = asset<StaticModel> PackageName Assets.Default.LightbulbModelName
        let [<Uniform>] LightProbeModel = asset<StaticModel> PackageName Assets.Default.LightProbeModelName
        let [<Uniform>] AnimatedModel = asset<AnimatedModel> PackageName Assets.Default.AnimatedModelName
        let [<Uniform>] Sound = asset<Sound> PackageName Assets.Default.SoundName
        let [<Uniform>] Song = asset<Song> PackageName Assets.Default.SongName
        let [<Uniform>] RawIconIcon = asset<Image> PackageName Assets.Default.RawIconName
        let [<Uniform>] TileMapIcon = asset<Image> PackageName Assets.Default.TileMapIconName
        let [<Uniform>] StaticModelIcon = asset<Image> PackageName Assets.Default.StaticModelIconName
        let [<Uniform>] AnimatedModelIcon = asset<Image> PackageName Assets.Default.AnimatedModelIconName
        let [<Uniform>] SoundIcon = asset<Image> PackageName Assets.Default.SoundIconName
        let [<Uniform>] SongIcon = asset<Image> PackageName Assets.Default.SongIconName
        let [<Uniform>] Icons = Set.map (asset<Image> PackageName) Assets.Default.IconNames