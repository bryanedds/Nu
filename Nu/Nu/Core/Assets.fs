// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Global =
        
        let [<Literal>] AssetGraphFilePath = "AssetGraph.nuag"
        let [<Literal>] OverlayerFilePath = "Overlayer.nuol"

    [<RequireQualifiedAccess>]
    module Default =

        let [<Literal>] PackageName = "Default"
        let [<Literal>] ImageName = "Image"
        let [<Literal>] ImageEmptyName = "ImageEmpty"
        let [<Literal>] BlackName = "Black"
        let [<Literal>] WhiteName = "White"
        let [<Literal>] ButtonUpName = "ButtonUp"
        let [<Literal>] ButtonDownName = "ButtonDown"
        let [<Literal>] LabelName = "Label"
        let [<Literal>] BorderName = "Border"
        let [<Literal>] BlockName = "Block"
        let [<Literal>] BoxName = "Box"
        let [<Literal>] NuSlideName = "NuSlide"
        let [<Literal>] Character2dIdleImageName = "Character2dIdle"
        let [<Literal>] Character2dJumpImageName = "Character2dJump"
        let [<Literal>] Character2dWalkImageName = "Character2dWalk"
        let [<Literal>] HighlightImageName = "Highlight"
        let [<Literal>] HeightMapName = "HeightMap"
        let [<Literal>] FontName = "Font"
        let [<Literal>] TileMapName = "TileMap"
        let [<Literal>] TileMapEmptyName = "TileMapEmpty"
        let [<Literal>] MaterialAlbedoName = "MaterialAlbedo"
        let [<Literal>] MaterialRoughnessName = "MaterialRoughness"
        let [<Literal>] MaterialMetallicName = "MaterialMetallic"
        let [<Literal>] MaterialAmbientOcclusionName = "MaterialAmbientOcclusion"
        let [<Literal>] MaterialEmissionName = "MaterialEmission"
        let [<Literal>] MaterialNormalName = "MaterialNormal"
        let [<Literal>] MaterialHeightName = "MaterialHeight"
        let [<Literal>] TerrainLayerAlbedoName = "TerrainLayerAlbedo"
        let [<Literal>] TerrainLayerRoughnessName = "TerrainLayerRoughness"
        let [<Literal>] TerrainLayerAmbientOcclusionName = "TerrainLayerAmbientOcclusion"
        let [<Literal>] TerrainLayerNormalName = "TerrainLayerNormal"
        let [<Literal>] TerrainLayerHeightName = "TerrainLayerHeight"
        let [<Literal>] TerrainLayerSplatName = "TerrainLayerSplat"
        let [<Literal>] TerrainLayer2AlbedoName = "TerrainLayer2Albedo"
        let [<Literal>] TerrainLayer2RoughnessName = "TerrainLayer2Roughness"
        let [<Literal>] TerrainLayer2AmbientOcclusionName = "TerrainLayer2AmbientOcclusion"
        let [<Literal>] TerrainLayer2NormalName = "TerrainLayer2Normal"
        let [<Literal>] TerrainLayer2HeightName = "TerrainLayer2Height"
        let [<Literal>] TerrainLayer2SplatName = "TerrainLayer2Splat"
        let [<Literal>] TerrainTintName = "TerrainTint"
        let [<Literal>] SkyBoxMapName = "SkyBoxMap"
        let [<Literal>] StaticModelName = "StaticModel"
        let [<Literal>] HighlightModelName = "HighlightModel"
        let [<Literal>] LightbulbModelName = "LightbulbModel"
        let [<Literal>] AnimatedModelName = "AnimatedModel"
        let [<Literal>] SoundName = "Sound"
        let [<Literal>] SongName = "Song"
        let [<Literal>] ImageString = "[Default Image]"
        let [<Literal>] TileMapString = "[Default TileMap]"
        let [<Literal>] FontString = "[Default Font]"
        let [<Literal>] SoundString = "[Default Sound]"
        let [<Literal>] SongString = "[Default Song]"