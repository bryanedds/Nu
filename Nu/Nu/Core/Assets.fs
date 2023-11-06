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
        let [<Literal>] SideViewCharacterIdleImageName = "SideViewCharacterIdle"
        let [<Literal>] SideViewCharacterJumpImageName = "SideViewCharacterJump"
        let [<Literal>] SideViewCharacterWalkImageName = "SideViewCharacterWalk"
        let [<Literal>] HighlightImageName = "Highlight"
        let [<Literal>] FontName = "Font"
        let [<Literal>] TileMapName = "TileMap"
        let [<Literal>] TileMapEmptyName = "TileMapEmpty"
        let [<Literal>] MaterialAlbedoName = "MaterialAlbedo"
        let [<Literal>] MaterialMetallicName = "MaterialMetallic"
        let [<Literal>] MaterialRoughnessName = "MaterialRoughness"
        let [<Literal>] MaterialAmbientOcclusionName = "MaterialAmbientOcclusion"
        let [<Literal>] MaterialEmissionName = "MaterialEmission"
        let [<Literal>] MaterialNormalName = "MaterialNormal"
        let [<Literal>] MaterialHeightName = "MaterialHeight"
        let [<Literal>] SkyBoxMapName = "SkyBoxMap"
        let [<Literal>] StaticModelName = "StaticModel"
        let [<Literal>] HighlightModelName = "HighlightModel"
        let [<Literal>] LightbulbModelName = "LightbulbModel"
        let [<Literal>] SoundName = "Sound"
        let [<Literal>] SongName = "Song"
        let [<Literal>] ImageString = "[Default Image]"
        let [<Literal>] TileMapString = "[Default TileMap]"
        let [<Literal>] FontString = "[Default Font]"
        let [<Literal>] SoundString = "[Default Sound]"
        let [<Literal>] SongString = "[Default Song]"