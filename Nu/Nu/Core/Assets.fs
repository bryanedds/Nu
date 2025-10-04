// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu

/// Active patterns for recognizing asset file extensions.
[<AutoOpen>]
module AssetPatterns =

    let (|RawExtension|_|) extension = match extension with ".raw" -> Some extension | _ -> None
    let (|ImageExtension|_|) extension = match extension with ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" | ".dds" -> Some extension | _ -> None
    let (|FontExtension|_|) extension = match extension with ".ttf" -> Some extension | _ -> None
    let (|TileMapExtension|_|) extension = match extension with ".tmx" -> Some extension | _ -> None
    let (|SpineSkeletonExtension|_|) extension = match extension with ".skel" | ".json" -> Some extension | _ -> None
    let (|CubeMapExtension|_|) extension = match extension with ".cbm" -> Some extension | _ -> None
    let (|SoundExtension|_|) extension = match extension with ".wav" -> Some extension | _ -> None
    let (|SongExtension|_|) extension = match extension with ".ogg" | ".mp3" -> Some extension | _ -> None
    let (|ModelExtension|_|) extension = match extension with ".fbx" | ".gltf" | ".glb" | ".dae" | ".obj" -> Some extension | _ -> None
    let (|CsvExtension|_|) extension = match extension with ".csv" -> Some extension | _ -> None
    let (|CursorExtension|_|) extension = match extension with ".cur" -> Some extension | _ -> None

/// The assets that come with the Nu Game Engine.
[<RequireQualifiedAccess>]
module Assets =

    /// The global assets that come with the Nu Game Engine.
    [<RequireQualifiedAccess>]
    module Global =

        let [<Literal>] AssetGraphFilePath = "AssetGraph.nuag"
        let [<Literal>] OverlayerFilePath = "Overlayer.nuol"

    /// The default assets that come with the Nu Game Engine.
    [<RequireQualifiedAccess>]
    module Default =

        let [<Literal>] PackageName = "Default"
        let [<Literal>] ImageName = "Image"
        let [<Literal>] EmptyImageName = "EmptyImage"
        let [<Literal>] BlackName = "Black"
        let [<Literal>] WhiteName = "White"
        let [<Literal>] ButtonUpName = "ButtonUp"
        let [<Literal>] ButtonDownName = "ButtonDown"
        let [<Literal>] LabelName = "Label"
        let [<Literal>] BorderName = "Border"
        let [<Literal>] PanelName = "Panel"
        let [<Literal>] BlockName = "Block"
        let [<Literal>] BallName = "Ball"
        let [<Literal>] BrickName = "Brick"
        let [<Literal>] PaddleName = "Paddle"
        let [<Literal>] FluidName = "Fluid"
        let [<Literal>] StaticSpriteName = "StaticSprite"
        let [<Literal>] AnimatedSpriteName = "AnimatedSprite"
        let [<Literal>] SpineSkeletonName = "SpineSkeleton-pro"
        let [<Literal>] CursorName = "Cursor"
        let [<Literal>] NuSlideName = "NuSlide"
        let [<Literal>] Character2dIdleName = "Character2dIdle"
        let [<Literal>] Character2dJumpName = "Character2dJump"
        let [<Literal>] Character2dWalkName = "Character2dWalk"
        let [<Literal>] HighlightSpriteName = "Highlight"
        let [<Literal>] HeightMapName = "HeightMap"
        let [<Literal>] FontName = "Font"
        let [<Literal>] TileMapName = "TileMap"
        let [<Literal>] EmptyTileMapName = "EmptyTileMap"
        let [<Literal>] SkyBoxMapName = "SkyBoxMap"
        let [<Literal>] MaterialAlbedoName = "MaterialAlbedo"
        let [<Literal>] MaterialRoughnessName = "MaterialRoughness"
        let [<Literal>] MaterialMetallicName = "MaterialMetallic"
        let [<Literal>] MaterialAmbientOcclusionName = "MaterialAmbientOcclusion"
        let [<Literal>] MaterialEmissionName = "MaterialEmission"
        let [<Literal>] MaterialNormalName = "MaterialNormal"
        let [<Literal>] MaterialHeightName = "MaterialHeight"
        let [<Literal>] MaterialSubdermalName = "MaterialSubdermal"
        let [<Literal>] MaterialFinenessName = "MaterialFineness"
        let [<Literal>] MaterialScatterName = "MaterialScatter"
        let [<Literal>] TerrainLayer0AlbedoName = "TerrainLayer0Albedo"
        let [<Literal>] TerrainLayer0RoughnessName = "TerrainLayer0Roughness"
        let [<Literal>] TerrainLayer0AmbientOcclusionName = "TerrainLayer0AmbientOcclusion"
        let [<Literal>] TerrainLayer0NormalName = "TerrainLayer0Normal"
        let [<Literal>] TerrainLayer0HeightName = "TerrainLayer0Height"
        let [<Literal>] TerrainLayer0BlendName = "TerrainLayer0Blend"
        let [<Literal>] TerrainLayer1AlbedoName = "TerrainLayer1Albedo"
        let [<Literal>] TerrainLayer1RoughnessName = "TerrainLayer1Roughness"
        let [<Literal>] TerrainLayer1AmbientOcclusionName = "TerrainLayer1AmbientOcclusion"
        let [<Literal>] TerrainLayer1NormalName = "TerrainLayer1Normal"
        let [<Literal>] TerrainLayer1HeightName = "TerrainLayer1Height"
        let [<Literal>] TerrainLayer1BlendName = "TerrainLayer1Blend"
        let [<Literal>] TerrainTintName = "TerrainTint"
        let [<Literal>] StaticModelName = "StaticModel"
        let [<Literal>] BallModelName = "BallModel"
        let [<Literal>] HighlightModelName = "HighlightModel"
        let [<Literal>] LightbulbModelName = "LightbulbModel"
        let [<Literal>] LightProbeModelName = "LightProbeModel"
        let [<Literal>] AnimatedModelName = "AnimatedModel"
        let [<Literal>] SoundName = "Sound"
        let [<Literal>] SongName = "Song"