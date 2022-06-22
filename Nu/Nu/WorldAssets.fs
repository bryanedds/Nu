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

        let (*Literal*) PackageName = Assets.Default.PackageName
        let (*Literal*) Empty = asset<obj> PackageName Assets.Default.EmptyName
        let (*Literal*) Image = asset<Image> PackageName Assets.Default.ImageName
        let (*Literal*) Image2 = asset<Image> PackageName Assets.Default.Image2Name
        let (*Literal*) Image3 = asset<Image> PackageName Assets.Default.Image3Name
        let (*Literal*) Image4 = asset<Image> PackageName Assets.Default.Image4Name
        let (*Literal*) Image5 = asset<Image> PackageName Assets.Default.Image5Name
        let (*Literal*) Image6 = asset<Image> PackageName Assets.Default.Image6Name
        let (*Literal*) Image7 = asset<Image> PackageName Assets.Default.Image7Name
        let (*Literal*) Image8 = asset<Image> PackageName Assets.Default.Image8Name
        let (*Literal*) Image9 = asset<Image> PackageName Assets.Default.Image9Name
        let (*Literal*) Image10 = asset<Image> PackageName Assets.Default.Image10Name
        let (*Literal*) Image11 = asset<Image> PackageName Assets.Default.Image11Name
        let (*Literal*) Image12 = asset<Image> PackageName Assets.Default.Image12Name
        let (*Literal*) ImageEmpty = asset<Image> PackageName Assets.Default.ImageEmptyName
        let (*Literal*) Font = asset<Font> PackageName Assets.Default.FontName
        let (*Literal*) SideViewCharacterIdleImage = asset<Image> PackageName Assets.Default.SideViewCharacterIdleImageName
        let (*Literal*) SideViewCharacterJumpImage = asset<Image> PackageName Assets.Default.SideViewCharacterJumpImageName
        let (*Literal*) SideViewCharacterWalkImage = asset<Image> PackageName Assets.Default.SideViewCharacterWalkImageName
        let (*Literal*) TileMap = asset<TileMap> PackageName Assets.Default.TileMapName
        let (*Literal*) TileMapEmpty = asset<TileMap> PackageName Assets.Default.TileMapEmptyName
        let (*Literal*) StaticModel = asset<StaticModel> PackageName Assets.Default.StaticModelName
        let (*Literal*) Sound = asset<Sound> PackageName Assets.Default.SoundName
        let (*Literal*) Song = asset<Song> PackageName Assets.Default.SongName