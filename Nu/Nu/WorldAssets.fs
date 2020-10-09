// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open Nu
open Nu.Assets

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Assets =

    let (*Literal*) DefaultImage = asset<Image> DefaultPackageName DefaultImageName
    let (*Literal*) DefaultImage2 = asset<Image> DefaultPackageName DefaultImage2Name
    let (*Literal*) DefaultImage3 = asset<Image> DefaultPackageName DefaultImage3Name
    let (*Literal*) DefaultImage4 = asset<Image> DefaultPackageName DefaultImage4Name
    let (*Literal*) DefaultImage5 = asset<Image> DefaultPackageName DefaultImage5Name
    let (*Literal*) DefaultImage6 = asset<Image> DefaultPackageName DefaultImage6Name
    let (*Literal*) DefaultImage7 = asset<Image> DefaultPackageName DefaultImage7Name
    let (*Literal*) DefaultImage8 = asset<Image> DefaultPackageName DefaultImage8Name
    let (*Literal*) DefaultImage9 = asset<Image> DefaultPackageName DefaultImage9Name
    let (*Literal*) DefaultImage10 = asset<Image> DefaultPackageName DefaultImage10Name
    let (*Literal*) DefaultImage11 = asset<Image> DefaultPackageName DefaultImage11Name
    let (*Literal*) DefaultImage12 = asset<Image> DefaultPackageName DefaultImage12Name
    let (*Literal*) DefaultCharacterIdleImage = asset<Image> DefaultPackageName DefaultCharacterIdleImageName
    let (*Literal*) DefaultCharacterJumpImage = asset<Image> DefaultPackageName DefaultCharacterJumpImageName
    let (*Literal*) DefaultCharacterWalkImage = asset<Image> DefaultPackageName DefaultCharacterWalkImageName
    let (*Literal*) DefaultTileMap = asset<TileMap> DefaultPackageName DefaultTileMapName
    let (*Literal*) DefaultFont = asset<Font> DefaultPackageName DefaultFontName
    let (*Literal*) DefaultSound = asset<Sound> DefaultPackageName DefaultSoundName
    let (*Literal*) DefaultSong = asset<Song> DefaultPackageName DefaultSongName
    let [<Literal>] AssetGraphFilePath = Assets.AssetGraphFilePath
    let [<Literal>] OverlayerFilePath = Assets.OverlayerFilePath
    let [<Literal>] PreludeFilePath = Assets.PreludeFilePath