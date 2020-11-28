namespace InfinityRpg
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Global =

        let SaveFilePath = "InfinityRpg.sav"

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
        let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
        let ButterflyGirlSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "ButterflyGirl" }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let PickupSheetImage = asset<Image> PackageName "PickupItems"
        let PlayerImage = asset<Image> PackageName "Player"
        let GoopyImage = asset<Image> PackageName "Goopy"
        let BatsyImage = asset<Image> PackageName "Batsy"
        let ZommieImage = asset<Image> PackageName "Zommie"
        let FieldTileSheetImage = asset<Image> PackageName "FieldTileSheet"
        let HerosVengeanceSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Hero'sVengeance" }