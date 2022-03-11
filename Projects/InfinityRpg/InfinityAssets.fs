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
        let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
        let ButterflyGirlSong = { FadeInMs = 0; FadeOutMs = Constants.Audio.FadeOutMsDefault; Volume = Constants.Audio.SongVolumeDefault; Start = 0.0; Song = asset<Song> PackageName "ButterflyGirl" }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let HealthPickupImage = asset<Image> PackageName "HealthPickup"
        let MagicMissile = asset<Image> PackageName "MagicMissile"
        let LongGrassImage = asset<Image> PackageName "LongGrass"
        let PlayerImage = asset<Image> PackageName "Player"
        let GoopyImage = asset<Image> PackageName "Goopy"
        let BatsyImage = asset<Image> PackageName "Batsy"
        let ZommieImage = asset<Image> PackageName "Zommie"
        let OakSwordStrikeUp = asset<Image> PackageName "OakSwordStrikeUp"
        let OakSwordStrikeRight = asset<Image> PackageName "OakSwordStrikeRight"
        let OakSwordStrikeDown = asset<Image> PackageName "OakSwordStrikeDown"
        let OakSwordStrikeLeft = asset<Image> PackageName "OakSwordStrikeLeft"
        let MagicMissileImpact = asset<Image> PackageName "MagicMissileImpact"
        let FieldTileSheetImage = asset<Image> PackageName "FieldTileSheet"
        let HerosVengeanceSong = { FadeInMs = 0; FadeOutMs = Constants.Audio.FadeOutMsDefault; Volume = Constants.Audio.SongVolumeDefault; Start = 0.0; Song = asset<Song> PackageName "Hero'sVengeance" }