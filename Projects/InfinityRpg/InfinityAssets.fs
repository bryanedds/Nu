namespace InfinityRpg
open Nu
module Assets =

    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"
    let PickupSheetImage = asset<Image> GameplayPackageName "PickupItems"
    let PlayerImage = asset<Image> GameplayPackageName "Player"
    let GoopyImage = asset<Image> GameplayPackageName "Goopy"
    let BatsyImage = asset<Image> GameplayPackageName "Batsy"
    let ZommieImage = asset<Image> GameplayPackageName "Zommie"
    let FieldTileSheetImage = asset<Image> GameplayPackageName "FieldTileSheet"
    let HerosVengeanceSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> GameplayPackageName "Hero'sVengeance" }
    let ButterflyGirlSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> GuiPackageName "ButterflyGirl" }
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let SaveFilePath = "InfinityRpg.sav"