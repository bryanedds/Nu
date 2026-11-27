namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let GuiSong = { FadeInTime = GameTime.zero; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = 1.0f; Song = asset PackageName "Gui" }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let DesertSong = { FadeInTime = GameTime.zero; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = 1.0f; Song = asset PackageName "Desert" }
        let SlashSound = asset<Sound> PackageName "Slash"
        let Slash2Sound = asset<Sound> PackageName "Slash2"
        let InjureSound = asset<Sound> PackageName "Injure"
        let HeartFullImage = asset<Image> PackageName "HeartFull"
        let HeartEmptyImage = asset<Image> PackageName "HeartEmpty"
        let JoanModel = asset<AnimatedModel> PackageName "Joan"
        let GreatSwordModel = asset<StaticModel> PackageName "GreatSword"
        let GrassTuftModel = asset<StaticModel> PackageName "GrassTuft"
        let GrassTuftAlbedoImage = asset<Image> PackageName "GrassTuftAlbedo"
        let FoliageWindNoiseImage = asset<Image> PackageName "FoliageWindNoise_u"
        let TreeModel = asset<StaticModel> PackageName "Tree"
        let TreeAlbedoImage = asset<Image> PackageName "TreeAlbedo"
        let TreeRoughnessImage = asset<Image> PackageName "TreeRoughness"
        let TreeNormalImage = asset<Image> PackageName "TreeNormal"