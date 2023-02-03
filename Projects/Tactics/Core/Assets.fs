// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Tactics
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Global =

        let SaveFilePath1 = "Tactics1.sav"
        let SaveFilePath2 = "Tactics2.sav"
        let SaveFilePath3 = "Tactics3.sav"

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let Font = asset<Font> PackageName "Font" // TODO: P1: See if this is openly licensed (manaspace font).
        let Splash = asset<Image> PackageName "Splash"
        let HeaderImage = asset<Image> PackageName "Header"
        let DialogThinImage = asset<Image> PackageName "DialogThin"
        let DialogThickImage = asset<Image> PackageName "DialogThick"
        let DialogFatImage = asset<Image> PackageName "DialogFat"
        let DialogXLImage = asset<Image> PackageName "DialogXL"
        let DialogXXLImage = asset<Image> PackageName "DialogXXL"
        let ButtonSmallUpImage = asset<Image> PackageName "ButtonSmallUp"
        let ButtonSmallDownImage = asset<Image> PackageName "ButtonSmallDown"
        let ButtonUpImage = asset<Image> PackageName "ButtonUp"
        let ButtonDownImage = asset<Image> PackageName "ButtonDown"
        let ButtonBigUpImage = asset<Image> PackageName "ButtonBigUp"
        let ButtonBigDownImage = asset<Image> PackageName "ButtonBigDown"
        let ButtonLongUpImage = asset<Image> PackageName "ButtonLongUp"
        let ButtonLongDownImage = asset<Image> PackageName "ButtonLongDown"
        let ButtonSquishedUpImage = asset<Image> PackageName "ButtonSquishedUp"
        let ButtonSquishedDownImage = asset<Image> PackageName "ButtonSquishedDown"
        let ButtonShortUpImage = asset<Image> PackageName "ButtonShortUp"
        let ButtonShortDownImage = asset<Image> PackageName "ButtonShortDown"
        let AffirmSound = asset<Sound> PackageName "Affirm"
        let MistakeSound = asset<Sound> PackageName "Mistake"
        let SlotSound = asset<Sound> PackageName "Slot"
        let TitleSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Title" }
        let IntroSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Intro" }
        let TitleGroupFilePath = "Assets/Gui/Groups/Title.nugroup"
        let PickGroupFilePath = "Assets/Gui/Groups/Pick.nugroup"
        let IntroGroupFilePath = "Assets/Gui/Groups/Intro.nugroup"
        let Intro2GroupFilePath = "Assets/Gui/Groups/Intro2.nugroup"
        let Intro3GroupFilePath = "Assets/Gui/Groups/Intro3.nugroup"
        let Intro4GroupFilePath = "Assets/Gui/Groups/Intro4.nugroup"
        let Intro5GroupFilePath = "Assets/Gui/Groups/Intro5.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Groups/Credits.nugroup"

    [<RequireQualifiedAccess>]
    module Atlas =

        let PackageName = "Atlas"

    [<RequireQualifiedAccess>]
    module Field =

        let PackageName = "Field"