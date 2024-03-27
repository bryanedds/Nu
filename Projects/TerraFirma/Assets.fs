// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let GuiSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Gui" }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let DesertSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Desert" }
        let SlashSound = asset<Sound> PackageName "Slash"
        let Slash2Sound = asset<Sound> PackageName "Slash2"
        let InjureSound = asset<Sound> PackageName "Injure"
        let JoanModel = asset<AnimatedModel> PackageName "Joan"
        let GreatSwordModel = asset<StaticModel> PackageName "GreatSword"