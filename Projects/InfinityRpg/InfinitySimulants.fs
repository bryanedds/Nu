namespace InfinityRpg
open System
open Nu
module Simulants =

    // gameplay simulants
    let Gameplay = Screen "Gameplay"
    let Hud = Gameplay => "Hud"
    let HudBack = Hud => "Back"
    let HudSaveGame = Hud => "SaveGame"
    let HudHalt = Hud => "Halt"
    let HudFeeler = Hud => "Feeler"
    let HudDetailUp = Hud => "DetailUp"
    let HudDetailRight = Hud => "DetailRight"
    let HudDetailDown = Hud => "DetailDown"
    let HudDetailLeft = Hud => "DetailLeft"
    let Scene = Gameplay => "Scene"
    let Field = Scene => "Field"
    let Player = Scene => "Player"