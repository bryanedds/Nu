namespace InfinityRpg
open System
open Prime
open Nu
module Simulants =

    // globally-accessible simulants
    let Title = Screen "Title"
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