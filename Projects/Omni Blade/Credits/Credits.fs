// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type Credits =
    { ScrollPosition : Vector3
      QuitVisible : bool }

    static member empty =
        { ScrollPosition = v3Zero
          QuitVisible = true }

    static member make quitVisible =
        { Credits.empty with
            ScrollPosition = v3 -72.0f -1800.0f 0.0f
            QuitVisible = quitVisible }

type CreditsMessage =
    | StartScrolling
    | Scroll
    interface Message

type CreditsCommand =
    | StartQuitting
    interface Command

[<AutoOpen>]
module CreditsExtensions =
    type Screen with
        member this.GetCredits world = this.GetModelGeneric<Credits> world
        member this.SetCredits value world = this.SetModelGeneric<Credits> value world
        member this.Credits = this.ModelGeneric<Credits> ()
        member this.QuitCreditsEvent = Events.QuitCreditsEvent --> this

type CreditsDispatcher () =
    inherit ScreenDispatcher<Credits, CreditsMessage, CreditsCommand> (Credits.empty)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Credits.make true
        else Credits.empty

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartScrolling
         Screen.UpdateEvent => Scroll]

    override this.Message (credits, message, _, world) =

        match message with
        | StartScrolling ->
            World.playSong
                Assets.Gui.CreditsSong.FadeInTime
                Assets.Gui.CreditsSong.FadeOutTime
                Assets.Gui.CreditsSong.StartTime
                Assets.Gui.CreditsSong.RepeatLimitOpt
                Assets.Gui.CreditsSong.Volume
                Assets.Gui.CreditsSong.Song
                world
            let credits = { credits with ScrollPosition = v3 -72.0f -1800.0f 0.0f }
            just credits

        | Scroll ->
            let credits = { credits with ScrollPosition = credits.ScrollPosition + v3 0.0f (1.0f / 3.0f) 0.0f }
            if credits.ScrollPosition.Y >= 620.0f
            then withSignal (signal StartQuitting) credits
            else just credits

    override this.Command (_, StartQuitting, screen, world) =
        World.fadeOutSong 60L world
        let world = World.publish () screen.QuitCreditsEvent screen world
        just world

    override this.Content (credits, _) =
        [Content.group Simulants.CreditsGui.Name []
            [Content.staticSprite "Background"
                [Entity.Size == v3 960.0f 540.0f 0.0f
                 Entity.PerimeterCentered == true
                 Entity.StaticImage == Assets.Default.Black
                 Entity.Absolute == true]
             Content.button "Quit"
                [Entity.Position == v3 324.0f -258.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                 Entity.UpImage == Assets.Gui.ButtonClearUpImage
                 Entity.DownImage == Assets.Gui.ButtonClearDownImage
                 Entity.TextColor == Color.White.WithA8 (byte 127)
                 Entity.Visible := credits.QuitVisible
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]
             Content.panel "Panel"
                [Entity.Position := credits.ScrollPosition
                 Entity.Size == v3 0.0f 3000.0f 0.0f
                 Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                 Entity.PerimeterCentered == true]
                [Content.text "- Omni Blade -" [Entity.Text == "- Omni Blade -"]
                 Content.text "Separator A" []
                 Content.text "Separator A 2" []
                 Content.text "Separator A 3" []
                 Content.text "Separator A 4" []
                 Content.text "- Direction & Design -" [Entity.Text == "- Direction & Design -"]
                 Content.text "Bryan Edds" [Entity.Text == "Bryan Edds"]
                 Content.text "Space" []
                 Content.text "- Engine & Game Programming -" [Entity.Text == "- Engine & Game Programming -"]
                 Content.text "Bryan Edds 2" [Entity.Text == "Bryan Edds"]
                 Content.text "Dean J. Lee" [Entity.Text == "Dean J. Lee"]
                 Content.text "Space 2" []
                 Content.text "- Art -" [Entity.Text == "- Art -"]
                 Content.text "Ansimuz" [Entity.Text == "Ansimuz"]
                 Content.text "Creative Kind" [Entity.Text == "Creative Kind"]
                 Content.text "Final Boss Blues" [Entity.Text == "Final Boss Blues"]
                 Content.text "Narehop" [Entity.Text == "Narehop"]
                 Content.text "Pimen Art" [Entity.Text == "Pimen Art"]
                 Content.text "Seliel the Shaper" [Entity.Text == "Seliel the Shaper"]
                 Content.text "Szadi Art" [Entity.Text == "Szadi Art"]
                 Content.text "Space 3" []
                 Content.text "- Music -" [Entity.Text == "- Music -"]
                 Content.text "Clement Panchout" [Entity.Text == "Clement Panchout"]
                 Content.text "Drass Ray - Jacob Mann" [Entity.Text == "Drass Ray - Jacob Mann"]
                 Content.text "JP Soundworks" [Entity.Text == "JP Soundworks"]
                 Content.text "Owl Theory Music" [Entity.Text == "Owl Theory Music"]
                 Content.text "Pdk Music" [Entity.Text == "Pdk Music"]
                 Content.text "Peri Tune" [Entity.Text == "Peri Tune"]
                 Content.text "Suat" [Entity.Text == "Suat"]
                 Content.text "Thomas Brunet" [Entity.Text == "Thomas Brunet"]
                 Content.text "Space 4" []
                 Content.text "- Sound -" [Entity.Text == "- Sound -"]
                 Content.text "Kronbits" [Entity.Text == "Kronbits"]
                 Content.text "Simon 1366" [Entity.Text == "Simon 1366"]
                 Content.text "Sound Works 12" [Entity.Text == "Sound Works 12"]
                 Content.text "Space 5" []
                 Content.text "- Testing -" [Entity.Text == "- Testing -"]
                 Content.text "Chosen Wolf" [Entity.Text == "Chosen Wolf"]
                 Content.text "Space 6" []
                 Content.text "- Supporters -" [Entity.Text == "- Supporters -"]
                 Content.text "Rei Miyasaka" [Entity.Text == "Rei Miyasaka"]
                 Content.text "pauloud" [Entity.Text == "pauloud"]
                 Content.text "Chris Reynolds" [Entity.Text == "Chris Reynolds"]
                 Content.text "arsmilitaris" [Entity.Text == "arsmilitaris"]
                 Content.text "ibrahim324" [Entity.Text == "ibrahim324"]
                 Content.text "Separator B" []
                 Content.text "Separator B 2" []
                 Content.text "Separator B 3" []
                 Content.text "Separator B 4" []
                 Content.text "Separator B 5" []
                 Content.text "Separator B 6" []
                 Content.text "Separator B 7" []
                 Content.text "Separator B 8" []
                 Content.text "Separator B 9" []
                 Content.text "Separator B 10" []
                 Content.text "Separator B 11" []
                 Content.text "Thank you for playing." [Entity.Text == "Thank you for playing."]]]]