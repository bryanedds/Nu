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
            ScrollPosition = v3 -960.0f -1800.0f 0.0f
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
            let credits = { credits with ScrollPosition = v3 -480.0f -1800.0f 0.0f }
            just credits

        | Scroll ->
            let credits = { credits with ScrollPosition = credits.ScrollPosition + v3 0.0f (1.0f / 3.0f) 0.0f }
            if credits.ScrollPosition.Y >= 625.0f
            then withSignal (signal StartQuitting) credits
            else just credits

    override this.Command (_, StartQuitting, screen, world) =
        World.fadeOutSong 60L world
        World.publish () screen.QuitCreditsEvent screen world

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
                 Entity.PerimeterCentered == true
                 Entity.Layout == Flow (FlowDownward, FlowUnlimited)]
                [Content.text "OmniBlade" [Entity.Text == "Omni Blade"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "SeparatorA" []
                 Content.text "SeparatorA2" []
                 Content.text "SeparatorA3" []
                 Content.text "SeparatorA4" []
                 Content.text "DirectionAndDesign" [Entity.Text == "- Direction & Design -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "BryanEdds" [Entity.Text == "Bryan Edds"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space" []
                 Content.text "EngineAndGameProgramming" [Entity.Text == "- Engine & Game Programming -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "BryanEdds2" [Entity.Text == "Bryan Edds"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "DeanJLee" [Entity.Text == "Dean J. Lee"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space2" []
                 Content.text "Art" [Entity.Text == "- Art -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Ansimuz" [Entity.Text == "Ansimuz"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "CreativeKind" [Entity.Text == "Creative Kind"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "FinalBossBlues" [Entity.Text == "Final Boss Blues"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Narehop" [Entity.Text == "Narehop"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "PimenArt" [Entity.Text == "Pimen Art"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "SelielTheShaper" [Entity.Text == "Seliel the Shaper"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "SzadiArt" [Entity.Text == "Szadi Art"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space3" []
                 Content.text "Music" [Entity.Text == "- Music -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ClementPanchout" [Entity.Text == "Clement Panchout"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "DrassRayJacobMann" [Entity.Text == "Drass Ray - Jacob Mann"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "JPSoundworks" [Entity.Text == "JP Soundworks"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "OwlTheoryMusic" [Entity.Text == "Owl Theory Music"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "PdkMusic" [Entity.Text == "Pdk Music"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "PeriTune" [Entity.Text == "Peri Tune"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "PjotrKolster" [Entity.Text == "Pjotr Kolster"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Suat" [Entity.Text == "Suat"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ThomasBrunet" [Entity.Text == "Thomas Brunet"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space4" []
                 Content.text "Sound" [Entity.Text == "- Sound -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Kronbits" [Entity.Text == "Kronbits"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Simon1366" [Entity.Text == "Simon 1366"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "SoundWorks12" [Entity.Text == "Sound Works 12"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space5" []
                 Content.text "Testing" [Entity.Text == "- Testing -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ChosenWolf" [Entity.Text == "Chosen Wolf"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "Space6" []
                 Content.text "Supporters" [Entity.Text == "- Supporters -"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ReiMiyasaka" [Entity.Text == "Rei Miyasaka"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "pauloud" [Entity.Text == "pauloud"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ChrisReynolds" [Entity.Text == "Chris Reynolds"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "arsmilitaris" [Entity.Text == "arsmilitaris"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "ibrahim324" [Entity.Text == "ibrahim324"; Entity.Size == v3 960.0f 32.0f 0.0f]
                 Content.text "SeparatorB" []
                 Content.text "SeparatorB2" []
                 Content.text "SeparatorB3" []
                 Content.text "SeparatorB4" []
                 Content.text "SeparatorB5" []
                 Content.text "SeparatorB6" []
                 Content.text "SeparatorB7" []
                 Content.text "SeparatorB8" []
                 Content.text "SeparatorB9" []
                 Content.text "SeparatorB10" []
                 Content.text "ThankYouForPlaying." [Entity.Text == "Thank you for playing."; Entity.Size == v3 960.0f 32.0f 0.0f]]]]