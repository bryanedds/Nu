// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type Credits =
    { ScrollPosition : Vector3 }

    static member empty =
        { ScrollPosition = v3Zero }

    static member initial =
        { Credits.empty with ScrollPosition = v3 -72.0f -1800.0f 0.0f }

type CreditsMessage =
    | Start
    | Scroll
    interface Message

type CreditsCommand =
    | QuitCredits
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

    override this.Definitions (_, _) =
        [Screen.SelectEvent => Start
         Screen.UpdateEvent => Scroll]

    override this.Message (credits, message, _, _) =

        match message with
        | Start ->
            just Credits.initial

        | Scroll ->
            let credits = { credits with ScrollPosition = credits.ScrollPosition + v3 0.0f (1.0f / 3.0f) 0.0f }
            if credits.ScrollPosition.Y > 605.0f
            then withSignal (signal QuitCredits) credits
            else just credits

    override this.Command (_, _, screen, world) =
        let world = World.publish () screen.QuitCreditsEvent screen world
        just world

    override this.Content (credits, _) =
        [Content.group Simulants.CreditsGui.Name []
            [Content.staticSprite "Background"
                [Entity.Size == v3 960.0f 540.0f 0.0f
                 Entity.PerimeterCentered == true
                 Entity.StaticImage == Assets.Default.Black
                 Entity.Absolute == true]
             Content.panel "Panel"
                [Entity.Position := credits.ScrollPosition
                 Entity.Size == v3 0.0f 3000.0f 0.0f
                 Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                 Entity.PerimeterCentered == true]
                [Content.text "OmniBlade" [Entity.Text == "OmniBlade"; Entity.PerimeterCentered == true]
                 Content.text "Separator A" [Entity.PerimeterCentered == true]
                 Content.text "Separator A 2" [Entity.PerimeterCentered == true]
                 Content.text "Separator A 3" [Entity.PerimeterCentered == true]
                 Content.text "Separator A 4" [Entity.PerimeterCentered == true]
                 Content.text "Separator A 5" [Entity.PerimeterCentered == true]
                 Content.text "- Direction & Design -" [Entity.Text == "- Direction & Design -"; Entity.PerimeterCentered == true]
                 Content.text "Bryan Edds" [Entity.Text == "Bryan Edds"; Entity.PerimeterCentered == true]
                 Content.text "Space" [Entity.PerimeterCentered == true]
                 Content.text "- Engine Programming -" [Entity.Text == "- Engine Programming -"; Entity.PerimeterCentered == true]
                 Content.text "Bryan Edds 2" [Entity.Text == "Bryan Edds"; Entity.PerimeterCentered == true]
                 Content.text "Dean J. Lee" [Entity.Text == "Dean J. Lee"; Entity.PerimeterCentered == true]
                 Content.text "Space 2" [Entity.PerimeterCentered == true]
                 Content.text "- Game Programming -" [Entity.Text == "- Game Programming -"; Entity.PerimeterCentered == true]
                 Content.text "Bryan Edds 3" [Entity.Text == "Bryan Edds"; Entity.PerimeterCentered == true]
                 Content.text "Space 3" [Entity.PerimeterCentered == true]
                 Content.text "- Effects Programming -" [Entity.Text == "- Effects Programming -"; Entity.PerimeterCentered == true]
                 Content.text "Bryan Edds 4" [Entity.Text == "Bryan Edds"; Entity.PerimeterCentered == true]
                 Content.text "Dean J. Lee 2" [Entity.Text == "Dean J. Lee"; Entity.PerimeterCentered == true]
                 Content.text "Space 4" [Entity.PerimeterCentered == true]
                 Content.text "- Art -" [Entity.Text == "- Art -"; Entity.PerimeterCentered == true]
                 Content.text "Ansimuz" [Entity.Text == "Ansimuz"; Entity.PerimeterCentered == true]
                 Content.text "CreativeKind" [Entity.Text == "CreativeKind"; Entity.PerimeterCentered == true]
                 Content.text "FinalBossBlues" [Entity.Text == "FinalBossBlues"; Entity.PerimeterCentered == true]
                 Content.text "Narehop" [Entity.Text == "Narehop"; Entity.PerimeterCentered == true]
                 Content.text "Pimen Art" [Entity.Text == "Pimen Art"; Entity.PerimeterCentered == true]
                 Content.text "Selier the Shaper" [Entity.Text == "Selier the Shaper"; Entity.PerimeterCentered == true]
                 Content.text "Szadi Art" [Entity.Text == "Szadi Art"; Entity.PerimeterCentered == true]
                 Content.text "Space 5" [Entity.PerimeterCentered == true]
                 Content.text "- Music -" [Entity.Text == "- Music -"; Entity.PerimeterCentered == true]
                 Content.text "Clement Panchout" [Entity.Text == "Clement Panchout"; Entity.PerimeterCentered == true]
                 Content.text "Drass Ray - Jacob Mann" [Entity.Text == "Drass Ray - Jacob Mann"; Entity.PerimeterCentered == true]
                 Content.text "JP Soundworks" [Entity.Text == "JP Soundworks"; Entity.PerimeterCentered == true]
                 Content.text "Owl Theory Music" [Entity.Text == "Owl Theory Music"; Entity.PerimeterCentered == true]
                 Content.text "Pdk Music" [Entity.Text == "Pdk Music"; Entity.PerimeterCentered == true]
                 Content.text "PeriTune" [Entity.Text == "PeriTune"; Entity.PerimeterCentered == true]
                 Content.text "Suat" [Entity.Text == "Suat"; Entity.PerimeterCentered == true]
                 Content.text "Thomas Brunet" [Entity.Text == "Thomas Brunet"; Entity.PerimeterCentered == true]
                 Content.text "Space 6" [Entity.PerimeterCentered == true]
                 Content.text "- Sound -" [Entity.Text == "- Sound -"; Entity.PerimeterCentered == true]
                 Content.text "Kronbits" [Entity.Text == "Kronbits"; Entity.PerimeterCentered == true]
                 Content.text "Simon 13" [Entity.Text == "Simon 13"; Entity.PerimeterCentered == true]
                 Content.text "Sound Works 12" [Entity.Text == "Sound Works 12"; Entity.PerimeterCentered == true]
                 Content.text "Space 7" [Entity.PerimeterCentered == true]
                 Content.text "- Supporters -" [Entity.Text == "- Supporters -"; Entity.PerimeterCentered == true]
                 Content.text "reinux" [Entity.Text == "reinux"; Entity.PerimeterCentered == true]
                 Content.text "Chris Reynolds" [Entity.Text == "Chris Reynolds"; Entity.PerimeterCentered == true]
                 Content.text "pauloud" [Entity.Text == "pauloud"; Entity.PerimeterCentered == true]
                 Content.text "arsmilitaris" [Entity.Text == "arsmilitaris"; Entity.PerimeterCentered == true]
                 Content.text "ibrahim324" [Entity.Text == "ibrahim324"; Entity.PerimeterCentered == true]
                 Content.text "Separator B" [Entity.PerimeterCentered == true]
                 Content.text "Separator B 2" [Entity.PerimeterCentered == true]
                 Content.text "Separator B 3" [Entity.PerimeterCentered == true]
                 Content.text "Separator B 4" [Entity.PerimeterCentered == true]
                 Content.text "Separator B 5" [Entity.PerimeterCentered == true]
                 Content.text "Thank you for playing." [Entity.Text == "Thank you for playing."; Entity.PerimeterCentered == true]]]]