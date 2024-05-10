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

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartScrolling
         Screen.UpdateEvent => Scroll]

    override this.Message (credits, message, _, _) =

        match message with
        | StartScrolling ->
            just Credits.initial

        | Scroll ->
            let credits = { credits with ScrollPosition = credits.ScrollPosition + v3 0.0f (1.0f / 3.0f) 0.0f }
            if credits.ScrollPosition.Y > 607.0f
            then withSignal (signal StartQuitting) credits
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
                [Content.text "OmniBlade" [Entity.Text == "OmniBlade"]
                 Content.text "Separator A" []
                 Content.text "Separator A 2" []
                 Content.text "Separator A 3" []
                 Content.text "Separator A 4" []
                 Content.text "Separator A 5" []
                 Content.text "- Direction & Design -" [Entity.Text == "- Direction & Design -"]
                 Content.text "Bryan Edds" [Entity.Text == "Bryan Edds"]
                 Content.text "Space" []
                 Content.text "- Engine Programming -" [Entity.Text == "- Engine Programming -"]
                 Content.text "Bryan Edds 2" [Entity.Text == "Bryan Edds"]
                 Content.text "Dean J. Lee" [Entity.Text == "Dean J. Lee"]
                 Content.text "Space 2" []
                 Content.text "- Game Programming -" [Entity.Text == "- Game Programming -"]
                 Content.text "Bryan Edds 3" [Entity.Text == "Bryan Edds"]
                 Content.text "Space 3" []
                 Content.text "- Effects Programming -" [Entity.Text == "- Effects Programming -"]
                 Content.text "Bryan Edds 4" [Entity.Text == "Bryan Edds"]
                 Content.text "Dean J. Lee 2" [Entity.Text == "Dean J. Lee"]
                 Content.text "Space 4" []
                 Content.text "- Art -" [Entity.Text == "- Art -"]
                 Content.text "Ansimuz" [Entity.Text == "Ansimuz"]
                 Content.text "CreativeKind" [Entity.Text == "CreativeKind"]
                 Content.text "FinalBossBlues" [Entity.Text == "FinalBossBlues"]
                 Content.text "Narehop" [Entity.Text == "Narehop"]
                 Content.text "Pimen Art" [Entity.Text == "Pimen Art"]
                 Content.text "Selier the Shaper" [Entity.Text == "Selier the Shaper"]
                 Content.text "Szadi Art" [Entity.Text == "Szadi Art"]
                 Content.text "Space 5" []
                 Content.text "- Music -" [Entity.Text == "- Music -"]
                 Content.text "Clement Panchout" [Entity.Text == "Clement Panchout"]
                 Content.text "Drass Ray - Jacob Mann" [Entity.Text == "Drass Ray - Jacob Mann"]
                 Content.text "JP Soundworks" [Entity.Text == "JP Soundworks"]
                 Content.text "Owl Theory Music" [Entity.Text == "Owl Theory Music"]
                 Content.text "Pdk Music" [Entity.Text == "Pdk Music"]
                 Content.text "PeriTune" [Entity.Text == "PeriTune"]
                 Content.text "Suat" [Entity.Text == "Suat"]
                 Content.text "Thomas Brunet" [Entity.Text == "Thomas Brunet"]
                 Content.text "Space 6" []
                 Content.text "- Sound -" [Entity.Text == "- Sound -"]
                 Content.text "Kronbits" [Entity.Text == "Kronbits"]
                 Content.text "Simon 13" [Entity.Text == "Simon 13"]
                 Content.text "Sound Works 12" [Entity.Text == "Sound Works 12"]
                 Content.text "Space 7" []
                 Content.text "- Supporters -" [Entity.Text == "- Supporters -"]
                 Content.text "Rei Miyasaka" [Entity.Text == "Rei Miyasaka"]
                 Content.text "Chris Reynolds" [Entity.Text == "Chris Reynolds"]
                 Content.text "pauloud" [Entity.Text == "pauloud"]
                 Content.text "arsmilitaris" [Entity.Text == "arsmilitaris"]
                 Content.text "ibrahim324" [Entity.Text == "ibrahim324"]
                 Content.text "Separator B" []
                 Content.text "Separator B 2" []
                 Content.text "Separator B 3" []
                 Content.text "Separator B 4" []
                 Content.text "Separator B 5" []
                 Content.text "Thank you for playing." [Entity.Text == "Thank you for playing."]]]]