// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Tactics
open System
open System.IO
open Prime
open Nu

[<AutoOpen>]
module TacticsGame =

    type Gui =
        | Splash
        | Title
        | Credits
        | Pick
        | Intro of SaveSlot

    type Model =
        | Gui of Gui
        | Atlas of Atlas

    type Message =
        | ShowTitle
        | ShowCredits
        | ShowPick
        | ShowIntro of SaveSlot
        | TryLoad of SaveSlot
        | UpdateMessage
        interface Nu.Message

    type Command =
        | Exit
        | UpdateCommand
        interface Nu.Command

    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()

    type TacticsDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Gui Splash)

        override this.Initialize (model, _) =
            [Game.DesiredScreen :=
                match model with
                | Gui gui ->
                    match gui with
                    | Splash -> Desire Simulants.Splash
                    | Title -> Desire Simulants.Title
                    | Credits -> Desire Simulants.Credits
                    | Pick -> Desire Simulants.Pick
                    | Intro _ -> Desire Simulants.Intro
                | Atlas atlas ->
                    match atlas.AtlasState with
                    | Playing ->
                        match atlas.FieldOpt with
                        | Some field ->
                            match field.FieldState with
                            | FieldQuitting (_, _) | FieldQuit -> Desire Simulants.Atlas
                            | _ -> Desire Simulants.Field
                        | None -> Desire Simulants.Atlas
                    | Quitting | Quit -> Desire Simulants.Title
             match model with
             | Gui _ -> ()
             | Atlas atlas ->
                 match atlas.FieldOpt with
                 | None -> Simulants.Atlas.Atlas := atlas
                 | Some field -> Simulants.Field.Field := field
             Game.UpdateEvent => UpdateMessage
             Game.UpdateEvent => UpdateCommand
             Simulants.Splash.DeselectingEvent => ShowTitle
             Simulants.TitlePlay.ClickEvent => ShowPick
             Simulants.TitleCredits.ClickEvent => ShowCredits
             Simulants.TitleExit.ClickEvent => Exit
             Simulants.PickNewGame1.ClickEvent => ShowIntro Slot1
             Simulants.PickNewGame2.ClickEvent => ShowIntro Slot2
             Simulants.PickNewGame3.ClickEvent => ShowIntro Slot3
             Simulants.PickLoadGame1.ClickEvent => TryLoad Slot1
             Simulants.PickLoadGame2.ClickEvent => TryLoad Slot2
             Simulants.PickLoadGame3.ClickEvent => TryLoad Slot3
             Simulants.PickBack.ClickEvent => ShowTitle
             Simulants.CreditsBack.ClickEvent => ShowTitle]

        override this.Message (model, message, _, world) =

            match message with
            | ShowTitle ->
                just (Gui Title)

            | ShowCredits ->
                just (Gui Credits)

            | ShowPick ->
                just (Gui Pick)

            | ShowIntro slot ->
                just (Gui (Intro slot))

            | TryLoad saveSlot ->
                match Atlas.tryLoad saveSlot world with
                | Some loaded -> just (Atlas loaded)
                | None -> just model

            | UpdateMessage ->

                // pull model state from atlas and field screens
                let model =
                    match model with
                    | Gui _ -> model
                    | Atlas atlas ->
                        match atlas.FieldOpt with
                        | None ->
                            let atlas' = Simulants.Atlas.GetAtlas world
                            if atlas =/= atlas' then Atlas atlas' else model
                        | Some field ->
                            let field' = Simulants.Field.GetField world
                            if field =/= field' then Atlas (Atlas.updateFieldOpt (constant (Some field')) atlas) else model

                // update model
                let model =
                    match model with
                    | Gui gui ->
                        match gui with
                        | Intro slot ->
                            match Simulants.Intro5.GetTransitionState world with
                            | OutgoingState _ -> Atlas (Atlas.debug world)
                            | _ -> model
                        | _ -> model
                    | Atlas atlas ->
                        match atlas.FieldOpt with
                        | Some field ->
                            match field.FieldState with
                            | FieldQuitting (_, outcome) ->
                                if outcome then
                                    let atlas = Atlas.synchronizeFromField field atlas
                                    Atlas (Atlas.updateFieldOpt (constant None) atlas)
                                else Atlas (Atlas.updateAtlasState (constant Quitting) atlas)
                            | _ -> model
                        | None -> model

                // fin
                just model

        override this.Command (_, command, _, world) =

            match command with
            | Exit ->
                if world.Unaccompanied
                then just (World.exit world)
                else just world

            | UpdateCommand ->

                // update picks
                let world =
                    if Simulants.Pick.Selected world then
                        let world = Simulants.PickNewGame1.SetVisible (not (File.Exists Assets.Global.SaveFilePath1)) world
                        let world = Simulants.PickNewGame2.SetVisible (not (File.Exists Assets.Global.SaveFilePath2)) world
                        let world = Simulants.PickNewGame3.SetVisible (not (File.Exists Assets.Global.SaveFilePath3)) world
                        let world = Simulants.PickLoadGame1.SetVisible (File.Exists Assets.Global.SaveFilePath1) world
                        let world = Simulants.PickLoadGame2.SetVisible (File.Exists Assets.Global.SaveFilePath2) world
                        let world = Simulants.PickLoadGame3.SetVisible (File.Exists Assets.Global.SaveFilePath3) world
                        world
                    else world

                // update full screen toggle
                let world =
                    if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.Return world then
                        match World.tryGetWindowFullScreen world with
                        | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                        | None -> world
                    else world

                // fin
                just world

        override this.Content (_, _) =

            [// splash
             Content.screen Simulants.Splash.Name (Slide (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title)) [] []

             // title
             Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []

             // credits
             Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath [] []

             // pick
             Content.screenWithGroupFromFile Simulants.Pick.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []

             // atlas
             Content.screen<AtlasDispatcher> Simulants.Atlas.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // intros
             Content.screenWithGroupFromFile Simulants.Intro.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2)) Assets.Gui.IntroGroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro2.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3)) Assets.Gui.Intro2GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro3.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4)) Assets.Gui.Intro3GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro4.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5)) Assets.Gui.Intro4GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro5.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Atlas)) Assets.Gui.Intro5GroupFilePath [] []]