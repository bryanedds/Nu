// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Tactics
open System.IO
open Prime
open Nu
open Nu.Declarative
open Tactics

[<AutoOpen>]
module TacticsGame =

    type Gui =
        | Splash
        | Title
        | Credits
        | Pick
        | Intro of SaveSlot

    type [<NoComparison>] Model =
        | Gui of Gui
        | Atlas of Atlas

    type Message =
        | ShowTitle
        | ShowCredits
        | ShowPick
        | ShowIntro of SaveSlot
        | TryLoad of SaveSlot
        | UpdateMessage

    type Command =
        | Exit
        | UpdateCommand

    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()

    type TacticsDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Gui Splash)

        override this.Register (game, world) =
#if DEV
            let world = World.setMasterSongVolume 0.0f world
#endif
            base.Register (game, world)

        override this.Initialize (model, _) =
            [Game.DesiredScreen :=
                match model with
                | Gui gui ->
                    match gui with
                    | Splash -> Desire Simulants.Splash.Screen
                    | Title -> Desire Simulants.Title.Screen
                    | Credits -> Desire Simulants.Credits.Screen
                    | Pick -> Desire Simulants.Pick.Screen
                    | Intro _ -> Desire Simulants.Intro.Screen
                | Atlas atlas ->
                    match atlas.AtlasState with
                    | Playing ->
                        match atlas.FieldOpt with
                        | Some field ->
                            match field.FieldState with
                            | FieldQuitting (_, _) | FieldQuit -> Desire Simulants.Atlas.Screen
                            | _ -> Desire Simulants.Field.Screen
                        | None -> Desire Simulants.Atlas.Screen
                    | Quitting | Quit -> Desire Simulants.Title.Screen
             match model with
             | Gui _ -> ()
             | Atlas atlas ->
                 match atlas.FieldOpt with
                 | None -> Simulants.Atlas.Screen.Atlas := atlas
                 | Some field -> Simulants.Field.Screen.Field := field
             Game.UpdateEvent => msg UpdateMessage
             Game.UpdateEvent => cmd UpdateCommand
             Simulants.Splash.Screen.DeselectingEvent => msg ShowTitle
             Simulants.Title.Gui.Play.ClickEvent => msg ShowPick
             Simulants.Title.Gui.Credits.ClickEvent => msg ShowCredits
             Simulants.Title.Gui.Exit.ClickEvent => cmd Exit
             Simulants.Pick.Gui.NewGame1.ClickEvent => msg (ShowIntro Slot1)
             Simulants.Pick.Gui.NewGame2.ClickEvent => msg (ShowIntro Slot2)
             Simulants.Pick.Gui.NewGame3.ClickEvent => msg (ShowIntro Slot3)
             Simulants.Pick.Gui.LoadGame1.ClickEvent => msg (TryLoad Slot1)
             Simulants.Pick.Gui.LoadGame2.ClickEvent => msg (TryLoad Slot2)
             Simulants.Pick.Gui.LoadGame3.ClickEvent => msg (TryLoad Slot3)
             Simulants.Pick.Gui.Back.ClickEvent => msg ShowTitle
             Simulants.Credits.Gui.Back.ClickEvent => msg ShowTitle]

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
                            let atlas' = Simulants.Atlas.Screen.GetAtlas world
                            if atlas =/= atlas' then Atlas atlas' else model
                        | Some field ->
                            let field' = Simulants.Field.Screen.GetField world
                            if field =/= field' then Atlas (Atlas.updateFieldOpt (constant (Some field')) atlas) else model

                // update model
                let model =
                    match model with
                    | Gui gui ->
                        match gui with
                        | Intro slot ->
                            match Simulants.Intro5.Screen.GetTransitionState world with
                            | OutgoingState -> Atlas (Atlas.debug world)
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
                just (World.exit world)

            | UpdateCommand ->

                // update picks
                let world =
                    if Simulants.Pick.Screen.IsSelected world then
                        let world = Simulants.Pick.Gui.NewGame1.SetVisible (not (File.Exists Assets.Global.SaveFilePath1)) world
                        let world = Simulants.Pick.Gui.NewGame2.SetVisible (not (File.Exists Assets.Global.SaveFilePath2)) world
                        let world = Simulants.Pick.Gui.NewGame3.SetVisible (not (File.Exists Assets.Global.SaveFilePath3)) world
                        let world = Simulants.Pick.Gui.LoadGame1.SetVisible (File.Exists Assets.Global.SaveFilePath1) world
                        let world = Simulants.Pick.Gui.LoadGame2.SetVisible (File.Exists Assets.Global.SaveFilePath2) world
                        let world = Simulants.Pick.Gui.LoadGame3.SetVisible (File.Exists Assets.Global.SaveFilePath3) world
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
             Content.screen Simulants.Splash.Screen.Name (WorldTypes.Splash (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title.Screen)) [] []

             // title
             Content.screenWithGroupFromFile Simulants.Title.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []

             // credits
             Content.screenWithGroupFromFile Simulants.Credits.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath [] []

             // pick
             Content.screenWithGroupFromFile Simulants.Pick.Screen.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []

             // atlas
             Content.screen<AtlasDispatcher> Simulants.Atlas.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // intros
             Content.screenWithGroupFromFile Simulants.Intro.Screen.Name (WorldTypes.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2.Screen)) Assets.Gui.IntroGroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro2.Screen.Name (WorldTypes.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3.Screen)) Assets.Gui.Intro2GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro3.Screen.Name (WorldTypes.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4.Screen)) Assets.Gui.Intro3GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro4.Screen.Name (WorldTypes.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5.Screen)) Assets.Gui.Intro4GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro5.Screen.Name (WorldTypes.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Atlas.Screen)) Assets.Gui.Intro5GroupFilePath [] []]