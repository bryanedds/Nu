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
        | UpdateCommand
        | Exit

    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()
        member this.Atlas =
            this.Model |>
            Lens.narrow (fun world -> match this.GetModel world with Atlas _ -> true | _ -> false) |>
            Lens.bimap (function Atlas atlas -> atlas | _ -> failwithumf ()) Atlas
        member this.Field =
            this.Model |>
            Lens.narrow (fun world -> match this.GetModel world with Atlas atlas -> Option.isSome atlas.FieldOpt | _ -> false) |>
            Lens.bimapWorld
                (fun model _ -> match model with Atlas atlas when Option.isSome atlas.FieldOpt -> Option.get atlas.FieldOpt | _ -> failwithumf ())
                (fun field world -> match this.GetModel world with Atlas atlas -> Atlas (Atlas.updateFieldOpt (constant (Some field)) atlas) | _ -> failwithumf ())

    type TacticsDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Gui Splash)

        override this.Initializers (tactics, game) =
            [game.Atlas <=> Simulants.Atlas.Screen.Atlas
             game.Field <=> Simulants.Field.Screen.Field
             game.DesiredScreenOpt <== tactics --> fun tactics ->
                match tactics with
                | Gui gui ->
                    match gui with
                    | Splash -> Some Simulants.Splash.Screen
                    | Title -> Some Simulants.Title.Screen
                    | Credits -> Some Simulants.Credits.Screen
                    | Pick -> Some Simulants.Pick.Screen
                    | Intro _ -> Some Simulants.Intro.Screen
                | Atlas atlas ->
                    match atlas.AtlasState with
                    | Playing ->
                        match atlas.FieldOpt with
                        | Some field ->
                            match field.FieldState with
                            | FieldQuitting (_, _) -> Some Simulants.Atlas.Screen
                            | _ -> Some Simulants.Field.Screen
                        | None -> Some Simulants.Atlas.Screen
                    | Quitting -> Some Simulants.Title.Screen]

        override this.Channel (_, _) =
            [Simulants.Game.UpdateEvent => msg UpdateMessage
             Simulants.Game.UpdateEvent => cmd UpdateCommand
             Simulants.Splash.Screen.DeselectEvent => msg ShowTitle
             Simulants.Title.Gui.Play.ClickEvent =|> fun _ -> msg ShowPick
             Simulants.Title.Gui.Credits.ClickEvent => msg ShowCredits
             Simulants.Pick.Gui.NewGame1.ClickEvent => msg (ShowIntro Slot1)
             Simulants.Pick.Gui.NewGame2.ClickEvent => msg (ShowIntro Slot2)
             Simulants.Pick.Gui.NewGame3.ClickEvent => msg (ShowIntro Slot3)
             Simulants.Pick.Gui.LoadGame1.ClickEvent => msg (TryLoad Slot1)
             Simulants.Pick.Gui.LoadGame2.ClickEvent => msg (TryLoad Slot2)
             Simulants.Pick.Gui.LoadGame3.ClickEvent => msg (TryLoad Slot3)
             Simulants.Pick.Gui.Back.ClickEvent => msg ShowTitle
             Simulants.Credits.Gui.Back.ClickEvent => msg ShowTitle
             Simulants.Title.Gui.Exit.ClickEvent => cmd Exit]

        override this.Message (tactics, message, _, world) =

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
                | None -> just tactics

            | UpdateMessage ->
                match tactics with
                | Gui gui ->
                    match gui with
                    | Intro slot ->
                        match Simulants.Intro5.Screen.GetTransitionState world with
                        | OutgoingState -> just (Atlas (Atlas.initial slot))
                        | _ -> just tactics
                    | _ -> just tactics
                | Atlas atlas ->
                    match atlas.FieldOpt with
                    | Some field ->
                        match field.FieldState with
                        | FieldQuitting (_, outcome) ->
                            if outcome then
                                let atlas = Atlas.synchronizeFromField field atlas
                                just (Atlas (Atlas.updateFieldOpt (constant None) atlas))
                            else just (Atlas (Atlas.updateAtlasState (constant Quitting) atlas))
                        | _ -> just tactics
                    | None -> just tactics

        override this.Command (_, command, _, world) =

            match command with
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
                    if KeyboardState.isAltDown () && KeyboardState.isKeyDown KeyboardKey.Return then
                        match World.tryGetWindowFullScreen world with
                        | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                        | None -> world
                    else world

                // fin
                just world

            | Exit -> just (World.exit world)

        override this.Content (_, _) =

            [// splash
             Content.screen Simulants.Splash.Screen.Name (Nu.Splash (Constants.Gui.Dissolve, Constants.Gui.Splash, None, None)) [] []

             // title
             Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath

             // credits
             Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath

             // pick
             Content.screenFromGroupFile Simulants.Pick.Screen.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath

             // atlas
             Content.screen<AtlasDispatcher> Simulants.Atlas.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // intros
             Content.screenFromGroupFile Simulants.Intro.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Some Simulants.Intro2.Screen)) Assets.Gui.IntroGroupFilePath
             Content.screenFromGroupFile Simulants.Intro2.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Some Simulants.Intro3.Screen)) Assets.Gui.Intro2GroupFilePath
             Content.screenFromGroupFile Simulants.Intro3.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Some Simulants.Intro4.Screen)) Assets.Gui.Intro3GroupFilePath
             Content.screenFromGroupFile Simulants.Intro4.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Some Simulants.Intro5.Screen)) Assets.Gui.Intro4GroupFilePath
             Content.screenFromGroupFile Simulants.Intro5.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Some Simulants.Atlas.Screen)) Assets.Gui.Intro5GroupFilePath]