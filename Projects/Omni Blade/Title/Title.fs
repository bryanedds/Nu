// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type Title =
    { TitleTime : int64
      TitleSelection : int } // play, credits or exit, as chosen with a gamepad or the keyboard

    static member val empty =
        { TitleTime = 0L; TitleSelection = 0 }

    static member val initial =
        { TitleTime = 0L; TitleSelection = 0 }

type TitleMessage =
    | Scroll
    | ProcessInput
    | ResetSelection
    interface Message

type TitleCommand =
    | Choose of Entity
    interface Command

[<AutoOpen>]
module TitleExtensions =
    type Screen with
        member this.GetTitle world = this.GetModelGeneric<Title> world
        member this.SetTitle value world = this.SetModelGeneric<Title> value world
        member this.Title = this.ModelGeneric<Title> ()

type TitleDispatcher () =
    inherit ScreenDispatcher<Title, TitleMessage, TitleCommand> (Title.empty)

    static let getChoices () =
        [|Simulants.TitlePlay; Simulants.TitleCredits; Simulants.TitleExit|]

    let scroll speed which title =
        let offsetX = single Constants.Render.DisplayVirtualResolution.X * 0.5f * if which then -1.0f else 1.0f
        let offsetY = single Constants.Render.DisplayVirtualResolution.Y * -0.5f
        let progress = single title.TitleTime * speed % single Constants.Render.DisplayVirtualResolution.X / single Constants.Render.DisplayVirtualResolution.X
        let scroll = progress * single Constants.Render.DisplayVirtualResolution.X
        v3 (offsetX - scroll) offsetY 0.0f

    override this.GetFallbackModel (_, screen, world) =
        if not (screen.GetSelected world)
        then Title.initial
        else Title.empty

    override this.Definitions (_, _) =
        [Screen.SelectEvent => ResetSelection
         Screen.UpdateEvent => Scroll
         Screen.UpdateEvent => ProcessInput]

    override this.Message (title, message, screen, world) =
        match message with
        | Scroll ->
            let title = { title with TitleTime = inc title.TitleTime }
            just title
        | ResetSelection ->
            just { title with TitleSelection = 0 }
        | ProcessInput ->
            if screen.GetIdling world then
                let intent = Game.GetIntent world
                let choices = getChoices ()
                let title =
                    match intent.Step with
                    | Some direction -> { title with TitleSelection = Selection.wrap (Selection.rowDelta direction) choices.Length title.TitleSelection }
                    | None -> title
                if intent.Confirm
                then withSignal (Choose choices[title.TitleSelection]) title
                else just title
            else just title

    override this.Command (_, command, _, world) =
        match command with
        | Choose button ->
            Selection.playAffirm world
            World.publish () button.ClickEvent button world

    override this.Render (title, renderPass, screen, world) =
        match renderPass with
        | NormalPass when screen.GetIdling world ->
            let choices = getChoices ()
            Pointer.render choices[title.TitleSelection] title.TitleTime world
        | _ -> ()

    override this.Content (title, _) =
        [Content.group Simulants.TitleScene.Name []
            [let foregroundScrollSpeed = if Globals.Render.DisplayScalar = 1 then 1.5f else 1.2f // HACK: get rid of aliasing when VirtualScalar = 1.
             let groundSizeHack = if Globals.Render.DisplayScalar = 1 then v3 0.1f 0.0f 0.0f else v3Zero // HACK: perhaps due to change in tex coords results in Vulkan, we get some shaky aliasing in the texture without this.
             Content.staticSprite "TitleForeground" [Entity.StaticImage == asset "Gui" "TitleForeground"; Entity.Position := scroll foregroundScrollSpeed false title; Entity.Elevation == -1.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleForeground2" [Entity.StaticImage == asset "Gui" "TitleForeground"; Entity.Position := scroll foregroundScrollSpeed true title; Entity.Elevation == -1.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleGround" [Entity.StaticImage == asset "Gui" "TitleGround"; Entity.Position := scroll 1.0f false title; Entity.Elevation == -2.0f; Entity.Size == (Constants.Render.DisplayVirtualResolution.V3 + groundSizeHack); Entity.Absolute == true]
             Content.staticSprite "TitleGround2" [Entity.StaticImage == asset "Gui" "TitleGround"; Entity.Position := scroll 1.0f true title; Entity.Elevation == -2.0f; Entity.Size == (Constants.Render.DisplayVirtualResolution.V3 + groundSizeHack); Entity.Absolute == true]
             Content.staticSprite "TitleForest" [Entity.StaticImage == asset "Gui" "TitleForest"; Entity.Position := scroll 0.4f false title; Entity.Elevation == -3.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleForest2" [Entity.StaticImage == asset "Gui" "TitleForest"; Entity.Position := scroll 0.4f true title; Entity.Elevation == -3.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleMountains" [Entity.StaticImage == asset "Gui" "TitleMountains"; Entity.Position := scroll 0.25f false title; Entity.Elevation == -4.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleMountains2" [Entity.StaticImage == asset "Gui" "TitleMountains"; Entity.Position := scroll 0.25f true title; Entity.Elevation == -4.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleClouds" [Entity.StaticImage == asset "Gui" "TitleClouds"; Entity.Position := scroll 0.2f false title; Entity.Elevation == -5.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleClouds2" [Entity.StaticImage == asset "Gui" "TitleClouds"; Entity.Position := scroll 0.2f true title; Entity.Elevation == -5.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleSky" [Entity.StaticImage == asset "Gui" "TitleSky"; Entity.Position := scroll 0.2f false title; Entity.Elevation == -6.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]
             Content.staticSprite "TitleSky2" [Entity.StaticImage == asset "Gui" "TitleSky"; Entity.Position := scroll 0.2f true title; Entity.Elevation == -6.0f; Entity.Size == Constants.Render.DisplayVirtualResolution.V3; Entity.Absolute == true]]]