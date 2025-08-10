﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module DialogContent =

    [<RequireQualifiedAccess>]
    module Content =
    
        let dialog name elevation promptLeft promptRight (detokenize : string -> string) (dialogOpt : Dialog option) =
            [match dialogOpt with
             | Some dialog ->
                let perimeter =
                    match dialog.DialogForm with
                    | DialogShort -> box3 (v3 -270.0f 162.0f 0.0f) (v3 540.0f 81.0f 0.0f)
                    | DialogThin -> box3 (v3 -432.0f 162.0f 0.0f) (v3 864.0f 81.0f 0.0f)
                    | DialogThick -> box3 (v3 -432.0f 81.0f 0.0f) (v3 864.0f 174.0f 0.0f)
                    | DialogNarration -> box3 (v3 -432.0f 81.0f 0.0f) (v3 864.0f 174.0f 0.0f)
                let backdropImageOpt =
                    match dialog.DialogForm with
                    | DialogShort -> Some Assets.Gui.DialogShortImage
                    | DialogThin -> Some Assets.Gui.DialogThinImage
                    | DialogThick -> Some Assets.Gui.DialogThickImage
                    | DialogNarration -> Some Assets.Default.EmptyImage
                let text =
                    Dialog.getText detokenize dialog
                let justification =
                    match dialog.DialogForm with
                    | DialogShort | DialogThin | DialogThick -> Unjustified true
                    | DialogNarration -> Justified (JustifyCenter, JustifyMiddle)
                let textMargin =
                    v2 30.0f 30.0f
                Content.text name
                    [Entity.Perimeter := perimeter
                     Entity.Elevation := elevation + 0.5f // NOTE: not sure why + 0.5f is needed here.
                     Entity.BackdropImageOpt := backdropImageOpt
                     Entity.Text := text
                     Entity.Justification := justification
                     Entity.TextMargin == textMargin]
                if dialog.DialogForm = DialogNarration then
                    for x in 2.0f .. 2.0f .. 4.0f do
                        for y in 2.0f .. 2.0f .. 4.0f do
                            let offset = v3 -x -y 0.0f
                            Content.text ("DropShadow+" + scstring offset)
                                [Entity.Position == perimeter.Min + offset
                                 Entity.Size := perimeter.Size
                                 Entity.Elevation == elevation + 0.5f // NOTE: not sure why + 0.5f is needed here.
                                 Entity.BackdropImageOpt == None
                                 Entity.Text := text
                                 Entity.TextShift == Constants.Gui.TextShiftDefault * 0.5f
                                 Entity.Justification := justification
                                 Entity.TextMargin == textMargin
                                 Entity.TextColor == Color.Black]
                if Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog then
                    Content.button "Left"
                        [Entity.Position == perimeter.Min + v3 186.0f 18.0f 0.0f
                         Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Elevation == elevation + 1.0f
                         Entity.Text := match dialog.DialogPromptOpt with Some ((promptText, _), _) -> promptText | None -> ""
                         Entity.ClickEvent => promptLeft]
                    Content.button "Right"
                        [Entity.Position == perimeter.Min + v3 486.0f 18.0f 0.0f
                         Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Elevation == elevation + 1.0f
                         Entity.Text := match dialog.DialogPromptOpt with Some (_, (promptText, _)) -> promptText | None -> ""
                         Entity.ClickEvent => promptRight]
             | None -> ()]