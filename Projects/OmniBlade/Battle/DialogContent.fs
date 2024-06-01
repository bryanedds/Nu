// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
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
                    | DialogShort -> box3 (v3 -270.0f 159.0f 0.0f) (v3 540.0f 81.0f 0.0f)
                    | DialogThin -> box3 (v3 -432.0f 159.0f 0.0f) (v3 864.0f 81.0f 0.0f)
                    | DialogThick -> box3 (v3 -432.0f 78.0f 0.0f) (v3 864.0f 174.0f 0.0f)
                    | DialogNarration -> box3 (v3 -432.0f 78.0f 0.0f) (v3 864.0f 174.0f 0.0f)
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
                Content.composite<TextDispatcher> name
                    [Entity.Perimeter := perimeter
                     Entity.Elevation := elevation
                     Entity.BackdropImageOpt := backdropImageOpt
                     Entity.Text := text
                     Entity.Justification := justification
                     Entity.TextMargin == textMargin]
                    [Content.text "DropShadow"
                        [Entity.PositionLocal == v3 -2.0f 0.0f 0.0f; Entity.Size := perimeter.Size; Entity.ElevationLocal == -0.1f
                         Entity.VisibleLocal := dialog.DialogForm = DialogNarration
                         Entity.BackdropImageOpt == None
                         Entity.Text := text
                         Entity.Justification := justification
                         Entity.TextMargin == textMargin
                         Entity.TextColor == Color.Black]
                     Content.text "DropShadow2"
                        [Entity.PositionLocal == v3 -2.0f -2.0f 0.0f; Entity.Size := perimeter.Size; Entity.ElevationLocal == -0.1f
                         Entity.VisibleLocal := dialog.DialogForm = DialogNarration
                         Entity.BackdropImageOpt == None
                         Entity.Text := text
                         Entity.Justification := justification
                         Entity.TextMargin == textMargin
                         Entity.TextColor == Color.Black]
                     Content.text "DropShadow3"
                        [Entity.PositionLocal == v3 0.0f -2.0f 0.0f; Entity.Size := perimeter.Size; Entity.ElevationLocal == -0.1f
                         Entity.VisibleLocal := dialog.DialogForm = DialogNarration
                         Entity.BackdropImageOpt == None
                         Entity.Text := text
                         Entity.Justification := justification
                         Entity.TextMargin == textMargin
                         Entity.TextColor == Color.Black]
                     Content.button "Left"
                        [Entity.PositionLocal == v3 186.0f 18.0f 0.0f; Entity.Size == v3 192.0f 48.0f 0.0f; Entity.ElevationLocal == 2.0f
                         Entity.VisibleLocal := Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                         Entity.Text := match dialog.DialogPromptOpt with Some ((promptText, _), _) -> promptText | None -> ""
                         Entity.ClickEvent => promptLeft]
                     Content.button "Right"
                        [Entity.PositionLocal == v3 486.0f 18.0f 0.0f; Entity.Size == v3 192.0f 48.0f 0.0f; Entity.ElevationLocal == 2.0f
                         Entity.VisibleLocal := Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                         Entity.Text := match dialog.DialogPromptOpt with Some (_, (promptText, _)) -> promptText | None -> ""
                         Entity.ClickEvent => promptRight]]
             | None -> ()]