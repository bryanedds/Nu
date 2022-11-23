// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

type [<NoComparison>] DialogForm =
    | DialogThin
    | DialogThick
    | DialogNarration

type [<NoComparison>] Dialog =
    { DialogForm : DialogForm
      DialogTokenized : string
      DialogProgress : int
      DialogPage : int
      DialogPromptOpt : ((string * Cue) * (string * Cue)) option
      DialogBattleOpt : (BattleType * Advent Set) option }

    static member getText (detokenize : string -> string) dialog =
        let detokenized = detokenize dialog.DialogTokenized
        let text = detokenized.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage] |> Dialog.wordWrap 48
        String.tryTake dialog.DialogProgress text

    static member advance (detokenize : string -> string) dialog world =
        let dialog =
            if World.getUpdateTime world % 3L = 0L
            then { dialog with DialogProgress = inc dialog.DialogProgress }
            else dialog
        let dialog =
            if Seq.tryLast (Dialog.getText detokenize dialog) = Some ' '
            then { dialog with DialogProgress = inc dialog.DialogProgress }
            else dialog
        dialog

    static member canAdvance (detokenize : string -> string) dialog =
        dialog.DialogProgress > (Dialog.getText detokenize dialog).Length
        
    static member tryAdvance (detokenize : string -> string) dialog =
        let detokenized = detokenize dialog.DialogTokenized
        if dialog.DialogPage < detokenized.Split(Constants.Gameplay.DialogSplit).Length - 1 then
            let dialog = { dialog with DialogProgress = 0; DialogPage = inc dialog.DialogPage }
            (true, dialog)
        else (false, dialog)

    static member isExhausted (detokenize : string -> string) dialog =
        let detokenized = detokenize dialog.DialogTokenized
        let pages = detokenized.Split Constants.Gameplay.DialogSplit
        let lastPage = dec (Array.length pages)
        dialog.DialogPage = lastPage &&
        dialog.DialogProgress > pages.[lastPage].Length

    // TODO: use a fold here instead of an inner function if possible.
    static member wordWrap limit (text : string) =
        let rec wordWrap acc (text : string) =
            if text.Length > limit then
                let (left, right) =
                    let seq = String.explode text |> Seq.ofList
                    let seqLeft = Seq.take limit seq
                    if Seq.exists (fun c -> c = '\n' || c = ' ') seqLeft then
                        let index =
                            match Seq.tryFindIndex (fun c -> c = '\n') seqLeft with
                            | Some index -> index
                            | None -> Seq.findIndexBack (fun c -> c = ' ') seqLeft
                        (Seq.take index seq, Seq.skip (index + 1) seq)
                    else (seqLeft, Seq.skip limit seq)
                let (left, right) = (Seq.toList left |> String.implode, Seq.toList right |> String.implode)
                let acc = left :: acc
                wordWrap acc right
            else text :: acc
        wordWrap [] text |> List.rev |> String.join "\n"

[<AutoOpen>]
module DialogContent =

    [<RequireQualifiedAccess>]
    module Content =
    
        let dialog name elevation promptLeft promptRight (detokenize : string -> string) (dialogOpt : Dialog option) =
            [match dialogOpt with
             | Some dialog ->
                Content.composite<TextDispatcher> name
                    [Entity.Perimeter :=
                        match dialog.DialogForm with
                        | DialogThin -> box3 (v3 -432.0f 150.0f 0.0f) (v3 864.0f 90.0f 0.0f)
                        | DialogThick -> box3 (v3 -432.0f 78.0f 0.0f) (v3 864.0f 174.0f 0.0f)
                        | DialogNarration -> box3 (v3 -432.0f 78.0f 0.0f) (v3 864.0f 174.0f 0.0f)
                     Entity.Elevation := elevation
                     Entity.BackgroundImageOpt :=
                        match dialog.DialogForm with
                        | DialogThin -> Some Assets.Gui.DialogThinImage
                        | DialogThick -> Some Assets.Gui.DialogThickImage
                        | DialogNarration -> Some Assets.Default.ImageEmpty
                     Entity.Text := Dialog.getText detokenize dialog
                     Entity.Justification :=
                        match dialog.DialogForm with
                        | DialogThin | DialogThick -> Unjustified true
                        | DialogNarration -> Justified (JustifyCenter, JustifyMiddle)
                     Entity.Margins == v3 30.0f 30.0f 0.0f]
                    [Content.button "Left"
                        [Entity.PositionLocal == v3 186.0f 18.0f 0.0f; Entity.ElevationLocal == 2.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.VisibleLocal := Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                         Entity.Text := match dialog.DialogPromptOpt with Some ((promptText, _), _) -> promptText | None -> ""
                         Entity.ClickEvent => msg promptLeft]
                     Content.button "Right"
                        [Entity.PositionLocal == v3 486.0f 18.0f 0.0f; Entity.ElevationLocal == 2.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.VisibleLocal := Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                         Entity.Text := match dialog.DialogPromptOpt with Some (_, (promptText, _)) -> promptText | None -> ""
                         Entity.ClickEvent => msg promptRight]]
             | None -> ()]