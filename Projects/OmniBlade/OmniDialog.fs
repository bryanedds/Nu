// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative

type [<StructuralEquality; NoComparison>] DialogForm =
    | DialogThin    
    | DialogThick
    | DialogNarration

type [<ReferenceEquality; NoComparison>] Dialog =
    { DialogForm : DialogForm
      DialogTokenized : string
      DialogProgress : int
      DialogPage : int
      DialogPromptOpt : ((string * Cue) * (string * Cue)) option
      DialogBattleOpt : (BattleType * Advent Set) option }

    static member update dialog world =
        let increment = if World.getUpdateTime world % 2L = 0L then 1 else 0
        let dialog = { dialog with DialogProgress = dialog.DialogProgress + increment }
        dialog

    static member canAdvance (detokenize : string -> string) dialog =
        let detokenized = detokenize dialog.DialogTokenized
        dialog.DialogProgress > detokenized.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage].Length

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

    static member content name elevation promptLeft promptRight (detokenizeAndDialogOpt : Lens<(string -> string) * Dialog option, World>) =
        Content.entityWithContent<TextDispatcher> name
            [Entity.Bounds <== detokenizeAndDialogOpt --> fun (_, dialogOpt) ->
                match dialogOpt with
                | Some dialog ->
                    match dialog.DialogForm with
                    | DialogThin -> v4Bounds (v2 -432.0f 150.0f) (v2 864.0f 90.0f)
                    | DialogThick -> v4Bounds (v2 -432.0f 60.0f) (v2 864.0f 192.0f)
                    | DialogNarration -> v4Bounds (v2 -432.0f 60.0f) (v2 864.0f 192.0f)
                | None -> v4Zero
             Entity.Elevation == elevation
             Entity.BackgroundImageOpt <== detokenizeAndDialogOpt --> fun (_, dialogOpt) ->
                let image =
                    match dialogOpt with
                    | Some dialog ->
                        match dialog.DialogForm with
                        | DialogThin -> Assets.Gui.DialogThinImage
                        | DialogThick -> Assets.Gui.DialogThickImage
                        | DialogNarration -> Assets.Default.ImageEmpty
                    | None -> Assets.Gui.DialogThickImage
                Some image
             Entity.Text <== detokenizeAndDialogOpt --> fun (detokenize, dialogOpt) ->
                match dialogOpt with
                | Some dialog ->
                    let detokenized = detokenize dialog.DialogTokenized
                    let textPage = dialog.DialogPage
                    let text = detokenized.Split(Constants.Gameplay.DialogSplit).[textPage] |> Dialog.wordWrap 48
                    let textToShow = String.tryTake dialog.DialogProgress text
                    textToShow
                | None -> ""
             Entity.Justification <== detokenizeAndDialogOpt --> fun (_, dialogOpt) ->
                match dialogOpt with
                | Some dialog ->
                    match dialog.DialogForm with
                    | DialogThin | DialogThick -> Unjustified true
                    | DialogNarration -> Justified (JustifyCenter, JustifyMiddle)
                | None -> Unjustified true
             Entity.Margins == v2 30.0f 30.0f]
            [Content.button (name + "+Left")
                [Entity.PositionLocal == v2 186.0f 18.0f; Entity.ElevationLocal == 2.0f
                 Entity.VisibleLocal <== detokenizeAndDialogOpt --> fun (detokenize, dialogOpt) ->
                    match dialogOpt with
                    | Some dialog -> Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                    | None -> false
                 Entity.Text <== detokenizeAndDialogOpt --> fun (_, dialogOpt) ->
                    match dialogOpt with
                    | Some dialog -> match dialog.DialogPromptOpt with Some ((promptText, _), _) -> promptText | None -> ""
                    | None -> ""
                 Entity.ClickEvent ==> msg promptLeft]
             Content.button (name + "+Right")
                [Entity.PositionLocal == v2 486.0f 18.0f; Entity.ElevationLocal == 2.0f
                 Entity.VisibleLocal <== detokenizeAndDialogOpt --> fun (detokenize, dialogOpt) ->
                    match dialogOpt with
                    | Some dialog -> Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted detokenize dialog
                    | None -> false
                 Entity.Text <== detokenizeAndDialogOpt --> fun (_, dialogOpt) ->
                     match dialogOpt with
                     | Some dialog -> match dialog.DialogPromptOpt with Some (_, (promptText, _)) -> promptText | None -> ""
                     | None -> ""
                 Entity.ClickEvent ==> msg promptRight]]