// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type DialogForm =
    | DialogShort
    | DialogThin
    | DialogThick
    | DialogNarration // TODO: rename to DialogFloating, along with all data using it.

type [<SymbolicExpansion>] Dialog =
    { DialogForm : DialogForm
      DialogTokenized : string
      DialogPromptOpt : ((string * CueSystem.Cue) * (string * CueSystem.Cue)) option
      DialogBattleOpt : (BattleType * Advent Set) option
      DialogProgress : int
      DialogPage : int }

    static member getText (detokenize : string -> string) dialog =
        let detokenized = detokenize dialog.DialogTokenized
        let text =
            match dialog.DialogForm with
            | DialogShort | DialogThin | DialogThick -> detokenized.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage] |> Dialog.wordWrap 48
            | DialogNarration -> detokenized
        String.tryTake dialog.DialogProgress text

    static member update (detokenize : string -> string) time dialog =
        let dialog =
            if time % 3L = 0L
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

    static member makePlus dialogForm textTokenized promptOpt battleOpt =
        { DialogForm = dialogForm
          DialogTokenized = textTokenized
          DialogPromptOpt = promptOpt
          DialogBattleOpt = battleOpt
          DialogProgress = 0
          DialogPage = 0 }

    static member makePrompt dialogForm textTokenized prompt =
        Dialog.makePlus dialogForm textTokenized (Some prompt) None

    static member make dialogForm textTokenized =
        Dialog.makePlus dialogForm textTokenized None None