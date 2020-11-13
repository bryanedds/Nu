// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative

type [<NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<ReferenceEquality; NoComparison>] Dialog =
    { DialogForm : DialogForm
      DialogText : string
      DialogProgress : int
      DialogPage : int
      DialogBattleOpt : (Advent Set * BattleType) option }

    static member update dialog world =
        let increment = if World.getTickTime world % 2L = 0L then 1 else 0
        let dialog = { dialog with DialogProgress = dialog.DialogProgress + increment }
        dialog

    static member canAdvance dialog =
        dialog.DialogProgress > dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage].Length

    static member tryAdvance dialog =
        if dialog.DialogPage < dialog.DialogText.Split(Constants.Gameplay.DialogSplit).Length - 1 then
            let dialog = { dialog with DialogProgress = 0; DialogPage = inc dialog.DialogPage }
            (true, dialog)
        else (false, dialog)
        
    static member content name (dialogOpt : Lens<Dialog option, World>) =
        Content.text name
           [Entity.Bounds <== dialogOpt --> fun dialogOpt ->
               match dialogOpt with
               | Some dialog ->
                   match dialog.DialogForm with
                   | DialogThin -> v4Bounds (v2 -448.0f 128.0f) (v2 896.0f 112.0f)
                   | DialogMedium -> v4Bounds (v2 -448.0f 0.0f) (v2 640.0f 256.0f)
                   | DialogLarge -> v4Bounds (v2 -448.0f 0.0f) (v2 896.0f 256.0f)
               | None -> v4Zero
            Entity.BackgroundImageOpt <== dialogOpt --> fun dialogOpt ->
               let image =
                   match dialogOpt with
                   | Some dialog ->
                       match dialog.DialogForm with
                       | DialogThin -> Assets.DialogThinImage
                       | DialogMedium -> Assets.DialogMediumImage
                       | DialogLarge -> Assets.DialogLargeImage
                   | None -> Assets.DialogLargeImage
               Some image
            Entity.Text <== dialogOpt --> fun dialogOpt ->
               match dialogOpt with
               | Some dialog ->
                   let textPage = dialog.DialogPage
                   let text = dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[textPage]
                   let textToShow = String.tryTake dialog.DialogProgress text
                   textToShow
               | None -> ""
            Entity.Visible <== dialogOpt --> Option.isSome
            Entity.Justification == Unjustified true
            Entity.Margins == v2 40.0f 40.0f]