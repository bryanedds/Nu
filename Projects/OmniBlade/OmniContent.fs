// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<RequireQualifiedAccess>]
module Content =

    let private pageItems5 pageSize pageIndex filter sort (items : Map<ItemType, int>) =
        let items =
            items |>
            Map.toSeq |>
            (fun items -> if filter then ItemType.filterSellableItems items else items) |>
            (fun items -> if sort then ItemType.sortItems items else items)
        let itemsPaged =
            items |>
            Seq.indexed |>
            Seq.chunkBySize pageSize |>
            Seq.trySkip pageIndex |>
            Seq.map List.ofArray |>
            Seq.tryHead |>
            Option.defaultValue [] |>
            Seq.indexed |>
            Map.ofSeq |>
            Map.map (fun _ (i, (item, count)) -> (i, (item, Some count)))
        let pageUp =
            if itemsPaged.Count <> 0 then
                let firstItemPaged = Seq.head itemsPaged
                fst (snd firstItemPaged.Value) <> fst (Seq.head items)
            else false
        let pageDown =
            if itemsPaged.Count <> 0 then
                let lastItemPaged = Seq.last itemsPaged
                fst (snd lastItemPaged.Value) <> fst (Seq.last items)
            else false
        (pageUp, pageDown, itemsPaged)

    let pageItems rows (field : Field) =
        match field.Menu.MenuState with
        | MenuItem menu -> pageItems5 rows menu.ItemPage false true field.Inventory.Items
        | _ ->
            match field.ShopOpt with
            | Some shop ->
                match shop.ShopState with
                | ShopBuying ->
                    match Map.tryFind shop.ShopType Data.Value.Shops with
                    | Some shopData -> pageItems5 rows shop.ShopPage false false (Map.ofListBy (flip Pair.make 1) shopData.ShopItems)
                    | None -> (false, false, Map.empty)
                | ShopSelling -> pageItems5 rows shop.ShopPage true true field.Inventory.Items
            | None -> (false, false, Map.empty)

    let sidebar name position (field : Field) menuTeamOpen menuItemsOpen menuTechOpen menuOptionsOpen menuClose =
        Content.association name []
            [Content.button "TeamButton"
                [Entity.PositionLocal == position; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "TeamButtonUp"
                 Entity.DownImage == asset "Field" "TeamButtonDown"
                 Entity.EnabledLocal := match field.Menu.MenuState with MenuTeam _ -> false | _ -> true
                 Entity.ClickEvent => msg (menuTeamOpen ())]
             Content.button "InventoryButton"
                [Entity.PositionLocal == position - v3 0.0f 81.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "InventoryButtonUp"
                 Entity.DownImage == asset "Field" "InventoryButtonDown"
                 Entity.EnabledLocal := match field.Menu.MenuState with MenuItem _ -> false | _ -> true
                 Entity.ClickEvent => msg (menuItemsOpen ())]
             Content.button "TechButton"
                [Entity.PositionLocal == position - v3 0.0f 162.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "TechButtonUp"
                 Entity.DownImage == asset "Field" "TechButtonDown"
                 Entity.EnabledLocal := match field.Menu.MenuState with MenuTech _ -> false | _ -> true
                 Entity.ClickEvent => msg (menuTechOpen ())]
             Content.button "OptionsButton"
                [Entity.PositionLocal == position - v3 0.0f 243.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "OptionsButtonUp"
                 Entity.DownImage == asset "Field" "OptionsButtonDown"
                 Entity.EnabledLocal := match field.Menu.MenuState with MenuOptions -> false | _ -> true
                 Entity.ClickEvent => msg (menuOptionsOpen ())]
             Content.button "HelpButton"
                [Entity.PositionLocal == position - v3 0.0f 324.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "HelpButtonUp"
                 Entity.DownImage == asset "Field" "HelpButtonDown"]
             Content.button "CloseButton"
                [Entity.PositionLocal == position - v3 0.0f 405.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                 Entity.UpImage == asset "Field" "CloseButtonUp"
                 Entity.DownImage == asset "Field" "CloseButtonDown"
                 Entity.ClickEvent => msg (menuClose ())]]

    let team (position : Vector3) rows (field : Field) filter fieldMsg =
        [for (index, teammate) in field.Team.Pairs do
            let teammateName = "Teammate+" + string teammate.TeamIndex
            let x = position.X + if index < rows then 0.0f else 252.0f + 48.0f
            let y = position.Y - single (index % rows) * 81.0f
            Content.button teammateName
                [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 252.0f 72.0f 0.0f
                 Entity.EnabledLocal := filter teammate field.Menu
                 Entity.Text := CharacterType.getName teammate.CharacterType
                 Entity.UpImage == Assets.Gui.ButtonBigUpImage
                 Entity.DownImage == Assets.Gui.ButtonBigDownImage
                 Entity.ClickEvent => msg (fieldMsg index)]]

    let items (position : Vector3) rows columns field fieldMsg =
        let items = pageItems rows field |> __c // TOOD: DIFF: consider memoizing.
        [for entry in items do
            let index = entry.Key
            let (_, (itemType, itemCountOpt)) as page = entry.Value
            let itemName = ItemType.getName itemType
            let x = if index < columns then position.X else position.X + 375.0f
            let y = position.Y - single (index % columns) * 81.0f
            Content.button itemName
                [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 72.0f 0.0f
                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v3 16.0f 0.0f 0.0f
                 Entity.Text :=
                    match itemCountOpt with
                    | Some count when count > 1 -> itemName + String (Array.create (17 - itemName.Length) ' ') + "x" + string count
                    | _ -> itemName
                 Entity.EnabledLocal := match itemType with Consumable _ | Equipment _ -> true | KeyItem _ | Stash _ -> false
                 Entity.UpImage == Assets.Gui.ButtonLongUpImage
                 Entity.DownImage == Assets.Gui.ButtonLongDownImage
                 Entity.ClickEvent => msg (fieldMsg page)]]

    let techs (position : Vector3) (field : Field) fieldMsg =
        let techs =
            match field.Menu.MenuState with
            | MenuTech menuTech ->
                match Map.tryFind menuTech.TeammateIndex field.Team with
                | Some teammate -> teammate.Techs |> Seq.indexed |> Map.ofSeq
                | None -> Map.empty
            | _ -> Map.empty
        [for (index, tech) in techs.Pairs do
            let techName = scstringm tech
            let x = position.X
            let y = position.Y - single index * 60.0f
            Content.button techName
                [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 60.0f 0.0f
                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v3 16.0f 0.0f 0.0f
                 Entity.Text := techName
                 Entity.EnabledLocal == false
                 Entity.UpImage == Assets.Gui.ButtonSquishedUpImage
                 Entity.DownImage == Assets.Gui.ButtonSquishedDownImage
                 Entity.ClickEvent => msg (fieldMsg index)]]

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