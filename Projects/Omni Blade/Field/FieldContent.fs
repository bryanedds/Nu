// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module FieldContent =

    [<RequireQualifiedAccess>]
    module Content =

        let teammate entityName initializers =
            Content.entity<TeammateDispatcher> entityName initializers

        let sidebar name position (field : Field) menuTeamOpen menuInventoryOpen menuTechOpen menuKeyItemsOpen menuOptionsOpen menuClose =
            Content.association name []
                [Content.button "TeamButton"
                    [Entity.PositionLocal == position; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "TeamButtonUp"
                     Entity.DownImage == asset "Field" "TeamButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuTeam _ -> false | _ -> true
                     Entity.ClickEvent => menuTeamOpen ()]
                 Content.button "InventoryButton"
                    [Entity.PositionLocal == position - v3 0.0f 81.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "InventoryButtonUp"
                     Entity.DownImage == asset "Field" "InventoryButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuInventory _ -> false | _ -> true
                     Entity.ClickEvent => menuInventoryOpen ()]
                 Content.button "TechButton"
                    [Entity.PositionLocal == position - v3 0.0f 162.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "TechButtonUp"
                     Entity.DownImage == asset "Field" "TechButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuTechs _ -> false | _ -> true
                     Entity.ClickEvent => menuTechOpen ()]
                 Content.button "KeyItemsButton"
                    [Entity.PositionLocal == position - v3 0.0f 243.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "KeyItemsButtonUp"
                     Entity.DownImage == asset "Field" "KeyItemsButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuKeyItems _ -> false | _ -> true
                     Entity.ClickEvent => menuKeyItemsOpen ()]
                 Content.button "OptionsButton"
                    [Entity.PositionLocal == position - v3 0.0f 324.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "OptionsButtonUp"
                     Entity.DownImage == asset "Field" "OptionsButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuOptions _ -> false | _ -> true
                     Entity.ClickEvent => menuOptionsOpen ()]
                 Content.button "CloseButton"
                    [Entity.PositionLocal == position - v3 0.0f 405.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                     Entity.UpImage == asset "Field" "CloseButtonUp"
                     Entity.DownImage == asset "Field" "CloseButtonDown"
                     Entity.EnabledLocal := match field.Menu.MenuState with MenuCloseSelected _ -> false | _ -> true
                     Entity.ClickEvent => menuClose ()]]

        let team (position : Vector3) rows (field : Field) filter fieldMsg =
            [for (index, teammateValue) in field.Team.Pairs do
                let teammateName = "Teammate+" + string teammateValue.TeamIndex
                let (w, upImage, downImage) =
                    match field.Menu.MenuState with
                    | MenuTechs _ -> (336.0f, Assets.Gui.ButtonLongUpImage, Assets.Gui.ButtonLongDownImage)
                    | MenuTeam _ | _ -> (252.0f, Assets.Gui.ButtonBigUpImage, Assets.Gui.ButtonBigDownImage)
                let h = 72.0f
                let x = position.X + if index < rows then 0.0f else w + 48.0f
                let y = position.Y - single (index % rows) * 81.0f
                teammate teammateName
                    [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 w h 0.0f
                     Entity.EnabledLocal := filter teammateValue field.Menu
                     Entity.Teammate := teammateValue
                     Entity.UpImage := upImage
                     Entity.DownImage := downImage
                     Entity.ClickEvent => fieldMsg index]]

        let items (position : Vector3) pageSize rows field fieldMsg =
            let items = Field.pageItems pageSize field |> __c // TOOD: consider memoizing.
            [for entry in items do
                let index = entry.Key
                let (_, (itemType, itemCountOpt)) as page = entry.Value
                let itemName = itemType.Name
                let x = if index < rows then position.X else position.X + 375.0f
                let y = position.Y - single (index % rows) * 81.0f
                Content.button itemName
                    [Entity.PositionLocal := v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 72.0f 0.0f
                     Entity.EnabledLocal := match itemType with Consumable _ | Equipment _ | KeyItem _ -> true | Stash _ -> false
                     Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.TextMargin == v2 16.0f 0.0f
                     Entity.Text :=
                        match itemCountOpt with
                        | Some count when count > 1 -> itemName + String (Array.create (17 - itemName.Length) ' ') + "x" + string count
                        | _ -> itemName
                     Entity.UpImage == Assets.Gui.ButtonLongUpImage
                     Entity.DownImage == Assets.Gui.ButtonLongDownImage
                     Entity.ClickEvent => fieldMsg page]]

        let techs (position : Vector3) (field : Field) fieldMsg =
            let techs =
                match field.Menu.MenuState with
                | MenuTechs menuTech ->
                    match Map.tryFind menuTech.TeamIndex field.Team with
                    | Some teammate -> teammate.Techs |> Seq.indexed |> Map.ofSeq
                    | None -> Map.empty
                | _ -> Map.empty
            let useSmallButtons = techs.Count > 6
            [for (index, tech) in techs.Pairs do
                let x = position.X
                let y =
                    position.Y -
                    single index * (if useSmallButtons then 60.0f else 81.0f) +
                    if useSmallButtons then 18.0f else 0.0f
                let w = 336.0f
                let h = if useSmallButtons then 54.0f else 72.0f
                Content.button (scstringMemo tech)
                    [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 w h 0.0f
                     Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.TextMargin == v2 16.0f 0.0f
                     Entity.Text := tech.Name
                     Entity.UpImage := if useSmallButtons then Assets.Gui.ButtonSquishedUpImage else Assets.Gui.ButtonLongUpImage
                     Entity.DownImage := if useSmallButtons then Assets.Gui.ButtonSquishedDownImage else Assets.Gui.ButtonLongDownImage
                     Entity.ClickEvent => fieldMsg index]]