// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module RingMenuDispatcher =

    type [<StructuralEquality; NoComparison>] RingMenu =
        { Items : Map<string, int * bool>
          ItemCancelOpt : string option }

    type [<NoEquality; NoComparison>] RingMenuCommand =
        | ItemCancel
        | ItemSelect of string

    type Entity with
        member this.GetRadius = this.Get Property? Radius
        member this.SetRadius = this.Set Property? Radius
        member this.Radius = lens<single> Property? Radius this.GetRadius this.SetRadius this
        member this.GetRingMenu = this.GetModel<RingMenu>
        member this.SetRingMenu = this.SetModel<RingMenu>
        member this.RingMenu = this.Model<RingMenu> ()
        member this.ItemSelectEvent = Events.ItemSelect --> this
        member this.CancelEvent = Events.Cancel --> this

    type RingMenuDispatcher () =
        inherit GuiDispatcher<RingMenu, unit, RingMenuCommand> ({ Items = Map.empty; ItemCancelOpt = None })

        static member Properties =
            [define Entity.Radius Constants.Battle.RingMenuRadius
             define Entity.Rotation 0.0f
             define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Command (_, command, menu, world) =
            match command with
            | ItemCancel -> just (World.publishPlus () menu.CancelEvent [] menu true world)
            | ItemSelect item -> just (World.publishPlus item menu.ItemSelectEvent [] menu true world)

        override this.Content (ringMenu, menu) =
            [Content.entities ringMenu
                (fun ringMenu _ -> ringMenu.Items)
                (fun items _ ->
                    let items =
                        let mutable i = -1
                        items |>
                        Map.toSeq |>
                        Map.ofSeqBy (fun (k, (v, v2)) -> (v, (k, v2))) |>
                        Map.toSeqBy (fun _ (v, v2) -> (v, (i <- inc i; i, v2))) |>
                        Map.ofSeq
                    Map.map (constant (Triple.insert (Map.count items))) items)
                (fun itemName itemIndexAndCountAndEnabled world ->
                    let buttonName = menu.Name + "+" + scstring itemName
                    let button = menu.Parent / buttonName
                    Content.button button.Name
                        [Entity.EnabledLocal <== itemIndexAndCountAndEnabled --> Triple.thd
                         Entity.PositionLocal <== itemIndexAndCountAndEnabled --> fun (itemIndex, itemCount, _) ->
                            let radius = menu.GetRadius world
                            let progress = single itemIndex / single itemCount
                            let rotation = progress * single Math.PI * 2.0f
                            let position = v2 (radius * sin rotation) (radius * cos rotation)
                            position - button.GetSize world * 0.5f
                         Entity.Size == v2 48.0f 48.0f
                         Entity.Elevation <== menu.Elevation
                         Entity.UpImage == asset Assets.Battle.PackageName (itemName + "Up")
                         Entity.DownImage == asset Assets.Battle.PackageName (itemName + "Down")
                         Entity.ClickEvent ==> cmd (ItemSelect itemName)])
             Content.entityOpt ringMenu (fun ringMenu _ -> ringMenu.ItemCancelOpt) $ fun itemCancel world ->
                let itemCancelValue = itemCancel.Get world
                let buttonName = menu.Name + "+" + itemCancelValue
                let button = menu.Parent / buttonName
                Content.button button.Name
                    [Entity.ParentNodeOpt == None
                     Entity.Visible <== menu.Visible
                     Entity.Size == v2 48.0f 48.0f
                     Entity.Position == Constants.Battle.CancelPosition
                     Entity.Elevation <== menu.Elevation
                     Entity.UpImage == asset Assets.Battle.PackageName (itemCancelValue + "Up")
                     Entity.DownImage == asset Assets.Battle.PackageName (itemCancelValue + "Down")
                     Entity.ClickEvent ==> cmd ItemCancel]]