namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

[<AutoOpen>]
module OmniRingMenu =

    type [<NoComparison>] RingMenuCommand =
        | ItemCancel
        | ItemSelect of string
        | ArrangeItemButton of Entity * int

    type Entity with
        
        member this.GetRadius = this.Get Property? Radius
        member this.SetRadius = this.Set Property? Radius
        member this.Radius = lens<single> Property? Radius this.GetRadius this.SetRadius this
        member this.GetRingMenuModel = this.GetModel<RingMenuModel>
        member this.SetRingMenuModel = this.SetModel<RingMenuModel>
        member this.RingMenuModel = this.Model<RingMenuModel> ()
        member this.ItemSelectEvent = Events.ItemSelect --> this
        member this.CancelEvent = Events.Cancel --> this

    type RingMenuDispatcher () =
        inherit GuiDispatcher<RingMenuModel, unit, RingMenuCommand> ({ Items = []; ItemCancelOpt = None })

        override this.Command (model, command, menu, world) =
            match command with
            | ItemCancel -> just (World.publish () menu.CancelEvent [] menu world)
            | ItemSelect item -> just (World.publish item menu.ItemSelectEvent [] menu world)
            | ArrangeItemButton (button, index) ->
                let radius = menu.GetRadius world
                let itemCount = List.length model.Items
                let progress = single index / single itemCount
                let rotation = progress * single Math.PI * 2.0f
                let position = v2 (radius * sin rotation) (radius * cos rotation)
                let world = button.SetPositionLocal (position - button.GetSize world * 0.5f) world
                just world

        static member Properties =
            [define Entity.Lens.Radius 128.0f
             define Entity.Lens.Rotation 0.0f
             define Entity.Lens.SwallowMouseLeft false
             define Entity.Lens.Visible false]

        override this.Content (model, menu, _) =
            [Content.entities (model --> fun model -> model.Items) $ fun index item world ->
                let itemValue = item.Get world
                let buttonName = menu.Name + "+" + itemValue
                let button = menu.Parent / buttonName
                Content.button buttonName
                    [button.Size == v2 64.0f 64.0f
                     button.Depth <== menu.Depth
                     button.UpImage == asset Assets.BattlePackage (itemValue + "Up")
                     button.DownImage == asset Assets.BattlePackage (itemValue + "Down")
                     button.Persistent == false
                     button.ParentNodeOpt == Some (relate button menu)
                     button.ClickEvent ==> cmd (ItemSelect itemValue)
                     button.UpdateEvent ==> cmd (ArrangeItemButton (button, index))]
             Content.entityOpt (model --> fun model -> model.ItemCancelOpt) $ fun itemCancel world ->
                let itemCancelValue = itemCancel.Get world
                let buttonName = menu.Name + "+" + itemCancelValue
                let button = menu.Parent / buttonName
                Content.button buttonName
                    [button.PositionLocal == v2 -32.0f -96.0f
                     button.Size == v2 64.0f 64.0f
                     button.Depth <== menu.Depth
                     button.UpImage == asset Assets.BattlePackage (itemCancelValue + "Up")
                     button.DownImage == asset Assets.BattlePackage (itemCancelValue + "Down")
                     button.ParentNodeOpt == Some (relate button menu)
                     button.Persistent == false
                     button.ClickEvent ==> cmd ItemCancel]]