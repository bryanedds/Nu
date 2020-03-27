namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
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
            [define Entity.Radius 128.0f
             define Entity.Rotation 0.0f
             define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Content (model, menu, _) =
            [Content.entities (model --> fun model -> model.Items) $ fun index item world ->
                let itemValue = item.Get world
                let buttonName = menu.Name + "+" + itemValue
                let button = menu.Parent / buttonName
                Content.button button.Name
                    [Entity.Size == v2 64.0f 64.0f
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackage (itemValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackage (itemValue + "Down")
                     Entity.Persistent == false
                     Entity.ClickEvent ==> cmd (ItemSelect itemValue)
                     Entity.UpdateEvent ==> cmd (ArrangeItemButton (button, index))]
             Content.entityOpt (model --> fun model -> model.ItemCancelOpt) $ fun itemCancel world ->
                let itemCancelValue = itemCancel.Get world
                let buttonName = menu.Name + "+" + itemCancelValue
                let button = menu.Parent / buttonName
                Content.button button.Name
                    [Entity.CenterLocal == v2 -32.0f -96.0f
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackage (itemCancelValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackage (itemCancelValue + "Down")
                     Entity.Persistent == false
                     Entity.ClickEvent ==> cmd ItemCancel]]