namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniRingMenu =

    type RingMenuModel =
        { Items : (int * (bool * string)) list
          ItemCancelOpt : string option }

    type RingMenuCommand =
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
            [define Entity.Radius 112.0f
             define Entity.Rotation 0.0f
             define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Content (model, menu, _) =
            [Content.entitiesIndexedByFst (model --> fun model -> model.Items |> seq) $ fun index item world ->
                let itemValue = item.Get world |> snd
                let buttonName = menu.Name + "+" + itemValue
                let button = menu.Parent / buttonName
                Content.button button.Name
                    [Entity.EnabledLocal <== item --> fun item -> fst item
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackageName (itemValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackageName (itemValue + "Down")
                     Entity.ClickEvent ==> cmd (ItemSelect itemValue)
                     Entity.UpdateEvent ==> cmd (ArrangeItemButton (button, index))]
             Content.entityOpt (model --> fun model -> model.ItemCancelOpt) $ fun itemCancel world ->
                let itemCancelValue = itemCancel.Get world
                let buttonName = menu.Name + "+" + itemCancelValue
                let button = menu.Parent / buttonName
                Content.button button.Name
                    [Entity.ParentNodeOpt == None
                     Entity.Visible <== menu.Visible
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Position == Constants.Battle.CancelPosition
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackageName (itemCancelValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackageName (itemCancelValue + "Down")
                     Entity.ClickEvent ==> cmd ItemCancel]]