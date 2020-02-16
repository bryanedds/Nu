namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniRingMenu =

    type [<NoComparison>] RingMenuMessage =
        | Dirty

    type [<NoComparison>] RingMenuCommand =
        | Cancel
        | ItemSelect of string

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
        inherit GuiDispatcher<RingMenuModel, RingMenuMessage, RingMenuCommand> ({ Items = []; ItemCancelOpt = None; Dirt = Gen.id })

        static let computeButtonPositionLocal index rotation radius itemCount =
            let progress = single index / single itemCount
            let rotation = (progress * single Math.PI * 2.0f) + (rotation * single Math.PI * 2.0f)
            let position = v2 (radius * sin rotation) (radius * -cos rotation)
            position

        override this.Message (model, message, _, _) =
            match message with
            | Dirty -> just { model with Dirt = Gen.id }

        override this.Command (_, command, menu, world) =
            match command with
            | Cancel -> just (World.publish () menu.CancelEvent [] menu world)
            | ItemSelect item -> just (World.publish item menu.ItemSelectEvent [] menu world)

        static member Properties =
            [define Entity.Radius 128.0f
             define Entity.Rotation 0.0f
             define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Content (model, menu, _) =
            [Content.entities (model --> fun model -> model.Items) $ fun index item _ world ->
                let itemValue = item.Get world
                Content.button (menu.Name + "+" + itemValue)
                    [Entity.PositionLocal <== model.MapWorld (fun model world -> computeButtonPositionLocal index (menu.GetRadius world) (menu.GetRotation world) (List.length model.Items))
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackage (itemValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackage (itemValue + "Down")
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Persistent == false
                     Entity.ChangeEvent Property? Rotation ==> msg Dirty
                     Entity.ChangeEvent Property? Radius ==> msg Dirty]
             Content.entityOpt (model --> fun model -> model.ItemCancelOpt) $ fun itemCancel _ world ->
                let itemCancelValue = itemCancel.Get world
                Content.button (menu.Name + "+" + itemCancelValue)
                    [Entity.PositionLocal == v2 0.0f -48.0f
                     Entity.Depth <== menu.Depth
                     Entity.UpImage == asset Assets.BattlePackage (itemCancelValue + "Up")
                     Entity.DownImage == asset Assets.BattlePackage (itemCancelValue + "Down")
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Persistent == false]]