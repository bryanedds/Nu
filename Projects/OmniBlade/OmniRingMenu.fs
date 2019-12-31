namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniRingMenu =

    type Entity with
        
        member this.GetItems = this.Get Property? Items
        member this.SetItems = this.Set Property? Items
        member this.Items = lens<string list> Property? Items this.GetItems this.SetItems this
        member this.GetItemButtonsNp = this.Get Property? ItemButtonsNp
        member this.SetItemButtonsNp = this.Set Property? ItemButtonsNp
        member this.ItemButtonsNp = lens<Entity list> Property? ItemButtonsNp this.GetItemButtonsNp this.SetItemButtonsNp this
        member this.GetItemCancelOpt = this.Get Property? ItemCancelOpt
        member this.SetItemCancelOpt = this.Set Property? ItemCancelOpt
        member this.ItemCancelOpt = lens<string option> Property? ItemCancelOpt this.GetItemCancelOpt this.SetItemCancelOpt this
        member this.GetItemCancelButtonOptNp = this.Get Property? ItemCancelButtonOptNp
        member this.SetItemCancelButtonOptNp = this.Set Property? ItemCancelButtonOptNp
        member this.ItemCancelButtonOptNp = lens<Entity option> Property? ItemCancelButtonOptNp this.GetItemCancelButtonOptNp this.SetItemCancelButtonOptNp this
        member this.GetRadius = this.Get Property? Radius
        member this.SetRadius = this.Set Property? Radius
        member this.Radius = lens<single> Property? Radius this.GetRadius this.SetRadius this
        member this.ItemSelectEvent = Events.ItemSelect --> this
        member this.CancelEvent = Events.Cancel --> this

    type RingMenuDispatcher () =
        inherit GuiDispatcher ()

        static let CancelButtonOffset =
            v2 0.0f -48.0f

        static let computeButtonPositionLocal index (menu : Entity) world =
            let items = menu.GetItems world
            let radius = menu.GetRadius world
            let progress = single index / single (List.length items)
            let rotation = (progress * single Math.PI * 2.0f) + (menu.GetRotation world * single Math.PI * 2.0f)
            let position = v2 (radius * sin rotation) (radius * -cos rotation)
            position

        static let destroyButton button world =
            World.destroyEntity button world

        static let createButton item (menu : Entity) world =
            let (button, world) = World.createEntity5 typeof<ButtonDispatcher>.Name (Some (menu.Name + "+" + item)) DefaultOverlay (etol menu) world
            let world = button.SetDepth (menu.GetDepth world) world // keep at same depth as menu
            let world = button.SetUpImage (asset Assets.BattlePackage (item + "Up")) world
            let world = button.SetDownImage (asset Assets.BattlePackage (item + "Down")) world
            let world = button.QuickSize world
            let world = button.SetPersistent false world
            let world = button.SetParentNodeOptWithAdjustment (Some (Relation.unresolve button.EntityAddress menu.EntityAddress)) world
            (button, world)

        static let createItemButton index item (menu : Entity) world =
            let (button, world) = createButton item menu world
            let world = button.SetPositionLocal (computeButtonPositionLocal index menu world) world
            let world = World.monitor (fun _ world -> World.publish item menu.ItemSelectEvent [] menu world) button.ClickEvent button world
            (button, world)

        static let createCancelButton item (menu : Entity) world =
            let (button, world) = createButton item menu world
            let world = button.SetPositionLocal CancelButtonOffset world
            let world = World.monitor (fun _ world -> World.publish () menu.CancelEvent [] menu world) button.ClickEvent button world
            (button, world)

        static let destroyButtons (menu : Entity) world =

            // destroy ring buttons
            let world = List.fold (flip destroyButton) world (menu.GetItemButtonsNp world)
            let world = menu.SetItemButtonsNp [] world

            // destroy cancel button
            let world = Option.fold (flip destroyButton) world (menu.GetItemCancelButtonOptNp world)
            menu.SetItemCancelButtonOptNp None world

        static let createButtons (menu : Entity) world =

            // create item buttons
            let (ringButtons, world) =
                Seq.foldi (fun index (buttons, world) item ->
                    let (button, world) = createItemButton index item menu world
                    (button :: buttons, world))
                    ([], world) (menu.GetItems world)
            let world = menu.SetItemButtonsNp ringButtons world

            // create cancel button
            match menu.GetItemCancelOpt world with
            | Some cancelStr ->
                let (cancelButton, world) = createCancelButton cancelStr menu world
                menu.SetItemCancelButtonOptNp (Some cancelButton) world
            | None -> world

        static let handleItemsChange evt world =
            let menu = evt.Subscriber : Entity
            let world = destroyButtons menu world
            let world = createButtons menu world
            world

        static let handleTransformChange evt world =
            let menu = evt.Subscriber : Entity
            Seq.foldi (fun index world (button : Entity) ->
                button.SetPositionLocal (computeButtonPositionLocal index menu world) world)
                world (menu.GetItemButtonsNp world)

        static member Properties =
            [define Entity.Items []
             define Entity.ItemButtonsNp []
             define Entity.ItemCancelOpt None
             define Entity.ItemCancelButtonOptNp None
             define Entity.Radius 128.0f
             define Entity.Rotation 0.0f
             define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override dispatcher.Register (menu, world) =
            let world = createButtons menu world
            let world = World.monitor handleTransformChange (menu.GetChangeEvent Property? Radius) menu world
            let world = World.monitor handleTransformChange (menu.GetChangeEvent Property? Rotation) menu world
            let world = World.monitor handleItemsChange (menu.GetChangeEvent Property? Items) menu world
            world

        override dispatcher.Unregister (menu, world) =
            destroyButtons menu world