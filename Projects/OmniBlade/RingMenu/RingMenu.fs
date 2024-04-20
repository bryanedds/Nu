// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type RingMenu =
    { Items : Map<string, int * bool>
      Cancellable : bool }

type RingMenuCommand =
    | ItemCancel
    | ItemSelect of string
    interface Command

[<AutoOpen>]
module RingMenuExtensions =
    type Entity with
        member this.GetRingMenu world = this.GetModelGeneric<RingMenu> world
        member this.SetRingMenu value world = this.SetModelGeneric<RingMenu> value world
        member this.RingMenu = this.ModelGeneric<RingMenu> ()
        member this.ItemSelectEvent = Events.ItemSelectEvent --> this
        member this.CancelEvent = Events.CancelEvent --> this

type RingMenuDispatcher () =
    inherit GuiDispatcher<RingMenu, Message, RingMenuCommand> ({ Items = Map.empty; Cancellable = false })

    override this.Command (_, command, entity, world) =
        match command with
        | ItemCancel -> just (World.publish () entity.CancelEvent entity world)
        | ItemSelect item -> just (World.publish item entity.ItemSelectEvent entity world)

    override this.Content (ringMenu, _) =
        let mutable i = -1
        let items =
            ringMenu.Items |>
            Map.toSeq |>
            Map.ofSeqBy (fun (k, (v, v2)) -> (v, (k, v2))) |>
            Map.toSeqBy (fun _ (v, v2) -> (v, (i <- inc i; i, v2))) |>
            Map.ofSeq
        let items = Map.map (constant (Triple.insert (Map.count items))) items
        [for (itemName, (itemIndex, itemCount, itemEnabled)) in items.Pairs do
            let buttonSize = v3 48.0f 48.0f 0.0f
            Content.button (scstring itemName)
                [Entity.EnabledLocal := itemEnabled
                 Entity.PositionLocal :=
                    (let radius = Constants.Battle.RingMenuRadius
                     let progress = single itemIndex / single itemCount
                     let rotation = progress * MathF.TWO_PI
                     let position = v3 (radius * sin rotation) (radius * cos rotation) 0.0f
                     position - buttonSize * 0.5f)
                 Entity.Size == buttonSize
                 Entity.ElevationLocal == 1.0f
                 Entity.UpImage := asset Assets.Battle.PackageName (itemName + "Up")
                 Entity.DownImage := asset Assets.Battle.PackageName (itemName + "Down")
                 Entity.ClickEvent => ItemSelect itemName]
         if ringMenu.Cancellable then
            Content.button "Cancel"
                [Entity.MountOpt == None
                 Entity.Size == v3 48.0f 48.0f 0.0f
                 Entity.Position == Constants.Battle.CancelPosition
                 Entity.ElevationLocal == 1.0f
                 Entity.UpImage == asset Assets.Battle.PackageName "CancelUp"
                 Entity.DownImage == asset Assets.Battle.PackageName "CancelDown"
                 Entity.ClickEvent => ItemCancel]]