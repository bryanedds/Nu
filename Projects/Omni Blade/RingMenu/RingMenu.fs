// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type RingMenu =
    { Items : Map<string, int * bool>
      Cancellable : bool
      Rotation : single } // how far the ring's turned clockwise, in radians, its items placed accordingly

    /// The items as the ring lays them out clockwise from the top - each with its place, the count, and whether it
    /// can be chosen.
    static member getLayout ringMenu =
        let items = ringMenu.Items |> Map.toArray |> Array.sortBy (fun (_, (tag, _)) -> tag)
        items |> Array.mapi (fun index (name, (_, enabled)) -> (name, index, items.Length, enabled))

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
    inherit GuiDispatcher<RingMenu, Message, RingMenuCommand> ({ Items = Map.empty; Cancellable = false; Rotation = 0.0f })

    /// Where an item sits on a ring turned by a rotation, clockwise from the top.
    static let getItemPosition index count rotation =
        let radius = Constants.Battle.RingMenuRadius
        let angle = single index / single count * MathF.TWO_PI + rotation
        v3 (radius * sin angle) (radius * cos angle) 0.0f - Constants.Battle.RingMenuButtonSize * 0.5f

    override this.Command (_, command, entity, world) =
        match command with
        | ItemCancel -> World.publish () entity.CancelEvent entity world
        | ItemSelect item -> World.publish item entity.ItemSelectEvent entity world

    override this.Content (ringMenu, _) =
        [for (itemName, itemIndex, itemCount, itemEnabled) in RingMenu.getLayout ringMenu do
            Content.button (scstring itemName)
                [Entity.EnabledLocal := itemEnabled
                 Entity.PositionLocal := getItemPosition itemIndex itemCount ringMenu.Rotation
                 Entity.Size == Constants.Battle.RingMenuButtonSize
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