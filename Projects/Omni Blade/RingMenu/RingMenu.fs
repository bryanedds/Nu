// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type RingMenu =
    { Items : Map<string, int * bool>
      Cancellable : bool }

    /// The items as the ring lays them out clockwise from the top - each with its place, the count, and whether it
    /// can be chosen.
    static member getLayout ringMenu =
        let items = ringMenu.Items |> Map.toArray |> Array.sortBy (fun (_, (tag, _)) -> tag)
        items |> Array.mapi (fun index (name, (_, enabled)) -> (name, index, items.Length, enabled))

type RingMenuCommand =
    | ItemCancel
    | ItemSelect of string
    | Turn
    interface Command

[<AutoOpen>]
module RingMenuExtensions =
    type Entity with
        member this.GetRingMenu world = this.GetModelGeneric<RingMenu> world
        member this.SetRingMenu value world = this.SetModelGeneric<RingMenu> value world
        member this.RingMenu = this.ModelGeneric<RingMenu> ()
        member this.GetRingMenuFocusOpt world : string option = this.Get (nameof this.RingMenuFocusOpt) world
        member this.SetRingMenuFocusOpt (value : string option) world = this.Set (nameof this.RingMenuFocusOpt) value world
        member this.RingMenuFocusOpt = lens (nameof this.RingMenuFocusOpt) this this.GetRingMenuFocusOpt this.SetRingMenuFocusOpt
        member this.GetRingMenuSpin world : single = this.Get (nameof this.RingMenuSpin) world
        member this.SetRingMenuSpin (value : single) world = this.Set (nameof this.RingMenuSpin) value world
        member this.RingMenuSpin = lens (nameof this.RingMenuSpin) this this.GetRingMenuSpin this.SetRingMenuSpin
        member this.GetRingMenuRotation world : single = this.Get (nameof this.RingMenuRotation) world
        member this.SetRingMenuRotation (value : single) world = this.Set (nameof this.RingMenuRotation) value world
        member this.RingMenuRotation = lens (nameof this.RingMenuRotation) this this.GetRingMenuRotation this.SetRingMenuRotation
        member this.ItemSelectEvent = Events.ItemSelectEvent --> this
        member this.CancelEvent = Events.CancelEvent --> this

type RingMenuDispatcher () =
    inherit GuiDispatcher<RingMenu, Message, RingMenuCommand> ({ Items = Map.empty; Cancellable = false })

    static let ButtonSize = v3 48.0f 48.0f 0.0f
    static let TurnRate = 0.3f // how much of the way to the focused item the ring turns each frame

    /// Where an item sits on a ring turned by a rotation, clockwise from the top.
    static let getItemPosition index count rotation =
        let radius = Constants.Battle.RingMenuRadius
        let angle = single index / single count * MathF.TWO_PI + rotation
        v3 (radius * sin angle) (radius * cos angle) 0.0f - ButtonSize * 0.5f

    /// The rotation that puts a ring's focused item at the top, if it's focusing one.
    static let tryGetFocusRotation (entity : Entity) world =
        let layout = RingMenu.getLayout (entity.GetRingMenu world)
        match entity.GetRingMenuFocusOpt world |> Option.bind (fun focus -> layout |> Array.tryFind (fun (name, _, _, _) -> name = focus)) with
        | Some (_, index, count, _) -> Some (-(single index / single count) * MathF.TWO_PI)
        | None -> None

    /// Turn a ring to a rotation, placing its items.
    static let place (entity : Entity) rotation world =
        entity.SetRingMenuRotation rotation world
        for (itemName, index, count, _) in RingMenu.getLayout (entity.GetRingMenu world) do
            let item = entity / scstring itemName
            if item.GetExists world then item.SetPositionLocal (getItemPosition index count rotation) world

    /// Turn a ring to its focused item at once rather than over the frames to come, as when it's shown again.
    static member snapToFocus (entity : Entity) world =
        match tryGetFocusRotation entity world with
        | Some rotation -> place entity rotation world
        | None -> ()

    // the ring's turn is kept apart from its model, which the battle sets anew each frame
    static member Properties =
        [nonPersistent Entity.RingMenuFocusOpt None
         nonPersistent Entity.RingMenuSpin 1.0f // which way the ring last turned, 1 clockwise, for half turns
         nonPersistent Entity.RingMenuRotation 0.0f]

    override this.Definitions (_, _) =
        [Entity.UpdateEvent => Turn]

    override this.Command (_, command, entity, world) =
        match command with
        | ItemCancel -> World.publish () entity.CancelEvent entity world
        | ItemSelect item -> World.publish item entity.ItemSelectEvent entity world
        | Turn ->

            // turn quickly the short way round toward the focused item being at the top, placing the items as it goes
            let rotation = entity.GetRingMenuRotation world
            let rotation =
                match tryGetFocusRotation entity world with
                | Some target ->
                    let delta = target - rotation
                    let delta = delta - MathF.Round (delta / MathF.TWO_PI) * MathF.TWO_PI
                    // a half turn's as short either way, so it goes the way the ring was turned
                    let delta =
                        if abs (abs delta - MathF.PI) < 0.01f
                        then (if entity.GetRingMenuSpin world < 0.0f then -MathF.PI else MathF.PI)
                        else delta
                    if abs delta < 0.01f then target else rotation + delta * TurnRate
                | None -> rotation
            place entity rotation world

    override this.Content (ringMenu, _) =
        [for (itemName, itemIndex, itemCount, itemEnabled) in RingMenu.getLayout ringMenu do
            Content.button (scstring itemName)
                [Entity.EnabledLocal := itemEnabled
                 Entity.PositionLocal == getItemPosition itemIndex itemCount 0.0f // then placed as the ring turns
                 Entity.Size == ButtonSize
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