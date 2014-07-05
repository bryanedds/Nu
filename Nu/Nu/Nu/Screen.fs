namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module ScreenModule =

    type Screen with

        static member register (address : Address) (screen : Screen) (world : World) : World = screen?Register (address, world)
        static member unregister (address : Address) (screen : Screen) (world : World) : World = screen?Unregister (address, world)

    type ScreenDispatcher () =

        abstract member Register : Address * World -> World
        default dispatcher.Register (_, world) = world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (_, world) = world

[<RequireQualifiedAccess>]
module Screen =

    let makeDefault dispatcherName =
        { Id = NuCore.makeId ()
          State = IdlingState
          Incoming = Transition.makeDefault Incoming
          Outgoing = Transition.makeDefault Outgoing
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDissolve dispatcherName incomingTime outgoingTime =
        let optDissolveSprite = Some <| { SpriteAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
        let incomingDissolve = { Transition.makeDefault Incoming with TransitionLifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { Transition.makeDefault Outgoing with TransitionLifetime = outgoingTime; OptDissolveSprite = optDissolveSprite }
        let screen = makeDefault dispatcherName
        { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }

[<AutoOpen>]
module WorldScreenModule =

    type World with

        static member private optScreenFinder (address : Address) world =
            Map.tryFind (List.at 0 address) world.Screens

        static member private screenAdder (address : Address) world child =
            { world with Screens = Map.add (List.at 0 address) child world.Screens }

        static member private screenRemover (address : Address) world =
            { world with Screens = Map.remove (List.at 0 address) world.Screens }

        static member private screenIncoming =
            { Get = fun screen -> screen.Incoming
              Set = fun value screen -> { screen with Incoming = value }}

        static member private screenOutgoing =
            { Get = fun screen -> screen.Outgoing
              Set = fun value screen -> { screen with Outgoing = value }}

        static member private worldScreen address =
            { Get = fun world -> Option.get <| World.optScreenFinder address world
              Set = fun screen world -> World.screenAdder address world screen }

        static member private worldOptScreen address =
            { Get = fun world -> World.optScreenFinder address world
              Set = fun optScreen world -> match optScreen with None -> World.screenRemover address world | Some screen -> World.screenAdder address world screen }

        static member private worldScreens address =
            { Get = fun world ->
                match address with
                | [] -> world.Screens
                | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'."
              Set = fun screens world ->
                match address with
                | [] -> { world with Screens = Map.addMany (Map.toSeq screens) world.Screens }
                | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'." }

        static member worldScreenIncoming address = World.worldScreen address >>| World.screenIncoming
        static member worldScreenOutgoing address = World.worldScreen address >>| World.screenOutgoing

        static member getScreen address world = get world <| World.worldScreen address
        static member setScreen address screen world = set screen world <| World.worldScreen address
        static member withScreen fn address world = Sim.withSimulant World.worldScreen fn address world
        static member withScreenAndWorld fn address world = Sim.withSimulantAndWorld World.worldScreen fn address world
    
        static member getOptScreen address world = get world <| World.worldOptScreen address
        static member containsScreen address world = Option.isSome <| World.getOptScreen address world
        static member private setOptScreen address optScreen world = set optScreen world <| World.worldOptScreen address
        static member tryWithScreen fn address world = Sim.tryWithSimulant World.worldOptScreen World.worldScreen fn address world
        static member tryWithScreenAndWorld fn address world = Sim.tryWithSimulantAndWorld World.worldOptScreen World.worldScreen fn address world

        static member getScreens address world = get world <| World.worldScreens address
        static member private setScreens address screens world = set screens world <| World.worldScreens address

        static member registerScreen address (screen : Screen) world =
            Screen.register address screen world

        static member unregisterScreen address world =
            let screen = World.getScreen address world
            Screen.unregister address screen world

        static member removeScreenImmediate (address : Address) world =
            let world = World.publish4 (RemovingEvent @ address) address NoData world
            let world = World.clearGroupsImmediate address world
            let world = World.unregisterScreen address world
            World.setOptScreen address None world

        static member removeScreen address world =
            let task =
                { Time = world.Ticks
                  Operation = fun world -> if World.containsScreen address world then World.removeScreenImmediate address world else world }
            { world with Tasks = task :: world.Tasks }

        static member addScreen address screen groupDescriptors world =
            let world =
                match World.getOptScreen address world with
                | None -> world
                | Some _ -> World.removeScreenImmediate address world
            let world = World.setScreen address screen world
            let world = World.addGroups address groupDescriptors world
            let world = World.registerScreen address screen world
            World.publish4 (AddedEvent @ address) address NoData world