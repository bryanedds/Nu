namespace Nu
open System
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

    type Screen with

        static member makeDefault dispatcherName =
            { Id = NuCore.makeId ()
              State = IdlingState
              Incoming = Transition.makeDefault Incoming
              Outgoing = Transition.makeDefault Outgoing
              Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

        static member makeDissolve dispatcherName incomingTime outgoingTime =
            let optDissolveImage = Some <| { ImageAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
            let incomingDissolve = { Transition.makeDefault Incoming with TransitionLifetime = incomingTime; OptDissolveImage = optDissolveImage }
            let outgoingDissolve = { Transition.makeDefault Outgoing with TransitionLifetime = outgoingTime; OptDissolveImage = optDissolveImage }
            let screen = Screen.makeDefault dispatcherName
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

        static member getScreen address world = Option.get <| World.optScreenFinder address world
        static member setScreen address screen world = World.screenAdder address world screen
        static member getOptScreen address world = World.optScreenFinder address world
        static member containsScreen address world = Option.isSome <| World.getOptScreen address world
        static member private setOptScreen address optScreen world =
            match optScreen with
            | None -> World.screenRemover address world
            | Some screen -> World.setScreen address screen world
            
        static member withScreen fn address world = Sim.withSimulant World.getScreen World.setScreen fn address world
        static member withScreenAndWorld fn address world = Sim.withSimulantAndWorld World.getScreen World.setScreen fn address world
        static member tryWithScreen fn address world = Sim.tryWithSimulant World.getOptScreen World.setScreen fn address world
        static member tryWithScreenAndWorld fn address world = Sim.tryWithSimulantAndWorld World.getOptScreen World.setScreen fn address world

        static member getScreens address world =
            match address with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'."

        static member registerScreen address (screen : Screen) world =
            Screen.register address screen world

        static member unregisterScreen address world =
            let screen = World.getScreen address world
            Screen.unregister address screen world

        static member removeScreenImmediate (address : Address) world =
            let world = World.publish4 (RemovingEventName @ address) address NoData world
            let world = World.clearGroupsImmediate address world
            let world = World.unregisterScreen address world
            World.setOptScreen address None world

        static member removeScreen address world =
            let task =
                { ScheduledTime = world.TickTime
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
            World.publish4 (AddEventName @ address) address NoData world