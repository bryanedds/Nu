namespace Nu
open System
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module ScreenModule =

    type Screen with

        static member setScreenState state screen = { screen with ScreenState = state }
        static member setIncoming incoming screen = { screen with Incoming = incoming }
        static member setOutgoing outgoing screen = { screen with Outgoing = outgoing }

        static member register address (screen : Screen) (world : World) : Screen * World =
            screen.DispatcherNp.Register (address, screen, world)

        static member unregister address (screen : Screen) (world : World) : Screen * World =
            screen.DispatcherNp.Unregister (address, screen, world)

        static member isIdling screen =
            screen.ScreenState = IdlingState

        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              ScreenState = IdlingState
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

[<AutoOpen>]
module WorldScreenModule =

    type World with

        static member private optScreenFinder address world =
            match address.AddrList with
            | [screenName] -> Map.tryFind screenName world.Screens
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenAdder address world child =
            match address.AddrList with
            | [screenName] -> { world with Screens = Map.add screenName child world.Screens }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenRemover address world =
            match address.AddrList with
            | [screenName] -> { world with Screens = Map.remove screenName world.Screens }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member getScreen address world = Option.get <| World.optScreenFinder address world
        static member setScreen address screen world = World.screenAdder address world screen
        static member getOptScreen address world = World.optScreenFinder address world
        static member containsScreen address world = Option.isSome <| World.getOptScreen address world
        static member private setOptScreen address optScreen world =
            match optScreen with
            | Some screen -> World.setScreen address screen world
            | None -> World.screenRemover address world

        static member getScreens1 world =
            seq {
                for screenKvp in world.Entities do
                    let address = Address.make [screenKvp.Key]
                    yield (address, screenKvp.Value) }

        static member getScreens address world =
            match address.AddrList with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid game address '" + acstring address + "'. Game address is always empty."

        static member getScreens3 gameAddress screenNames world =
            let screenNames = Set.ofSeq screenNames
            let screens = World.getGroups gameAddress world
            Map.filter (fun screenName _ -> Set.contains screenName screenNames) screens

        static member private registerScreen address screen world =
            Screen.register address screen world

        static member private unregisterScreen address screen world =
            Screen.unregister address screen world

        static member removeScreenImmediate address screen world =
            let world = World.publish4 (RemovingEventAddress ->- address) address () world
            let groups = World.getGroups address world
            let world = snd <| World.removeGroupsImmediate address groups world
            let (screen, world) = World.unregisterScreen address screen world
            let world = World.setOptScreen address None world
            (screen, world)

        static member removeScreen address (screen : Screen) world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world ->
                    match World.getOptScreen address world with
                    | Some screen -> snd <| World.removeScreenImmediate address screen world
                    | None -> world }
            let world = World.addTask task world
            (screen, world)

        static member addScreen address screen groupDescriptors world =
            if not <| World.containsScreen address world then
                let (screen, world) =
                    match World.getOptScreen address world with
                    | Some _ -> World.removeScreenImmediate address screen world
                    | None -> (screen, world)
                let world = World.setScreen address screen world
                let world = snd <| World.addGroups address groupDescriptors world
                let (screen, world) = World.registerScreen address screen world
                let world = World.publish4 (AddEventAddress ->- address) address () world
                (screen, world)
            else failwith <| "Adding a screen that the world already contains at address '" + acstring address + "'."

        static member makeScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher optName
            Reflection.attachFields dispatcher screen
            screen
        
        static member makeDissolveScreen dispatcherName optName incomingTime outgoingTime dissolveImage world =
            let optDissolveImage = Some dissolveImage
            let screen = World.makeScreen dispatcherName optName world
            let incomingDissolve = { Transition.make Incoming with TransitionLifetime = incomingTime; OptDissolveImage = optDissolveImage }
            let outgoingDissolve = { Transition.make Outgoing with TransitionLifetime = outgoingTime; OptDissolveImage = optDissolveImage }
            { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }