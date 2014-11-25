namespace Nu
open System
open System.Xml
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module ScreenModule =

    type [<StructuralEquality; NoComparison>] DissolveData =
        { IncomingTime : int64
          OutgoingTime : int64
          DissolveImage : Image }

    type [<StructuralEquality; NoComparison>] SplashData =
        { DissolveData : DissolveData
          IdlingTime : int64
          SplashImage : Image }

    type Screen with

        static member setScreenState state screen = { screen with ScreenStateNp = state }
        static member setTransitionTicks state screen = { screen with TransitionTicksNp = state }
        static member setIncoming incoming screen = { screen with Incoming = incoming }
        static member setOutgoing outgoing screen = { screen with Outgoing = outgoing }
        static member setPersistent persistent (screen : Screen) = { screen with Persistent = persistent }

        static member register address (screen : Screen) (world : World) : Screen * World =
            screen.DispatcherNp.Register (address, screen, world)

        static member unregister address (screen : Screen) (world : World) : Screen * World =
            screen.DispatcherNp.Unregister (address, screen, world)

        static member isIdling screen =
            screen.ScreenStateNp = IdlingState

        static member dispatchesAs (dispatcherTargetType : Type) (screen : Screen) =
            Reflection.dispatchesAs dispatcherTargetType screen.DispatcherNp

        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              ScreenStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

[<AutoOpen>]
module WorldScreenModule =

    type World with

        static member private optScreenFinder (address : Screen Address) world =
            match address.Names with
            | [screenName] -> Map.tryFind screenName world.Screens
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenAdder (address : Screen Address) world child =
            match address.Names with
            | [screenName] -> { world with Screens = Map.add screenName child world.Screens }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenRemover (address : Screen Address) world =
            match address.Names with
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

        static member getOptScreenHierarchy address world =
            match World.getOptScreen address world with
            | Some screen ->
                let groupHierarchies = World.getGroupHierarchies address world
                Some (screen, groupHierarchies)
            | None -> None
        
        static member getScreenHierarchy address world =
            Option.get <| World.getOptScreenHierarchy address world

        static member getScreenMap world =
            world.Screens

        static member getScreens world =
            let screenMap = World.getScreenMap world
            Map.toValueSeq screenMap

        static member getScreenMap2 screenNames world =
            let screenNames = Set.ofSeq screenNames
            let screenMap = World.getScreenMap world
            Map.filter (fun screenName _ -> Set.contains screenName screenNames) screenMap

        static member getScreens2 screenNames world =
            let screenMap = World.getScreenMap2 screenNames world
            Map.toValueSeq screenMap

        static member getScreenHierarchies world =
            let screens = World.getScreenMap world
            Map.map
                (fun screenName screen ->
                    let screenAddress = ltoa<Screen> [screenName]
                    let groupHierarchies = World.getGroupHierarchies screenAddress world
                    (screen, groupHierarchies))
                screens

        static member private registerScreen address screen world =
            Screen.register address screen world

        static member private unregisterScreen address screen world =
            Screen.unregister address screen world

        static member removeScreenImmediate address screen world =
            let world = World.publish4 () (RemovingEventAddress ->>- address) address world
            let groupMap = World.getGroupMap address world
            let world = snd <| World.removeGroupsImmediate address groupMap world
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

        static member addScreen address screenHierarchy world =
            let (screen, groupHierarchies) = screenHierarchy
            if not <| World.containsScreen address world then
                let (screen, world) =
                    match World.getOptScreen address world with
                    | Some _ -> World.removeScreenImmediate address screen world
                    | None -> (screen, world)
                let world = World.setScreen address screen world
                let world = snd <| World.addGroups address groupHierarchies world
                let (screen, world) = World.registerScreen address screen world
                let world = World.publish4 () (AddEventAddress ->>- address) address world
                (screen, world)
            else failwith <| "Adding a screen that the world already contains at address '" + acstring address + "'."

        static member writeScreen (writer : XmlWriter) screenHierarchy world =
            let (screen : Screen, groupHierarchies) = screenHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (screen.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology2 writer screen
            writer.WriteStartElement GroupsNodeName
            World.writeGroups writer groupHierarchies world
            writer.WriteEndElement ()

        static member writeScreens (writer : XmlWriter) screenHierarchies world =
            let screenHierarchies =
                List.sortBy
                    (fun (screen : Screen, _) -> screen.CreationTimeNp)
                    (Map.toValueList screenHierarchies)
            let screenHierarchies = List.filter (fun (screen : Screen, _) -> screen.Persistent) screenHierarchies
            for screenHierarchy in screenHierarchies do
                writer.WriteStartElement ScreenNodeName
                World.writeScreen writer screenHierarchy world
                writer.WriteEndElement ()

        static member readScreen
            (screenNode : XmlNode)
            defaultDispatcherName
            defaultGroupDispatcherName
            defaultEntityDispatcherName
            world =
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName screenNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.ScreenDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher None
            Reflection.attachFields screen.DispatcherNp screen
            Serialization.readPropertiesToTarget screenNode screen
            let groupHierarchies = World.readGroups (screenNode : XmlNode) defaultGroupDispatcherName defaultEntityDispatcherName world
            (screen, groupHierarchies)

        static member readScreens
            (parentNode : XmlNode)
            defaultDispatcherName
            defaultGroupDispatcherName
            defaultEntityDispatcherName
            world =
            match parentNode.SelectSingleNode ScreensNodeName with
            | null -> Map.empty
            | screensNode ->
                let screenNodes = screensNode.SelectNodes ScreenNodeName
                Seq.fold
                    (fun screenHierarchies screenNode ->
                        let screenHierarchy =
                            World.readScreen
                                screenNode
                                defaultDispatcherName
                                defaultGroupDispatcherName
                                defaultEntityDispatcherName
                                world
                        let screenName = (fst screenHierarchy).Name
                        Map.add screenName screenHierarchy screenHierarchies)
                    Map.empty
                    (enumerable screenNodes)

        static member makeScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher optName
            Reflection.attachFields dispatcher screen
            screen
        
        static member makeDissolveScreen dissolveData dispatcherName optName world =
            let optDissolveImage = Some dissolveData.DissolveImage
            let screen = World.makeScreen dispatcherName optName world
            let incomingDissolve = { Transition.make Incoming with TransitionLifetime = dissolveData.IncomingTime; OptDissolveImage = optDissolveImage }
            let outgoingDissolve = { Transition.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; OptDissolveImage = optDissolveImage }
            { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }