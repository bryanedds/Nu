namespace Nu
open System
open System.IO
open System.Xml
open FSharpx
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module ScreenModule =

    /// Describes the behavior of the screen dissolving algorithm.
    type [<StructuralEquality; NoComparison>] DissolveData =
        { IncomingTime : int64
          OutgoingTime : int64
          DissolveImage : AssetTag }

    /// Describes the behavior of the screen splash algorithm.
    type [<StructuralEquality; NoComparison>] SplashData =
        { DissolveData : DissolveData
          IdlingTime : int64
          SplashImage : AssetTag }

    type Screen with

        static member getName (screen : Screen) = screen.Name
        static member getScreenStateNp (screen : Screen) = screen.ScreenStateNp
        static member setScreenStateNp value screen = { screen with ScreenStateNp = value }
        static member getTransitionTicksNp (screen : Screen) = screen.TransitionTicksNp
        static member setTransitionTicksNp value screen = { screen with TransitionTicksNp = value }
        static member getIncoming (screen : Screen) = screen.Incoming
        static member setIncoming value screen = { screen with Incoming = value }
        static member getOutgoing (screen : Screen) = screen.Outgoing
        static member setOutgoing value screen = { screen with Outgoing = value }
        static member getPublishChanges (screen : Screen) = screen.PublishChanges
        static member setPublishChanges value (screen : Screen) = { screen with PublishChanges = value }
        static member getPersistent (screen : Screen) = screen.Persistent
        static member setPersistent value (screen : Screen) = { screen with Persistent = value }

        /// Register a screen when adding it to the world.
        static member register (screen : Screen) address world =
            screen.DispatcherNp.Register (screen, address, world)

        /// Unregister a screen when removing it from the world.
        static member unregister (screen : Screen) address world =
            screen.DispatcherNp.Unregister (screen, address, world)

        /// Query that a screen idling (that is, not currently transitioning in or out via another screen).
        static member isIdling screen =
            screen.ScreenStateNp = IdlingState

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (screen : Screen) =
            Reflection.dispatchesAs dispatcherTargetType screen.DispatcherNp

        /// Make a screen.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              ScreenStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

[<AutoOpen>]
module WorldScreenModule =

    type World with

        static member private optScreenFinder (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, _) -> Some screen
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenAdder (screen : Screen) (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    let screenMap = Map.add screenName (screen, groupMap) screenMap
                    { world with Simulants = (game, screenMap) }
                | None ->
                    let screenMap = Map.add screenName (screen, Map.empty) screenMap
                    { world with Simulants = (game, screenMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenRemover (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    if Map.isEmpty groupMap then
                        let screenMap = Map.remove screenName screenMap
                        { world with Simulants = (game, screenMap) }
                    else failwith <| "Cannot remove screen " + acstring address + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        /// Query that the world contains a group at the given address.
        static member containsScreen address world =
            Option.isSome <| World.optScreenFinder address world

        /// Try to get a group at the given address.
        static member getOptScreen address world =
            World.optScreenFinder address world

        /// Get a group at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getScreenBy by address world =
            by ^^ Option.get ^^ World.getOptScreen address world

        /// Get a group at the given address (failing with an exception otherwise).
        static member getScreen address world =
            World.getScreenBy id address world

        /// Try to get a screen hierarchy (that is, a screen with a map to all of its group
        /// hierarchies) at the given address.
        static member getOptScreenHierarchy address world =
            match World.getOptScreen address world with
            | Some screen ->
                let groupMap = World.getGroupMapInScreen address world
                Some (screen, groupMap)
            | None -> None
        
        /// Get a screen hierarchy (that is, a screen with a map to all of its group hierarchies)
        /// at the given address (failing with an exception if there isn't one).
        static member getScreenHierarchy address world =
            Option.get <| World.getOptScreenHierarchy address world

        /// Get a screen's address with the given name (failing with an exception if there isn't
        /// one).
        static member getScreenAddress screenName world =
            let address = ntoa<Screen> screenName
            ignore <| World.getScreen address world // ensures address is valid
            address

        static member private setScreenWithoutEvent screen address world =
            World.screenAdder screen address world

        static member private setOptScreenWithoutEvent optScreen address world =
            match optScreen with 
            | Some screen -> World.screenAdder screen address world
            | None -> World.screenRemover address world

        /// Set a screen at the given address (failing with an exception if one doesn't exist).
        static member setScreen screen address world =
            let oldScreen = Option.get <| World.optScreenFinder address world
            let world = World.screenAdder screen address world
            if screen.PublishChanges
            then World.publish4 { OldSimulant = oldScreen } (ScreenChangeEventAddress ->>- address) address world
            else world

        /// Update a screen at the given address with the given 'updater' procedure.
        static member updateScreenAndW updater address world =
            let screen = World.getScreen address world
            let (screen, world) = updater screen world
            World.setScreen screen address world

        /// Update a screen with the given 'updater' procedure at the given address.
        static member updateScreenW updater address world =
            World.updateScreenAndW (fun screen world -> (updater screen world, world)) address world

        /// Update a screen with the given 'updater' procedure at the given address.
        static member updateScreen updater address world =
            World.updateScreenW (fun screen _ -> updater screen) address world

        /// Update the world with the given 'updater' procedure that uses the screen at given
        /// address in its computation.
        static member updateByScreen updater address world : World =
            let screen = World.getScreen address world
            updater screen world

        /// Lens a screen at the given address.
        static member lensScreen address =
            { Get = World.getScreen address
              Set = fun screen -> World.setScreen screen address }

        /// Get the screen hierarches at the given addresses.
        static member getScreenHierarchies addresses world =
            Seq.map (fun address -> World.getScreenHierarchy address world) addresses

        /// Get the screens at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getScreensBy by addresses world =
            Seq.map (fst >> by) <| World.getScreenHierarchies addresses world

        /// Get the screens at the given addresses.
        static member getScreens addresses world =
            World.getScreensBy id addresses world

        /// Get all the screens in the game as mapped by their names.
        static member getScreenMap world =
            snd world.Simulants

        /// Get all the screens in the game.
        static member getScreensInGame world =
            Map.toValueSeqBy fst <| World.getScreenMap world
            
        /// Get the addresses of all the world's screens.
        static member getScreenAddresses world =
            Map.fold (fun addresses screenName _ -> ntoa<Screen> screenName :: addresses) [] (World.getScreenMap world)

        /// Set the screens at the given addresses.
        static member setScreens screens addresses world =
            Seq.fold2 (fun world screen address -> World.setScreen screen address world) world screens addresses
        
        /// Set all the screens in the game.
        static member setScreensInGame screens world =
            Seq.fold (fun world (screen : Screen) -> World.setScreen screen (ntoa screen.Name) world) world screens

        /// Update the screens at the given addresses and the world with the given 'updater' procedure.
        static member updateScreensAndW updater addresses world =
            Seq.fold (fun world address -> World.updateScreenAndW updater address world) world addresses

        /// Update the screens at the given addresses with the given 'updater' procedure.
        static member updateScreensW updater addresses world =
            Seq.fold (fun world address -> World.updateScreenW updater address world) world addresses
        
        /// Update the screens at the given addresses with the given 'updater' procedure.
        static member updateScreens updater addresses world =
            Seq.fold (fun world address -> World.updateScreen updater address world) world addresses

        /// Lens the screens at the given addresses.
        static member lensScreens addresses =
            { Get = World.getScreens addresses
              Set = fun screens -> World.setScreens screens addresses }

        /// Lens all screens in the game at the given address.
        static member lensScreensInGame =
            { Get = World.getScreensInGame
              Set = fun screens -> World.setScreensInGame screens }

        /// Filter the given screen addresses by applying the 'pred' procedure to each screen at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterScreenAddressesW pred addresses world =
            Seq.filter (fun address -> World.getScreenBy (fun screen -> pred screen world) address world) addresses

        /// Filter the given screen addresses by applying the 'pred' procedure to each screen at
        /// its respected address.
        static member filterScreenAddresses pred addresses world =
            World.filterScreenAddressesW (fun screen _ -> pred screen) addresses world

        static member private registerScreen screen address world =
            Screen.register screen address world

        static member private unregisterScreen screen address world =
            Screen.unregister screen address world

        /// Remove a screen from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the screen's existence. Use with caution.
        static member removeScreenImmediate address world =
            let world = World.publish4 () (ScreenRemovingEventAddress ->>- address) address world
            match World.getOptScreen address world with
            | Some screen ->
                let (screen, world) = World.unregisterScreen screen address world
                let groupAddresses = World.getGroupAddressesInScreen address world
                let world = snd <| World.removeGroupsImmediate groupAddresses world
                let world = World.setOptScreenWithoutEvent None address world
                (Some screen, world)
            | None -> (None, world)

        /// Remove a screen from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeScreen address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeScreenImmediate address world }
            World.addTask task world

        /// Add a screen at the given address to the world.
        static member addScreen screenHierarchy address world =
            let (screen, groupHierarchies) = screenHierarchy
            if not <| World.containsScreen address world then
                let world = World.setScreenWithoutEvent screen address world
                let world = snd <| World.addGroups groupHierarchies address world
                let (screen, world) = World.registerScreen screen address world
                let world = World.publish4 () (ScreenAddEventAddress ->>- address) address world
                (screen, world)
            else failwith <| "Adding a screen that the world already contains at address '" + acstring address + "'."

        /// Make a screen (does NOT add the screen to the world!)
        static member makeScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher optName
            Reflection.attachFields dispatcher screen
            screen
        
        /// Make a screen (does NOT add the screen to the world!)
        static member makeDissolveScreen dissolveData dispatcherName optName world =
            let optDissolveImage = Some dissolveData.DissolveImage
            let screen = World.makeScreen dispatcherName optName world
            let incomingDissolve = { Transition.make Incoming with TransitionLifetime = dissolveData.IncomingTime; OptDissolveImage = optDissolveImage }
            let outgoingDissolve = { Transition.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; OptDissolveImage = optDissolveImage }
            { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }

        /// Write a screen hierarchy to an xml writer.
        static member writeScreenHierarchy (writer : XmlWriter) screenHierarchy world =
            let (screen : Screen, groupHierarchies) = screenHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (screen.DispatcherNp.GetType ()).Name)
            Reflection.writePropertiesFromTarget tautology3 writer screen
            writer.WriteStartElement GroupsNodeName
            World.writeGroupHierarchies writer groupHierarchies world
            writer.WriteEndElement ()

        /// Write a screen hierarchy to an xml file.
        static member writeScreenHierarchyToFile (filePath : string) screenHierarchy world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement ScreenNodeName
            World.writeScreenHierarchy writer screenHierarchy world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple screen hierarchies to an xml writer.
        static member writeScreenHierarchies (writer : XmlWriter) screenHierarchies world =
            let screenHierarchies =
                List.sortBy
                    (fun (screen : Screen, _) -> screen.CreationTimeNp)
                    (Map.toValueList screenHierarchies)
            let screenHierarchies = List.filter (fun (screen : Screen, _) -> screen.Persistent) screenHierarchies
            for screenHierarchy in screenHierarchies do
                writer.WriteStartElement ScreenNodeName
                World.writeScreenHierarchy writer screenHierarchy world
                writer.WriteEndElement ()

        /// Read a screen hierarchy from an xml node.
        static member readScreenHierarchy
            (screenNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName screenNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.ScreenDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName world.Components.ScreenDispatchers
            let screen = Screen.make dispatcher None
            Reflection.attachFields screen.DispatcherNp screen
            Reflection.readPropertiesToTarget screenNode screen
            let groupHierarchies = World.readGroupHierarchies (screenNode : XmlNode) defaultGroupDispatcherName defaultEntityDispatcherName world
            (screen, groupHierarchies)

        /// Read a screen hierarchy from an xml file.
        static member readScreenHierarchyFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let screenNode = rootNode.[ScreenNodeName]
            World.readScreenHierarchy
                screenNode
                typeof<ScreenDispatcher>.Name
                typeof<GroupDispatcher>.Name
                typeof<EntityDispatcher>.Name
                world

        /// Read multiple screen hierarchies from an xml node.
        static member readScreenHierarchies
            (gameNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            match gameNode.SelectSingleNode ScreensNodeName with
            | null -> Map.empty
            | screensNode ->
                let screenNodes = screensNode.SelectNodes ScreenNodeName
                Seq.fold
                    (fun screenHierarchies screenNode ->
                        let screenHierarchy =
                            World.readScreenHierarchy
                                screenNode
                                defaultDispatcherName
                                defaultGroupDispatcherName
                                defaultEntityDispatcherName
                                world
                        let screenName = (fst screenHierarchy).Name
                        Map.add screenName screenHierarchy screenHierarchies)
                    Map.empty
                    (enumerable screenNodes)