namespace Nu
open System
open System.IO
open System.Xml
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module GameModule =

    type Game with

        static member getPublishChanges (game : Game) = game.PublishChanges
        static member setPublishChanges value (game : Game) = { game with PublishChanges = value }
        static member getOptSelectedScreenAddress game = game.OptSelectedScreenAddress
        static member setOptSelectedScreenAddress optSelectedScreenAddress game = { game with OptSelectedScreenAddress = optSelectedScreenAddress }

        /// Register a game when adding it to the world. Note that there is not corresponding
        /// Unregister method due to the inability to remove a game from the world.
        static member register (game : Game) (world : World) : Game * World =
            game.DispatcherNp.Register (game, world)

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (game : Game) =
            Reflection.dispatchesAs dispatcherTargetType game.DispatcherNp

        /// Make a game.
        static member make dispatcher =
            { Id = Core.makeId ()
              OptSelectedScreenAddress = None
              PublishChanges = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}
            
[<AutoOpen>]
module WorldGameModule =

    type World with

        /// Get the game, then transform it with the 'by' procudure.
        static member getGameBy by world =
            by <| fst world.Simulants

        /// Get the game.
        static member getGame world =
            World.getGameBy id world

        static member getGameHierarchy world =
            let game = World.getGame world
            let screenMap = World.getScreenMap world
            (game, screenMap)

        /// Set the game.
        static member setGame game world =
            let oldGame = World.getGame world
            let screenMap = World.getScreenMap world
            let world = { world with Simulants = (game, screenMap) }
            if game.PublishChanges
            then World.publish4 { OldSimulant = oldGame } (GameChangeEventAddress ->>- GameAddress) GameAddress world
            else world

        /// Update the game with the given 'updater' procedure. Also passes the current world value
        /// to the procedure.
        static member updateGameW updater world =
            let game = World.getGame world
            let game = updater game world
            World.setGame game world

        /// Update the game with the given 'updater' procedure.
        static member updateGame updater world =
            World.updateGameW (fun game _ -> updater game) world

        /// Update the world with the given 'updater' procedure that uses the game in its
        /// computation.
        static member updateByGame updater world : World =
            let game = World.getGame world
            updater game world

        static member getOptSelectedScreenAddress world =
            let game = World.getGame world
            game.OptSelectedScreenAddress
        
        static member setOptSelectedScreenAddress optAddress world =
            World.updateGame (Game.setOptSelectedScreenAddress optAddress) world
        
        /// Get the address of the currently selected screen (failing with an exception if there
        /// isn't one).
        static member getSelectedScreenAddress world =
            Option.get <| World.getOptSelectedScreenAddress world
        
        /// Set the address of the currently selected screen. Be careful using this function
        /// directly as you may be wanting to use the higher-level World.transitionScreen function.
        static member setSelectedScreenAddress address world =
            World.setOptSelectedScreenAddress (Some address) world
        
        /// Try to get the address of the currently selected screen.
        static member getOptSelectedScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.getOptScreen selectedScreenAddress world
            | None -> None

        /// Set the address of the currently selected screen to Some Address or None. Be careful
        /// using this function directly as you may be wanting to use the higher-level
        /// World.transitionScreen function.
        static member setOptSelectedScreen optScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.setScreen (Option.get optScreen) selectedScreenAddress world
            | None -> failwith "Cannot set a non-existent screen."

        /// Get the screen that is currently selected, failing with an exception if there isn't one.
        static member getSelectedScreen world =
            Option.get <| World.getOptSelectedScreen world
        
        /// Set the currently selected screen.
        static member setSelectedScreen screen world =
            World.setOptSelectedScreen (Some screen) world

        /// Query that a simulant at the given address is contained by the currently selected
        /// screen or one of its groups.
        static member isAddressSelected address world =
            let optScreenAddress = World.getOptSelectedScreenAddress world
            match (address.Names, Option.map (fun address -> address.Names) optScreenAddress) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

        /// Make a game.
        static member makeGame dispatcher =
            let game = Game.make dispatcher
            Reflection.attachFields dispatcher game
            game

        /// Write a game hierarchy to an xml writer.
        static member writeGameHierarchy (writer : XmlWriter) gameHierarchy world =
            let (game : Game, screenHierarchies) = gameHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (game.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology3 writer game
            writer.WriteStartElement ScreensNodeName
            World.writeScreenHierarchies writer screenHierarchies world
            writer.WriteEndElement ()

        /// Write a game hierarchy to an xml file.
        static member writeGameHierarchyToFile (filePath : string) gameHierarchy world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GameNodeName
            World.writeGameHierarchy writer gameHierarchy world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game hierarchy from an xml node.
        static member readGameHierarchy
            (gameNode : XmlNode) defaultDispatcherName defaultScreenDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName gameNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GameDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName world.Components.GameDispatchers
            let game = World.makeGame dispatcher
            Serialization.readPropertiesToTarget gameNode game
            let screenHierarchies =
                World.readScreenHierarchies
                    (gameNode : XmlNode)
                    defaultScreenDispatcherName
                    defaultGroupDispatcherName
                    defaultEntityDispatcherName
                    world
            (game, screenHierarchies)

        /// Read a game hierarchy from an xml file.
        static member readGameHierarchyFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let gameNode = rootNode.[GameNodeName]
            World.readGameHierarchy
                gameNode
                typeof<GameDispatcher>.Name
                typeof<ScreenDispatcher>.Name
                typeof<GroupDispatcher>.Name
                typeof<EntityDispatcher>.Name
                world