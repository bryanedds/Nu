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
module WorldGameModule =

    type World with

        /// Try to get the address of the currently selected screen.
        static member getOptSelectedScreenRep world =
            let game = World.getGame world
            game.OptSelectedScreenRep
        
        /// Set the address of the currently selected screen to Some Address or None. Be careful
        /// using this function directly as you may be wanting to use the higher-level
        /// World.transitionScreen function.
        static member setOptSelectedScreenRep optScreenRep world =
            World.updateGame (Game.setOptSelectedScreenRep optScreenRep) world
        
        /// Get the address of the currently selected screen (failing with an exception if there
        /// isn't one).
        static member getSelectedScreenRep world =
            Option.get <| World.getOptSelectedScreenRep world
        
        /// Set the address of the currently selected screen. Be careful using this function
        /// directly as you may be wanting to use the higher-level World.transitionScreen function.
        static member setSelectedScreenRep screenRep world =
            World.setOptSelectedScreenRep (Some screenRep) world

        /// Query that a simulant at the given address is the currently selected screen, is
        /// contained by the currently selected screen or its groups.
        static member isSimulantSelected<'s when 's :> SimulantRep> (simulantRep : 's) world =
            let optScreenRep = World.getOptSelectedScreenRep world
            let optScreenNames = Option.map (fun (screenRep : ScreenRep) -> screenRep.ScreenAddress.Names) optScreenRep
            match (simulantRep.SimulantAddress.Names, optScreenNames) with
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
            Reflection.writePropertiesFromTarget tautology3 writer game
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
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName gameNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GameDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName world.Components.GameDispatchers
            let game = World.makeGame dispatcher
            Reflection.readPropertiesToTarget gameNode game
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