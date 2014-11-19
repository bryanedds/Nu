namespace Nu
open System
open System.IO
open System.Xml
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module GameModule =

    type Game with

        static member register (game : Game) (world : World) : Game * World =
            game.DispatcherNp.Register (game, world)

        static member setOptSelectedScreenAddress optSelectedScreenAddress game =
            { game with OptSelectedScreenAddress = optSelectedScreenAddress }

        static member make dispatcher optName =
            let id = Core.makeId ()
            let game =
                { Id = id
                  Name = match optName with None -> acstring id | Some name -> name
                  OptSelectedScreenAddress = None
                  CreationTimeNp = DateTime.UtcNow
                  DispatcherNp = dispatcher
                  Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }
            Reflection.attachFields dispatcher game
            game

[<AutoOpen>]
module WorldGameModule =

    type World with

        static member getGame world = world.Game
        static member setGame game world = { world with Game = game }

        static member getGame' world =
            let game = world.Game
            let screensHierarchy = World.getScreens' world
            (game, screensHierarchy)

        static member getOptSelectedScreenAddress world = world.Game.OptSelectedScreenAddress
        static member setOptSelectedScreenAddress optAddress world = World.setGame (Game.setOptSelectedScreenAddress optAddress world.Game) world
        static member getSelectedScreenAddress world = Option.get <| World.getOptSelectedScreenAddress world
        static member setSelectedScreenAddress address world = World.setOptSelectedScreenAddress (Some address) world
        
        static member getOptSelectedScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.getOptScreen selectedScreenAddress world
            | None -> None

        static member setOptSelectedScreen optScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.setScreen selectedScreenAddress (Option.get optScreen) world
            | None -> failwith "Cannot set a non-existent screen."

        static member getSelectedScreen world = Option.get <| World.getOptSelectedScreen world
        static member setSelectedScreen screen world = World.setOptSelectedScreen (Some screen) world

        static member isAddressSelected address world =
            let optScreenAddress = World.getOptSelectedScreenAddress world
            match (address.Names, Option.map (fun address -> address.Names) optScreenAddress) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

        static member writeGame (writer : XmlWriter) gameHierarchy world =
            let (game : Game, screensHierarchy) = gameHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (game.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology writer game
            writer.WriteStartElement ScreensNodeName
            World.writeScreens writer screensHierarchy world
            writer.WriteEndElement ()

        static member writeGameToFile (filePath : string) gameHierarchy world =
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePath, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GameNodeName
            World.writeGame writer gameHierarchy world
            writer.WriteEndElement ()
            writer.WriteEndElement ()

        static member readGame
            (gameNode : XmlNode)
            defaultDispatcherName
            defaultScreenDispatcherName
            defaultGroupDispatcherName
            defaultEntityDispatcherName
            world =
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName gameNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GameDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName world.Components.GameDispatchers
            let game = Game.make dispatcher None
            Reflection.attachFields game.DispatcherNp game
            Serialization.readPropertiesToTarget gameNode game
            let screensHierarchy =
                World.readScreens
                    (gameNode : XmlNode)
                    defaultScreenDispatcherName
                    defaultGroupDispatcherName
                    defaultEntityDispatcherName
                    world
            (game, screensHierarchy)

        static member readGameFromFile (filePath : string) world =
            use reader = XmlReader.Create (new FileStream (filePath, FileMode.Open) :> Stream)
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            World.readGame
                ((document.SelectSingleNode RootNodeName).SelectSingleNode GameNodeName)
                typeof<GameDispatcher>.Name
                typeof<ScreenDispatcher>.Name
                typeof<GroupDispatcher>.Name
                typeof<EntityDispatcher>.Name
                world