// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldGameModule =

    type Game with
        
        member this.GetId world = (World.getGameState world).Id
        member this.GetCreationTimeStampNp world = (World.getGameState world).CreationTimeStampNp
        member this.GetDispatcherNp world = (World.getGameState world).DispatcherNp
        member this.GetOptSelectedScreen world = (World.getGameState world).OptSelectedScreen
        member this.SetOptSelectedScreen value world = World.updateGameState (fun (gameState : GameState) -> { gameState with OptSelectedScreen = value }) world
        member this.GetPublishChanges world = (World.getGameState world).PublishChanges
        member this.SetPublishChanges value world = World.updateGameState (fun gameState -> { gameState with PublishChanges = value }) world
        member this.GetXtension world = (World.getGameState world).Xtension
        member this.UpdateXtension updater world = World.updateGameState (fun gameState -> { gameState with Xtension = updater gameState.Xtension}) world

        /// Get an xtension property by name.
        member this.GetXProperty name world =
            let xtension = this.GetXtension world
            let xProperty = Xtension.getProperty name xtension
            xProperty.PropertyValue

        /// Query that a game dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member internal registerGame (world : World) : World =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            dispatcher.Register (Simulants.Game, world)
        
        static member internal updateGame world =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            let world = dispatcher.Update (Simulants.Game, world)
            let eventTrace = EventTrace.record "World" "updateGame" EventTrace.empty
            World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () Events.Update eventTrace Simulants.Game world
        
        static member internal actualizeGame world =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            dispatcher.Actualize (Simulants.Game, world)

        static member internal makeGameState dispatcher =
            let gameState = GameState.make dispatcher
            Reflection.attachProperties dispatcher gameState
            gameState

        // Get all the entities in the world.
        static member proxyEntities1 world =
            World.proxyGroups1 world |>
            Seq.map (fun group -> World.proxyEntities group world) |>
            Seq.concat

        // Get all the groups in the world.
        static member proxyGroups1 world =
            World.proxyScreens world |>
            Seq.map (fun screen -> World.proxyGroups screen world) |>
            Seq.concat

        /// Try to get the currently selected screen.
        static member getOptSelectedScreen world =
            Simulants.Game.GetOptSelectedScreen world

        /// Set the currently selected screen or None. Be careful using this function directly as
        //// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member setOptSelectedScreen optScreen world =
            Simulants.Game.SetOptSelectedScreen optScreen world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        static member getSelectedScreen world =
            Option.get ^ World.getOptSelectedScreen world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreen screen world =
            World.setOptSelectedScreen (Some screen) world

        /// Determine if an entity is selected by being in a group of the currently selected screeen.
        static member isEntitySelected entity world =
            let screenName = Address.head entity.EntityAddress
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> screenName = selectedScreen.ScreenName
            | None -> false

        /// Determine if a group is selected by being in the currently selected screeen.
        static member isGroupSelected group world =
            let screenName = Address.head group.GroupAddress
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> screenName = selectedScreen.ScreenName
            | None -> false

        /// Determine if a screen is the currently selected screeen.
        static member isScreenSelected screen world =
            World.getOptSelectedScreen world = Some screen

        /// Determine if a simulant is contained by, or is the same as, the currently selected screen.
        /// Game is always considered 'selected' as well.
        static member isSimulantSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [] -> true
            | screenName :: _ ->
                match World.getOptSelectedScreen world with
                | Some screen -> screen.ScreenName = screenName
                | None -> false

        /// Write a game to a game descriptor.
        static member writeGame gameDescriptor world =
            let gameState = World.getGameState world
            let gameDispatcherName = getTypeName gameState.DispatcherNp
            let gameDescriptor = { gameDescriptor with GameDispatcher = gameDispatcherName }
            let gameProperties = Reflection.writeMemberValuesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = gameProperties }
            let screens = World.proxyScreens world
            World.writeScreens screens gameDescriptor world

        /// Write a game to a file.
        static member writeGameToFile (filePath : string) world =
            let filePathTmp = filePath + ".tmp"
            let gameDescriptor = World.writeGame GameDescriptor.empty world
            let gameDescriptorStr = scstring gameDescriptor
            let gameDescriptorPretty = SymbolIndex.prettyPrint gameDescriptorStr
            File.WriteAllText (filePathTmp, gameDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from a game descriptor.
        static member readGame gameDescriptor world =

            // create the dispatcher
            let dispatcherName = gameDescriptor.GameDispatcher
            let dispatchers = World.getGameDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName dispatchers
            
            // make the bare game state
            let gameState = World.makeGameState dispatcher

            // read the game state's value
            Reflection.readMemberValuesToTarget gameDescriptor.GameProperties gameState

            // set the game's state in the world
            let world = World.setGameState gameState world
            
            // read the game's screens
            World.readScreens gameDescriptor world |> snd

        /// Read a game from a file.
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world

namespace Debug
open Prime
open Nu
open System.Reflection
open System.Collections.Generic
type Game =

    /// Provides a view of all the built-in properties of a game. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewProperties world =
        let state = World.getGameState world
        let properties = Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state)) ((state.GetType ()).GetProperties ())
        Map.ofSeq properties
        
    /// Provides a view of all the xtension properties of a game. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXProperties world =
        let state = World.getGameState world
        let properties = Map.ofSeq ^ Xtension.toSeq state.Xtension
        Map.map (fun _ property -> property.PropertyValue) properties

    /// Provides a full view of all the member values of a game. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view world =
        Game.viewProperties world @@ Game.viewXProperties world