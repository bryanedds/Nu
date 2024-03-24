// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.IO
open Prime

[<AutoOpen>]
module WorldGameModule =

    type Game with

        member this.GetDispatcher world = World.getGameDispatcher this world
        member this.Dispatcher = lensReadOnly (nameof this.Dispatcher) this this.GetDispatcher
        member this.GetModelGeneric<'a> world = World.getGameModel<'a> this world
        member this.SetModelGeneric<'a> value world = World.setGameModel<'a> false value this world |> snd'
        member this.ModelGeneric<'a> () = lens Constants.Engine.ModelPropertyName this this.GetModelGeneric<'a> this.SetModelGeneric<'a>
        member this.GetOmniScreenOpt world = World.getGameOmniScreenOpt this world
        member this.SetOmniScreenOpt value world = World.setGameOmniScreenOpt value this world |> snd'
        member this.OmniScreenOpt = lens (nameof this.OmniScreenOpt) this this.GetOmniScreenOpt this.SetOmniScreenOpt
        member this.GetSelectedScreenOpt world = World.getGameSelectedScreenOpt this world
        member this.SelectedScreenOpt = lensReadOnly (nameof this.SelectedScreenOpt) this this.GetSelectedScreenOpt
        member this.GetDesiredScreen world = World.getGameDesiredScreen this world
        member this.SetDesiredScreen value world = World.setGameDesiredScreen value this world |> snd'
        member this.DesiredScreen = lens (nameof this.DesiredScreen) this this.GetDesiredScreen this.SetDesiredScreen
        member this.GetScreenTransitionDestinationOpt world = World.getGameScreenTransitionDestinationOpt this world
        member this.SetScreenTransitionDestinationOpt value world = World.setGameScreenTransitionDestinationOpt value this world |> snd'
        member this.ScreenTransitionDestinationOpt = lens (nameof this.ScreenTransitionDestinationOpt) this this.GetScreenTransitionDestinationOpt this.SetScreenTransitionDestinationOpt
        member this.GetEye2dCenter world = World.getGameEye2dCenter this world
        member this.SetEye2dCenter value world = World.setGameEye2dCenter value this world |> snd'
        member this.Eye2dCenter = lens (nameof this.Eye2dCenter) this this.GetEye2dCenter this.SetEye2dCenter
        member this.GetEye2dSize world = World.getGameEye2dSize this world
        member this.SetEye2dSize value world = World.setGameEye2dSize value this world |> snd'
        member this.Eye2dSize = lens (nameof this.Eye2dSize) this this.GetEye2dSize this.SetEye2dSize
        member this.GetEye3dCenter world = World.getGameEye3dCenter this world
        member this.SetEye3dCenter value world = World.setGameEye3dCenter value this world |> snd'
        member this.Eye3dCenter = lens (nameof this.Eye3dCenter) this this.GetEye3dCenter this.SetEye3dCenter
        member this.GetEye3dRotation world = World.getGameEye3dRotation this world
        member this.SetEye3dRotation value world = World.setGameEye3dRotation value this world |> snd'
        member this.Eye3dRotation = lens (nameof this.Eye3dRotation) this this.GetEye3dRotation this.SetEye3dRotation
        member this.GetOrder world = World.getGameOrder this world
        member this.Order = lensReadOnly (nameof this.Order) this this.GetOrder
        member this.GetId world = World.getGameId this world
        member this.Id = lensReadOnly (nameof this.Id) this this.GetId

        member this.RegisterEvent = Events.RegisterEvent --> Game.Handle
        member this.UnregisteringEvent = Events.UnregisteringEvent --> Game.Handle
        member this.ChangeEvent propertyName = Events.ChangeEvent propertyName --> Game.Handle
        member this.LifeCycleEvent simulantTypeName = Events.LifeCycleEvent simulantTypeName --> Game.Handle
        member this.IntegrationEvent = Events.IntegrationEvent --> Game.Handle
        member this.PreUpdateEvent = Events.PreUpdateEvent --> Game.Handle
        member this.UpdateEvent = Events.UpdateEvent --> Game.Handle
        member this.PostUpdateEvent = Events.PostUpdateEvent --> Game.Handle
        member this.TimeUpdateEvent = Events.TimeUpdateEvent --> Game.Handle
        member this.MouseMoveEvent = Events.MouseMoveEvent --> Game.Handle
        member this.MouseDragEvent = Events.MouseDragEvent --> Game.Handle
        member this.MouseWheelEvent = Events.MouseWheelEvent --> Game.Handle
        member this.MouseLeftChangeEvent = Events.MouseLeftChangeEvent --> Game.Handle
        member this.MouseLeftDownEvent = Events.MouseLeftDownEvent --> Game.Handle
        member this.MouseLeftUpEvent = Events.MouseLeftUpEvent --> Game.Handle
        member this.MouseMiddleChangeEvent = Events.MouseMiddleChangeEvent --> Game.Handle
        member this.MouseMiddleDownEvent = Events.MouseMiddleDownEvent --> Game.Handle
        member this.MouseMiddleUpEvent = Events.MouseMiddleUpEvent --> Game.Handle
        member this.MouseRightChangeEvent = Events.MouseRightChangeEvent --> Game.Handle
        member this.MouseRightDownEvent = Events.MouseRightDownEvent --> Game.Handle
        member this.MouseRightUpEvent = Events.MouseRightUpEvent --> Game.Handle
        member this.MouseX1ChangeEvent = Events.MouseX1ChangeEvent --> Game.Handle
        member this.MouseX1DownEvent = Events.MouseX1DownEvent --> Game.Handle
        member this.MouseX1UpEvent = Events.MouseX1UpEvent --> Game.Handle
        member this.MouseX2ChangeEvent = Events.MouseX2ChangeEvent --> Game.Handle
        member this.MouseX2DownEvent = Events.MouseX2DownEvent --> Game.Handle
        member this.MouseX2UpEvent = Events.MouseX2UpEvent --> Game.Handle
        member this.KeyboardKeyChangeEvent = Events.KeyboardKeyChangeEvent --> Game.Handle
        member this.KeyboardKeyDownEvent = Events.KeyboardKeyDownEvent --> Game.Handle
        member this.KeyboardKeyUpEvent = Events.KeyboardKeyUpEvent --> Game.Handle
        member this.GamepadDirectionChangeEvent index = Events.GamepadDirectionChangeEvent index --> Game.Handle
        member this.GamepadButtonChangeEvent index = Events.GamepadButtonChangeEvent index --> Game.Handle
        member this.GamepadButtonDownEvent index = Events.GamepadButtonDownEvent index --> Game.Handle
        member this.GamepadButtonUpEvent index = Events.GamepadButtonUpEvent index --> Game.Handle
        member this.TextInputEvent = Events.TextInputEvent --> Game.Handle
        member this.AssetsReloadEvent = Events.AssetsReloadEvent --> Game.Handle
        member this.BodyAddingEvent = Events.BodyAddingEvent --> Game.Handle
        member this.BodyRemovingEvent = Events.BodyRemovingEvent --> Game.Handle
        member this.BodySeparationImplicitEvent = Events.BodySeparationImplicitEvent --> Game.Handle

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetGameProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getGameProperty propertyName this world

        /// Get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a =
            World.tryGetGameXtensionValue<'a> propertyName this world

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getGameXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetGameProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setGameProperty propertyName property this world |> snd'

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetGameXtensionProperty propertyName property this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.setGameXtensionProperty propertyName property this world

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Get a game's change event address.
        member this.GetChangeEvent propertyName = Game.Handle.ChangeEvent propertyName

        /// Send a signal to a game.
        member this.Signal<'message, 'command> (signal : Signal) world =
            (this.GetDispatcher world).Signal (signal, this, world)

    type World with

        static member internal registerGame (game : Game) world =
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Register (game, world)
            let eventTrace = EventTrace.debug "World" "registerGame" "Register" EventTrace.empty
            let world = World.publishPlus () game.RegisterEvent eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData game) (game.LifeCycleEvent (nameof Game)) eventTrace game true false world

        static member internal unregisterGame (game : Game) world =
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            let world = World.publishPlus () game.UnregisteringEvent eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "unregisteringGame" "" EventTrace.empty
            let world = World.publishPlus (UnregisteringData game) (game.LifeCycleEvent (nameof Game)) eventTrace game true false world
            dispatcher.Unregister (game, world)

        static member internal preUpdateGame (game : Game) world =
                
            // pre-update via dispatcher
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PreUpdate (game, world)

            // publish pre-update event
            let eventTrace = EventTrace.debug "World" "preUpdateGame" "" EventTrace.empty
            World.publishPlus () game.PreUpdateEvent eventTrace game false false world

        static member internal updateGame (game : Game) world =

            // update via dispatcher
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Update (game, world)

            // publish update event
            let eventTrace = EventTrace.debug "World" "updateGame" "" EventTrace.empty
            World.publishPlus () game.UpdateEvent eventTrace game false false world

        static member internal postUpdateGame (game : Game) world =
                
            // post-update via dispatcher
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PostUpdate (game, world)

            // publish post-update event
            let eventTrace = EventTrace.debug "World" "postUpdateGame" "" EventTrace.empty
            World.publishPlus () game.PostUpdateEvent eventTrace game false false world

        static member internal renderGame renderPass (game : Game) world =
            let dispatcher = game.GetDispatcher world
            dispatcher.Render (renderPass, game, world)

        /// Edit a game with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member editGame operation (game : Game) world =
            let dispatcher = game.GetDispatcher world
            dispatcher.Edit (operation, game, world)

        /// Attempt to truncate a game model.
        static member tryTruncateGameModel<'model> (model : 'model) (game : Game) world =
            let dispatcher = game.GetDispatcher world
            dispatcher.TryTruncateModel model

        /// Attempt to untruncate a game model.
        static member tryUntruncateGameModel<'model> (model : 'model) (game : Game) world =
            let dispatcher = game.GetDispatcher world
            dispatcher.TryUntruncateModel (model, game, world)

        /// Get all the entities in the world.
        static member getEntities1 world =
            World.getGroups1 world |>
            Seq.map (fun group -> World.getEntities group world) |>
            Seq.concat

        /// Get all the groups in the world.
        static member getGroups1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getGroups screen world) |>
            Seq.concat

        /// Write a game to a game descriptor.
        static member writeGame writePropagationHistory gameDescriptor game world =
            let gameState = World.getGameState game world
            let gameDispatcherName = getTypeName gameState.Dispatcher
            let gameDescriptor = { gameDescriptor with GameDispatcherName = gameDispatcherName }
            let gameProperties = Reflection.writePropertiesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = gameProperties }
            let screens = World.getScreens world
            { gameDescriptor with ScreenDescriptors = World.writeScreens writePropagationHistory screens world }

        /// Write a game to a file.
        static member writeGameToFile writePropagationHistory (filePath : string) game world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<GameDescriptor>).PrettyPrinter
            let gameDescriptor = World.writeGame writePropagationHistory GameDescriptor.empty game world
            let gameDescriptorStr = scstring gameDescriptor
            let gameDescriptorPretty = PrettyPrinter.prettyPrint gameDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, gameDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from a game descriptor.
        static member readGame gameDescriptor name world =

            // make the dispatcher
            let dispatcherName = gameDescriptor.GameDispatcherName
            let dispatchers = World.getGameDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find a GameDispatcher named '" + dispatcherName + "'.")

            // make the game state and populate its properties
            let gameState = GameState.make dispatcher
            let gameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState world
            let gameState = Reflection.readPropertiesToTarget GameState.copy gameDescriptor.GameProperties gameState

            // set the game's state in the world
            let game = Game name
            let world = World.setGameState gameState game world

            // read the game's screens
            let world = World.readScreens gameDescriptor.ScreenDescriptors world |> snd
            (game, world)

        /// Read a game from a file.
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world