// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldGameModule =

    type Game with

        member this.GetDispatcher world = World.getGameDispatcher world
        member this.Dispatcher = lensReadOnly (nameof this.Dispatcher) this this.GetDispatcher
        member this.GetModelGeneric<'a> world = World.getGameModel<'a> world
        member this.SetModelGeneric<'a> value world = World.setGameModel<'a> false value world |> snd'
        member this.ModelGeneric<'a> () = lens Constants.Engine.ModelPropertyName this this.GetModelGeneric<'a> this.SetModelGeneric<'a> // TODO: make this string a constant.
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOptPlus value world |> snd'
        member this.OmniScreenOpt = lens (nameof this.OmniScreenOpt) this this.GetOmniScreenOpt this.SetOmniScreenOpt
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SelectedScreenOpt = lensReadOnly (nameof this.SelectedScreenOpt) this this.GetSelectedScreenOpt
        member this.GetDesiredScreen world = World.getDesiredScreen world
        member this.SetDesiredScreen value world = World.setDesiredScreenPlus value world |> snd'
        member this.DesiredScreen = lens (nameof this.DesiredScreen) this this.GetDesiredScreen this.SetDesiredScreen
        member this.GetEyeCenter2d world = World.getEyeCenter2d world
        member this.SetEyeCenter2d value world = World.setEyeCenter2dPlus value world |> snd'
        member this.EyeCenter2d = lens (nameof this.EyeCenter2d) this this.GetEyeCenter2d this.SetEyeCenter2d
        member this.GetEyeSize2d world = World.getEyeSize2d world
        member this.SetEyeSize2d value world = World.setEyeSize2dPlus value world |> snd'
        member this.EyeSize2d = lens (nameof this.EyeSize2d) this this.GetEyeSize2d this.SetEyeSize2d
        member this.GetEyeCenter3d world = World.getEyeCenter3d world
        member this.SetEyeCenter3d value world = World.setEyeCenter3dPlus value world |> snd'
        member this.EyeCenter3d = lens (nameof this.EyeCenter3d) this this.GetEyeCenter3d this.SetEyeCenter3d
        member this.GetEyeRotation3d world = World.getEyeRotation3d world
        member this.SetEyeRotation3d value world = World.setEyeRotation3dPlus value world |> snd'
        member this.EyeRotation3d = lens (nameof this.EyeRotation3d) this this.GetEyeRotation3d this.SetEyeRotation3d
        member this.GetEyeFrustum3dEnclosed world = World.getEyeFrustum3dEnclosed world
        member this.EyeFrustum3dEnclosed = lensReadOnly (nameof this.EyeFrustum3dEnclosed) this this.GetEyeFrustum3dEnclosed
        member this.GetEyeFrustum3dExposed world = World.getEyeFrustum3dExposed world
        member this.EyeFrustum3dExposed = lensReadOnly (nameof this.EyeFrustum3dExposed) this this.GetEyeFrustum3dExposed
        member this.GetEyeFrustum3dImposter world = World.getEyeFrustum3dImposter world
        member this.EyeFrustum3dImposter = lensReadOnly (nameof this.EyeFrustum3dImposter) this this.GetEyeFrustum3dImposter
        member this.GetScriptFrame world = World.getGameScriptFrame world
        member this.ScriptFrame = lensReadOnly (nameof this.ScriptFrame) this this.GetScriptFrame
        member this.GetOrder world = World.getGameOrder world
        member this.Order = lensReadOnly (nameof this.Order) this this.GetOrder
        member this.GetId world = World.getGameId world
        member this.Id = lensReadOnly (nameof this.Id) this this.GetId

        member this.RegisterEvent = Events.Register
        member this.UnregisteringEvent = Events.Unregistering
        member this.ChangeEvent propertyName = Events.Change propertyName
        member this.PreUpdateEvent = Events.PreUpdate
        member this.UpdateEvent = Events.Update
        member this.PostUpdateEvent = Events.PostUpdate
        member this.RenderEvent = Events.Render
        member this.MouseMoveEvent = Events.MouseMove
        member this.MouseDragEvent = Events.MouseDrag
        member this.MouseLeftChangeEvent = Events.MouseLeftChange
        member this.MouseLeftDownEvent = Events.MouseLeftDown
        member this.MouseLeftUpEvent = Events.MouseLeftUp
        member this.MouseMiddleChangeEvent = Events.MouseMiddleChange
        member this.MouseMiddleDownEvent = Events.MouseMiddleDown
        member this.MouseMiddleUpEvent = Events.MouseMiddleUp
        member this.MouseRightChangeEvent = Events.MouseRightChange
        member this.MouseRightDownEvent = Events.MouseRightDown
        member this.MouseRightUpEvent = Events.MouseRightUp
        member this.MouseX1ChangeEvent = Events.MouseX1Change
        member this.MouseX1DownEvent = Events.MouseX1Down
        member this.MouseX1UpEvent = Events.MouseX1Up
        member this.MouseX2ChangeEvent = Events.MouseX2Change
        member this.MouseX2DownEvent = Events.MouseX2Down
        member this.MouseX2UpEvent = Events.MouseX2Up
        member this.KeyboardKeyChangeEvent = Events.KeyboardKeyChange
        member this.KeyboardKeyDownEvent = Events.KeyboardKeyDown
        member this.KeyboardKeyUpEvent = Events.KeyboardKeyUp
        member this.GamepadDirectionChangeEvent index = Events.GamepadDirectionChange index
        member this.GamepadButtonChangeEvent index = Events.GamepadButtonChange index
        member this.GamepadButtonDownEvent index = Events.GamepadButtonDown index
        member this.GamepadButtonUpEvent index = Events.GamepadButtonUp index
        member this.AssetsReloadEvent = Events.AssetsReload
        member this.BodyAddingEvent = Events.BodyAdding
        member this.BodyRemovingEvent = Events.BodyRemoving

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetGameProperty (propertyName, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getGameProperty propertyName world

        /// Get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a =
            World.tryGetGameXtensionValue<'a> propertyName world

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getGameXtensionValue<'a> propertyName world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetGameProperty propertyName property world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setGameProperty propertyName property world |> snd'

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetGameXtensionProperty propertyName property world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.setGameXtensionProperty propertyName property world

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Get a game's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName

        /// Send a signal to a game.
        member this.Signal<'message, 'command> (signal : Signal) world =
            (this.GetDispatcher world).Signal (signal, this, world)

    type World with

        static member internal registerGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Register (game, world)
            let eventTrace = EventTrace.debug "World" "registerGame" "Register" EventTrace.empty
            let world = World.publishPlus () Events.Register eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            let world = World.publishPlus () Events.Unregistering eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "unregisteringGame" "" EventTrace.empty
            let world = World.publishPlus (UnregisteringData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world
            dispatcher.Unregister (game, world)

        static member internal preUpdateGame world =
                
            // pre-update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PreUpdate (game, world)

            // publish pre-update event
            let eventTrace = EventTrace.debug "World" "preUpdateGame" "" EventTrace.empty
            World.publishPlus () Events.PreUpdate eventTrace game false false world

        static member internal updateGame world =

            // update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Update (game, world)

            // publish update event
            let eventTrace = EventTrace.debug "World" "updateGame" "" EventTrace.empty
            World.publishPlus () Events.Update eventTrace game false false world

        static member internal postUpdateGame world =
                
            // post-update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PostUpdate (game, world)

            // publish post-update event
            let eventTrace = EventTrace.debug "World" "postUpdateGame" "" EventTrace.empty
            World.publishPlus () Events.PostUpdate eventTrace game false false world

        static member internal renderGame world =

            // render via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Render (game, world)

            // publish render event
            let eventTrace = EventTrace.debug "World" "renderGame" "" EventTrace.empty
            World.publishPlus () Events.Render eventTrace game false false world

        /// Edit a game with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member editGame operation (game : Game) world =
            let dispatcher = game.GetDispatcher world
            dispatcher.Edit (operation, game, world)

        /// Get all the entities in the world.
        [<FunctionBinding "getEntities0">]
        static member getEntities1 world =
            World.getGroups1 world |>
            Seq.map (fun group -> World.getEntitiesFlattened group world) |>
            Seq.concat

        /// Get all the groups in the world.
        [<FunctionBinding "getGroups0">]
        static member getGroups1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getGroups screen world) |>
            Seq.concat

        /// Write a game to a game descriptor.
        static member writeGame gameDescriptor world =
            let gameState = World.getGameState world
            let gameDispatcherName = getTypeName gameState.Dispatcher
            let gameDescriptor = { gameDescriptor with GameDispatcherName = gameDispatcherName }
            let gameProperties = Reflection.writePropertiesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = gameProperties }
            let screens = World.getScreens world
            { gameDescriptor with ScreenDescriptors = World.writeScreens screens world }

        /// Write a game to a file.
        [<FunctionBinding>]
        static member writeGameToFile (filePath : string) world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<GameDescriptor>).PrettyPrinter
            let gameDescriptor = World.writeGame GameDescriptor.empty world
            let gameDescriptorStr = scstring gameDescriptor
            let gameDescriptorPretty = PrettyPrinter.prettyPrint gameDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, gameDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from a game descriptor.
        static member readGame gameDescriptor world =

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
            let world = World.setGameState gameState world

            // read the game's screens
            World.readScreens gameDescriptor.ScreenDescriptors world |> snd

        /// Read a game from a file.
        [<FunctionBinding>]
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world