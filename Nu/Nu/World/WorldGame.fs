// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldGameModule =

    type Game with

        member this.GetDispatcher world = World.getGameDispatcher world
        member this.Dispatcher = lensReadOnly (nameof this.Dispatcher) this.GetDispatcher this
        member this.GetModelGeneric<'a> world = World.getGameModel<'a> world
        member this.SetModelGeneric<'a> value world = World.setGameModel<'a> value world |> snd'
        member this.ModelGeneric<'a> () = lens "Model" this.GetModelGeneric<'a> this.SetModelGeneric<'a> this
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOptPlus value world |> snd'
        member this.OmniScreenOpt = lens (nameof this.OmniScreenOpt) this.GetOmniScreenOpt this.SetOmniScreenOpt this
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SelectedScreenOpt = lensReadOnly (nameof this.SelectedScreenOpt) this.GetSelectedScreenOpt this
        member this.GetDesiredScreenOpt world = World.getDesiredScreenOpt world
        member this.SetDesiredScreenOpt value world = World.setDesiredScreenOpt value world |> snd'
        member this.DesiredScreenOpt = lens (nameof this.DesiredScreenOpt) this.GetDesiredScreenOpt this.SetDesiredScreenOpt this
        member this.GetEyePosition2d world = World.getEyePosition2d world
        member this.SetEyePosition2d value world = World.setEyePosition2dPlus value world |> snd'
        member this.EyePosition2d = lens (nameof this.EyePosition2d) this.GetEyePosition2d this.SetEyePosition2d this
        member this.GetEyeSize2d world = World.getEyeSize2d world
        member this.SetEyeSize2d value world = World.setEyeSize2dPlus value world |> snd'
        member this.EyeSize2d = lens (nameof this.EyeSize2d) this.GetEyeSize2d this.SetEyeSize2d this
        member this.GetEyePosition3d world = World.getEyePosition3d world
        member this.SetEyePosition3d value world = World.setEyePosition3dPlus value world |> snd'
        member this.EyePosition3d = lens (nameof this.EyePosition3d) this.GetEyePosition3d this.SetEyePosition3d this
        member this.GetEyeRotation3d world = World.getEyeRotation3d world
        member this.SetEyeRotation3d value world = World.setEyeRotation3dPlus value world |> snd'
        member this.EyeRotation3d = lens (nameof this.EyeRotation3d) this.GetEyeRotation3d this.SetEyeRotation3d this
        member this.GetEyeFrustum3dEnclosed world = World.getEyeFrustum3dEnclosed world
        member this.EyeFrustum3dEnclosed = lensReadOnly (nameof this.EyeFrustum3dEnclosed) this.GetEyeFrustum3dEnclosed this
        member this.GetEyeFrustum3dExposed world = World.getEyeFrustum3dExposed world
        member this.EyeFrustum3dExposed = lensReadOnly (nameof this.EyeFrustum3dExposed) this.GetEyeFrustum3dExposed this
        member this.GetEyeFrustum3dImposter world = World.getEyeFrustum3dImposter world
        member this.EyeFrustum3dImposter = lensReadOnly (nameof this.EyeFrustum3dImposter) this.GetEyeFrustum3dImposter this
        member this.GetScriptFrame world = World.getGameScriptFrame world
        member this.ScriptFrame = lensReadOnly (nameof this.ScriptFrame) this.GetScriptFrame this
        member this.GetOrder world = World.getGameOrder world
        member this.Order = lensReadOnly (nameof this.Order) this.GetOrder this
        member this.GetId world = World.getGameId world
        member this.Id = lensReadOnly (nameof this.Id) this.GetId this

        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this
        member this.RenderEvent = Events.Render --> this
        member this.MouseMoveEvent = Events.MouseMove --> this
        member this.MouseDragEvent = Events.MouseDrag --> this
        member this.MouseLeftChangeEvent = Events.MouseLeftChange --> this
        member this.MouseLeftDownEvent = Events.MouseLeftDown --> this
        member this.MouseLeftUpEvent = Events.MouseLeftUp --> this
        member this.MouseCenterChangeEvent = Events.MouseCenterChange --> this
        member this.MouseCenterDownEvent = Events.MouseCenterDown --> this
        member this.MouseCenterUpEvent = Events.MouseCenterUp --> this
        member this.MouseRightChangeEvent = Events.MouseRightChange --> this
        member this.MouseRightDownEvent = Events.MouseRightDown --> this
        member this.MouseRightUpEvent = Events.MouseRightUp --> this
        member this.MouseX1ChangeEvent = Events.MouseX1Change --> this
        member this.MouseX1DownEvent = Events.MouseX1Down --> this
        member this.MouseX1UpEvent = Events.MouseX1Up --> this
        member this.MouseX2ChangeEvent = Events.MouseX2Change --> this
        member this.MouseX2DownEvent = Events.MouseX2Down --> this
        member this.MouseX2UpEvent = Events.MouseX2Up --> this
        member this.KeyboardKeyChangeEvent = Events.KeyboardKeyChange --> this
        member this.KeyboardKeyDownEvent = Events.KeyboardKeyDown --> this
        member this.KeyboardKeyUpEvent = Events.KeyboardKeyUp --> this
        member this.GamepadDirectionChangeEvent index = Events.GamepadDirectionChange index --> this
        member this.GamepadButtonChangeEvent index = Events.GamepadButtonChange index --> this
        member this.GamepadButtonDownEvent index = Events.GamepadButtonDown index --> this
        member this.GamepadButtonUpEvent index = Events.GamepadButtonUp index --> this
        member this.AssetsReloadEvent = Events.AssetsReload --> this
        member this.BodyAddingEvent = Events.BodyAdding --> this
        member this.BodyRemovingEvent = Events.BodyRemoving --> this

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
            let mutable property = Unchecked.defaultof<Property>
            if World.tryGetGameXtensionProperty (propertyName, world, &property)
            then property.PropertyValue :?> 'a
            else Unchecked.defaultof<'a>

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
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.GameAddress

        /// Try to signal a game.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal registerGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Register (game, world)
            let eventTrace = EventTrace.debug "World" "registerGame" "Register" EventTrace.empty
            let world = World.publishPlus () Events.Register eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            let world = World.publishPlus (RegisterData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world
            World.choose world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            let world = World.publishPlus () Events.Unregistering eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "unregisteringGame" "" EventTrace.empty
            let world = World.publishPlus (UnregisteringData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world
            let world = dispatcher.Unregister (game, world)
            World.choose world

        static member internal updateGame world =

            // update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Update (game, world)

            // publish update event
            let eventTrace = EventTrace.debug "World" "updateGame" "" EventTrace.empty
            let world = World.publishPlus () Events.Update eventTrace game false false world
            World.choose world

        static member internal postUpdateGame world =
                
            // post-update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PostUpdate (game, world)

            // publish post-update event
            let eventTrace = EventTrace.debug "World" "postUpdateGame" "" EventTrace.empty
            let world = World.publishPlus () Events.PostUpdate eventTrace game false false world
            World.choose world

        static member internal renderGame world =

            // render via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Render (game, world)

            // publish render event
            let eventTrace = EventTrace.debug "World" "renderGame" "" EventTrace.empty
            let world = World.publishPlus () Events.Render eventTrace game false false world
            World.choose world

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
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
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
            let world = World.readScreens gameDescriptor.ScreenDescriptors world |> snd

            // choose the world
            World.choose world

        /// Read a game from a file.
        [<FunctionBinding>]
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world

namespace Debug
open Nu
type Game =

    /// Provides a full view of all the properties of a game. Useful for debugging such as with the
    /// Watch feature in Visual Studio.
    static member view world = World.viewGameProperties world