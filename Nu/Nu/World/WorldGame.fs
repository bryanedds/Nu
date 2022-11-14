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
        static member Dispatcher = lensReadOnly (nameof Game.Dispatcher) (fun (this : Game) -> this.GetDispatcher)
        member this.GetModelGeneric<'model> world = World.getGameModel<'model> world
        member this.SetModelGeneric<'model> value world = World.setGameModel<'model> value world |> snd'
        static member ModelGeneric<'model> () = lens "Model" (fun (this : Game) -> this.GetModelGeneric<'model>) (fun value this -> this.SetModelGeneric<'model> value)
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOptPlus value world |> snd'
        static member OmniScreenOpt = lens (nameof Game.OmniScreenOpt) (fun (this : Game) -> this.GetOmniScreenOpt) (fun value this -> this.SetOmniScreenOpt value)
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        static member SelectedScreenOpt = lensReadOnly (nameof Game.SelectedScreenOpt) (fun (this : Game) -> this.GetSelectedScreenOpt)
        member this.GetDesiredScreen world = World.getDesiredScreen world
        member this.SetDesiredScreen value world = World.setDesiredScreen value world |> snd'
        static member DesiredScreen = lens (nameof Game.DesiredScreen) (fun (this : Game) -> this.GetDesiredScreen) (fun value this -> this.SetDesiredScreen value)
        member this.GetEyePosition2d world = World.getEyePosition2d world
        member this.SetEyePosition2d value world = World.setEyePosition2dPlus value world |> snd'
        static member EyePosition2d = lens (nameof Game.EyePosition2d) (fun (this : Game) -> this.GetEyePosition2d) (fun value this -> this.SetEyePosition2d value)
        member this.GetEyeSize2d world = World.getEyeSize2d world
        member this.SetEyeSize2d value world = World.setEyeSize2dPlus value world |> snd'
        static member EyeSize2d = lens (nameof Game.EyeSize2d) (fun (this : Game) -> this.GetEyeSize2d) (fun value this -> this.SetEyeSize2d value)
        member this.GetEyePosition3d world = World.getEyePosition3d world
        member this.SetEyePosition3d value world = World.setEyePosition3dPlus value world |> snd'
        static member EyePosition3d = lens (nameof Game.EyePosition3d) (fun (this : Game) -> this.GetEyePosition3d) (fun value this -> this.SetEyePosition3d value)
        member this.GetEyeRotation3d world = World.getEyeRotation3d world
        member this.SetEyeRotation3d value world = World.setEyeRotation3dPlus value world |> snd'
        static member EyeRotation3d = lens (nameof Game.EyeRotation3d) (fun (this : Game) -> this.GetEyeRotation3d) (fun value this -> this.SetEyeRotation3d value)
        member this.GetEyeFrustum3dEnclosed world = World.getEyeFrustum3dEnclosed world
        static member EyeFrustum3dEnclosed = lensReadOnly (nameof Game.EyeFrustum3dEnclosed) (fun (this : Game) -> this.GetEyeFrustum3dEnclosed)
        member this.GetEyeFrustum3dExposed world = World.getEyeFrustum3dExposed world
        static member EyeFrustum3dExposed = lensReadOnly (nameof Game.EyeFrustum3dExposed) (fun (this : Game) -> this.GetEyeFrustum3dExposed)
        member this.GetEyeFrustum3dImposter world = World.getEyeFrustum3dImposter world
        static member EyeFrustum3dImposter = lensReadOnly (nameof Game.EyeFrustum3dImposter) (fun (this : Game) -> this.GetEyeFrustum3dImposter)
        member this.GetScriptFrame world = World.getGameScriptFrame world
        static member ScriptFrame = lensReadOnly (nameof Game.ScriptFrame) (fun (this : Game) -> this.GetScriptFrame)
        member this.GetOrder world = World.getGameOrder world
        static member Order = lensReadOnly (nameof Game.Order) (fun (this : Game) -> this.GetOrder)
        member this.GetId world = World.getGameId world
        static member Id = lensReadOnly (nameof Game.Id) (fun (this : Game) -> this.GetId)

        member this.RegisterEvent = Events.Register
        member this.UnregisteringEvent = Events.Unregistering
        member this.ChangeEvent propertyName = Events.Change propertyName
        member this.UpdateEvent = Events.Update
        member this.PostUpdateEvent = Events.PostUpdate
        member this.RenderEvent = Events.Render
        member this.MouseMoveEvent = Events.MouseMove
        member this.MouseDragEvent = Events.MouseDrag
        member this.MouseLeftChangeEvent = Events.MouseLeftChange
        member this.MouseLeftDownEvent = Events.MouseLeftDown
        member this.MouseLeftUpEvent = Events.MouseLeftUp
        member this.MouseCenterChangeEvent = Events.MouseCenterChange
        member this.MouseCenterDownEvent = Events.MouseCenterDown
        member this.MouseCenterUpEvent = Events.MouseCenterUp
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
        member this.GetChangeEvent propertyName = Events.Change propertyName

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
            World.publishPlus (RegisterData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.debug "World" "registerGame" "LifeCycle" EventTrace.empty
            let world = World.publishPlus () Events.Unregistering eventTrace game true false world
            let eventTrace = EventTrace.debug "World" "unregisteringGame" "" EventTrace.empty
            let world = World.publishPlus (UnregisteringData game) (Events.LifeCycle (nameof Game)) eventTrace game true false world
            dispatcher.Unregister (game, world)

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

    [<RequireQualifiedAccess>]
    module Game =

        let RegisterEvent = Events.Register
        let UnregisteringEvent = Events.Unregistering
        let ChangeEvent propertyName = (Events.Change propertyName)
        let UpdateEvent = Events.Update
        let PostUpdateEvent = Events.PostUpdate
        let RenderEvent = Events.Render
        let MouseMoveEvent = Events.MouseMove
        let MouseDragEvent = Events.MouseDrag
        let MouseLeftChangeEvent = Events.MouseLeftChange
        let MouseLeftDownEvent = Events.MouseLeftDown
        let MouseLeftUpEvent = Events.MouseLeftUp
        let MouseCenterChangeEvent = Events.MouseCenterChange
        let MouseCenterDownEvent = Events.MouseCenterDown
        let MouseCenterUpEvent = Events.MouseCenterUp
        let MouseRightChangeEvent = Events.MouseRightChange
        let MouseRightDownEvent = Events.MouseRightDown
        let MouseRightUpEvent = Events.MouseRightUp
        let MouseX1ChangeEvent = Events.MouseX1Change
        let MouseX1DownEvent = Events.MouseX1Down
        let MouseX1UpEvent = Events.MouseX1Up
        let MouseX2ChangeEvent = Events.MouseX2Change
        let MouseX2DownEvent = Events.MouseX2Down
        let MouseX2UpEvent = Events.MouseX2Up
        let KeyboardKeyChangeEvent = Events.KeyboardKeyChange
        let KeyboardKeyDownEvent = Events.KeyboardKeyDown
        let KeyboardKeyUpEvent = Events.KeyboardKeyUp
        let GamepadDirectionChangeEvent index = (Events.GamepadDirectionChange index)
        let GamepadButtonChangeEvent index = (Events.GamepadButtonChange index)
        let GamepadButtonDownEvent index = (Events.GamepadButtonDown index)
        let GamepadButtonUpEvent index = (Events.GamepadButtonUp index)
        let AssetsReloadEvent = Events.AssetsReload
        let BodyAddingEvent = Events.BodyAdding
        let BodyRemovingEvent = Events.BodyRemoving