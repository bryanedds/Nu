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
        member this.Dispatcher = lensReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetModel<'a> world = World.getGameModel<'a> world
        member this.SetModel<'a> value world = World.setGameModel<'a> value world
        member this.Model<'a> () = lens Property? Model this.GetModel<'a> this.SetModel<'a> this
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOpt value world
        member this.OmniScreenOpt = lens Property? OmniScreenOpt this.GetOmniScreenOpt this.SetOmniScreenOpt this
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SelectedScreenOpt = lensReadOnly Property? SelectedScreenOpt this.GetSelectedScreenOpt this
        member this.GetScreenTransitionDestinationOpt world = World.getScreenTransitionDestinationOpt world
        member this.ScreenTransitionDestinationOpt = lensReadOnly Property? ScreenTransitionDestinationOpt this.GetScreenTransitionDestinationOpt this
        member this.GetEyeCenter world = World.getEyeCenter world
        member this.SetEyeCenter value world = World.setEyeCenter value world
        member this.EyeCenter = lens Property? EyeCenter this.GetEyeCenter this.SetEyeCenter this
        member this.GetEyeSize world = World.getEyeSize world
        member this.SetEyeSize value world = World.setEyeSize value world
        member this.EyeSize = lens Property? EyeSize this.GetEyeSize this.SetEyeSize this
        member this.GetScriptFrame world = World.getGameScriptFrame world
        member this.ScriptFrame = lensReadOnly Property? Script this.GetScriptFrame this
        member this.GetCreationTimeStamp world = World.getGameCreationTimeStamp world
        member this.CreationTimeStamp = lensReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetId world = World.getGameId world
        member this.Id = lensReadOnly Property? Id this.GetId this

        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this
        member this.SubscribeEvent = Events.Subscribe --> this
        member this.UnsubscribeEvent = Events.Unsubscribe --> this
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

        /// Diverge the game state.
        member this.Diverge world = World.divergeGame world

        /// The state of a game.
        /// The only place this accessor should be used is in performance-sensitive code.
        /// Otherwise, you should get and set the required game properties via the Game interface.
        member this.State world = World.getGameState world

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetGameProperty propertyName world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getGameProperty propertyName world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = (World.getGameProperty propertyName world).PropertyValue :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetGameProperty propertyName property world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setGameProperty propertyName property world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setGameProperty propertyName { PropertyType = typeof<'a>; PropertyValue = value } world

        /// Get the view of the eye in absolute terms (world space).
        member this.GetViewAbsolute (_ : World) = World.getViewAbsolute
        
        /// Get the view of the eye in absolute terms (world space) with translation sliced on
        /// integers.
        member this.GetViewAbsoluteI (_ : World) = World.getViewAbsoluteI

        /// The relative view of the eye with original single values. Due to the problems with
        /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
        /// coordinates is very, very bad for rendering.
        member this.GetViewRelative world = World.getViewRelative world

        /// The relative view of the eye with translation sliced on integers. Good for rendering.
        member this.GetViewRelativeI world = World.getViewRelativeI world

        /// Get the bounds of the eye's sight relative to its position.
        member this.GetViewBoundsRelative world = World.getViewBoundsRelative world

        /// Get the bounds of the eye's sight not relative to its position.
        member this.GetViewBoundsAbsolute world = World.getViewAbsolute world

        /// Get the bounds of the eye's sight.
        member this.GetViewBounds absolute world = World.getViewBounds absolute world

        /// Check that the given bounds is within the eye's sight.
        member this.GetInView absolute bounds world = World.isBoundsInView absolute bounds world

        /// Transform the given mouse position to screen space.
        member this.MouseToScreen mousePosition world = World.mouseToScreen mousePosition world

        /// Transform the given mouse position to world space.
        member this.MouseToWorld absolute mousePosition world = World.mouseToWorld absolute mousePosition world

        /// Transform the given mouse position to entity space.
        member this.MouseToEntity absolute entityPosition mousePosition world = World.mouseToEntity absolute entityPosition mousePosition world

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Resolve a relation in the context of a game.
        member this.Resolve relation = resolve<Game> this relation

        /// Relate a game to a simulant.
        member this.Relate simulant = relate<Game> this simulant

        /// Get a game's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.GameAddress

        /// Try to signal a game.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal registerGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Register (game, world)
            let eventTrace = EventTrace.record "World" "registerGame" EventTrace.empty
            let world = World.publish () (rtoa<unit> [|"Register"; "Event"|]) eventTrace game true world
            World.choose world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.record "World" "unregisteringGame" EventTrace.empty
            let world = World.publish () (rtoa<unit> [|"Unregistering"; "Event"|]) eventTrace game true world
            let world = dispatcher.Unregister (game, world)
            World.choose world

        static member internal updateGame world =

            // update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Update (game, world)

            // publish update event
            let eventTrace = EventTrace.record "World" "updateGame" EventTrace.empty
            let world = World.publishPlus () Events.Update eventTrace game false world
            World.choose world

        static member internal postUpdateGame world =
                
            // post-update via dispatcher
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.PostUpdate (game, world)

            // publish post-update event
            let eventTrace = EventTrace.record "World" "postUpdateGame" EventTrace.empty
            let world = World.publishPlus () Events.PostUpdate eventTrace game false world
            World.choose world

        static member internal actualizeGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Actualize (game, world)
            World.choose world

        // Get all the entities in the world.
        [<FunctionBinding "getEntities0">]
        static member getEntities1 world =
            World.getLayers1 world |>
            Seq.map (fun layer -> World.getEntities layer world) |>
            Seq.concat

        // Get all the layers in the world.
        [<FunctionBinding "getLayers0">]
        static member getLayers1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getLayers screen world) |>
            Seq.concat

        /// Determine if a simulant is contained by, or is the same as, the currently selected screen or the omni-screen.
        /// Game is always considered 'selected' as well.
        [<FunctionBinding>]
        static member isSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [||] -> true
            | names ->
                let screenName = Array.head names
                match World.getOmniScreenOpt world with
                | Some omniScreen when Address.getName omniScreen.ScreenAddress = screenName -> true
                | _ ->
                    match World.getSelectedScreenOpt world with
                    | Some screen when Address.getName screen.ScreenAddress = screenName -> true
                    | _ -> false

        /// Write a game to a game descriptor.
        static member writeGame gameDescriptor world =
            let writeScreens gameDescriptor world =
                let screens = World.getScreens world
                World.writeScreens screens gameDescriptor world
            World.writeGame3 writeScreens gameDescriptor world

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
            World.readGame3 World.readScreens gameDescriptor world

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