// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldGameModule =

    type Game with

        member this.GetId world = World.getGameId world
        member this.Id = Lens.makeReadOnly Property? Id this.GetId this
        member this.GetDispatcher world = World.getGameDispatcher world
        member this.Dispatcher = Lens.makeReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetCreationTimeStamp world = World.getGameCreationTimeStamp world
        member this.CreationTimeStamp = Lens.makeReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetImperative world = World.getGameImperative world
        member this.Imperative = Lens.makeReadOnly Property? Imperative this.GetImperative this
        member this.GetScriptOpt world = World.getGameScriptOpt world
        member this.SetScriptOpt value world = World.setGameScriptOpt value world
        member this.ScriptOpt = Lens.make Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world = World.getGameScript world
        member this.SetScript value world = World.setGameScript value world
        member this.Script = Lens.make Property? Script this.GetScript this.SetScript this
        member this.GetScriptFrame world = World.getGameScriptFrame world
        member this.ScriptFrame = Lens.makeReadOnly Property? Script this.GetScriptFrame this
        member internal this.GetScriptUnsubscriptions world = World.getGameScriptUnsubscriptions world
        member internal this.SetScriptUnsubscriptions value world = World.setGameScriptUnsubscriptions value world
        member internal this.ScriptUnsubscriptions = Lens.make Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetOnRegister world = World.getGameOnRegister world
        member this.SetOnRegister value world = World.setGameOnRegister value world
        member this.OnRegister = Lens.make Property? OnRegister this.GetOnRegister this.SetOnRegister this
        member this.GetOnUnregister world = World.getGameOnUnregister world
        member this.SetOnUnregister value world = World.setGameOnUnregister value world
        member this.OnUnregister = Lens.make Property? OnUnregister this.GetOnUnregister this.SetOnUnregister this
        member this.GetOnUpdate world = World.getGameOnUpdate world
        member this.SetOnUpdate value world = World.setGameOnUpdate value world
        member this.OnUpdate = Lens.make Property? OnUpdate this.GetOnUpdate this.SetOnUpdate this
        member this.GetOnPostUpdate world = World.getGameOnPostUpdate world
        member this.SetOnPostUpdate value world = World.setGameOnPostUpdate value world
        member this.OnPostUpdate = Lens.make Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate this
        member this.GetOnSignal world = World.getGameOnSignal world
        member this.SetOnSignal value world = World.setGameOnSignal value world
        member this.OnSignal = Lens.make Property? OnSignal this.GetOnSignal this.SetOnSignal this
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOpt value world
        member this.OmniScreenOpt = Lens.make Property? OmniScreenOpt this.GetOmniScreenOpt this.SetOmniScreenOpt this
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SetSelectedScreenOpt value world = World.setSelectedScreenOpt value world
        member this.SelectedScreenOpt = Lens.make Property? SelectedScreenOpt this.GetSelectedScreenOpt this.SetSelectedScreenOpt this
        member this.GetScreenTransitionDestinationOpt world = World.getScreenTransitionDestinationOpt world
        member this.SetScreenTransitionDestinationOpt value world = World.setScreenTransitionDestinationOpt value world
        member this.ScreenTransitionDestinationOpt = Lens.make Property? ScreenTransitionDestinationOpt this.GetScreenTransitionDestinationOpt this.SetScreenTransitionDestinationOpt this
        member this.GetEyeCenter world = World.getEyeCenter world
        member this.SetEyeCenter value world = World.setEyeCenter value world
        member this.EyeCenter = Lens.make Property? EyeCenter this.GetEyeCenter this.SetEyeCenter this
        member this.GetEyeSize world = World.getEyeSize world
        member this.SetEyeSize value world = World.setEyeSize value world
        member this.EyeSize = Lens.make Property? EyeSize this.GetEyeSize this.SetEyeSize this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this
        member this.SignalEvent = Events.Signal --> this
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
        member this.GetViewBounds viewType world = World.getViewBounds viewType world

        /// Check that the given bounds is within the eye's sight.
        member this.GetInView viewType bounds world = World.isBoundsInView viewType bounds world

        /// Transform the given mouse position to screen space.
        member this.MouseToScreen mousePosition world = World.mouseToScreen mousePosition world

        /// Transform the given mouse position to world space.
        member this.MouseToWorld viewType mousePosition world = World.mouseToWorld viewType mousePosition world

        /// Transform the given mouse position to entity space.
        member this.MouseToEntity viewType entityPosition mousePosition world = World.mouseToEntity viewType entityPosition mousePosition world

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a game dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs<'a> world = this.DispatchesAs (typeof<'a>, world)

        /// Resolve a relation in the context of a game.
        member this.Resolve relation = Game (Relation.resolve this.GameAddress relation)

        /// Get a game's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.GameAddress

        /// Send a signal to the game.
        member this.Signal signal world =
            World.withEventContext (fun world ->
                let world =
                    match ScriptingSystem.tryImport typeof<Symbol> signal world with
                    | Some signalExpr ->
                        ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("signal", signalExpr) }) world
                        let world = World.eval (this.GetOnSignal world) (this.GetScriptFrame world) this world |> snd'
                        ScriptingSystem.removeProceduralBindings world
                        world
                    | None -> failwithumf ()
                let dispatcher = this.GetDispatcher world
                dispatcher.Signal (signal, this, world))
                this
                world

    type World with

        static member private gameOnRegisterChanged _ world =
            let world = World.unregisterGame world
            World.registerGame world

        static member private gameScriptOptChanged evt world =
            let game = evt.Subscriber : Game
            match game.GetScriptOpt world with
            | Some script ->
                let symbolLoadMetadata = { ImplicitDelimiters = true; StripCsvHeader = false }
                match World.assetTagToValueOpt<Scripting.Expr array> symbolLoadMetadata script world with
                | (Some script, world) -> game.SetScript script world
                | (None, world) -> world
            | None -> world

        static member internal registerGame world =
            let game = Default.Game
            let world = World.monitor World.gameOnRegisterChanged (Events.Change Property? OnRegister) game world
            let world = World.monitor World.gameScriptOptChanged (Events.Change Property? ScriptOpt) game world
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = game.GetDispatcher world
                    let world = dispatcher.Register (game, world)
                    let eventTrace = EventTrace.record "World" "registerGame" EventTrace.empty
                    let world = World.publish () (ltoa<unit> ["Register"; "Event"] --> game) eventTrace game world
                    World.eval (game.GetOnRegister world) (game.GetScriptFrame world) game world |> snd')
                    game
                    world
            World.choose world

        static member internal unregisterGame world =
            let game = Default.Game
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = game.GetDispatcher world
                    let eventTrace = EventTrace.record "World" "unregisteringGame" EventTrace.empty
                    let world = World.publish () (ltoa<unit> ["Unregistering"; "Event"] --> game) eventTrace game world
                    let world = World.eval (game.GetOnUnregister world) (game.GetScriptFrame world) game world |> snd'
                    dispatcher.Unregister (game, world))
                    game
                    world
            World.choose world

        static member internal updateGame world =
            let game = Default.Game
            World.withEventContext (fun world ->

                // update via dispatcher
                let dispatcher = game.GetDispatcher world
                let world = dispatcher.Update (game, world)

                // run script update
                let world = World.evalWithLogging (game.GetOnUpdate world) (game.GetScriptFrame world) game world |> snd'

                // publish update event
                let eventTrace = EventTrace.record "World" "updateGame" EventTrace.empty
                let world = World.publishPlus World.sortSubscriptionsByHierarchy () Events.Update eventTrace game true world
                World.choose world)
                game
                world

        static member internal postUpdateGame world =
            let game = Default.Game
            World.withEventContext (fun world ->
                
                // post-update via dispatcher
                let dispatcher = game.GetDispatcher world
                let world = dispatcher.PostUpdate (game, world)

                // run script post-update
                let world = World.evalWithLogging (game.GetOnPostUpdate world) (game.GetScriptFrame world) game world |> snd'

                // publish post-update event
                let eventTrace = EventTrace.record "World" "postUpdateGame" EventTrace.empty
                let world = World.publishPlus World.sortSubscriptionsByHierarchy () Events.PostUpdate eventTrace game true world
                World.choose world)
                game
                world

        static member internal actualizeGame world =
            let game = Default.Game
            World.withEventContext (fun world ->
                let dispatcher = game.GetDispatcher world
                let world = dispatcher.Actualize (game, world)
                World.choose world)
                game
                world

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
        static member isSimulantSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [] -> true
            | screenName :: _ ->
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