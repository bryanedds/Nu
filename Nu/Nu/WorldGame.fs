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
        member this.GetModelGeneric<'a> world = World.getGameModel<'a> world
        member this.SetModelGeneric<'a> value world = World.setGameModel<'a> value world |> snd'
        member this.ModelGeneric<'a> () = lens Property? Model this.GetModelGeneric<'a> this.SetModelGeneric<'a> this
        member this.GetOmniScreenOpt world = World.getOmniScreenOpt world
        member this.SetOmniScreenOpt value world = World.setOmniScreenOptPlus value world |> snd'
        member this.OmniScreenOpt = lens Property? OmniScreenOpt this.GetOmniScreenOpt this.SetOmniScreenOpt this
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SelectedScreenOpt = lensReadOnly Property? SelectedScreenOpt this.GetSelectedScreenOpt this
        member this.GetDesiredScreenOpt world = World.getDesiredScreenOpt world
        member this.SetDesiredScreenOpt value world = World.setDesiredScreenOpt value world |> snd'
        member this.DesiredScreenOpt = lens Property? DesiredScreenOpt this.GetDesiredScreenOpt this.SetDesiredScreenOpt this
        member this.GetEyeCenter world = World.getEyeCenter world
        member this.SetEyeCenter value world = World.setEyeCenterPlus value world |> snd'
        member this.EyeCenter = lens Property? EyeCenter this.GetEyeCenter this.SetEyeCenter this
        member this.GetEyeSize world = World.getEyeSize world
        member this.SetEyeSize value world = World.setEyeSizePlus value world |> snd'
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
            let eventTrace = EventTrace.debug "World" "registerGame" "" EventTrace.empty
            let world = World.publishPlus () (rtoa<unit> [|"Register"; "Event"|]) eventTrace game true false world
            World.choose world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let eventTrace = EventTrace.debug "World" "unregisteringGame" "" EventTrace.empty
            let world = World.publishPlus () (rtoa<unit> [|"Unregistering"; "Event"|]) eventTrace game true false world
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

        static member internal actualizeGame world =
            let game = Simulants.Game
            let dispatcher = game.GetDispatcher world
            let world = dispatcher.Actualize (game, world)
            World.choose world

        // Get all the entities in the world.
        [<FunctionBinding "getEntities0">]
        static member getEntities1 world =
            World.getGroups1 world |>
            Seq.map (fun group -> World.getEntities group world) |>
            Seq.concat

        // Get all the groups in the world.
        [<FunctionBinding "getGroups0">]
        static member getGroups1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getGroups screen world) |>
            Seq.concat

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