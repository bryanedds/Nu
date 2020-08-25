// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleGame =

    /// Dynamic property getters.
    let internal Getters = Dictionary<string, World -> Property> HashIdentity.Structural

    /// Dynamic property setters.
    let internal Setters = Dictionary<string, Property -> World -> bool * World> HashIdentity.Structural

    type World with

        static member private publishGameChange propertyName (propertyValue : obj) world =
            let game = Game ()
            let world =
                let changeData = { Name = propertyName; Value = propertyValue }
                let changeEventAddress = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
                let eventTrace = EventTrace.record "World" "publishGameChange" EventTrace.empty
                World.publishPlus changeData changeEventAddress eventTrace game false world
            world

        static member internal getGameState world =
            world.GameState

        static member private setGameState gameState world =
            World.choose { world with GameState = gameState }

        static member private updateGameStateWithoutEvent updater world =
            let gameState = World.getGameState world
            let changed = updater gameState
            (changed, world)

        static member private updateGameState updater propertyName propertyValue world =
            let (changed, world) = World.updateGameStateWithoutEvent updater world
            if changed
            then World.publishGameChange propertyName propertyValue world
            else world

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameCreationTimeStamp world = (World.getGameState world).CreationTimeStamp
        static member internal getGameDispatcher world = (World.getGameState world).Dispatcher
        static member internal getGameModelProperty world = (World.getGameState world).Model
        static member internal getGameModel<'a> world = (World.getGameState world).Model.DesignerValue :?> 'a
        static member internal getGameScriptFrame world = (World.getGameState world).ScriptFrame
        static member internal setGameScriptFrame value world = World.updateGameState (fun gameState -> if value <> gameState.ScriptFrame then gameState.ScriptFrame <- value; true else false) Property? ScriptFrame value world

        static member internal setGameModelProperty (value : DesignerProperty) world =
            World.updateGameState
                (fun gameState -> if value.DesignerValue <> gameState.Model.DesignerValue then gameState.Model.DesignerValue <- value.DesignerValue; true else false)
                Property? Model value.DesignerValue world

        static member internal setGameModel<'a> (value : 'a) world =
            World.updateGameState
                (fun gameState ->
                    let valueObj = value :> obj
                    if valueObj <> gameState.Model.DesignerValue then gameState.Model <- { DesignerType = typeof<'a>; DesignerValue = valueObj }; true else false)
                Property? Model value world

        /// Get the current eye center.
        [<FunctionBinding>]
        static member getEyeCenter world =
            (World.getGameState world).EyeCenter

        /// Set the current eye center.
        [<FunctionBinding>]
        static member setEyeCenter value world =
            World.updateGameState (fun gameState -> if value <> gameState.EyeCenter then gameState.EyeCenter <- value; true else false) Property? EyeCenter value world

        /// Get the current eye size.
        [<FunctionBinding>]
        static member getEyeSize world =
            (World.getGameState world).EyeSize

        /// Set the current eye size.
        [<FunctionBinding>]
        static member setEyeSize value world =
            World.updateGameState (fun gameState -> if value <> gameState.EyeSize then gameState.EyeSize <- value; true else false) Property? EyeSize value world

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        [<FunctionBinding>]
        static member setOmniScreenOpt value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreen to SelectedScreen."
            World.updateGameState (fun gameState -> if value <> gameState.OmniScreenOpt then gameState.OmniScreenOpt <- value; true else false) Property? OmniScreenOpt value world

        /// Get the omniScreen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getOmniScreen world =
            Option.get (World.getOmniScreenOpt world)
        
        /// Set the omniScreen.
        [<FunctionBinding>]
        static member setOmniScreen value world =
            World.setOmniScreenOpt (Some value) world

        /// Get the currently selected screen, if any.
        [<FunctionBinding>]
        static member getSelectedScreenOpt world =
            (World.getGameState world).SelectedScreenOpt

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        [<FunctionBinding>]
        static member setSelectedScreenOpt value world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getOmniScreenOpt world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // raise change event for none selection
            let world = World.updateGameState (constant true) Property? SelectedScreenOpt None world

            // clear out singleton states
            let world =
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen ->
                    let world = WorldModule.unregisterScreenPhysics screen world
                    let world = WorldModule.evictScreenElements screen world
                    world
                | None -> world
                
            // actually set selected screen (no events)
            let (_, world) =
                World.updateGameStateWithoutEvent
                    (fun gameState -> if value <> gameState.SelectedScreenOpt then gameState.SelectedScreenOpt <- value; true else false)
                    world

            // handle some case
            match value with
            | Some screen ->

                // populate singleton states
                let world = WorldModule.admitScreenElements screen world
                let world = WorldModule.registerScreenPhysics screen world

                // raise change event for some selection
                World.updateGameState (constant true) Property? SelectedScreenOpt (Some screen) world

            // fin
            | None -> world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getSelectedScreen world =
            Option.get (World.getSelectedScreenOpt world)
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        [<FunctionBinding>]
        static member setSelectedScreen value world =
            World.setSelectedScreenOpt (Some value) world

        /// Get the current destination screen if a screen transition is currently underway.
        [<FunctionBinding>]
        static member getScreenTransitionDestinationOpt world =
            (World.getGameState world).ScreenTransitionDestinationOpt

        /// Set the current destination screen or None. Be careful using this function as calling
        /// it is predicated that no screen transition is currently underway.
        /// TODO: consider asserting such predication here.
        [<FunctionBinding>]
        static member internal setScreenTransitionDestinationOpt destination world =
            World.updateGameState
                (fun gameState -> if destination <> gameState.ScreenTransitionDestinationOpt then gameState.ScreenTransitionDestinationOpt <- destination; true else false)
                Property? ScreenTransitionDestinationOpt
                destination
                world

        /// Get the view of the eye in absolute terms (world space).
        static member getViewAbsolute world =
            Math.getViewAbsolute (World.getEyeCenter world) (World.getEyeSize world)
        
        /// Get the view of the eye in absolute terms (world space) with translation sliced on
        /// integers.
        static member getViewAbsoluteI world =
            Math.getViewAbsolute (World.getEyeCenter world) (World.getEyeSize world)

        /// The relative view of the eye with original single values. Due to the problems with
        /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
        /// coordinates is very, very bad for rendering.
        static member getViewRelative world =
            Math.getViewRelative (World.getEyeCenter world) (World.getEyeSize world)

        /// The relative view of the eye with translation sliced on integers. Good for rendering.
        static member getViewRelativeI world =
            Math.getViewRelativeI (World.getEyeCenter world) (World.getEyeSize world)

        /// Get the bounds of the eye's sight relative to its position.
        [<FunctionBinding>]
        static member getViewBoundsRelative world =
            let gameState = World.getGameState world
            Vector4
                (gameState.EyeCenter.X - gameState.EyeSize.X * 0.5f,
                 gameState.EyeCenter.Y - gameState.EyeSize.Y * 0.5f,
                 gameState.EyeCenter.X + gameState.EyeSize.X * 0.5f,
                 gameState.EyeCenter.Y + gameState.EyeSize.Y * 0.5f)

        /// Get the bounds of the eye's sight not relative to its position.
        [<FunctionBinding>]
        static member getViewBoundsAbsolute world =
            let gameState = World.getGameState world
            Vector4
                (gameState.EyeSize.X * -0.5f,
                 gameState.EyeSize.Y * -0.5f,
                 gameState.EyeSize.X * 0.5f,
                 gameState.EyeSize.Y * 0.5f)

        /// Get the bounds of the eye's sight.
        [<FunctionBinding>]
        static member getViewBounds absolute world =
            if absolute
            then World.getViewBoundsAbsolute world
            else World.getViewBoundsRelative world

        /// Check that the given bounds is within the eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView absolute (bounds : Vector4) world =
            let viewBounds = World.getViewBounds absolute world
            Math.isBoundsIntersectingBounds bounds viewBounds

        /// Transform the given mouse position to screen space.
        [<FunctionBinding>]
        static member mouseToScreen (mousePosition : Vector2) world =
            let gameState = World.getGameState world
            let positionScreen =
                Vector2
                    (mousePosition.X - gameState.EyeSize.X * 0.5f,
                     -(mousePosition.Y - gameState.EyeSize.Y * 0.5f)) // negation for right-handedness
            positionScreen

        /// Transform the given mouse position to world space.
        [<FunctionBinding>]
        static member mouseToWorld absolute mousePosition world =
            let positionScreen = World.mouseToScreen mousePosition world
            let view =
                if absolute
                then World.getViewAbsolute world
                else World.getViewRelative world
            let positionWorld = positionScreen * view
            positionWorld

        /// Transform the given mouse position to entity space.
        [<FunctionBinding>]
        static member mouseToEntity absolute entityPosition mousePosition world =
            let mousePositionWorld = World.mouseToWorld absolute mousePosition world
            entityPosition - mousePositionWorld

        /// Fetch an asset with the given tag and convert it to a value of type 'a.
        static member assetTagToValueOpt<'a> assetTag metadata world =
            match World.tryFindSymbol assetTag metadata world with
            | Some symbol ->
                try let script = symbolToValue<'a> symbol in Some script
                with exn -> Log.debug ("Failed to convert symbol '" + scstring symbol + "' to value due to: " + scstring exn); None
            | None -> None

        /// Fetch assets with the given tags and convert it to values of type 'a.
        static member assetTagsToValueOpts<'a> assetTags metadata world =
            List.map (fun assetTag -> World.assetTagToValueOpt<'a> assetTag metadata world) assetTags

        static member internal tryGetGameProperty propertyName world =
            match Getters.TryGetValue propertyName with
            | (false, _) -> GameState.tryGetProperty propertyName (World.getGameState world)
            | (true, getter) -> Some (getter world)

        static member internal getGameProperty propertyName world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match GameState.tryGetProperty propertyName (World.getGameState world) with
                | None -> failwithf "Could not find property '%s'." propertyName
                | Some property -> property
            | (true, getter) -> getter world

        static member internal trySetGameProperty propertyName property world =
            match Setters.TryGetValue propertyName with
            | (true, setter) -> setter property world
            | (false, _) ->
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let world =
                    World.updateGameState
                        (fun gameState ->
                            match GameState.tryGetProperty propertyName gameState with
                            | Some propertyOld ->
                                if property.PropertyValue <> propertyOld.PropertyValue then
                                    let successInner = GameState.trySetProperty propertyName property gameState
                                    success <- successInner
                                    true
                                else false
                            | None -> false)
                        propertyName property.PropertyValue world
                (success, world)

        static member internal setGameProperty propertyName property world =
            match Setters.TryGetValue propertyName with
            | (true, setter) ->
                match setter property world with
                | (true, world) -> world
                | (false, _) -> failwith ("Cannot change game property " + propertyName + ".")
            | (false, _) ->
                World.updateGameState
                    (fun gameState ->
                        let propertyOld = GameState.getProperty propertyName gameState
                        if property.PropertyValue <> propertyOld.PropertyValue then GameState.setProperty propertyName property gameState; true else false)
                    propertyName property.PropertyValue world

        static member internal attachGameProperty propertyName property world =
            World.updateGameState
                (fun gameState -> GameState.attachProperty propertyName property gameState)
                propertyName property.PropertyValue world

        static member internal detachGameProperty propertyName world =
            World.updateGameStateWithoutEvent
                (fun gameState -> GameState.detachProperty propertyName gameState)
                world |>
            snd

        static member internal writeGame3 writeScreens gameDescriptor world =
            let gameState = World.getGameState world
            let gameDispatcherName = getTypeName gameState.Dispatcher
            let gameDescriptor = { gameDescriptor with GameDispatcherName = gameDispatcherName }
            let gameProperties = Reflection.writePropertiesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = gameProperties }
            writeScreens gameDescriptor world

        static member internal readGame3 readScreens gameDescriptor world =

            // make the dispatcher
            let dispatcherName = gameDescriptor.GameDispatcherName
            let dispatchers = World.getGameDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ("Could not find GameDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
                    let dispatcherName = typeof<GameDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the game state and populate its properties
            let gameState = GameState.make dispatcher
            let gameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState world
            let gameState = Reflection.readPropertiesToTarget GameState.copy gameDescriptor.GameProperties gameState

            // set the game's state in the world
            let world = World.setGameState gameState world
            
            // read the game's screens
            let world = readScreens gameDescriptor world |> snd
            
            // choose the world
            World.choose world

        /// View all of the properties of a game.
        static member internal viewGameProperties world =
            let state = World.getGameState world
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c

    /// Initialize property getters.
    let private initGetters () =
        Getters.Add ("Dispatcher", fun world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher world })
        Getters.Add ("Model", fun world -> let designerProperty = World.getGameModelProperty world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        Getters.Add ("OmniScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        Getters.Add ("SelectedScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        Getters.Add ("ScreenTransitionDestinationOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getScreenTransitionDestinationOpt world })
        Getters.Add ("EyeCenter", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeCenter world })
        Getters.Add ("EyeSize", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeSize world })
        Getters.Add ("ScriptFrame", fun world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScriptFrame world })
        Getters.Add ("CreationTimeStamp", fun world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameCreationTimeStamp world })
        Getters.Add ("Id", fun world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId world })

    /// Initialize property setters.
    let private initSetters () =
        Setters.Add ("Dispatcher", fun _ world -> (false, world))
        Setters.Add ("Model", fun property world -> (true, World.setGameModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } world))
        Setters.Add ("OmniScreenOpt", fun property world -> (true, World.setOmniScreenOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("SelectedScreenOpt", fun property world -> (true, World.setSelectedScreenOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("ScreenTransitionDestinationOpt", fun property world -> (true, World.setScreenTransitionDestinationOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("EyeCenter", fun property world -> (true, World.setEyeCenter (property.PropertyValue :?> Vector2) world))
        Setters.Add ("EyeSize", fun property world -> (true, World.setEyeSize (property.PropertyValue :?> Vector2) world))
        Setters.Add ("ScriptFrame", fun _ world -> (false, world))
        Setters.Add ("CreationTimeStamp", fun _ world -> (false, world))
        Setters.Add ("Id", fun _ world -> (false, world))

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()