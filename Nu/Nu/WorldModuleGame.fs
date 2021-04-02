// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleGame =

    /// Dynamic property getters / setters.
    let internal GameGetters = Dictionary<string, World -> Property> StringComparer.Ordinal
    let internal GameSetters = Dictionary<string, Property -> World -> struct (bool * World)> StringComparer.Ordinal

    type World with

        static member private publishGameChange propertyName (propertyValue : obj) world =

            // the game reference
            let game = Game ()

            // publish change binding
            let world =
                World.publishChangeBinding propertyName game world

            // publish change event
            let world =
                let changeData = { Name = propertyName; Value = propertyValue }
                let changeEventAddress = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
                let eventTrace = EventTrace.debug "World" "publishGameChange" "" EventTrace.empty
                World.publishPlus changeData changeEventAddress eventTrace game false world

            // fin
            world

        static member internal getGameState world =
            world.GameState

        static member private setGameState gameState world =
            World.choose { world with GameState = gameState }

        static member private updateGameStateWithoutEvent updater world =
            let gameStateOpt = updater (World.getGameState world)
            match gameStateOpt :> obj with
            | null -> struct (false, world)
            | _ -> struct (true, World.setGameState gameStateOpt world)

        static member private updateGameState updater propertyName propertyValue world =
            let struct (changed, world) = World.updateGameStateWithoutEvent updater world
            let world =
                if changed
                then World.publishGameChange propertyName propertyValue world
                else world
            struct (changed, world)

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameCreationTimeStamp world = (World.getGameState world).CreationTimeStamp
        static member internal getGameDispatcher world = (World.getGameState world).Dispatcher
        static member internal getGameModelProperty world = (World.getGameState world).Model
        static member internal getGameModel<'a> world = (World.getGameState world).Model.DesignerValue :?> 'a
        static member internal getGameScriptFrame world = (World.getGameState world).ScriptFrame
        static member internal setGameScriptFrame value world = World.updateGameState (fun gameState -> if value <> gameState.ScriptFrame then { gameState with ScriptFrame = value } else Unchecked.defaultof<_>) Property? ScriptFrame value world

        static member internal setGameModelProperty (value : DesignerProperty) world =
            World.updateGameState
                (fun gameState ->
                    if value.DesignerValue =/= gameState.Model.DesignerValue
                    then { gameState with Model = { gameState.Model with DesignerValue = value.DesignerValue }}
                    else Unchecked.defaultof<_>)
                Property? Model value.DesignerValue world

        static member internal setGameModel<'a> (value : 'a) world =
            World.updateGameState
                (fun gameState ->
                    let valueObj = value :> obj
                    if valueObj =/= gameState.Model.DesignerValue
                    then { gameState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    else Unchecked.defaultof<_>)
                Property? Model value world

        /// Get the current eye center.
        [<FunctionBinding>]
        static member getEyeCenter world =
            (World.getGameState world).EyeCenter

        /// Set the current eye center.
        static member internal setEyeCenterPlus value world =
            World.updateGameState (fun gameState -> if v2Neq value gameState.EyeCenter then { gameState with EyeCenter = value } else Unchecked.defaultof<_>) Property? EyeCenter value world

        /// Set the current eye center.
        [<FunctionBinding>]
        static member setEyeCenter value world =
            World.setEyeCenterPlus value world |> snd'

        /// Get the current eye size.
        [<FunctionBinding>]
        static member getEyeSize world =
            (World.getGameState world).EyeSize

        /// Set the current eye size.
        static member internal setEyeSizePlus value world =
            World.updateGameState (fun gameState -> if v2Neq value gameState.EyeSize then { gameState with EyeSize = value } else Unchecked.defaultof<_>) Property? EyeSize value world

        /// Set the current eye size.
        [<FunctionBinding>]
        static member setEyeSize value world =
            World.setEyeSizePlus value world |> snd'

        /// Get the current eye bounds
        [<FunctionBinding>]
        static member getEyeBounds world =
            let eyeCenter = World.getEyeCenter world
            let eyeSize = World.getEyeSize world
            v4Bounds (eyeCenter - eyeSize * 0.5f) eyeSize

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        static member internal setOmniScreenOptPlus value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreen to SelectedScreen."
            World.updateGameState (fun gameState -> if value <> gameState.OmniScreenOpt then { gameState with OmniScreenOpt = value } else Unchecked.defaultof<_>) Property? OmniScreenOpt value world

        /// Set the omni-screen or None.
        [<FunctionBinding>]
        static member setOmniScreenOpt value world =
            World.setOmniScreenOptPlus value world |> snd'

        /// Get the omniScreen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getOmniScreen world =
            Option.get (World.getOmniScreenOpt world)

        /// Set the omniScreen.
        static member internal setOmniScreenPlus value world =
            World.setOmniScreenOptPlus (Some value) world
        
        /// Set the omniScreen.
        [<FunctionBinding>]
        static member setOmniScreen value world =
            World.setOmniScreenPlus value world |> snd'

        /// Get the currently selected screen, if any.
        [<FunctionBinding>]
        static member getSelectedScreenOpt world =
            (World.getGameState world).SelectedScreenOpt

        /// Constrain the eye to the given bounds.
        [<FunctionBinding>]
        static member constrainEyeBounds (bounds : Vector4) world =
            let eyeBounds = World.getEyeBounds world
            let eyeBoundsConstrained =
                eyeBounds.WithPosition
                    (v2
                        (if eyeBounds.X < bounds.X then bounds.X
                         elif eyeBounds.Right.X > bounds.Right.X then bounds.Right.X - eyeBounds.Size.X
                         else eyeBounds.X)
                        (if eyeBounds.Y < bounds.Y then bounds.Y
                         elif eyeBounds.Top.Y > bounds.Top.Y then bounds.Top.Y - eyeBounds.Size.Y
                         else eyeBounds.Y))
            World.setEyeCenter eyeBoundsConstrained.Center world
            
        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenOptPlus value world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getOmniScreenOpt world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // raise change event for none selection
            let struct (_, world) = World.updateGameState id Property? SelectedScreenOpt None world

            // clear out singleton states
            let world =
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen ->
                    let world = WorldModule.unregisterScreenPhysics screen world
                    let world = WorldModule.evictScreenElements screen world
                    world
                | None -> world
                
            // actually set selected screen (no events)
            let struct (_, world) =
                World.updateGameStateWithoutEvent
                    (fun gameState ->
                        if value <> gameState.SelectedScreenOpt
                        then { gameState with SelectedScreenOpt = value }
                        else Unchecked.defaultof<_>)
                    world

            // handle some case
            match value with
            | Some screen ->

                // populate singleton states
                let world = WorldModule.admitScreenElements screen world
                let world = WorldModule.registerScreenPhysics screen world

                // raise change event for some selection
                let world = World.updateGameState id Property? SelectedScreenOpt (Some screen) world |> snd'
                (true, world)

            // fin
            | None -> (true, world)

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        [<FunctionBinding>]
        static member setSelectedScreenOpt value world =
            World.setSelectedScreenOptPlus value world |> snd

        /// Get the currently selected screen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getSelectedScreen world =
            Option.get (World.getSelectedScreenOpt world)
            
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenPlus value world =
            World.setSelectedScreenOptPlus (Some value) world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        [<FunctionBinding>]
        static member setSelectedScreen value world =
            World.setSelectedScreenPlus value world |> snd

        /// Get the current destination screen if a screen transition is currently underway.
        static member getScreenTransitionDestinationOpt world =
            (World.getGameState world).ScreenTransitionDestinationOpt

        /// Set the current destination screen or None. Be careful using this function as calling
        /// it is predicated that no screen transition is currently underway.
        /// TODO: consider asserting such predication here.
        static member internal setScreenTransitionDestinationOpt destination world =
            World.updateGameState
                (fun gameState ->
                    if destination <> gameState.ScreenTransitionDestinationOpt
                    then { gameState with ScreenTransitionDestinationOpt = destination }
                    else Unchecked.defaultof<_>)
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
                 gameState.EyeSize.X,
                 gameState.EyeSize.Y)

        /// Get the bounds of the eye's sight not relative to its position.
        [<FunctionBinding>]
        static member getViewBoundsAbsolute world =
            let gameState = World.getGameState world
            Vector4
                (gameState.EyeSize.X * -0.5f,
                 gameState.EyeSize.Y * -0.5f,
                 gameState.EyeSize.X,
                 gameState.EyeSize.Y)

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
                v2
                    +(mousePosition.X / single Constants.Render.VirtualScalar - gameState.EyeSize.X * 0.5f)
                    -(mousePosition.Y / single Constants.Render.VirtualScalar - gameState.EyeSize.Y * 0.5f) // negation for right-handedness
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

        static member internal tryGetGameProperty (propertyName, world, property : _ outref) =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) -> property <- getter world; true
            | (false, _) -> GameState.tryGetProperty (propertyName, World.getGameState world, &property)

        static member internal getGameProperty propertyName world =
            match GameGetters.TryGetValue propertyName with
            | (false, _) ->
                let mutable property = Unchecked.defaultof<_>
                match GameState.tryGetProperty (propertyName, World.getGameState world, &property) with
                | true -> property
                | false -> failwithf "Could not find property '%s'." propertyName
            | (true, getter) -> getter world

        static member internal trySetGamePropertyFast propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property world |> snd'
            | (false, _) ->
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let struct (_, world) =
                    World.updateGameState
                        (fun gameState ->
                            let mutable propertyOld = Unchecked.defaultof<_>
                            match GameState.tryGetProperty (propertyName, gameState, &propertyOld) with
                            | true ->
                                if property.PropertyValue =/= propertyOld.PropertyValue then
                                    let struct (successInner, gameState) = GameState.trySetProperty propertyName property gameState
                                    success <- successInner
                                    gameState
                                else Unchecked.defaultof<_>
                            | false -> Unchecked.defaultof<_>)
                        propertyName property.PropertyValue world
                world

        static member internal trySetGameProperty propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) ->
                let struct (changed, world) = setter property world
                struct (true, changed, world)
            | (false, _) ->
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let struct (changed, world) =
                    World.updateGameState
                        (fun gameState ->
                            let mutable propertyOld = Unchecked.defaultof<_>
                            match GameState.tryGetProperty (propertyName, gameState, &propertyOld) with
                            | true ->
                                if property.PropertyValue =/= propertyOld.PropertyValue then
                                    let struct (successInner, gameState) = GameState.trySetProperty propertyName property gameState
                                    success <- successInner
                                    gameState
                                else Unchecked.defaultof<_>
                            | false -> Unchecked.defaultof<_>)
                        propertyName property.PropertyValue world
                struct (success, changed, world)

        static member internal setGameProperty propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (false, _) ->
                World.updateGameState
                    (fun gameState ->
                        let propertyOld = GameState.getProperty propertyName gameState
                        if property.PropertyValue =/= propertyOld.PropertyValue
                        then (GameState.setProperty propertyName property gameState)
                        else Unchecked.defaultof<_>)
                    propertyName property.PropertyValue world
            | (true, setter) -> setter property world

        static member internal attachGameProperty propertyName property world =
            let struct (_, world) =
                World.updateGameState
                    (fun gameState -> GameState.attachProperty propertyName property gameState)
                    propertyName property.PropertyValue world
            world

        static member internal detachGameProperty propertyName world =
            let struct (_, world) =
                World.updateGameStateWithoutEvent
                    (fun gameState -> GameState.detachProperty propertyName gameState)
                    world
            world

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
                    Log.info ("Could not find GameDispatcher '" + dispatcherName + "'.")
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
        GameGetters.Add ("Dispatcher", fun world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher world })
        GameGetters.Add ("Model", fun world -> let designerProperty = World.getGameModelProperty world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        GameGetters.Add ("OmniScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        GameGetters.Add ("SelectedScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        GameGetters.Add ("EyeCenter", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeCenter world })
        GameGetters.Add ("EyeSize", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeSize world })
        GameGetters.Add ("ScriptFrame", fun world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScriptFrame world })
        GameGetters.Add ("CreationTimeStamp", fun world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameCreationTimeStamp world })
        GameGetters.Add ("Id", fun world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId world })

    /// Initialize property setters.
    let private initSetters () =
        GameSetters.Add ("Model", fun property world -> World.setGameModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } world)
        GameSetters.Add ("OmniScreenOpt", fun property world -> World.setOmniScreenOptPlus (property.PropertyValue :?> Screen option) world)
        GameSetters.Add ("EyeCenter", fun property world -> World.setEyeCenterPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("EyeSize", fun property world -> World.setEyeSizePlus (property.PropertyValue :?> Vector2) world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()