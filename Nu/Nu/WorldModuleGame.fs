// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleGame =

    /// Dynamic property getters.
    let internal Getters = Dictionary<string, World -> Property> HashIdentity.Structural

    /// Dynamic property setters.
    let internal Setters = Dictionary<string, Property -> World -> bool * World> HashIdentity.Structural

    type World with

        static member private publishGameChange (propertyName : string) oldWorld world =
            let game = Game ()
            let world =
                let changeEventAddress = ltoa ["Change"; propertyName; "Event"] --> game.GameAddress
                let eventTrace = EventTrace.record "World" "publishGameChange" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy { PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace game false world
            world

        static member internal getGameState world =
            world.GameState

        static member private setGameState gameState world =
#if DEBUG
            // NOTE: this check will always succeed!
            if not (World.qualifyEventContext Address.empty world) then
                failwith "Cannot set the state of a game in an unqualifed event context."
#endif
            World.choose { world with GameState = gameState }

        static member private updateGameStateWithoutEvent updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

        static member private updateGameState updater propertyName world =
            let oldWorld = world
            let world = World.updateGameStateWithoutEvent updater world
            World.publishGameChange propertyName oldWorld world

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameDispatcher world = (World.getGameState world).Dispatcher
        static member internal getGameCreationTimeStamp world = (World.getGameState world).CreationTimeStamp
        static member internal getGameScriptOpt world = (World.getGameState world).ScriptOpt
        static member internal setGameScriptOpt value world = World.updateGameState (fun gameState -> { gameState with ScriptOpt = value }) Property? ScriptOpt world
        static member internal getGameScript world = (World.getGameState world).Script
        static member internal setGameScript value world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateGameState (fun gameState -> { gameState with Script = value }) Property? Script world
            let world = World.setGameScriptFrame scriptFrame world
            evalManyWithLogging value scriptFrame (Game ()) world |> snd'
        static member internal getGameScriptFrame world = (World.getGameState world).ScriptFrame
        static member internal setGameScriptFrame value world = World.updateGameState (fun gameState -> { gameState with ScriptFrame = value }) Property? ScriptFrame world
        static member internal getGameScriptUnsubscriptions world = (World.getGameState world).ScriptUnsubscriptions
        static member internal setGameScriptUnsubscriptions value world = World.updateGameState (fun gameState -> { gameState with ScriptUnsubscriptions = value }) Property? ScriptUnsubscriptions world
        static member internal getGameOnRegister world = (World.getGameState world).OnRegister
        static member internal setGameOnRegister value world = World.updateGameState (fun gameState -> { gameState with OnRegister = value }) Property? OnRegister world
        static member internal getGameOnUnregister world = (World.getGameState world).OnUnregister
        static member internal setGameOnUnregister value world = World.updateGameState (fun gameState -> { gameState with OnUnregister = value }) Property? OnUnregister world
        static member internal getGameOnUpdate world = (World.getGameState world).OnUpdate
        static member internal setGameOnUpdate value world = World.updateGameState (fun gameState -> { gameState with OnUpdate = value }) Property? OnUpdate world
        static member internal getGameOnPostUpdate world = (World.getGameState world).OnPostUpdate
        static member internal setGameOnPostUpdate value world = World.updateGameState (fun gameState -> { gameState with OnPostUpdate = value }) Property? OnPostUpdate world
        static member internal getGameOnSignal world = (World.getGameState world).OnSignal
        static member internal setGameOnSignal value world = World.updateGameState (fun gameState -> { gameState with OnSignal = value }) Property? OnSignal world

        /// Get the current eye center.
        [<FunctionBinding>]
        static member getEyeCenter world =
            (World.getGameState world).EyeCenter

        /// Set the current eye center.
        [<FunctionBinding>]
        static member setEyeCenter value world =
            World.updateGameState (fun gameState -> { gameState with EyeCenter = value }) Property? EyeCenter world

        /// Get the current eye size.
        [<FunctionBinding>]
        static member getEyeSize world =
            (World.getGameState world).EyeSize

        /// Set the current eye size.
        [<FunctionBinding>]
        static member setEyeSize value world =
            World.updateGameState (fun gameState -> { gameState with EyeSize = value }) Property? EyeSize world

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        [<FunctionBinding>]
        static member setOmniScreenOpt value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreen to SelectedScreen."
            World.updateGameState (fun gameState -> { gameState with OmniScreenOpt = value }) Property? OmniScreenOpt world

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
            if Option.isSome value && World.getOmniScreenOpt world = value then failwith "Cannot set SelectedScreen to OmniScreen."
            World.updateGameState (fun gameState -> { gameState with SelectedScreenOpt = value }) Property? SelectedScreenOpt world

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
            World.updateGameState (fun gameState -> { gameState with ScreenTransitionDestinationOpt = destination }) Property? ScreenTransitionDestinationOpt world

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
        static member getViewBounds viewType world =
            match viewType with
            | Relative -> World.getViewBoundsRelative world
            | Absolute -> World.getViewBoundsAbsolute world

        /// Check that the given bounds is within the eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView viewType (bounds : Vector4) world =
            let viewBounds = World.getViewBounds viewType world
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
        static member mouseToWorld viewType mousePosition world =
            let positionScreen = World.mouseToScreen mousePosition world
            let view =
                match viewType with
                | Relative -> World.getViewRelative world
                | Absolute -> World.getViewAbsolute world
            let positionWorld = positionScreen * view
            positionWorld

        /// Transform the given mouse position to entity space.
        [<FunctionBinding>]
        static member mouseToEntity viewType entityPosition mousePosition world =
            let mousePositionWorld = World.mouseToWorld viewType mousePosition world
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

        static member internal tryGetGameCalculatedProperty propertyName world =
            let game = Game ()
            let dispatcher = World.getGameDispatcher world
            dispatcher.TryGetCalculatedProperty (propertyName, game, world)

        static member internal tryGetGameProperty propertyName world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match GameState.tryGetProperty propertyName (World.getGameState world) with
                | None -> World.tryGetGameCalculatedProperty propertyName world
                | Some _ as propertyOpt -> propertyOpt
            | (true, getter) -> Some (getter world)

        static member internal getGameProperty propertyName world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match GameState.tryGetProperty propertyName (World.getGameState world) with
                | None ->
                    match World.tryGetGameCalculatedProperty propertyName world with
                    | None -> failwithf "Could not find property '%s'." propertyName
                    | Some property -> property
                | Some property -> property
            | (true, getter) -> getter world

        static member internal trySetGameProperty propertyName property world =
            match Setters.TryGetValue propertyName with
            | (false, _) ->
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let world =
                    World.updateGameState (fun entityState ->
                        let (successInner, entityState) = GameState.trySetProperty propertyName property entityState
                        success <- successInner
                        entityState)
                        propertyName world
                (success, world)
            | (true, setter) -> setter property world

        static member internal setGameProperty propertyName property world =
            match Setters.TryGetValue propertyName with
            | (false, _) -> World.updateGameState (GameState.setProperty propertyName property) propertyName world
            | (true, setter) ->
                match setter property world with
                | (true, world) -> world
                | (false, _) -> failwith ("Cannot change game property " + propertyName + ".")

        static member internal attachGameProperty propertyName property world =
            World.updateGameState (GameState.attachProperty propertyName property) propertyName world

        static member internal detachGameProperty propertyName world =
            World.updateGameState (GameState.detachProperty propertyName) propertyName world

        static member internal writeGame3 writeScreens gameDescriptor world =
            let gameState = World.getGameState world
            let gameDispatcherName = getTypeName gameState.Dispatcher
            let gameDescriptor = { gameDescriptor with GameDispatcherName = gameDispatcherName }
            let viewGameProperties = Reflection.writePropertiesFromTarget tautology3 gameDescriptor.GameProperties gameState
            let gameDescriptor = { gameDescriptor with GameProperties = viewGameProperties }
            writeScreens gameDescriptor world

        static member internal readGame3 readScreens gameDescriptor world =

            // create the dispatcher
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
        Getters.Add ("Id", fun world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId world })
        Getters.Add ("Dispatcher", fun world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher world })
        Getters.Add ("CreationTimeStamp", fun world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameCreationTimeStamp world })
        Getters.Add ("ScriptOpt", fun world -> { PropertyType = typeof<Symbol AssetTag option>; PropertyValue = World.getGameScriptOpt world })
        Getters.Add ("Script", fun world -> { PropertyType = typeof<Scripting.Expr array>; PropertyValue = World.getGameScript world })
        Getters.Add ("ScriptUnsubscriptions", fun world -> { PropertyType = typeof<Unsubscription list>; PropertyValue = World.getGameScriptUnsubscriptions world })
        Getters.Add ("ScriptFrame", fun world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScript world })
        Getters.Add ("OnRegister", fun world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getGameOnRegister world })
        Getters.Add ("OnUnregister", fun world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getGameOnUnregister world })
        Getters.Add ("OnUpdate", fun world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getGameOnUpdate world })
        Getters.Add ("OnPostUpdate", fun world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getGameOnPostUpdate world })
        Getters.Add ("OnSignal", fun world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getGameOnSignal world })
        Getters.Add ("OmniScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        Getters.Add ("SelectedScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        Getters.Add ("ScreenTransitionDestinationOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getScreenTransitionDestinationOpt world })
        Getters.Add ("EyeCenter", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeCenter world })
        Getters.Add ("EyeSize", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeSize world })

    /// Initialize property setters.
    let private initSetters () =
        Setters.Add ("Id", fun _ world -> (false, world))
        Setters.Add ("Dispatcher", fun _ world -> (false, world))
        Setters.Add ("CreationTimeStamp", fun _ world -> (false, world))
        Setters.Add ("Imperative", fun _ world -> (false, world))
        Setters.Add ("ScriptOpt", fun property world -> (true, World.setGameScriptOpt (property.PropertyValue :?> Symbol AssetTag option) world))
        Setters.Add ("Script", fun property world -> (true, World.setGameScript (property.PropertyValue :?> Scripting.Expr array) world))
        Setters.Add ("ScriptFrame", fun _ world -> (false, world))
        Setters.Add ("ScriptUnsubscriptions", fun property world -> (true, World.setGameScriptUnsubscriptions (property.PropertyValue :?> Unsubscription list) world))
        Setters.Add ("OnRegister", fun property world -> (true, World.setGameOnRegister (property.PropertyValue :?> Scripting.Expr) world))
        Setters.Add ("OnUnregister", fun property world -> (true, World.setGameOnUnregister (property.PropertyValue :?> Scripting.Expr) world))
        Setters.Add ("OnUpdate", fun property world -> (true, World.setGameOnUpdate (property.PropertyValue :?> Scripting.Expr) world))
        Setters.Add ("OnPostUpdate", fun property world -> (true, World.setGameOnPostUpdate (property.PropertyValue :?> Scripting.Expr) world))
        Setters.Add ("OnSignal", fun property world -> (true, World.setGameOnSignal (property.PropertyValue :?> Scripting.Expr) world))
        Setters.Add ("OmniScreenOpt", fun property world -> (true, World.setOmniScreenOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("SelectedScreenOpt", fun property world -> (true, World.setSelectedScreenOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("ScreenTransitionDestinationOpt", fun property world -> (true, World.setScreenTransitionDestinationOpt (property.PropertyValue :?> Screen option) world))
        Setters.Add ("EyeCenter", fun property world -> (true, World.setEyeCenter (property.PropertyValue :?> Vector2) world))
        Setters.Add ("EyeSize", fun property world -> (true, World.setEyeSize (property.PropertyValue :?> Vector2) world))

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()