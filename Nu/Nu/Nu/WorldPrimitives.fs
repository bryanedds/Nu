// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx
open FSharpx.Collections
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member internal toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member internal toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Query that the given mouse button is down.
        static member isMouseButtonDown mouseButton (_ : World) =
            MouseState.isButtonDown mouseButton

        /// Get the position of the mouse.
        static member getMousePosition (_ : World) =
            MouseState.getPosition ()

        /// Get the position of the mouse in floating-point coordinates.
        static member getMousePositionF (_ : World) =
            MouseState.getPositionF ()

        /// Query that the given keyboard key is down.
        static member isKeyboardKeyDown scanCode (_ : World) =
            KeyboardState.isKeyDown scanCode

        // TODO: implement isKeyboardModifierActive.

[<AutoOpen>]
module WorldSubsystemsModule =

    type World with

        static member internal getSubsystem<'s when 's :> Subsystem> name world =
            Map.find name world.Subsystems :?> 's

        static member internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
            let subsystem = World.getSubsystem<'s> name world
            by subsystem

        static member internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
            let subsystems = Map.add name (subsystem :> Subsystem) world.Subsystems
            { world with Subsystems = subsystems }

        static member internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
            let subsystem = World.getSubsystem<'s> name world
            let subsystem = updater subsystem world
            World.setSubsystem subsystem name world

        static member internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
            Map.fold
                (fun world name subsystem -> let subsystem = updater subsystem world in World.setSubsystem subsystem name world)
                world
                world.Subsystems

        static member internal clearSubsystemsMessages world =
            World.updateSubsystems (fun is _ -> is.ClearMessages ()) world

        /// Add a physics message to the world.
        static member addPhysicsMessage (message : PhysicsMessage) world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

        /// Add a rendering message to the world.
        static member addRenderMessage (message : RenderMessage) world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

[<AutoOpen>]
module WorldCallbacksModule =

    type World with

        static member internal clearTasklets world =
            let callbacks = { world.Callbacks with Tasklets = Queue.empty }
            { world with Callbacks = callbacks }

        static member internal restoreTasklets (tasklets : Tasklet Queue) world =
            let callbacks = { world.Callbacks with Tasklets = Queue.ofSeq <| Seq.append (world.Callbacks.Tasklets :> Tasklet seq) (tasklets :> Tasklet seq) }
            { world with Callbacks = callbacks }

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            let callbacks = { world.Callbacks with Tasklets = Queue.conj tasklet world.Callbacks.Tasklets }
            { world with Callbacks = callbacks }

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            let callbacks = { world.Callbacks with Tasklets = Queue.ofSeq <| Seq.append (tasklets :> Tasklet seq) (world.Callbacks.Tasklets :> Tasklet seq) }
            { world with Callbacks = callbacks }

        /// Add callback state to the world.
        static member addCallbackState key state world =
            let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Remove callback state from the world.
        static member removeCallbackState key world =
            let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Get callback state from the world.
        static member getCallbackState<'a> key world =
            let state = Map.find key world.Callbacks.CallbackStates
            state :?> 'a

[<AutoOpen>]
module WorldComponentsModule =

    type World with

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.Components.EntityDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.Components.GroupDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.Components.ScreenDispatchers

        /// Get the entity dispatchers of the world.
        static member getGameDispatchers world =
            world.Components.GameDispatchers

        /// Get the entity dispatchers of the world.
        static member getFacets world =
            world.Components.Facets

[<AutoOpen>]
module WorldStateModule =

    type World with

        static member private getState world =
            world.State

        static member private setStateWithoutEvent state world =
            { world with State = state }

        static member private setState state world =
            let oldWorld = world
            let world = World.setStateWithoutEvent state world
            World.publish4 { WorldStateChangeData.OldWorld = oldWorld } WorldStateChangeEventAddress Game world

        /// Get the world's tick rate.
        static member getTickRate world =
            world.State.TickRate

        /// Get the world's tick rate as a floating-point value.
        static member getTickRateF world =
            single <| World.getTickRate world

        /// Set the world's tick rate without waiting for the end of the current update. Only use
        /// this if you need it and understand the engine internals well enough to know the
        /// consequences.
        static member setTickRateImmediately tickRate world =
            let state = { world.State with TickRate = tickRate }
            World.setState state world

        /// Set the world's tick rate.
        static member setTickRate tickRate world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Operation = fun world -> World.setTickRateImmediately tickRate world }
            World.addTasklet tasklet world

        /// Get the world's tick time.
        static member getTickTime world =
            world.State.TickTime

        /// Query that the world is ticking.
        static member isTicking world =
            World.getTickRate world <> 0L

        static member internal updateTickTime world =
            let state = { world.State with TickTime = World.getTickTime world + World.getTickRate world }
            World.setStateWithoutEvent state world

        /// Get the world's update count.
        static member getUpdateCount world =
            world.State.UpdateCount

        static member internal incrementUpdateCount world =
            let state = { world.State with UpdateCount = inc <| World.getUpdateCount world }
            World.setStateWithoutEvent state world

        /// Get the the liveness state of the world.
        static member getLiveness world =
            world.State.Liveness

        /// Place the engine into a state such that the app will exit at the end of the current update.
        static member exit world =
            let state = { world.State with Liveness = Exiting }
            World.setState state world

        /// Get the a value from the camera used to view the world.
        static member getCameraBy by world =
            by world.State.Camera

        /// Get the camera used to view the world.
        static member getCamera world =
            World.getCameraBy id world

        static member private setCamera camera world =
            let state = { world.State with Camera = camera }
            World.setState state world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            let camera = updater <| World.getCamera world
            World.setCamera camera world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            world.State.OptScreenTransitionDestination

        static member internal setOptScreenTransitionDestination destination world =
            let state = { world.State with OptScreenTransitionDestination = destination }
            World.setState state world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            world.State.AssetMetadataMap

        static member internal setAssetMetadataMap assetMetadataMap world =
            let state = { world.State with AssetMetadataMap = assetMetadataMap }
            World.setState state world

        static member internal setOverlayer overlayer world =
            let state = { world.State with Overlayer = overlayer }
            World.setState state world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            world.State.UserState :?> 'u

        static member private setUserState (userState : 'u) world =
            let state = { world.State with UserState = userState }
            World.setState state world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            let state = World.getUserState world
            let state = updater state
            World.setUserState state world

[<AutoOpen>]
module WorldEntityStateModule =

    type World with

        static member private optEntityStateKeyEquality 
            (entityAddress : Entity Address, world : World)
            (entityAddress2 : Entity Address, world2 : World) =
            entityAddress === entityAddress2 && world === world2

        static member private optEntityGetFreshKeyAndValue entity world =
            let optEntityState =
                match entity.EntityAddress.NameKeys with
                | [screenNameKey; groupNameKey; entityNameKey] ->
                    let (_, screenStateMap) = world.SimulantStates 
                    match Map.tryFind screenNameKey.Name screenStateMap with
                    | Some (_, groupStateMap) ->
                        match Map.tryFind groupNameKey.Name groupStateMap with
                        | Some (_, entityStateMap) -> Map.tryFind entityNameKey.Name entityStateMap
                        | None -> None
                    | None -> None
                | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."
            ((entity.EntityAddress, world), optEntityState)

        static member private optEntityStateFinder entity world =
            KeyedCache.getValue
                World.optEntityStateKeyEquality
                (fun () -> World.optEntityGetFreshKeyAndValue entity world)
                (entity.EntityAddress, world)
                world.State.OptEntityCache

        static member private entityStateAdder (entityState : EntityState) entity world =
            match entity.EntityAddress.NameKeys with
            | [screenNameKey; groupNameKey; entityNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.add entityNameKey.Name entityState entityStateMap
                        let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateRemover entity world =
            match entity.EntityAddress.NameKeys with
            | [screenNameKey; groupNameKey; entityNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.remove entityNameKey.Name entityStateMap
                        let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member internal getEntityStateMap group world =
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (_, entityStateMap) -> entityStateMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member internal getEntityState (entity : Entity) world =
            (World.optEntityStateFinder entity world).Value // OPTIMIZATION: getting entity state as directly as possible

        static member internal setEntityStateWithoutEvent entityState entity world =
            World.entityStateAdder entityState entity world

        static member internal setOptEntityStateWithoutEvent optEntityState entity world =
            match optEntityState with 
            | Some entityState -> World.entityStateAdder entityState entity world
            | None -> World.entityStateRemover entity world

        static member internal setEntityState entityState (entity : Entity) world =
            let oldWorld = world
            let world = World.entityStateAdder entityState entity world
            if entityState.PublishChanges then
                World.publish4
                    { Simulant = entity; OldWorld = oldWorld }
                    (EntityChangeEventAddress ->>- entity.EntityAddress)
                    entity
                    world
            else world

        static member internal updateEntityState updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

[<AutoOpen>]
module WorldGroupStateModule =

    type World with

        static member private optGroupStateFinder group world =
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (groupState, _) -> Some groupState
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateAdder (groupState : GroupState) group world =
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (_, entityStateMap) ->
                        let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None ->
                        let groupStateMap = Map.add groupNameKey.Name (groupState, Map.empty) groupStateMap
                        let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                | None -> failwith <| "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateRemover group world =
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (_, entityStateMap) ->
                        if Map.isEmpty entityStateMap then
                            let groupStateMap = Map.remove groupNameKey.Name groupStateMap
                            let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                            { world with SimulantStates = (gameState, screenStateMap) }
                        else failwith <| "Cannot remove group " + acstring group.GroupAddress + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getGroupStateMap screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) -> groupStateMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getOptGroupState group world =
            World.optGroupStateFinder group world

        static member internal getGroupState group world =
            (World.optGroupStateFinder group world).Value // OPTIMIZATION: getting entity state as directly as possible

        static member internal setGroupStateWithoutEvent groupState group world =
            World.groupStateAdder groupState group world

        static member internal setOptGroupStateWithoutEvent optGroupState group world =
            match optGroupState with 
            | Some groupState -> World.groupStateAdder groupState group world
            | None -> World.groupStateRemover group world

        static member internal setGroupState groupState group world =
            let oldWorld = world
            let world = World.groupStateAdder groupState group world
            if groupState.PublishChanges then
                World.publish4
                    { Simulant = group; OldWorld = oldWorld }
                    (GroupChangeEventAddress ->>- group.GroupAddress)
                    group
                    world
            else world

        static member internal updateGroupState updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

[<AutoOpen>]
module WorldScreenStateModule =

    type World with

        static member private optScreenStateFinder screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, _) -> Some screenState
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateAdder (screenState : ScreenState) screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None ->
                    let screenStateMap = Map.add screenNameKey.Name (screenState, Map.empty) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateRemover screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    if Map.isEmpty groupStateMap then
                        let screenStateMap = Map.remove screenNameKey.Name screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    else failwith <| "Cannot remove screen " + acstring screen.ScreenAddress + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getScreenStateMap world =
            snd world.SimulantStates

        static member internal getOptScreenState screen world =
            World.optScreenStateFinder screen world

        static member internal getScreenState screen world =
            (World.optScreenStateFinder screen world).Value // OPTIMIZATION: getting entity state as directly as possible

        static member internal setScreenStateWithoutEvent screenState screen world =
            World.screenStateAdder screenState screen world

        static member internal setOptScreenStateWithoutEvent optScreenState screen world =
            match optScreenState with
            | Some screenState -> World.screenStateAdder screenState screen world
            | None -> World.screenStateRemover screen world

        static member internal setScreenState screenState screen world =
            let oldWorld = world
            let world = World.screenStateAdder screenState screen world
            if screenState.PublishChanges then
                World.publish4
                    { Simulant = screen; OldWorld = oldWorld }
                    (ScreenChangeEventAddress ->>- screen.ScreenAddress)
                    screen
                    world
            else world

        static member internal updateScreenState updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

[<AutoOpen>]
module WorldGameStateModule =

    type World with

        static member internal getGameStateMap world =
            let gameState = World.getGameState world
            let screenStateMap = World.getScreenStateMap world
            (gameState, screenStateMap)

        static member internal getGameState world : GameState =
            fst world.SimulantStates

        static member internal setGameState gameState world =
            let oldWorld = world
            let screenStateMap = World.getScreenStateMap world
            let world = { world with SimulantStates = (gameState, screenStateMap) }
            if gameState.PublishChanges then
                World.publish4
                    { OldWorld = oldWorld; Simulant = Game }
                    (GameChangeEventAddress ->>- Game.GameAddress)
                    Game
                    world
            else world

        static member internal updateGameState updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world