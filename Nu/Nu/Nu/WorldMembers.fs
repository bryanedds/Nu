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

        static member internal clearTasks world =
            let callbacks = { world.Callbacks with Tasks = Queue.empty }
            { world with Callbacks = callbacks }

        static member internal restoreTasks (tasks : Task Queue) world =
            let callbacks = { world.Callbacks with Tasks = Queue.ofSeq <| Seq.append (world.Callbacks.Tasks :> Task seq) (tasks :> Task seq) }
            { world with Callbacks = callbacks }

        /// Add a task to be executed by the engine at the specified task tick.
        static member addTask task world =
            let callbacks = { world.Callbacks with Tasks = Queue.conj task world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add multiple task to be executed by the engine at the specified task tick.
        static member addTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = Queue.ofSeq <| Seq.append (tasks :> Task seq) (world.Callbacks.Tasks :> Task seq) }
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
            let oldState = world.State
            let world = World.setStateWithoutEvent state world
            World.publish4 { OldWorldState = oldState } WorldStateChangeEventAddress Game world

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
            let task =
                { ScheduledTime = World.getTickTime world
                  Operation = fun world -> World.setTickRateImmediately tickRate world }
            World.addTask task world

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