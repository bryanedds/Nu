// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open FSharpx.Collections
open Prime
open Nu

/// A command that transforms the world in some manner.
type [<NoEquality; NoComparison>] 'w Command =
    { Execute : 'w -> 'w }

/// A tasklet to be completed at the scheduled tick time.
type [<NoEquality; NoComparison>] 'w Tasklet =
    { ScheduledTime : int64
      Command : 'w Command }

[<AutoOpen>]
module AmbientStateModule =

    /// The ambient state of the world.
    type [<ReferenceEquality>] 'w AmbientState =
        private
            { TickRate : int64
              TickTime : int64
              UpdateCount : int64
              Liveness : Liveness
              Tasklets : 'w Tasklet Queue
              Camera : Camera
              AssetMetadataMap : AssetMetadataMap
              Overlayer : Overlayer
              OverlayRouter : OverlayRouter
              SymbolStore : SymbolStore
              UserState : obj }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module AmbientState =

        /// Get the tick rate.
        let getTickRate state =
            state.TickRate

        /// Get the tick rate as a floating-point value.
        let getTickRateF state =
            single ^ getTickRate state

        /// Set the tick rate without waiting for the end of the current update. Only use
        /// this if you need it and understand the engine internals well enough to know the
        /// consequences.
        let setTickRateImmediately tickRate state =
            { state with TickRate = tickRate }

        /// Reset the tick time to 0.
        let resetTickTime state =
            { state with TickTime = 0L }

        /// Get the tick time.
        let getTickTime state =
            state.TickTime

        /// Query that ticking is enabled.
        let isTicking state =
            getTickRate state <> 0L

        /// Update the tick time by the tick rate.
        let updateTickTime state =
            { state with TickTime = getTickTime state + getTickRate state }

        /// Get the world's update count.
        let getUpdateCount state =
            state.UpdateCount

        /// Increment the update count.
        let incrementUpdateCount state =
            { state with UpdateCount = inc ^ getUpdateCount state }

        /// Get the the liveness state of the engine.
        let getLiveness state =
            state.Liveness

        /// Place the engine into a state such that the app will exit at the end of the current update.
        let exit state =
            { state with Liveness = Exiting }

        /// Get a value from the camera.
        let getCameraBy by state =
            by state.Camera

        /// Get the camera used to view the scene.
        let getCamera state =
            getCameraBy id state

        /// Update the camera used to view the scene.
        let updateCamera updater state =
            let camera = updater ^ getCamera state
            { state with Camera = camera }

        /// Get the tasklets scheduled for future processing.
        let getTasklets state =
            state.Tasklets

        /// Clear the tasklets from future processing.
        let clearTasklets state =
            { state with Tasklets = Queue.empty }

        /// Restore the given tasklets from future processing.
        let restoreTasklets tasklets state =
            { state with Tasklets = Queue.ofSeq ^ Seq.append (state.Tasklets :> _ seq) (tasklets :> _ seq) }

        /// Add a tasklet to be executed at the scheduled time.
        let addTasklet tasklet state =
            { state with Tasklets = Queue.conj tasklet state.Tasklets }

        /// Add multiple tasklets to be executed at the scheduled times.
        let addTasklets tasklets state =
            { state with Tasklets = Queue.ofSeq ^ Seq.append (tasklets :> _ seq) (state.Tasklets :> _ seq) }

        /// Get the asset metadata map.
        let getAssetMetadataMap state =
            state.AssetMetadataMap
    
        /// Set the asset metadata map.
        let setAssetMetadataMap assetMetadataMap state =
            { state with AssetMetadataMap = assetMetadataMap }
    
        /// Get the overlayer.
        let getOverlayer state =
            state.Overlayer
    
        /// Set the overlayer.
        let setOverlayer overlayer state =
            { state with Overlayer = overlayer }
    
        /// Get the overlay router.
        let getOverlayRouter state =
            state.OverlayRouter

        /// Get a value from the symbol store.
        let getSymbolStoreBy by state =
            by state.SymbolStore

        /// Get the symbol store.
        let getSymbolStore state =
            getSymbolStoreBy id state
    
        /// Set the symbol store.
        let setSymbolStore symbolStore state =
            { state with SymbolStore = symbolStore }

        /// Update the symbol store.
        let updateSymbolStore updater state =
            let camera = updater ^ getSymbolStore state
            { state with SymbolStore = camera }
    
        /// Get the user-defined state, casted to 'u.
        let getUserState state : 'u =
            state.UserState :?> 'u
    
        /// Update the user state of the world.
        let updateUserState (updater : 'u -> 'v) state =
            let userState = getUserState state
            let userState = updater userState
            { state with UserState = userState }
    
        /// Make an ambient state value.
        let make tickRate camera assetMetadataMap overlayRouter overlayer symbolStore userState =
            { TickRate = tickRate
              TickTime = 0L
              UpdateCount = 0L
              Liveness = Running
              Tasklets = Queue.empty
              Camera = camera
              AssetMetadataMap = assetMetadataMap
              OverlayRouter = overlayRouter
              Overlayer = overlayer
              SymbolStore = symbolStore
              UserState = userState }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientStateModule.AmbientState