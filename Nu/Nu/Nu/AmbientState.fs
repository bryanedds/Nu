namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module AmbientStateModule =

    /// The ambient state of the world.
    type [<ReferenceEquality>] AmbientState =
        private
            { TickRate : int64
              TickTime : int64
              UpdateCount : int64
              Liveness : Liveness
              AssetMetadataMap : AssetMetadataMap
              Overlayer : Overlayer
              OverlayRouter : OverlayRouter
              Camera : Camera
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
    
        /// Get the camera used to view the world.
        let getCamera state =
            getCameraBy id state
    
        /// Update the camera used to view the world.
        let updateCamera updater state =
            let camera = updater ^ getCamera state
            { state with Camera = camera }
    
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
    
        /// Get the user-defined state, casted to 'u.
        let getUserState state : 'u =
            state.UserState :?> 'u
    
        /// Update the user state of the world.
        let updateUserState (updater : 'u -> 'v) state =
            let userState = getUserState state
            let userState = updater userState
            { state with UserState = userState }
    
        /// Make an ambient state value.
        let make tickRate assetMetadataMap overlayRouter overlayer camera userState =
            { TickRate = tickRate
              TickTime = 0L
              UpdateCount = 0L
              Liveness = Running
              AssetMetadataMap = assetMetadataMap
              OverlayRouter = overlayRouter
              Overlayer = overlayer
              Camera = camera
              UserState = userState }