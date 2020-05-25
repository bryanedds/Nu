// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open SDL2
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

    /// Tracks the state of tasklet processing.
    let mutable private TaskletsProcessing = false

    /// The ambient state of the world.
    type [<ReferenceEquality; NoComparison>] 'w AmbientState =
        private
            { // cache line begin
              TickRate : int64 // NOTE: might be better to make this accessible from World to avoid cache misses
              TickTime : int64
              UpdateCount : int64
              ClockDelta : TimeSpan  // NOTE: might be better to make this accessible from World to avoid cache misses
              Liveness : Liveness
              Metadata : Metadata
              KeyValueStore : UMap<Guid, obj>
              // cache line ends here (actually, half-way through ClockTime below)
              ClockTime : DateTimeOffset // moved down here because it's 16 bytes according to - https://stackoverflow.com/a/38731608
              Tasklets : 'w Tasklet UList
              SdlDepsOpt : SdlDeps option
              SymbolStore : SymbolStore
              Overlayer : Overlayer
              OverlayRouter : OverlayRouter }

    [<RequireQualifiedAccess>]
    module AmbientState =

        /// Get the tick rate.
        let getTickRate state =
            state.TickRate

        /// Get the tick rate as a floating-point value.
        let getTickRateF state =
            single (getTickRate state)

        /// Set the tick rate.
        let setTickRateImmediate tickRate state =
            { state with TickRate = tickRate }

        /// Reset the tick time to 0.
        let resetTickTimeImmediate state =
            { state with TickTime = 0L }

        /// Increment the tick time.
        let incTickTimeImmediate state =
            { state with TickTime = inc state.TickTime }

        /// Increment the tick time.
        let decTickTimeImmediate state =
            { state with TickTime = dec state.TickTime }

        /// Get the tick time.
        let getTickTime state =
            state.TickTime

        /// Check that ticking is enabled.
        let isTicking state =
            getTickRate state <> 0L

        /// Get the world's update count.
        let getUpdateCount state =
            state.UpdateCount

        /// Get the clock delta.
        let getClockDelta state =
            state.ClockDelta

        /// Get the clock time.
        let getClockTime state =
            state.ClockTime

        /// Update the tick and clock times.
        let updateTime state =
            let now = DateTimeOffset.UtcNow
            { state with
                TickTime = state.TickTime + state.TickRate
                UpdateCount = inc state.UpdateCount
                ClockDelta = now - state.ClockTime
                ClockTime = now }

        /// Get the the liveness state of the engine.
        let getLiveness state =
            state.Liveness

        /// Place the engine into a state such that the app will exit at the end of the current update.
        let exit state =
            { state with Liveness = Exiting }

        /// Get the metadata.
        let getMetadata state =
            state.Metadata
    
        /// Set the metadata.
        let setMetadata metadata state =
            { state with Metadata = metadata }

        /// Get the key-value store with the by map.
        let getKeyValueStoreBy by state =
            by state.KeyValueStore

        /// Get the key-value store.
        let getKeyValueStore state =
            getKeyValueStoreBy id state
    
        /// Set the key-value store.
        let setKeyValueStore symbolStore state =
            { state with KeyValueStore = symbolStore }

        /// Update the key-value store.
        let updateKeyValueStore updater state =
            let store = updater (getKeyValueStore state)
            { state with KeyValueStore = store }

        /// Get the tasklets scheduled for future processing.
        let getTasklets state =
            state.Tasklets

        /// Clear the tasklets from future processing.
        let clearTasklets state =
            TaskletsProcessing <- true
            { state with Tasklets = UList.makeEmpty (UList.getConfig state.Tasklets) }

        /// Restore the given tasklets from future processing.
        let restoreTasklets tasklets state =
            let state = { state with Tasklets = UList.makeFromSeq (UList.getConfig state.Tasklets) (Seq.append (state.Tasklets :> _ seq) (tasklets :> _ seq)) }
            TaskletsProcessing <- false
            state

        /// Get the tasklets processing state.
        let getTaskletsProcessing (_ : 'w AmbientState) =
            TaskletsProcessing

        /// Add a tasklet to be executed at the scheduled time.
        let addTasklet tasklet state =
            { state with Tasklets = UList.add tasklet state.Tasklets }

        /// Add multiple tasklets to be executed at the scheduled times.
        let addTasklets tasklets state =
            { state with Tasklets = UList.makeFromSeq (UList.getConfig state.Tasklets) (Seq.append (tasklets :> _ seq) (state.Tasklets :> _ seq)) }

        /// Attempt to get the window flags.
        let tryGetWindowFlags state =
            match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
            | Some window -> Some (SDL.SDL_GetWindowFlags window)
            | None -> None

        /// Attempt to check that the window is minimized.
        let tryGetWindowMinimized state =
            Option.map (fun flags -> flags ||| uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED = 0u) (tryGetWindowFlags state)

        /// Attempt to check that the window is maximized.
        let tryGetWindowMaximized state =
            Option.map (fun flags -> flags ||| uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MAXIMIZED = 0u) (tryGetWindowFlags state)

        /// Get the symbol store with the by map.
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
            let store = updater (getSymbolStore state)
            { state with SymbolStore = store }
    
        /// Get the overlayer.
        let getOverlayer state =
            state.Overlayer
    
        /// Set the overlayer.
        let setOverlayer overlayer state =
            { state with Overlayer = overlayer }
    
        /// Get the overlay router.
        let getOverlayRouter state =
            state.OverlayRouter
    
        /// Get the overlay router.
        let getOverlayRouterBy by state =
            by state.OverlayRouter
    
        /// Make an ambient state value.
        let make tickRate assetMetadataMap overlayRouter overlayer symbolStore sdlDepsOpt =
            { TickRate = tickRate
              TickTime = 0L
              ClockDelta = TimeSpan.Zero
              ClockTime = DateTimeOffset.Now
              UpdateCount = 0L
              Liveness = Running
              Tasklets = UList.makeEmpty Constants.Engine.TaskletListConfig
              Metadata = assetMetadataMap
              OverlayRouter = overlayRouter
              Overlayer = overlayer
              SymbolStore = symbolStore
              KeyValueStore = UMap.makeEmpty Constants.Engine.KeyValueMapConfig
              SdlDepsOpt = sdlDepsOpt }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientStateModule.AmbientState