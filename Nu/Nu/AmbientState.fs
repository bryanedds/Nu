// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open SDL2
open Prime
open Nu

/// A tasklet to be completed at the scheduled tick time.
type [<NoEquality; NoComparison>] 'w Tasklet =
    { ScheduledTime : int64
      ScheduledOp : 'w -> 'w }

[<AutoOpen>]
module AmbientState =

    /// Tracks whether the engine is running imperatively.
    let mutable private Imperative = true

    /// Tracks whether the engine is running stand-alone.
    let mutable private StandAlone = true

    /// Tracks the state of tasklet processing.
    let mutable private TaskletsProcessing = false

    /// The ambient state of the world.
    type [<ReferenceEquality; NoComparison>] 'w AmbientState =
        private
            { // cache line 1 (assuming 16 byte header)
              Liveness : Liveness
              TickRate : int64 // NOTE: might be better to make this accessible from World to avoid cache misses
              TickTime : int64
              ClockDelta : single // NOTE: might be better to make this accessible from World to avoid cache misses
              // cache line 2
              Metadata : Metadata
              KeyValueStore : UMap<Guid, obj>
              ClockTime : DateTimeOffset // moved down here because it's 16 bytes according to - https://stackoverflow.com/a/38731608
              Tasklets : 'w Tasklet UList
              SdlDepsOpt : SdlDeps option
              SymbolStore : SymbolStore
              Overlayer : Overlayer
              // cache line 3 (oof!) - TODO: P1: see if we can reduce the size of this type to fit in 2 cache lines.
              OverlayRouter : OverlayRouter }

    /// Get whether the engine is running imperatively.
    let getImperative (_ : 'w AmbientState) =
        Imperative

    /// Get whether the engine is running stand-alone.
    let getStandAlone (_ : 'w AmbientState) =
        StandAlone

    /// Get the collection config value.
    let getCollectionConfig (_ : 'w AmbientState) =
        if Imperative then TConfig.Imperative else TConfig.Functional

    /// Get the the liveness state of the engine.
    let getLiveness state =
        state.Liveness

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

    /// Get the clock delta as a floating point number.
    let getClockDelta state =
        state.ClockDelta

    /// Get the clock time.
    let getClockTime state =
        state.ClockTime

    /// Update the tick and clock times.
    let updateTime state =
        let now = DateTimeOffset.UtcNow
        let delta = now - state.ClockTime
        let frameProgress = 1000.0f / single Constants.Engine.DesiredFps * single state.TickRate
        let clockDelta = single delta.TotalMilliseconds / frameProgress
        let clockDeltaNormalized = if clockDelta < 4.0f then clockDelta else 1.0f // assume timing is unnatural about a 4 frame delay
        { state with
            TickTime = state.TickTime + state.TickRate
            ClockDelta = clockDeltaNormalized
            ClockTime = now }

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit state =
        { state with Liveness = Dead }

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
    let make imperative standAlone tickRate assetMetadataMap overlayRouter overlayer symbolStore sdlDepsOpt =
        Imperative <- imperative
        StandAlone <- standAlone
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Liveness = Live
          TickRate = tickRate
          TickTime = 0L
          ClockDelta = 1.0f
          ClockTime = DateTimeOffset.Now
          Tasklets = UList.makeEmpty config
          Metadata = assetMetadataMap
          OverlayRouter = overlayRouter
          Overlayer = overlayer
          SymbolStore = symbolStore
          KeyValueStore = UMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientState.AmbientState