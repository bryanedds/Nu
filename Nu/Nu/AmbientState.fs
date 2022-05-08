// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open SDL2
open Prime
open Nu

/// A tasklet to be completed at the scheduled update time.
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
              UpdateRate : int64 // NOTE: might be better to make this accessible from World to avoid cache misses
              UpdateTime : int64
              ClockDelta : single // NOTE: might be better to make this accessible from World to avoid cache misses
              // cache line 2
              Metadata : Metadata
              KeyValueStore : UMap<Guid, obj>
              ClockTime : DateTimeOffset // moved down here because it's 16 bytes according to - https://stackoverflow.com/a/38731608
              Tasklets : OMap<Simulant, 'w Tasklet UList>
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
    let getConfig (_ : 'w AmbientState) =
        if Imperative then TConfig.Imperative else TConfig.Functional

    /// Get the the liveness state of the engine.
    let getLiveness state =
        state.Liveness

    /// Get the update rate.
    let getUpdateRate state =
        state.UpdateRate

    /// Set the update rate.
    let setUpdateRateImmediate updateRate state =
        { state with UpdateRate = updateRate }

    /// Check that update rate is non-zero.
    let isAdvancing state =
        getUpdateRate state <> 0L

    /// Check that update rate is zero.
    let isHalted state =
        getUpdateRate state = 0L

    /// Get the update time.
    let getUpdateTime state =
        state.UpdateTime

    /// Get the clock delta as a floating point number.
    let getClockDelta state =
        state.ClockDelta

    /// Get the clock time.
    let getClockTime state =
        state.ClockTime

    /// Update the update and clock times.
    let updateTime state =
        let now = DateTimeOffset.UtcNow
        let delta = now - state.ClockTime
        let frameProgress = 1000.0f / single Constants.Engine.DesiredFps * single state.UpdateRate
        let clockDelta = single delta.TotalMilliseconds / frameProgress
        let clockDeltaNormalized = if clockDelta < 4.0f then clockDelta else 1.0f // assume timing is unnatural about a 4 frame delay
        { state with
            UpdateTime = state.UpdateTime + state.UpdateRate
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

    /// Remove all tasklets associated with a simulant.
    let removeTasklets simulant state =
        { state with Tasklets = OMap.remove simulant state.Tasklets }

    /// Clear the tasklets from future processing.
    let clearTasklets state =
        TaskletsProcessing <- true
        { state with Tasklets = OMap.makeEmpty HashIdentity.Structural (OMap.getConfig state.Tasklets) }

    /// Restore the given tasklets from future processing.
    let restoreTasklets tasklets state =
        let state = { state with Tasklets = tasklets }
        TaskletsProcessing <- false
        state

    /// Get the tasklets processing state.
    let getTaskletsProcessing (_ : 'w AmbientState) =
        TaskletsProcessing

    /// Add a tasklet to be executed at the scheduled time.
    let addTasklet simulant tasklet state =
        { state with
            Tasklets =
                match state.Tasklets.TryGetValue simulant with
                | (true, taskletList) -> OMap.add simulant (UList.add tasklet taskletList) state.Tasklets
                | (false, _) -> OMap.add simulant (UList.singleton (OMap.getConfig state.Tasklets) tasklet) state.Tasklets }

    /// Attempt to get the window flags.
    let tryGetWindowFlags state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) -> Some (SDL.SDL_GetWindowFlags window.SglWindow)
        | _ -> None

    /// Attempt to check that the window is minimized.
    let tryGetWindowMinimized state =
        Option.map (fun flags -> flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> 0u) (tryGetWindowFlags state)

    /// Attempt to check that the window is maximized.
    let tryGetWindowMaximized state =
        Option.map (fun flags -> flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MAXIMIZED <> 0u) (tryGetWindowFlags state)

    /// Attempt to check that the window is in a full screen state.
    let tryGetWindowFullScreen state =
        Option.map (fun flags -> flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN <> 0u) (tryGetWindowFlags state)

    /// Attempt to set the window's full screen state.
    let trySetWindowFullScreen fullScreen state =
        match state.SdlDepsOpt with
        | Some deps -> { state with SdlDepsOpt = Some (SdlDeps.trySetWindowFullScreen fullScreen deps) }
        | None -> state

    /// Check whether we should sleep rather than run.
    let shouldSleep state =
        match tryGetWindowFlags state with
        | Some flags ->
            let minimized = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> 0u
            let focused = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS <> 0u
            let fullScreen = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN <> 0u
            minimized || not focused && fullScreen
        | None -> false

    /// Get the margin around the camera eye given the display mode's full screen state and resolution.
    let getEyeMargin (eyeSize : Vector2) state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) ->
            let (width, height) = (ref 0, ref 0)
            SDL.SDL_GetWindowSize (window.SglWindow, width, height) |> ignore
            let eyeMargin =
                v2
                    (single width.Value - eyeSize.X * single Constants.Render.VirtualScalar)
                    (single height.Value - eyeSize.Y * single Constants.Render.VirtualScalar)
            let eyeMargin = eyeMargin / 2.0f
            let eyeMargin = v2 (max eyeMargin.X 0.0f) (max eyeMargin.Y 0.0f) // avoid negative margins
            eyeMargin
        | _ -> v2Zero

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

    /// Set the overlay router.
    let setOverlayRouter router state =
        { state with OverlayRouter = router }

    /// Make an ambient state value.
    let make imperative standAlone updateRate assetMetadataMap symbolStore overlayer overlayRouter sdlDepsOpt =
        Imperative <- imperative
        StandAlone <- standAlone
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Liveness = Live
          UpdateRate = updateRate
          UpdateTime = 0L
          ClockDelta = 1.0f
          Metadata = assetMetadataMap
          KeyValueStore = UMap.makeEmpty HashIdentity.Structural config
          ClockTime = DateTimeOffset.Now
          Tasklets = OMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt
          SymbolStore = symbolStore
          Overlayer = overlayer
          OverlayRouter = overlayRouter }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientState.AmbientState