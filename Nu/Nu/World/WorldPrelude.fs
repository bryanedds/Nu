// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open SDL2
open TiledSharp
open Prime
open Nu

/// Describes a Tiled tile.
type [<StructuralEquality; NoComparison; Struct>] TileDescriptor =
    { mutable Tile : TmxLayerTile
      mutable TileI : int
      mutable TileJ : int
      mutable TilePositionI : Vector2i
      mutable TilePositionF : Vector2
      mutable TileSetTileOpt : TmxTilesetTile option }

/// Describes a Tiled tile animation.
type [<StructuralEquality; NoComparison; Struct>] TileAnimationDescriptor =
    { TileAnimationRun : int
      TileAnimationDelay : int64 }

/// Describes a Tiled tile map.
type [<NoComparison>] TileMapDescriptor =
    { TileMap : TmxMap
      TileSizeI : Vector2i
      TileSizeF : Vector2
      TileMapSizeM : Vector2i
      TileMapSizeI : Vector2i
      TileMapSizeF : Vector2
      TileMapPosition : Vector2 }

/// The type of a screen transition. Incoming means a new screen is being shown and Outgoing
/// means an existing screen being hidden.
type [<StructuralEquality; NoComparison; Struct>] TransitionType =
    | Incoming
    | Outgoing

/// The state of a screen's transition.
type [<StructuralEquality; NoComparison; Struct>] TransitionState =
    | IncomingState
    | OutgoingState
    | IdlingState

/// Describes one of a screen's transition processes.
/// TODO: figure out if this really needs to be CLIMutable.
type [<StructuralEquality; NoComparison; CLIMutable>] Transition =
    { TransitionType : TransitionType
      TransitionLifeTime : int64
      DissolveImageOpt : Image AssetTag option
      SongOpt : SongDescriptor option }

    /// Make a screen transition.
    static member make transitionType =
        { TransitionType = transitionType
          TransitionLifeTime = 0L
          DissolveImageOpt = None
          SongOpt = None }

/// Describes the behavior of the screen dissolving algorithm.
type [<NoComparison>] DissolveDescriptor =
    { IncomingTime : int64
      OutgoingTime : int64
      DissolveImage : Image AssetTag }

/// Describes the behavior of the screen slide algorithm.
type [<NoComparison>] SlideDescriptor =
    { DissolveDescriptor : DissolveDescriptor
      IdlingTime : int64
      SlideImageOpt : Image AssetTag option }

/// Describes the shape of a desired overlay.
type [<NoComparison>] OverlayNameDescriptor =
    | NoOverlay
    | RoutedOverlay
    | DefaultOverlay
    | ExplicitOverlay of string

/// A tasklet to be completed at the scheduled update time.
type [<ReferenceEquality; NoComparison>] 'w Tasklet =
    { ScheduledTime : int64
      ScheduledOp : 'w -> 'w }

/// Specifies that a module contains functions that need to be considered for binding generation.
type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>]
    ModuleBindingAttribute () =
    inherit Attribute ()

/// Specifies that a module contains functions that need to be considered for binding generation.
type [<AttributeUsage (AttributeTargets.Method); AllowNullLiteral>]
    FunctionBindingAttribute (bindingName : string) =
    inherit Attribute ()
    member this.BindingName = bindingName
    new () = FunctionBindingAttribute ""

/// Configuration parameters for Nu.
/// TODO: consider renaming StandAlone to Accompanied and flipping it's value.
type [<NoComparison>] NuConfig =
    { RunSynchronously : bool
      StandAlone : bool }

    /// The default configuration for Nu.
    static member defaultConfig =
        let runSynchronously =
#if SYNCHRONOUS
            true
#else
            false
#endif
        { RunSynchronously = runSynchronously
          StandAlone = true }

/// Configuration parameters for the world.
type [<ReferenceEquality; NoComparison>] WorldConfig =
    { Imperative : bool
      UpdateRate : int64
      ModeOpt : string option
      SdlConfig : SdlConfig
      NuConfig : NuConfig }

    /// The default configuration of the world.
    static member defaultConfig =
        { Imperative = true
          UpdateRate = 1L
          ModeOpt = None
          SdlConfig = SdlConfig.defaultConfig
          NuConfig = NuConfig.defaultConfig }

[<RequireQualifiedAccess>]
module EventTrace =

    /// Record event only in debug mode.
    let debug (moduleName : string) (functionName : string) (moreInfo : string) (eventTrace : EventTrace) =
#if DEBUG
        EventTrace.record moduleName functionName moreInfo eventTrace
#else
        ignore moduleName
        ignore functionName
        ignore moreInfo
        eventTrace
#endif

    /// Record event only in all modes.
    let trace moduleName functionName moreInfo eventTrace =
        EventTrace.record moduleName functionName moreInfo eventTrace

[<AutoOpen>]
module AmbientState =

    /// Tracks whether the engine is running imperatively.
    let mutable private Imperative = true

    /// Tracks whether the engine is running stand-alone.
    let mutable private StandAlone = true

    /// The ambient state of the world.
    type [<ReferenceEquality; NoComparison>] 'w AmbientState =
        private
            { // cache line 1 (assuming 16 byte header)
              Liveness : Liveness
              UpdateRate : int64 // NOTE: might be better to make this accessible from World to avoid cache misses
              UpdateTime : int64
              ClockDelta : single // NOTE: might be better to make this accessible from World to avoid cache misses
              // cache line 2
              KeyValueStore : UMap<Guid, obj>
              ClockTime : DateTimeOffset // moved down here because it's 16 bytes according to - https://stackoverflow.com/a/38731608
              Tasklets : OMap<Simulant, 'w Tasklet UList>
              SdlDepsOpt : SdlDeps option
              Symbolics : Symbolics
              Overlayer : Overlayer
              OverlayRouter : OverlayRouter }

    /// Get whether the engine is running imperatively.
    let getImperative (_ : 'w AmbientState) =
        Imperative

    /// Get whether the engine is running stand-alone.
    let getStandAlone (_ : 'w AmbientState) =
        StandAlone

    /// Get whether the engine is running accompanied, such as in an editor.
    let getAccompanied (_ : 'w AmbientState) =
        not StandAlone

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
        { state with AmbientState.UpdateRate = updateRate }

    /// Check that update rate is non-zero.
    let getAdvancing state =
        getUpdateRate state <> 0L

    /// Check that update rate is zero.
    let getHalted state =
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
        let frameProgress = 1000.0f / Constants.Engine.DesiredFpsF * single state.UpdateRate
        let clockDelta = single delta.TotalMilliseconds / frameProgress
        let clockDeltaNormalized = if clockDelta < 4.0f then clockDelta else 1.0f // assume timing is unnatural about a 4 frame delay
        { state with
            UpdateTime = state.UpdateTime + state.UpdateRate
            ClockDelta = clockDeltaNormalized
            ClockTime = now }

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit state =
        { state with Liveness = Dead }

    /// Regenerate metadata.
    let regenerateMetadata () =
        Metadata.regenerateMetadata ()

    /// Get the key-value store with the by map.
    let getKeyValueStoreBy by state =
        by state.KeyValueStore

    /// Get the key-value store.
    let getKeyValueStore state =
        getKeyValueStoreBy id state

    /// Set the key-value store.
    let setKeyValueStore store state =
        { state with KeyValueStore = store }

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
        { state with Tasklets = OMap.makeEmpty HashIdentity.Structural (OMap.getConfig state.Tasklets) }

    /// Restore the given tasklets from future processing.
    let restoreTasklets tasklets state =
        { state with Tasklets = tasklets }

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

    /// Attempt to get the window size.
    let tryGetWindowSize state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) ->
            let (width, height) = (ref 0, ref 0)
            SDL.SDL_GetWindowSize (window.SglWindow, width, height) |> ignore
            Some (v2i !width !height)
        | _ -> None

    /// Check whether we should sleep rather than run.
    let shouldSleep state =
        match tryGetWindowFlags state with
        | Some flags ->
            let minimized = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> 0u
            let focused = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS <> 0u
            let fullScreen = flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN <> 0u
            minimized || not focused && fullScreen
        | None -> false

    /// Get symbolics with the by map.
    let getSymbolicsBy by state =
        by state.Symbolics

    /// Get symbolics.
    let getSymbolics state =
        getSymbolicsBy id state

    /// Set symbolics.
    let setSymbolics symbolics state =
        { state with Symbolics = symbolics }

    /// Update symbolics.
    let updateSymbolics updater state =
        let store = updater (getSymbolics state)
        { state with Symbolics = store }

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
    let make imperative standAlone updateRate symbolics overlayer overlayRouter sdlDepsOpt =
        Imperative <- imperative
        StandAlone <- standAlone
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Liveness = Live
          UpdateRate = updateRate
          UpdateTime = 0L
          ClockDelta = 1.0f
          KeyValueStore = UMap.makeEmpty HashIdentity.Structural config
          ClockTime = DateTimeOffset.Now
          Tasklets = OMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt
          Symbolics = symbolics
          Overlayer = overlayer
          OverlayRouter = overlayRouter }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientState.AmbientState