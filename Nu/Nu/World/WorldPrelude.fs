// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open System.Numerics
open SDL2
open TiledSharp
open Prime

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
      TileAnimationStride : int
      TileAnimationDelay : GameTime }

/// Describes a Tiled tile map.
type TileMapDescriptor =
    { TileMap : TmxMap
      TileSizeI : Vector2i
      TileSizeF : Vector2
      TileMapSizeM : Vector2i
      TileMapSizeI : Vector2i
      TileMapSizeF : Vector2
      TileMapPosition : Vector2 }

/// The manner in which a gui entity may be docked by a parent entity.
type DockType =
    | DockCenter
    | DockTop
    | DockRight
    | DockBottom
    | DockLeft

/// The manner in which a layout limits the flow its children.
type FlowLimit =
    | FlowParent
    | FlowUnlimited
    | FlowTo of single

/// The direction in which a layout flows its children.
type FlowDirection =
    | FlowRightward
    | FlowDownward
    | FlowLeftward
    | FlowUpward

/// A gui layout.
[<Syntax
    ("Flow Dock Grid Manual",
     "FlowParent FlowUnlimited FlowTo " +
     "FlowUpward FlowRightward FlowDownward FlowLeftward",
     "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type Layout =
    | Flow of FlowDirection * FlowLimit
    | Dock of Vector4 * bool * bool
    | Grid of Vector2i * FlowDirection option * bool
    | Manual

/// The type of a screen transition. Incoming means a new screen is being shown and Outgoing
/// means an existing screen being hidden.
type [<StructuralEquality; NoComparison; Struct>] TransitionType =
    | Incoming
    | Outgoing

/// The state of a screen's transition.
type [<StructuralEquality; NoComparison>] TransitionState =
    | IncomingState of GameTime
    | OutgoingState of GameTime
    | IdlingState of GameTime
    member this.TransitionTime =
        match this with
        | IncomingState time -> time
        | OutgoingState time -> time
        | IdlingState time -> time

/// Describes one of a screen's transition processes.
/// TODO: figure out if this really needs to be CLIMutable.
type [<StructuralEquality; NoComparison; CLIMutable>] Transition =
    { TransitionType : TransitionType
      TransitionLifeTime : GameTime
      DissolveImageOpt : Image AssetTag option
      SongOpt : SongDescriptor option }

    /// Make a screen transition.
    static member make transitionType =
        let lifeTime = GameTime.zero
        { TransitionType = transitionType
          TransitionLifeTime = lifeTime
          DissolveImageOpt = None
          SongOpt = None }

/// Describes the behavior of the screen dissolving algorithm.
type DissolveDescriptor =
    { IncomingTime : GameTime
      OutgoingTime : GameTime
      DissolveImage : Image AssetTag }

/// Describes the behavior of the screen slide algorithm.
type SlideDescriptor =
    { DissolveDescriptor : DissolveDescriptor
      IdlingTime : GameTime
      SlideImageOpt : Image AssetTag option }

/// Describes the shape of a desired overlay.
type OverlayNameDescriptor =
    | NoOverlay
    | RoutedOverlay
    | DefaultOverlay
    | ExplicitOverlay of string

/// A tasklet to be completed at the scheduled update time.
type [<ReferenceEquality>] 'w Tasklet =
    { ScheduledTime : GameTime
      ScheduledOp : 'w -> 'w }

/// Configuration parameters for Nu.
type NuConfig =
    { RunSynchronously : bool
      Accompanied : bool }

    /// That Nu is to run unaccompanied.
    member this.Unaccompanied =
        not this.Accompanied

    /// The default configuration for Nu.
    static member defaultConfig =
        let runSynchronously =
#if SYNCHRONOUS
            true
#else
            false
#endif
        { RunSynchronously = runSynchronously
          Accompanied = false }

/// Configuration parameters for the world.
type [<ReferenceEquality>] WorldConfig =
    { Imperative : bool
      Advancing : bool
      ModeOpt : string option
      SdlConfig : SdlConfig
      NuConfig : NuConfig }

    /// The default configuration of the world.
    static member defaultConfig =
        { Imperative = true
          Advancing = true
          ModeOpt = None
          SdlConfig = SdlConfig.defaultConfig
          NuConfig = NuConfig.defaultConfig }

[<AutoOpen>]
module AmbientState =

    /// Tracks whether the engine is running imperatively.
    /// TODO: pack this flag into AmbientState.Advancing.
    let mutable private Imperative = true

    /// Tracks whether the engine is running accompanied by another program, such as an editor.
    /// TODO: pack this flag into AmbientState.Advancing.
    let mutable private Accompanied = true

    /// The ambient state of the world.
    type [<ReferenceEquality>] 'w AmbientState =
        private
            { // cache line 1 (assuming 16 byte header)
              Liveness : Liveness
              Advancing : bool
              UpdateTime : int64
              TickDelta : int64
              // cache line 2
              KeyValueStore : SUMap<Guid, obj>
              TickTime : int64
              TickWatch : Stopwatch
              Tasklets : OMap<Simulant, 'w Tasklet UList>
              SdlDepsOpt : SdlDeps option
              Symbolics : Symbolics
              Overlayer : Overlayer
              // cache line 3
              OverlayRouter : OverlayRouter
              UnculledRenderRequested : bool }

    /// Check that the world's state is advancing.
    let getAdvancing state =
        state.Advancing

    /// Check that the world's state is advancing.
    let getHalted state =
        not state.Advancing

    /// Set whether the world's state is advancing.
    let setAdvancing advancing (state : _ AmbientState) =
        if advancing <> state.Advancing then
            if advancing then state.TickWatch.Start () else state.TickWatch.Stop ()
            { state with Advancing = advancing }
        else state

    /// Check that the engine is executing with imperative semantics where applicable.
    let getImperative (_ : 'w AmbientState) =
        Imperative

    /// Check that the engine is executing with functional semantics.
    let getFunctional (_ : 'w AmbientState) =
        not Imperative

    /// Get whether the engine is running accompanied, such as in an editor.
    let getAccompanied (_ : 'w AmbientState) =
        Accompanied

    /// Get whether the engine is running unaccompanied.
    let getUnaccompanied (_ : 'w AmbientState) =
        not Accompanied

    /// Get the collection config value.
    let getConfig (_ : 'w AmbientState) =
        if Imperative then TConfig.Imperative else TConfig.Functional

    /// Get the the liveness state of the engine.
    let getLiveness state =
        state.Liveness

    /// Get the update time.
    let getUpdateTime state =
        state.UpdateTime

    /// Get the tick delta as a number of environment ticks.
    let getTickDelta state =
        state.TickDelta

    /// Get the tick time as a number of environment ticks.
    let getTickTime state =
        state.TickTime

    /// Get the clock delta as a number of seconds.
    let getClockDelta state =
        single state.TickDelta / single Stopwatch.Frequency

    /// Get the clock time as a number of seconds.
    let getClockTime state =
        single state.TickTime / single Stopwatch.Frequency

    /// Get the polymorphic engine time delta.
    let getGameDelta state =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (if state.Advancing then 1L else 0L)
        | DynamicFrameRate _ -> ClockTime (getClockDelta state)

    /// Get the polymorphic engine time.
    let getGameTime state =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (getUpdateTime state)
        | DynamicFrameRate _ -> ClockTime (getClockTime state)

    /// Update the update and clock times.
    let updateTime state =
        let updateDelta = if state.Advancing then 1L else 0L
        let tickTime = state.TickWatch.ElapsedTicks
        let tickDelta = tickTime - state.TickTime
        { state with
            UpdateTime = state.UpdateTime + updateDelta
            TickDelta = tickDelta
            TickTime = tickTime }

    /// Shelve the ambient state.
    let shelve (state : _ AmbientState) =
        state

    /// Unshelve the ambient state.
    let unshelve state =
        if state.Advancing
        then state.TickWatch.Start ()
        else state.TickWatch.Stop ()
        state

    /// Place the engine into a state such that the app will exit at the end of the current frame.
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

    /// Acknowledge an unculled render request.
    let acknowledgeUnculledRenderRequest state =
        { state with UnculledRenderRequested = false }

    /// Get whether an unculled render was requested.
    let getUnculledRenderRequested state =
        state.UnculledRenderRequested

    /// Request an unculled render for the current frame.
    let requestUnculledRender state =
        { state with UnculledRenderRequested = true }

    /// Make an ambient state value.
    let make imperative accompanied advancing symbolics overlayer overlayRouter sdlDepsOpt =
        Imperative <- imperative
        Accompanied <- accompanied
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Liveness = Live
          Advancing = advancing
          UpdateTime = 0L
          TickDelta = 0L
          KeyValueStore = SUMap.makeEmpty HashIdentity.Structural config
          TickTime = 0L
          TickWatch = if advancing then Stopwatch.StartNew () else Stopwatch ()
          Tasklets = OMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt
          Symbolics = symbolics
          Overlayer = overlayer
          OverlayRouter = overlayRouter
          UnculledRenderRequested = false }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientState.AmbientState