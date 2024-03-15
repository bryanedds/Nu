// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.Numerics
open SDL2
open TiledSharp
open DotRecast.Core.Collections
open DotRecast.Core.Numerics
open DotRecast.Recast
open DotRecast.Recast.Geom
open Prime

// The inferred attributes of an entity that are used to construct its bounds.
// HACK: added Important field to allow attributes to be marked as unimportant.
// TODO: P1: see if we can refactor this type to make its representation and algo less hacky.
type AttributesInferred =
    { Unimportant : bool
      SizeInferred : Vector3
      OffsetInferred : Vector3 }

    static member important size offset =
        { Unimportant = false
          SizeInferred = size
          OffsetInferred = offset }

    static member unimportant =
        { Unimportant = true
          SizeInferred = v3Zero
          OffsetInferred = v3Zero }

    static member choose (left : AttributesInferred) (right : AttributesInferred) =
        if left.Unimportant then right
        elif right.Unimportant then left
        elif right.OffsetInferred.MagnitudeSquared >= left.OffsetInferred.MagnitudeSquared then right // HACK: picking the attribute whose offset is more impactful...
        else left

/// Describes a Tiled tile.
type [<Struct>] TileDescriptor =
    { mutable Tile : TmxLayerTile
      mutable TileI : int
      mutable TileJ : int
      mutable TilePositionI : Vector2i
      mutable TilePositionF : Vector2
      mutable TileSetTileOpt : TmxTilesetTile option }

/// Describes a Tiled tile animation.
type [<Struct>] TileAnimationDescriptor =
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

/// Configure the construction of 3d navigation meshes.
type [<SymbolicExpansion>] Nav3dConfig =
    { CellSize : single
      CellHeight : single
      AgentHeight : single
      AgentRadius : single
      AgentMaxClimb : single
      AgentMaxSlope : single
      RegionMinSize : int
      RegionMergeSize : int
      EdgeMaxLength : single
      EdgeMaxError : single
      VertsPerPolygon : int
      DetailSampleDistance : single
      DetailSampleMaxError : single
      FilterLowHangingObstacles : bool
      FilterLedgeSpans : bool
      FilterWalkableLowHeightSpans : bool
      PartitionType : RcPartition }

    /// The default 3d navigation configuration.
    static member defaultConfig =
        { CellSize = 0.1f
          CellHeight = 0.1f
          AgentHeight = 1.5f
          AgentRadius = 0.4f // same as default character 3d radius (maybe should be slightly more?)
          AgentMaxClimb = 0.35f
          AgentMaxSlope = 45.0f
          RegionMinSize = 8
          RegionMergeSize = 20
          EdgeMaxLength = 6.0f
          EdgeMaxError = 1.3f
          VertsPerPolygon = 6
          DetailSampleDistance = 6.0f
          DetailSampleMaxError = 1.0f
          FilterLowHangingObstacles = true
          FilterLedgeSpans = true
          FilterWalkableLowHeightSpans = true
          PartitionType = RcPartition.LAYERS }

/// 3d navigation input geometry provider.
type Nav3dInputGeomProvider (vertices, indices, bounds : Box3) =
    let triMesh = RcTriMesh (vertices, indices)
    let meshes = RcImmutableArray.Create triMesh
    let offMeshConnections = List ()
    let convexVolumes = List ()
    interface IInputGeomProvider with
        member this.GetMesh () = triMesh
        member this.GetMeshBoundsMin () = RcVec3f (bounds.Min.X, bounds.Min.Y, bounds.Min.Z)
        member this.GetMeshBoundsMax () = RcVec3f (bounds.Max.X, bounds.Max.Y, bounds.Max.Z)
        member this.Meshes () = meshes
        member this.AddConvexVolume convexVolume = convexVolumes.Add convexVolume
        member this.ConvexVolumes () = convexVolumes
        member this.GetOffMeshConnections () = offMeshConnections
        member this.AddOffMeshConnection (start, end_, radius, bidir, area, flags) = offMeshConnections.Add (RcOffMeshConnection (start, end_, radius, bidir, area, flags))
        member this.RemoveOffMeshConnections filter = offMeshConnections.RemoveAll filter |> ignore<int>
        end

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
type Layout =
    | Flow of FlowDirection * FlowLimit
    | Dock of Vector4 * bool * bool
    | Grid of Vector2i * FlowDirection option * bool
    | Manual

/// The type of a screen transition. Incoming means a new screen is being shown and Outgoing
/// means an existing screen being hidden.
type [<Struct>] TransitionType =
    | Incoming
    | Outgoing

/// The state of a screen's transition.
type TransitionState =
    | IncomingState of GameTime
    | OutgoingState of GameTime
    | IdlingState of GameTime
    member this.TransitionTime =
        match this with
        | IncomingState time -> time
        | OutgoingState time -> time
        | IdlingState time -> time

/// Describes one of a screen's transition processes.
type Transition =
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

/// The manner in which an entity paste operation should be executed.
type PasteType =
    | PasteAtLook
    | PasteAtMouse
    | PasteAt of Vector3

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

/// Configuration parameters for the world.
type [<ReferenceEquality>] WorldConfig =
    { Imperative : bool
      Accompanied : bool
      Advancing : bool
      ModeOpt : string option
      SdlConfig : SdlConfig }

    /// That Nu is to run unaccompanied.
    member this.Unaccompanied =
        not this.Accompanied

    /// The default configuration of the world.
    static member defaultConfig =
        { Imperative = true
          Accompanied = false
          Advancing = true
          ModeOpt = None
          SdlConfig = SdlConfig.defaultConfig }

[<AutoOpen>]
module AmbientState =

    let [<Literal>] private ImperativeMask =    0b0001u
    let [<Literal>] private AccompaniedMask =   0b0010u
    let [<Literal>] private AdvancingMask =     0b0100u

    /// The ambient state of the world.
    type [<ReferenceEquality>] 'w AmbientState =
        private
            { // cache line 1 (assuming 16 byte header)
              Flags : uint
              Liveness : Liveness
              UpdateTime : int64
              TickDelta : int64
              KeyValueStore : SUMap<obj, obj>
              TickTime : int64
              // cache line 2
              TickTimeShavings : int64
              TickWatch : Stopwatch
              DateDelta : TimeSpan
              DateTime : DateTimeOffset
              Tasklets : OMap<Simulant, 'w Tasklet UList>
              SdlDepsOpt : SdlDeps option
              // cache line 3
              Symbolics : Symbolics
              Overlayer : Overlayer
              UnculledRenderRequested : bool }

        member this.Imperative = this.Flags &&& ImperativeMask <> 0u
        member this.Accompanied = this.Flags &&& AccompaniedMask <> 0u
        member this.Advancing = this.Flags &&& AdvancingMask <> 0u

    /// Get the the liveness state of the engine.
    let getLiveness state =
        state.Liveness

    /// Set whether the world's state is advancing.
    let setAdvancing advancing (state : _ AmbientState) =
        if advancing <> state.Advancing then
            if advancing then state.TickWatch.Start () else state.TickWatch.Stop ()
            { state with Flags = if advancing then state.Flags ||| AdvancingMask else state.Flags &&& ~~~AdvancingMask }
        else state

    /// Get the collection config value.
    let getConfig (state : 'w AmbientState) =
        if state.Imperative then TConfig.Imperative else TConfig.Functional

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
    let getGameDelta (state : 'w AmbientState) =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (if state.Advancing then 1L else 0L)
        | DynamicFrameRate _ -> ClockTime (getClockDelta state)

    /// Get the polymorphic engine time.
    let getGameTime state =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (getUpdateTime state)
        | DynamicFrameRate _ -> ClockTime (getClockTime state)

    /// Get the date delta as a TimeSpan.
    let getDateDelta state =
        state.DateDelta

    /// Get the date time as a DateTimeOffset.
    let getDateTime state =
        state.DateTime

    /// Update the update and clock times.
    let updateTime (state : 'w AmbientState) =
        let updateDelta = if state.Advancing then 1L else 0L
        let tickTimeShaved = state.TickWatch.ElapsedTicks - state.TickTimeShavings
        let tickDelta = tickTimeShaved - state.TickTime
        let tickDeltaShaved = min (tickTimeShaved - state.TickTime) Constants.Engine.TickDeltaMax
        let tickDeltaShavings = max (tickDelta - tickDeltaShaved) 0L
        let dateTime = DateTimeOffset.Now
        { state with
            UpdateTime = state.UpdateTime + updateDelta
            TickTime = tickTimeShaved - tickDeltaShavings
            TickTimeShavings = state.TickTimeShavings + tickDeltaShavings
            TickDelta = tickDeltaShaved
            DateTime = dateTime
            DateDelta = dateTime - state.DateTime }

    /// Switch simulation to use this ambient state.
    let switch (state : 'w AmbientState) =
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
            Some (v2i width.Value height.Value)
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
    let make imperative accompanied advancing symbolics overlayer sdlDepsOpt =
        let flags =
            (if imperative then ImperativeMask else 0u) |||
            (if accompanied then AccompaniedMask else 0u) |||
            (if advancing then AdvancingMask else 0u)
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Flags = flags
          Liveness = Live
          UpdateTime = 0L
          TickDelta = 0L
          KeyValueStore = SUMap.makeEmpty HashIdentity.Structural config
          TickTime = 0L
          TickTimeShavings = 0L
          TickWatch = if advancing then Stopwatch.StartNew () else Stopwatch ()
          DateTime = DateTime.Now
          DateDelta = TimeSpan.Zero
          Tasklets = OMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt
          Symbolics = symbolics
          Overlayer = overlayer
          UnculledRenderRequested = false }

/// The ambient state of the world.
type 'w AmbientState = 'w AmbientState.AmbientState