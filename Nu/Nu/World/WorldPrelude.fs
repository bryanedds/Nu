// Nu Game Engine.
// Copyright (C) Bryan Edds.

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

/// The result of an intersection-detecting operation.
type [<Struct>] Intersection =
    | Hit of single
    | Miss

    /// Convert from nullable intersection value.
    static member ofNullable (intersection : single Nullable) =
        if intersection.HasValue
        then Hit intersection.Value
        else Miss

/// The attributes of an entity that are used to infer its bounds. There are three general use cases for inferred
/// attributes -
/// 1) things with a definite, asset-based size like sprites and models. 
/// 2) things with a size that is only sensibly defined by users like emitters and effects.
/// 3) things where size won't matter in any case like sky boxes and configuration dispatchers.
/// TODO: see if we can refactor this type to make its representation and algo more understandable.
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

/// Describes a Spine animation for a given track.
type [<DefaultValue "[idle Loop]">] SpineAnimation =
    { SpineAnimationName : string
      SpineAnimationPlayback : Playback }

/// Represents the mutable backing state of an animating Spine skeleton.
/// NOTE: this is inherently imperative and therefore currently unsupported by undo / redo.
type SpineSkeletonState =
    { SpineSkeleton : Spine.Skeleton
      SpineAnimationState : Spine.AnimationState }

/// The timing with which an effect should be evaluated in a frame.
type RunMode =
    | RunEarly
    | RunLate

/// Configure the construction of 3d navigation meshes.
type [<SymbolicExpansion>] Nav3dConfig =
    { CellSize : single
      CellHeight : single
      AgentHeight : single
      AgentRadius : single
      AgentClimbMax : single
      AgentSlopeMax : single
      RegionSizeMin : int
      RegionSizeMerge : int
      EdgeLengthMax : single
      EdgeErrorMax : single
      VertsPerPolygon : int
      DetailSampleDistance : single
      DetailSampleErrorMax : single
      FilterLowHangingObstacles : bool
      FilterLedgeSpans : bool
      FilterWalkableLowHeightSpans : bool
      PartitionType : RcPartition }

    /// The default 3d navigation configuration.
    static member defaultConfig =
        { CellSize = 0.1f
          CellHeight = 0.1f
          AgentHeight = 1.5f
          AgentRadius = 0.3f
          AgentClimbMax = 0.4f
          AgentSlopeMax = 45.0f
          RegionSizeMin = 8
          RegionSizeMerge = 20
          EdgeLengthMax = 6.0f
          EdgeErrorMax = 1.3f
          VertsPerPolygon = 6
          DetailSampleDistance = 6.0f
          DetailSampleErrorMax = 1.0f
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

/// The result of a navigation computation.
type NavOutput =
    { NavPosition : Vector3
      NavRotation : Quaternion
      NavLinearVelocity : Vector3
      NavAngularVelocity : Vector3 }

/// The data collected from a navigation builder result.
type [<SymbolicExpansion>] NavBuilderResultData =
    { NavEdgesMinY : single
      NavEdgesMaxY : single
      NavInteriorEdges : Segment3 array
      NavExteriorEdges : Segment3 array }

    /// Make from an RcBuilderResult.
    static member make (builderResult : RcBuilderResult) =

        // compute interior edges
        let dmesh = builderResult.MeshDetail
        let mutable edgesMinY = Single.MaxValue
        let mutable edgesMaxY = Single.MinValue
        let interiorEdges =
            [|for i in 0 .. dec dmesh.nmeshes do
                let m = i * 4
                let bverts = dmesh.meshes.[m]
                let btris = dmesh.meshes.[m + 2]
                let ntris = dmesh.meshes.[m + 3]
                let verts = bverts * 3
                let tris = btris * 4
                for j in 0 .. dec ntris do
                    let t = tris + j * 4
                    let mutable k = 0
                    let mutable kp = 2
                    while k < 3 do
                        let ef = (dmesh.tris.[t + 3] >>> (kp * 2)) &&& 0x3
                        if ef = 0 then
                            let start =
                                v3
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3]
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3 + 1]
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3 + 2]
                            let stop =
                                v3
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3]
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3 + 1]
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3 + 2]
                            segment3 start stop
                        kp <- k
                        k <- inc k|]

        // compute exterior edges
        let exteriorEdges =
            [|for i in 0 .. dec dmesh.nmeshes do
                let m = i * 4
                let bverts = dmesh.meshes.[m]
                let btris = dmesh.meshes.[m + 2]
                let ntris = dmesh.meshes.[m + 3]
                let verts = bverts * 3
                let tris = btris * 4
                for j in 0 .. dec ntris do
                    let t = tris + j * 4
                    let mutable k = 0
                    let mutable kp = 2
                    while k < 3 do
                        let ef = (dmesh.tris.[t + 3] >>> (kp * 2)) &&& 0x3
                        if ef <> 0 then
                            let start =
                                v3
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3]
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3 + 1]
                                    dmesh.verts.[verts + dmesh.tris.[t + kp] * 3 + 2]
                            let stop =
                                v3
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3]
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3 + 1]
                                    dmesh.verts.[verts + dmesh.tris.[t + k] * 3 + 2]
                            if edgesMinY > start.Y then edgesMinY <- start.Y
                            if edgesMaxY < start.Y then edgesMaxY <- start.Y
                            if edgesMinY > stop.Y then edgesMinY <- stop.Y
                            if edgesMaxY < stop.Y then edgesMaxY <- stop.Y
                            segment3 start stop
                        kp <- k
                        k <- inc k|]

        // fin
        { NavEdgesMinY = edgesMinY
          NavEdgesMaxY = edgesMaxY
          NavInteriorEdges = interiorEdges
          NavExteriorEdges = exteriorEdges }

/// The manner in which a gui entity may be docked by a parent entity.
type DockType =

    /// Dock in the available center space of the parent entity.
    | DockCenter

    /// Dock in the available top side of the parent entity.
    | DockTop

    /// Dock in the available right side of the parent entity.
    | DockRight

    /// Dock in the available bottom side of the parent entity.
    | DockBottom

    /// Dock in the available left side of the parent entity.
    | DockLeft

/// The manner in which a layout limits the flow its children.
type FlowLimit =

    /// Flow within the parent's bounds.
    | FlowParent

    /// Flow without limit.
    | FlowUnlimited

    /// Flow to a specific distance.
    | FlowTo of single

/// The direction in which a layout flows its children.
type FlowDirection =

    /// Flow to the right.
    | FlowRightward

    /// Flow downward.
    | FlowDownward

    /// Flow to the left.
    | FlowLeftward

    /// Flow upward.
    | FlowUpward

/// A gui layout.
type Layout =

    /// Flow children in the given direcion up to the given limit.
    | Flow of FlowDirection : FlowDirection * FlowLimit : FlowLimit

    /// Dock children using the given margins (X = Left, Y = Top, Z = Right, W = Bottom).
    | Dock of Margins : Vector4 * PercentageBased : bool * ResizeChildren : bool

    /// Arrange children in a grid with the given dimensions, optional flow direction, and with optional resizing behavior.
    | Grid of Dims : Vector2i * FlowDirectionOpt : FlowDirection option * ResizeChildren : bool

    /// Utilize no layout, allowing children to be placed freely.
    | Manual

/// The type of a screen transition. Incoming means a new screen is being shown and Outgoing
/// means an existing screen being hidden.
type TransitionType =

    /// A transition that occurs when a screen has been selected.
    | Incoming

    /// A transition that occurs when a screen is being deselected.
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
type [<SymbolicExpansion>] Transition =
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

/// A tasklet to be completed at the scheduled time.
type [<ReferenceEquality>] 'w Tasklet =
    { ScheduledTime : GameTime
      ScheduledOp : 'w -> unit }

/// Configuration parameters for the world.
type [<ReferenceEquality>] WorldConfig =
    { Imperative : bool
      Accompanied : bool
      Advancing : bool
      FramePacing : bool
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
          FramePacing = false
          ModeOpt = None
          SdlConfig = SdlConfig.defaultConfig }

/// Engine timing objects.
type Timers =
    { InputTimer : Stopwatch
      PhysicsTimer : Stopwatch
      PreUpdateTimer : Stopwatch
      PreUpdateGatherTimer : Stopwatch
      PreUpdateGameTimer : Stopwatch
      PreUpdateScreensTimer : Stopwatch
      PreUpdateGroupsTimer : Stopwatch
      UpdateTimer : Stopwatch
      UpdateGatherTimer : Stopwatch
      UpdateGameTimer : Stopwatch
      UpdateScreensTimer : Stopwatch
      UpdateGroupsTimer : Stopwatch
      UpdateEntitiesTimer : Stopwatch
      PostUpdateTimer : Stopwatch
      PostUpdateGatherTimer : Stopwatch
      PostUpdateGameTimer : Stopwatch
      PostUpdateScreensTimer : Stopwatch
      PostUpdateGroupsTimer : Stopwatch
      CoroutinesTimer : Stopwatch
      TaskletsTimer : Stopwatch
      DestructionTimer : Stopwatch
      PerProcessTimer : Stopwatch
      PreProcessTimer : Stopwatch
      PostProcessTimer : Stopwatch
      RenderGatherTimer : Stopwatch
      RenderEntityMessagesTimer : Stopwatch
      RenderMessagesTimer : Stopwatch
      AudioTimer : Stopwatch
      FrameTimer : Stopwatch
      MainThreadTimer : Stopwatch
      mutable MainThreadTime : TimeSpan
      ImGuiTimer : Stopwatch
      mutable ImGuiTime : TimeSpan
      mutable GcTotalTime : TimeSpan
      mutable GcFrameTime : TimeSpan }

    /// Make timers.
    static member make () =
        let gcTime = GC.GetTotalPauseDuration ()
        { InputTimer = Stopwatch ()
          PhysicsTimer = Stopwatch ()
          PreUpdateTimer = Stopwatch ()
          PreUpdateGatherTimer = Stopwatch ()
          PreUpdateGameTimer = Stopwatch ()
          PreUpdateScreensTimer = Stopwatch ()
          PreUpdateGroupsTimer = Stopwatch ()
          UpdateTimer = Stopwatch ()
          UpdateGatherTimer = Stopwatch ()
          UpdateGameTimer = Stopwatch ()
          UpdateScreensTimer = Stopwatch ()
          UpdateGroupsTimer = Stopwatch ()
          UpdateEntitiesTimer = Stopwatch ()
          PostUpdateTimer = Stopwatch ()
          PostUpdateGatherTimer = Stopwatch ()
          PostUpdateGameTimer = Stopwatch ()
          PostUpdateScreensTimer = Stopwatch ()
          PostUpdateGroupsTimer = Stopwatch ()
          CoroutinesTimer = Stopwatch ()
          TaskletsTimer = Stopwatch ()
          DestructionTimer = Stopwatch ()
          PerProcessTimer = Stopwatch ()
          PreProcessTimer = Stopwatch ()
          PostProcessTimer = Stopwatch ()
          RenderGatherTimer = Stopwatch ()
          RenderEntityMessagesTimer = Stopwatch ()
          RenderMessagesTimer = Stopwatch ()
          AudioTimer = Stopwatch ()
          FrameTimer = Stopwatch ()
          MainThreadTimer = Stopwatch ()
          MainThreadTime = TimeSpan.Zero
          ImGuiTimer = Stopwatch ()
          ImGuiTime = TimeSpan.Zero
          GcTotalTime = gcTime
          GcFrameTime = gcTime }

[<AutoOpen>]
module internal AmbientState =

    let [<Literal>] private ImperativeMask =            0b00001u
    let [<Literal>] private AccompaniedMask =           0b00010u
    let [<Literal>] private AdvancingMask =             0b00100u
    let [<Literal>] private FramePacingMask =           0b01000u
    let [<Literal>] private AdvancementClearedMask =    0b10000u

    /// The 'ambient' state of the world (miscellaneous world state such as time).
    type [<ReferenceEquality>] internal 'w AmbientState =
        private
            { // cache line 1 (assuming 16 byte header)
              Flags : uint
              Alive : bool
              UpdateDelta : int64
              UpdateTime : int64
              ClockDelta : single
              // cache line 2
              ClockTime : single
              TickDelta : int64
              KeyValueStore : SUMap<string, obj>
              TickTime : int64
              TickWatch : Stopwatch
              DateDelta : TimeSpan
              // cache line 3
              TickDeltaPrevious : int64
              DateTime : DateTimeOffset
              Coroutines : OMap<uint64, GameTime * (unit -> bool) * CoroutineDelayed>
              // cache line 4
              Tasklets : OMap<Simulant, 'w Tasklet UList>
              SdlDepsOpt : SdlDeps option
              Symbolics : Symbolics
              Overlayer : Overlayer
              Timers : Timers
              LightMapRenderRequested : bool }

        member this.Imperative = this.Flags &&& ImperativeMask <> 0u
        member this.Accompanied = this.Flags &&& AccompaniedMask <> 0u
        member this.Advancing = this.Flags &&& AdvancingMask <> 0u
        member this.FramePacing = this.Flags &&& FramePacingMask <> 0u
        member this.AdvancementCleared = this.Flags &&& AdvancementClearedMask <> 0u

    let internal getAlive state =
        state.Alive

    let internal setAdvancing advancing (state : _ AmbientState) =
        if advancing <> state.Advancing then
            if advancing then state.TickWatch.Start () else state.TickWatch.Stop ()
            { state with Flags = if advancing then state.Flags ||| AdvancingMask else state.Flags &&& ~~~AdvancingMask }
        else state

    let internal setFramePacing framePacing (state : _ AmbientState) =
        { state with Flags = if framePacing then state.Flags ||| FramePacingMask else state.Flags &&& ~~~FramePacingMask }

    let internal getConfig (state : _ AmbientState) =
        if state.Imperative then TConfig.Imperative else TConfig.Functional

    let internal clearAdvancement (state : _ AmbientState) =
        { state with
            Flags = state.Flags &&& ~~~AdvancingMask ||| AdvancementClearedMask
            UpdateDelta = 0L
            ClockDelta = 0.0f
            TickDelta = 0L }

    let internal restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta (state : _ AmbientState) =
        let flags = state.Flags
        let flags = if advancing then flags ||| AdvancingMask else flags &&& ~~~AdvancingMask
        let flags = if advancementCleared then flags ||| AdvancementClearedMask else flags &&& ~~~AdvancementClearedMask
        { state with
            Flags = flags
            UpdateDelta = updateDelta
            ClockDelta = clockDelta
            TickDelta = tickDelta }

    let internal getUpdateDelta state =
        state.UpdateDelta

    let internal getUpdateTime state =
        state.UpdateTime

    let internal getClockDelta state =
        state.ClockDelta

    let internal getClockTime state =
        state.ClockTime

    let internal getTickDelta state =
        state.TickDelta

    let internal getTickTime state =
        state.TickTime

    let internal getGameDelta (state : 'w AmbientState) =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (if state.Advancing then 1L else 0L)
        | DynamicFrameRate _ -> TickTime (getTickDelta state)

    let internal getGameTime state =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime (getUpdateTime state)
        | DynamicFrameRate _ -> TickTime (getTickTime state)

    let internal getDateDelta state =
        state.DateDelta

    let internal getDateTime state =
        state.DateTime

    let internal updateTime (state : 'w AmbientState) =
        let tickDeltaCurrent =
            if state.Advancing
            then min state.TickWatch.ElapsedTicks Constants.Engine.TickDeltaMax
            else 0L
        state.TickWatch.Restart ()
        let updateDelta = if state.Advancing then 1L else 0L
        let tickDelta =
            if Constants.Engine.TickDeltaAveraging
            then (tickDeltaCurrent + state.TickDeltaPrevious) / 2L
            else tickDeltaCurrent
        let tickTime = state.TickTime + tickDelta
        let dateTime = DateTimeOffset.Now
        { state with
            UpdateDelta = updateDelta
            UpdateTime = state.UpdateTime + updateDelta
            ClockDelta = single tickDelta / single Stopwatch.Frequency
            ClockTime = single tickTime / single Stopwatch.Frequency
            TickDelta = tickDelta
            TickDeltaPrevious = tickDeltaCurrent
            TickTime = tickTime
            DateTime = dateTime
            DateDelta = dateTime - state.DateTime }

    let internal switch (state : 'w AmbientState) =
        if state.Advancing
        then state.TickWatch.Start ()
        else state.TickWatch.Stop ()
        state

    let internal exit state =
        { state with Alive = false }

    let internal getKeyValueStore state =
        state.KeyValueStore

    let internal getKeyValueStoreBy by state =
        by state.KeyValueStore

    let internal setKeyValueStore store state =
        { state with KeyValueStore = store }

    let internal mapKeyValueStore mapper state =
        let store = mapper (getKeyValueStore state)
        { state with KeyValueStore = store }

    let internal getCoroutines state =
        state.Coroutines

    let internal setCoroutines coroutines state =
        { state with Coroutines = coroutines }

    let internal addCoroutine coroutine state =
        let id = Gen.id64
        let coroutines = OMap.add id coroutine state.Coroutines
        { state with Coroutines = coroutines }

    let internal getTasklets state =
        state.Tasklets

    let internal removeTasklets simulant state =
        { state with Tasklets = OMap.remove simulant state.Tasklets }

    let internal clearTasklets state =
        { state with Tasklets = OMap.makeEmpty HashIdentity.Structural (OMap.config state.Tasklets) }

    let internal restoreTasklets tasklets state =
        { state with Tasklets = tasklets }

    let internal addTasklet (simulant : Simulant) tasklet state =
        { state with
            Tasklets =
                match state.Tasklets.TryGetValue simulant with
                | (true, taskletList) -> OMap.add simulant (UList.add tasklet taskletList) state.Tasklets
                | (false, _) -> OMap.add simulant (UList.singleton (OMap.config state.Tasklets) tasklet) state.Tasklets }

    let internal tryGetWindowFlags state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) -> Some (SDL.SDL_GetWindowFlags window.SglWindow)
        | _ -> None

    let internal tryGetWindowMinimized state =
        Option.map (fun flags -> flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> 0u) (tryGetWindowFlags state)

    let internal tryGetWindowMaximized state =
        Option.map (fun flags -> flags &&& uint32 SDL.SDL_WindowFlags.SDL_WINDOW_MAXIMIZED <> 0u) (tryGetWindowFlags state)

    let internal tryGetWindowFullScreen state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) ->            
            let (width, height) = (ref 0, ref 0)
            SDL.SDL_GetWindowSize (window.SglWindow, width, height) |> ignore
            let mutable displayMode = Unchecked.defaultof<_>
            SDL.SDL_GetDesktopDisplayMode (0, &displayMode) |> ignore<int>
            Some (width.Value = displayMode.w || height.Value = displayMode.h)
        | _ -> None

    let internal trySetWindowFullScreen fullScreen state =
        match state.SdlDepsOpt with
        | Some deps -> { state with SdlDepsOpt = Some (SdlDeps.trySetWindowFullScreen fullScreen deps) }
        | None -> state

    let internal tryToggleWindowFullScreen state =
        match tryGetWindowFullScreen state with
        | Some fullScreen -> trySetWindowFullScreen (not fullScreen) state
        | None -> state

    let internal tryGetWindowPosition state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) ->
            let (x, y) = (ref 0, ref 0)
            SDL.SDL_GetWindowPosition (window.SglWindow, x, y) |> ignore
            Some (v2i x.Value y.Value)
        | _ -> None

    let internal trySetWindowPosition (position : Vector2i) state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) -> SDL.SDL_SetWindowPosition (window.SglWindow, position.X, position.Y) |> ignore
        | None -> ()

    let internal tryGetWindowSize state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) ->
            let (width, height) = (ref 0, ref 0)
            SDL.SDL_GetWindowSize (window.SglWindow, width, height) |> ignore
            Some (v2i width.Value height.Value)
        | _ -> None

    let internal trySetWindowSize (size : Vector2i) state =
        match Option.flatten (Option.map SdlDeps.getWindowOpt state.SdlDepsOpt) with
        | Some (SglWindow window) -> SDL.SDL_SetWindowSize (window.SglWindow, size.X, size.Y) |> ignore
        | None -> ()

    let internal getSymbolicsBy by state =
        by state.Symbolics

    let internal getSymbolics state =
        getSymbolicsBy id state

    let internal setSymbolics symbolics state =
        { state with Symbolics = symbolics }

    let internal mapSymbolics mapper state =
        let store = mapper (getSymbolics state)
        { state with Symbolics = store }

    let internal getOverlayer state =
        state.Overlayer

    let internal setOverlayer overlayer state =
        { state with Overlayer = overlayer }

    let internal getTimers state =
        state.Timers

    let internal acknowledgeLightMapRenderRequest state =
        { state with LightMapRenderRequested = false }

    let internal getLightMapRenderRequested state =
        state.LightMapRenderRequested

    let internal requestLightMapRender state =
        { state with LightMapRenderRequested = true }

    let internal make imperative accompanied advancing framePacing symbolics overlayer timers sdlDepsOpt =
        let flags =
            (if imperative then ImperativeMask else 0u) |||
            (if accompanied then AccompaniedMask else 0u) |||
            (if advancing then AdvancingMask else 0u) |||
            (if framePacing then FramePacingMask else 0u)
        let config = if imperative then TConfig.Imperative else TConfig.Functional
        { Flags = flags
          Alive = true
          UpdateDelta = 0L
          UpdateTime = 0L
          ClockDelta = 0.0f
          ClockTime = 0.0f
          TickDelta = 0L
          KeyValueStore = SUMap.makeEmpty HashIdentity.Structural config
          TickTime = 0L
          TickWatch = if advancing then Stopwatch.StartNew () else Stopwatch ()
          DateDelta = TimeSpan.Zero
          TickDeltaPrevious = 0L
          DateTime = DateTime.Now
          Coroutines = OMap.makeEmpty HashIdentity.Structural config
          Tasklets = OMap.makeEmpty HashIdentity.Structural config
          SdlDepsOpt = sdlDepsOpt
          Symbolics = symbolics
          Overlayer = overlayer
          Timers = timers
          LightMapRenderRequested = false }

type internal 'w AmbientState = 'w AmbientState.AmbientState