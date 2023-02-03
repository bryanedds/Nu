// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open System
open System.Numerics
open System.Configuration
open Nu
open Prime

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] ExitCodeSuccess = 0
    let [<Literal>] ExitCodeFailure = 1
    let [<Literal>] LohSize = 85000 // the approximate size of the .NET Large-Object Heap in byte, although this seems misleading in some cases...
    let [<Literal>] Meter2d = 48.0f
    let [<Literal>] Meter3d = 1.0f
    let [<Uniform>] GravityDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let [<Literal>] GameSortPriority = Single.MaxValue
    let [<Uniform>] ScreenSortPriority = GameSortPriority - 1.0f
    let [<Uniform>] GroupSortPriority = ScreenSortPriority - 1.0f
    let [<Uniform>] EntitySortPriority = GroupSortPriority - 1.0f
    let [<Literal>] NamePropertyName = "Name"
    let [<Literal>] DispatcherPropertyName = "Dispatcher"
    let [<Literal>] PropertiesPropertyName = "Properties"
    let [<Literal>] FacetNamesPropertyName = "FacetNames"
    let [<Literal>] RequiredDispatcherPropertyName = "RequiredDispatcher"
    let [<Literal>] FacetsPropertyName = "Facets"
    let [<Literal>] XtensionPropertyName = "Xtension"
    let [<Literal>] TransformPropertyName = "Transform"
    let [<Literal>] ModelPropertyName = "Model"
    let [<Literal>] SurnamesPropertyName = "Surnames"
    let [<Literal>] OverlayNameOptPropertyName = "OverlayNameOpt"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] RefinementDir = "refinement"
    let [<Uniform>] mutable DesiredFrameRate = match ConfigurationManager.AppSettings.["DesiredFrameRate"] with null -> StaticFrameRate 60L | desiredFrameRate -> scvalue<FrameRate> desiredFrameRate
    let [<Literal>] DesiredFrameTimeMinimum = 0.001 // maximum frame rate of 1000 in all configurations.
    let [<Literal>] DesiredFrameTimeSlop = 0.0005
    let [<Uniform>] EntitySize2dDefault = Vector3 (Meter2d, Meter2d, 0.0f)
    let [<Uniform>] EntitySizeGuiDefault = Vector3 (Meter2d * 4.0f, Meter2d, 0.0f)
    let [<Uniform>] EntitySize3dDefault = Vector3 1.0f
    let [<Uniform>] mutable EntityCentered2dDefault = match ConfigurationManager.AppSettings.["EntityCentered2dDefault"] with null -> true | centered -> scvalue<bool> centered
    let [<Uniform>] mutable EntityCenteredGuiDefault = match ConfigurationManager.AppSettings.["EntityCenteredGuiDefault"] with null -> true | centered -> scvalue<bool> centered
    let [<Uniform>] mutable EntityCentered3dDefault = match ConfigurationManager.AppSettings.["EntityCentered3dDefault"] with null -> true | centered -> scvalue<bool> centered
    let [<Uniform>] ParticleSize2dDefault = Vector3 (12.0f, 12.0f, 0.0f)
    let [<Uniform>] ParticleSize3dDefault = Vector3 (0.01f, 0.01f, 0.01f)
    let [<Uniform>] EyeCenter3dDefault = Vector3 (0.0f, 1.0f, 4.0f)
    let [<Uniform>] EyeCenter3dOffset = Vector3 (0.0f, 0.0f, 4.0f)
    let [<Uniform>] QuadtreeGranularity = 3
    let [<Uniform>] QuadtreeDepth = 4
    let [<Uniform>] QuadtreeSize = Vector2 (single (Math.Pow (2.0, 18.0)))
    let [<Uniform>] QuadtreeBounds = Box2 (-QuadtreeSize * 0.5f, QuadtreeSize)
    let [<Uniform>] OctreeGranularity = 3
    let [<Uniform>] OctreeDepth = 4
    let [<Uniform>] OctreeSize = Vector3 (single (Math.Pow (2.0, 10.0)))
    let [<Uniform>] OctreeBounds = Box3 (-OctreeSize * 0.5f, OctreeSize)
    let [<Uniform>] mutable EventTracing = match ConfigurationManager.AppSettings.["EventTracing"] with null -> false | tracing -> scvalue<bool> tracing
    let [<Uniform>] mutable EventFilter = match ConfigurationManager.AppSettings.["EventFilter"] with null -> EventFilter.Empty | filter -> scvalue<EventFilter.Filter> filter

namespace Nu
open System
open System.ComponentModel
open Prime
open Nu

/// Type converter for GameTime.
type GameTimeConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<GameTime>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let gameTime = source :?> GameTime
            match gameTime with
            | UpdateTime time -> Number (string time, ValueNone) :> obj
            | ClockTime time -> Number (string time, ValueNone) :> obj
        elif destType = typeof<GameTime> then source
        else failconv "Invalid GameTime conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<GameTime>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Number (time, _) ->
                match Constants.Engine.DesiredFrameRate with
                | StaticFrameRate _ -> UpdateTime (Int64.Parse time) :> obj
                | DynamicFrameRate _ -> ClockTime (Single.Parse time) :> obj
            | _ -> failconv "Invalid GameTimeConverter conversion from source." (Some symbol)
        | :? GameTime -> source
        | _ -> failconv "Invalid GameTimeConverter conversion from source." None

/// Provide a variable representation of time based on whether the engine is configured to use a static or a dynamic
/// frame rate.
and [<Struct; CustomEquality; CustomComparison; TypeConverter (typeof<GameTimeConverter>)>] GameTime =
    | UpdateTime of UpdateTime : int64 // in updates
    | ClockTime of ClockTime : single // in seconds

    static member inline unary op op2 time =
        match time with
        | UpdateTime time -> op time
        | ClockTime time -> op2 time

    static member inline binary op op2 left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> op leftTime rightTime
        | (ClockTime leftTime, ClockTime rightTime) -> op2 leftTime rightTime
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member inline ap op op2 left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> UpdateTime (op leftTime rightTime)
        | (ClockTime leftTime, ClockTime rightTime) -> ClockTime (op2 leftTime rightTime)
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member make updateTime clockTime =
        match Constants.Engine.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime updateTime
        | DynamicFrameRate _ -> ClockTime clockTime

    static member ofUpdates updates =
        match Constants.Engine.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime updates
        | DynamicFrameRate (Some frameRate) -> ClockTime (1.0f / single frameRate * single updates)
        | DynamicFrameRate None -> failwith "Cannot construct GameTime from updates with uncapped dynamic frame rate."

    static member ofSeconds seconds =
        match Constants.Engine.DesiredFrameRate with
        | StaticFrameRate frameRate -> UpdateTime (int64 (single frameRate * seconds))
        | DynamicFrameRate _ -> ClockTime seconds

    static member toUpdates time =
        match (Constants.Engine.DesiredFrameRate, time) with
        | (_, UpdateTime time) -> time
        | (DynamicFrameRate (Some frameRate), ClockTime time) -> int64 (time / (1.0f / single frameRate))
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member toSeconds time =
        match (Constants.Engine.DesiredFrameRate, time) with
        | (StaticFrameRate frameRate, UpdateTime time) -> 1.0f / single frameRate * single time
        | (_, ClockTime time) -> time
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member equals left right =
        GameTime.binary (=) (=) left right

    static member compare left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | (ClockTime leftTime, ClockTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member progress startTime currentTime lifeTime =
        match (startTime, currentTime, lifeTime) with
        | (UpdateTime startTime, UpdateTime currentTime, UpdateTime lifeTime) -> (single (currentTime - startTime)) / single lifeTime
        | (ClockTime startTime, ClockTime currentTime, ClockTime lifeTime) -> (currentTime - startTime) / lifeTime
        | (_, _, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member (+) (left, right) = GameTime.ap (+) (+) left right
    static member (-) (left, right) = GameTime.ap (-) (-) left right
    static member (*) (left, right) = GameTime.ap (*) (*) left right
    static member (/) (left, right) = GameTime.ap (/) (/) left right
    static member (%) (left, right) = GameTime.ap (%) (%) left right
    static member (+) (left, right) = GameTime.unary ((+) (int64 right) >> UpdateTime) (flip (+) (single right) >> ClockTime) left
    static member (-) (left, right) = GameTime.unary ((-) (int64 right) >> UpdateTime) (flip (-) (single right) >> ClockTime) left
    static member (*) (left, right) = GameTime.unary ((*) (int64 right) >> UpdateTime) (flip (*) (single right) >> ClockTime) left
    static member (/) (left, right) = GameTime.unary ((/) (int64 right) >> UpdateTime) (flip (/) (single right) >> ClockTime) left
    static member (%) (left, right) = GameTime.unary ((%) (int64 right) >> UpdateTime) (flip (%) (single right) >> ClockTime) left
    static member (+) (left, right) = GameTime.unary ((+) (int64 left) >> UpdateTime) ((+) (single left) >> ClockTime) right
    static member (-) (left, right) = GameTime.unary ((-) (int64 left) >> UpdateTime) ((-) (single left) >> ClockTime) right
    static member (*) (left, right) = GameTime.unary ((*) (int64 left) >> UpdateTime) ((*) (single left) >> ClockTime) right
    static member (/) (left, right) = GameTime.unary ((/) (int64 left) >> UpdateTime) ((/) (single left) >> ClockTime) right
    static member (%) (left, right) = GameTime.unary ((%) (int64 left) >> UpdateTime) ((%) (single left) >> ClockTime) right
    static member op_Implicit (i : int64) = UpdateTime i
    static member op_Implicit (s : single) = ClockTime s
    static member op_Explicit time = match time with UpdateTime time -> int time | ClockTime time -> int time
    static member op_Explicit time = match time with UpdateTime time -> int64 time | ClockTime time -> int64 time
    static member op_Explicit time = match time with UpdateTime time -> single time | ClockTime time -> single time
    static member op_Explicit time = match time with UpdateTime time -> double time | ClockTime time -> double time
    static member isZero time = GameTime.unary isZero isZero time
    static member notZero time = GameTime.unary notZero notZero time
    static member zero = GameTime.ofSeconds 0.0f
    static member min (left : GameTime) right = if left <= right then left else right
    static member max (left : GameTime) right = if left >= right then left else right
    static member MinValue = GameTime.make Int64.MinValue Single.MinValue
    static member MaxValue = GameTime.make Int64.MaxValue Single.MaxValue

    member this.Updates =
        GameTime.toUpdates this

    member this.Seconds =
        GameTime.toSeconds this

    member this.IsZero =
        GameTime.isZero this

    member this.NotZero =
        GameTime.notZero this

    override this.Equals that =
        match that with
        | :? GameTime as that -> GameTime.equals this that
        | _ -> false

    override this.GetHashCode () =
        GameTime.unary hash hash this

    interface GameTime IEquatable with
        member this.Equals that =
            GameTime.equals this that

    interface GameTime IComparable with
        member this.CompareTo that =
            GameTime.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? GameTime as that -> (this :> GameTime IComparable).CompareTo that
            | _ -> failwithumf ()

namespace Nu.Constants
open System
open System.Configuration
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render2d = "Render2d"
    let [<Literal>] Render3d = "Render3d"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] VirtualResolutionX = 960
    let [<Literal>] VirtualResolutionY = 540
    let [<Uniform>] VirtualResolution = Vector2i (VirtualResolutionX, VirtualResolutionY)
    let [<Uniform>] VirtualResolutionF = Vector2 (single VirtualResolutionX, single VirtualResolutionY)
    let [<Uniform>] VirtualScalar = match ConfigurationManager.AppSettings.["VirtualScalar"] with null -> 2 | scalar -> scvalue<int> scalar
    let [<Uniform>] VirtualScalarF = single VirtualScalar
    let [<Uniform>] VirtualScalar2i = Vector2i VirtualScalar
    let [<Uniform>] VirtualScalar2 = Vector2 (single VirtualScalar2i.X, single VirtualScalar2i.Y)
    let [<Uniform>] ResolutionX = VirtualResolutionX * VirtualScalar
    let [<Uniform>] ResolutionY = VirtualResolutionY * VirtualScalar
    let [<Uniform>] ResolutionF = Vector2 (single ResolutionX, single ResolutionY)
    let [<Uniform>] Resolution = Vector2i (ResolutionX, ResolutionY)
    let [<Uniform>] FieldOfView = single (Math.PI / 3.0) // 60 degrees
    let [<Literal>] NearPlaneDistanceEnclosed = 0.1f
    let [<Literal>] FarPlaneDistanceEnclosed = 200.0f
    let [<Literal>] NearPlaneDistanceExposed = FarPlaneDistanceEnclosed
    let [<Literal>] FarPlaneDistanceExposed = 400.0f
    let [<Literal>] NearPlaneDistanceImposter = FarPlaneDistanceExposed
    let [<Literal>] FarPlaneDistanceImposter = 800.0f
    let [<Literal>] NearPlaneDistanceOmnipresent = NearPlaneDistanceEnclosed
    let [<Literal>] FarPlaneDistanceOmnipresent = 1600.0f
    let [<Uniform>] ViewportMargin (windowSize : Vector2i) = let size = Vector2i (ResolutionX, ResolutionY) in Vector2i ((windowSize.X - size.X) / 2, (windowSize.Y - size.Y) / 2)
    let [<Uniform>] ViewportOffset (windowSize : Vector2i) = Viewport (NearPlaneDistanceOmnipresent, FarPlaneDistanceOmnipresent, Box2i(ViewportMargin windowSize, Resolution))
    let [<Uniform>] Viewport = Viewport (NearPlaneDistanceOmnipresent, FarPlaneDistanceOmnipresent, Box2i(v2iZero, Resolution))
    let [<Uniform>] PlayBoxSize3d = Vector3 200.0f
    let [<Uniform>] LightBoxSize3d = Vector3 400.0f
    let [<Uniform>] WindowClearColor = Color.White // NOTE: do not change this color as the deferred2 renderer checks if normal color is equal to [1;1;1] to discard fragment.
    let [<Literal>] OpenGlVersionMajor = 4
    let [<Literal>] OpenGlVersionMinor = 1
    let [<Literal>] OpenGlCore = true
    let [<Uniform>] GlslVersionPragma = "#version " + string OpenGlVersionMajor + string OpenGlVersionMinor + "0 " + if OpenGlCore then "core" else ""
    let [<Literal>] TextureAnisotropyMax = 8.0f
    let [<Literal>] SpriteBatchSize = 192
    let [<Literal>] SpriteBorderTexelScalar = 0.01f
    let [<Literal>] SpriteMessagesPrealloc = 256
    let [<Literal>] StaticModelMessagesPrealloc = 256
    let [<Literal>] GeometryBatchPrealloc = 1024
    let [<Literal>] ShaderLightsMax = 8
    let [<Literal>] IrradianceMapResolution = 32
    let [<Literal>] EnvironmentFilterResolution = 128
    let [<Literal>] EnvironmentFilterMips = 5 // NOTE: changing this requires changing the REFLECTION_LOD_MAX constants in shader code.

[<RequireQualifiedAccess>]
module Assimp =

    let [<Literal>] PostProcessSteps = Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GlobalScale

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] SongVolumeDefault = 0.5f
    let [<Literal>] SoundVolumeDefault = 1.0f
    let [<Uniform>] FadeOutTimeDefault = GameTime.ofSeconds 0.5f
    let [<Uniform>] SongResumptionMaximum = GameTime.ofSeconds 90.0f // HACK: prevents songs from starting over too often due to hack in SdlAudioPlayer.playSong.
    let [<Literal>] Frequency = 44100
    let [<Literal>] BufferSizeDefault = 1024
    let [<Literal>] FadeInSecondsMinimum = 0.1f // NOTE: Mix_PlayMusic seems to sometimes cause audio 'popping' when starting a song, so a minimum fade is used instead.

[<RequireQualifiedAccess>]
module Physics =

    let [<Literal>] PhysicsToPixelRatio = Engine.Meter2d // 48 pixels = 1 meter
    let [<Uniform>] PixelToPhysicsRatio = 1.0f / Engine.Meter2d
    let [<Literal>] DensityDefault = 1.0f

[<RequireQualifiedAccess>]
module TileMap =

    let [<Literal>] AnimationPropertyName = "A"
    let [<Literal>] CollisionPropertyName = "C"
    let [<Literal>] ElevationPropertyName = "E"
    let [<Literal>] InfoPropertyName = "I"

[<RequireQualifiedAccess>]
module Particles =

    let [<Literal>] RestitutionDefault = 0.9f

[<RequireQualifiedAccess>]
module Effects =

    let [<Literal>] EffectHistoryMaxDefault = 60 // 1 second of effect history @ 60 fps

[<RequireQualifiedAccess>]
module Ecs =

    let [<Literal>] ArrayReserve = 256 // just large enough to amortize cache misses
    let [<Literal>] ParallelTaskSizeMinimum = 1024
    let [<Literal>] PreallocateAmount = ArrayReserve
    let [<Literal>] IntraComponentPrefix = "@"
    let [<Literal>] UnscheduledEventSuffix = "!U"
    let [<Literal>] ScheduledEventSuffix = "!S"

[<RequireQualifiedAccess>]
module Paths =

    let [<Literal>] SkyBoxShaderFilePath = "Assets/Default/SkyBox.glsl"
    let [<Literal>] IrradianceShaderFilePath = "Assets/Default/Irradiance.glsl"
    let [<Literal>] EnvironmentFilterShaderFilePath = "Assets/Default/EnvironmentFilter.glsl"
    let [<Literal>] PhysicallyBasedDeferredShaderFilePath = "Assets/Default/PhysicallyBasedDeferred.glsl"
    let [<Literal>] PhysicallyBasedDeferred2ShaderFilePath = "Assets/Default/PhysicallyBasedDeferred2.glsl"
    let [<Literal>] PhysicallyBasedForwardShaderFilePath = "Assets/Default/PhysicallyBasedForward.glsl"
    let [<Literal>] BrdfTextureFilePath = "Assets/Default/Brdf.png"