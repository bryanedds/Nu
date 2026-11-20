// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// The ground a layout lays - where walkers can go, and the walls props can be set against.
type GroundLaid =
    { IsOpen : Vector2 -> bool // whether a point of the ground is open, rather than in or beyond the walls
      WallFace : WallType -> single -> Vector3 * single // where a wall's face is at a point along it, and the way it faces into the area
      WalkMin : Vector2
      WalkMax : Vector2 }

/// A fixed eye of an area - the stretch of floor it watches, where it stands, what it looks at and how wide it sees.
type Camera =
    { CameraMin : Vector2
      CameraMax : Vector2
      CameraEye : Vector3
      CameraTarget : Vector3
      CameraFieldOfView : single } // radians

/// A creature of an area once placed - its name, what kind of creature it is, where it stands and faces, whether it
/// starts lying on the floor, and the condition it's there only while, if any.
type Enemy =
    { EnemyName : string
      EnemyCreatureType : CreatureType
      EnemyPosition : Vector3
      EnemyYaw : single
      EnemyLying : bool
      EnemyConditionOpt : CueCondition option }

/// An area of a stage, resolved from its scene - its props, the scenery of those always there, what blocks walkers,
/// where they may walk, its creatures, where one arrives other than through a door, its fixed eyes, the lights shining
/// in it, and its songs. The parts, colliders and lights of the props always there are gathered together, those of
/// the props there only while a condition holds kept apart on them.
type Area =
    { AreaType : AreaType
      AreaName : string
      Scene : string // the scene it's resolved from, by name
      Props : Prop list // the layout first, named "Layout", then the scene file's props in order
      Parts : Part array
      StaticParts : (int * Part) array // those frozen with the scenery, by their index among the parts
      DynamicParts : (int * Part) array // those whose look changes from frame to frame, declared every frame, likewise
      Colliders : Collider array
      Lights : Light list
      Enemies : Enemy list
      Arrivals : ArrivalData list
      Cameras : Camera list // in the order the scene gives them, the first watching a spot winning it
      WalkMin : Vector2
      WalkMax : Vector2
      IsOpen : Vector2 -> bool // whether a point of the ground is open, rather than in or beyond the walls
      Bounds : Box3 // for the light probe to cover
      ProbePosition : Vector3
      AmbientBrightness : single // how bright the light probe's ambient light is
      Songs : SongData list } // the first whose condition holds playing

    /// Whether a collider holds a point, give or take a little.
    static let nearlyContains collider (point : Vector2) =
        match collider with
        | CircleCollider (center, radius) -> (point - center).Length () <= radius + 0.1f
        | BoxCollider (center, halfExtents, angle) ->
            let c = cos angle
            let s = sin angle
            let delta = point - center
            let local = v2 (delta.X * c - delta.Y * s) (delta.X * s + delta.Y * c)
            abs local.X <= halfExtents.X + 0.1f && abs local.Y <= halfExtents.Y + 0.1f

    /// Where one arrives in an area by an arrival's name, and the way one faces there.
    static member getArrival name (area : Area) =
        match area.Arrivals |> List.tryFind (fun arrival -> arrival.Name = name) with
        | Some arrival -> (v3 arrival.At.X 0.0f arrival.At.Y, Math.DegreesToRadiansF arrival.Facing)
        | None -> failwith ("Scene '" + area.Scene + "' has no arrival named '" + name + "'.")

    /// Build an area from what its scene resolves to - what makes it an area, the ground its layout lays, its props
    /// placed and built, its enemies placed, its arrivals and its eyes.
    static member make scene (data : AreaData) (ground : GroundLaid) (props : Prop list) enemies arrivals cameras =
        let always = props |> List.filter (fun prop -> prop.PropConditionOpt.IsNone)
        let parts = always |> Array.ofList |> Array.collect _.PropParts
        { AreaType = data.Type
          AreaName = data.Name
          Scene = scene
          Props = props
          Parts = parts
          StaticParts = parts |> Array.indexed |> Array.filter (fun (_, part) -> not part.Dynamic)
          DynamicParts = parts |> Array.indexed |> Array.filter (fun (_, part) -> part.Dynamic)
          Colliders = always |> Array.ofList |> Array.collect _.PropColliders
          Lights = always |> List.collect _.PropLights
          Enemies = enemies
          Arrivals = arrivals
          Cameras = cameras
          WalkMin = ground.WalkMin
          WalkMax = ground.WalkMax
          IsOpen = ground.IsOpen
          Bounds = data.Bounds
          ProbePosition = data.Probe
          AmbientBrightness = data.Ambient
          Songs = data.Songs }

    /// The colliders of an area that stand taller than a creature's eyes, as walls and cabinets, so it can't see past
    /// them - found by how high the parts standing on each reach, rising from below the eyes past them, so what hangs
    /// overhead, as the ceiling, doesn't count. Props there only while a condition holds aren't among them.
    static member getSightBlockers (area : Area) =
        area.Colliders |>
        Array.filter (fun collider ->
            area.Parts |>
            Array.exists (fun part ->
                nearlyContains collider (v2 part.PartPosition.X part.PartPosition.Z) &&
                part.Bottom < Constants.Gameplay.SightBlockHeight &&
                part.Top >= Constants.Gameplay.SightBlockHeight))

    /// Where one comes into an area - at its first arrival, as where the van pulls in, or else through the first of its
    /// doors - and the way one faces there.
    static member getEntrance (area : Area) =
        match area.Arrivals with
        | arrival :: _ -> Area.getArrival arrival.Name area
        | [] -> area.Props |> List.find (fun prop -> prop.EnterOpt.IsSome) |> Prop.getArrival