// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// Every area as its scene builds it - that one can stand where one arrives and before every door, reach every door,
/// feature and pickup, that every creature starts clear, and that a fixed eye watches every spot one can stand - checked
/// on the areas' data, needing no world. And that the game, declaring an area, builds its scenery and freezes it.
module AreaTests =

    /// The flag sets an area's looked over with - as the game starts, and late on, everything conditional showing.
    let private FlagSets =
        [Set.empty
         Set.ofList [PowerRestored; Taken "MorgueFuse"; Examined "MorgueDrawer"; Killed "Surgeon"; Killed "Linebacker"; Examined "LineSwitch"; Killed "Sow"; Enraged "MeatKing"; Killed "MeatKing"]]

    /// An area as it stands with what's been done - what's solid in it, and what can be acted on.
    type private Look =
        { Area : Area
          Colliders : Collider array
          Targets : (string * Vector2 * single) list } // what can be acted on, by name, where its middle is, and how far its body reaches from it

    /// Whether a condition holds by the flags alone, nothing carried, if there is one.
    let private holdsOf (flags : Advent Set) conditionOpt =
        match conditionOpt with
        | Some condition -> CueCondition.holdsBy flags.Contains (fun _ -> false) condition
        | None -> true

    let private lookAt areaType (flags : Advent Set) =
        let area = Data.Areas[areaType]
        let colliders =
            Array.append area.Colliders
                (area.Props |> List.filter (fun prop -> prop.PropConditionOpt.IsSome && holdsOf flags prop.PropConditionOpt) |> Array.ofList |> Array.collect _.PropColliders)
        let targets =
            [for prop in area.Props do
                if prop.PropInteractOpt.IsSome && holdsOf flags prop.PropConditionOpt && not (prop.PickUpOpt.IsSome && flags.Contains (Taken prop.PropName)) then
                    (prop.PropName, Collider.toFloor prop.PropSpot, prop.PropReach)]
        { Area = area; Colliders = colliders; Targets = targets }

    /// Look over every area with every flag set.
    let private lookOver (check : AreaType -> Advent Set -> Look -> unit) =
        for flags in FlagSets do
            for area in Data.Areas.Keys do
                check area flags (lookAt area flags)

    let private canStand look (point : Vector2) =
        look.Colliders |> Array.forall (fun collider -> not (Collider.touches Constants.Gameplay.PlayerRadius point collider))

    /// What the player would act on, standing at a spot facing a way - the nearest thing it reaches, as the game picks.
    let private getActedOn look (spot : Vector2) yaw =
        look.Targets |>
        List.filter (fun (_, target, reach) -> Collider.reaches spot yaw target reach) |>
        List.sortBy (fun (_, target, reach) -> (target - spot).Length () - reach) |>
        List.tryHead |>
        Option.map (fun (name, _, _) -> name)

    /// The spots about the middle of a thing one might stand at to reach it, across its body.
    let private around (point : Vector2) (reach : single) =
        [for distance in [0.0f; 0.4f; 0.55f; 0.7f; 0.85f; 1.0f] do
            for i in 0 .. 23 do
                let angle = single i / 24.0f * MathF.Tau
                point + v2 (sin angle) (cos angle) * (distance + reach)]

    /// Whether something can be acted on from somewhere one can stand about it, facing it.
    let private canReach look name (target : Vector2) reach =
        around target reach |> List.exists (fun spot -> canStand look spot && getActedOn look spot (Math.YawOf (target - spot + v2 0.0f 0.0001f)) = Some name)

    let [<Test>] ``Every area's arrivals are where one can stand, and each door's acted on from where one arrives through it, turned round.`` () =
        lookOver (fun area flags look ->
            let name = scstring area + " " + scstring flags
            for arrival in look.Area.Arrivals do
                Assert.IsTrue (canStand look arrival.At, name + ": one can't stand at its arrival " + arrival.Name + ".")
            for (door, _) in getDoors area do
                let (stand, yaw) = getDoorStand door
                Assert.IsTrue (canStand look stand, name + ": one can't stand before " + door.PropName + ".")
                Assert.AreEqual (Some door.PropName, getActedOn look stand yaw, name + ": " + door.PropName + " isn't what's acted on from before it."))

    let [<Test>] ``Every prop that's examined or picked up can be acted on from somewhere one can stand.`` () =
        lookOver (fun area flags look ->
            let name = scstring area + " " + scstring flags
            for (target, at, reach) in look.Targets do
                if not (getDoors area |> List.exists (fun (door, _) -> door.PropName = target)) then
                    Assert.IsTrue (canReach look target at reach, name + ": " + target + " can't be reached."))

    let [<Test>] ``Every creature starts where it can stand.`` () =
        lookOver (fun area flags look ->
            for enemy in look.Area.Enemies do
                if holdsOf flags enemy.EnemyConditionOpt then
                    let radius = Data.Creatures[enemy.EnemyCreatureType].Radius
                    Assert.IsTrue (look.Colliders |> Array.forall (fun collider -> not (Collider.touches radius (Collider.toFloor enemy.EnemyPosition) collider)), scstring area + ": " + enemy.EnemyName + " starts in something solid."))

    /// The creatures set by hand to spring on the player near where it comes in, so spared the clearance below.
    let private Ambushers = Set.ofList ["EastHallDog"]

    let [<Test>] ``Every creature starts far enough from where one comes into its area for the player to react.`` () =
        let crowded =
            [for area in Data.Areas.Values do
                let arrivals =
                    [for (door, _) in getDoors area.AreaType -> (door.PropName, fst (getDoorStand door))] @
                    [for arrival in area.Arrivals -> (arrival.Name, arrival.At)]
                for enemy in area.Enemies |> List.filter (fun enemy -> not (Ambushers.Contains enemy.EnemyName)) do
                    let clearance =
                        if enemy.EnemyLying then Constants.Gameplay.LyingArrivalClearance
                        else
                            match Data.Creatures[enemy.EnemyCreatureType].Behavior with
                            | Hound -> Constants.Gameplay.HoundArrivalClearance
                            | Shambler | Brute -> Constants.Gameplay.ArrivalClearance
                    for (name, at) in arrivals do
                        let distance = (Collider.toFloor enemy.EnemyPosition - at).Length ()
                        if distance < clearance then
                            scstring area.AreaType + "'s " + enemy.EnemyName + " is " + distance.ToString "0.0" + " m from " + name + ", not " + string clearance]
        Assert.IsEmpty (crowded, "Creatures start too near where the player comes in:\n" + String.Join ("\n", crowded))

    let [<Test>] ``Every spot one can stand in an area is watched by a fixed eye's zone.`` () =
        lookOver (fun area _ look ->
            let low = look.Area.WalkMin - v2 0.3f 0.3f
            let size = look.Area.WalkMax - look.Area.WalkMin + v2 0.6f 0.6f
            let unwatched =
                [for x in 0 .. 30 do
                    for z in 0 .. 30 do
                        let point = low + size * v2 (single x / 30.0f) (single z / 30.0f)
                        let watched = look.Area.Cameras |> List.exists (fun camera -> point.X >= camera.CameraMin.X && point.X <= camera.CameraMax.X && point.Y >= camera.CameraMin.Y && point.Y <= camera.CameraMax.Y)
                        if canStand look point && not watched then point]
            Assert.IsEmpty (unwatched, scstring area + " has spots no fixed eye watches, as " + (unwatched |> List.truncate 3 |> List.map string |> String.concat ", ")))

    let [<Test>] ``Every area the game declares has its scenery built from its scene and frozen, and its doors, features and pickups declared.`` () =
        let world = makeWorld ()
        try
            for flags in FlagSets do
                for area in Data.Areas.Keys do
                    startIn area flags Armed world
                    holdStill world
                    let name = scstring area + " " + scstring flags
                    let scenery = Simulants.GameplayArea area.Name / "Scenery"
                    Assert.IsTrue (scenery.GetExists world, name + " has no scenery.")
                    let parts = scenery.GetChildren world |> Seq.filter (fun child -> child.Name.StartsWith "Part+") |> Seq.length
                    let expected = Data.Areas[area].StaticParts.Length
                    Assert.AreEqual (expected, parts, name + "'s scenery has the wrong number of parts.")
                    Assert.IsTrue (scenery.GetFrozen world, name + "'s scenery isn't frozen.")
                    Assert.IsNotEmpty (scenery.GetFrozenPreBatches world, name + "'s scenery froze into no batches.")
                    for (target, _, _) in (lookAt area flags).Targets do
                        Assert.IsTrue ((Simulants.GameplayArea area.Name / target).GetExists world, name + " didn't declare " + target + ".")
        finally endWorld world
