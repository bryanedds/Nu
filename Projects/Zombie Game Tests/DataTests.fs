// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.Numerics
open Microsoft.FSharp.Reflection
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// What needs no world to check - the data sheets, the puppets, the collision the game simulates, the inventory, and
/// each stage's plan of areas and doors.
module DataTests =

    let [<Test>] ``Every item, surface and creature has its data, and every weapon fires ammunition that is.`` () =
        for item in cases<ItemType> () do Assert.IsTrue (Data.Items.ContainsKey item, scstring item)
        for surface in cases<SurfaceType> () do Assert.IsTrue (Data.Surfaces.ContainsKey surface, scstring surface)
        for creatureType in cases<CreatureType> () do Assert.IsTrue (Data.Creatures.ContainsKey creatureType, scstring creatureType)
        for KeyValue (item, weapon) in Data.Weapons do
            match weapon.AmmunitionOpt with
            | Some ammunition -> Assert.AreEqual (Ammunition, Data.Items[ammunition].ItemKind, scstring item)
            | None -> ()

    (* Collision *)

    let [<Test>] ``A circle is pushed out of a box it overlaps, and slides along it rather than stopping.`` () =
        let wall = BoxCollider (v2 0.0f 0.0f, v2 1.0f 0.1f, 0.0f)
        let pushed = Collider.separate 0.3f (v2 0.2f 0.25f) wall
        Assert.AreEqual (float 0.4f, float pushed.Y, 0.001)
        Assert.AreEqual (float 0.2f, float pushed.X, 0.001)
        let settled = Collider.settle 0.3f (v2 0.5f 0.0f) [wall] []
        Assert.IsFalse (Collider.touches 0.299f settled wall)

    let [<Test>] ``A segment finds the first collider it meets, and passes ones it doesn't.`` () =
        let box = BoxCollider (v2 2.0f 0.0f, v2 0.5f 0.5f, 0.0f)
        match Collider.tryCast (v2 0.0f 0.0f) (v2 4.0f 0.0f) box with
        | Some t -> Assert.AreEqual (0.375, float t, 0.001)
        | None -> Assert.Fail "The segment missed the box."
        Assert.IsTrue ((Collider.tryCast (v2 0.0f 2.0f) (v2 4.0f 2.0f) box).IsNone)
        match Collider.tryCast (v2 0.0f 0.0f) (v2 4.0f 0.0f) (CircleCollider (v2 3.0f 0.0f, 0.5f)) with
        | Some t -> Assert.AreEqual (0.625, float t, 0.001)
        | None -> Assert.Fail "The segment missed the circle."

    let [<Test>] ``A turned box collides where it's turned to.`` () =
        let turned = BoxCollider (v2 0.0f 0.0f, v2 1.0f 0.1f, MathF.PI_OVER_2)
        Assert.IsTrue (Collider.contains (v2 0.0f 0.9f) turned)
        Assert.IsFalse (Collider.contains (v2 0.9f 0.0f) turned)

    (* Inventory *)

    let [<Test>] ``Antiseptics mix, antiseptic and coagulant mix, and things put in the item box come back out.`` () =
        let (two, _) = Inventory.add Antiseptic 2 Inventory.empty
        Assert.AreEqual (Some DoubleAntiseptic, Inventory.tryCombine 0 1 two |> Option.bind (fun inventory -> inventory.Slots[0]) |> Option.map _.SlotItem)
        let (pair, _) = Inventory.add Antiseptic 1 Inventory.empty
        let (pair, _) = Inventory.add Coagulant 1 pair
        Assert.AreEqual (Some TraumaMix, Inventory.tryCombine 0 1 pair |> Option.bind (fun inventory -> inventory.Slots[0]) |> Option.map _.SlotItem)
        let (ribbons, _) = Inventory.add InkRibbon 3 Inventory.empty
        let stored = Inventory.store 0 ribbons
        Assert.AreEqual ([(InkRibbon, 3)], Inventory.getBoxed stored)
        Assert.AreEqual (3, Inventory.count InkRibbon (Inventory.retrieve InkRibbon stored))

    (* Puppets *)

    /// Every character - the player and every kind of creature.
    let private Characters =
        Player :: [for creatureType in cases<CreatureType> () -> Creature creatureType]

    let private getPlayedAnimationTypes characterType =
        match characterType with
        | Player -> [AnimationType.Relax; AnimationType.Walk; AnimationType.WalkBack; AnimationType.Aim; AnimationType.Shoot; AnimationType.Ready; AnimationType.Swing; AnimationType.Hurt; AnimationType.Wound]
        | Creature creatureType ->
            match Data.Creatures[creatureType].Behavior with
            | Shambler -> [AnimationType.Idle; AnimationType.Walk; AnimationType.Grab; AnimationType.Hurt; AnimationType.Wound; AnimationType.Sprawl]
            | Hound -> [AnimationType.Idle; AnimationType.Walk; AnimationType.Crouch; AnimationType.Tackle; AnimationType.Hurt; AnimationType.Wound; AnimationType.Sprawl]
            | Brute -> [AnimationType.Idle; AnimationType.Walk; AnimationType.Swing; AnimationType.Crouch; AnimationType.Tackle; AnimationType.Hurt; AnimationType.Wound; AnimationType.Sprawl]

    let private getRequiredMarks animationType =
        match animationType with
        | AnimationType.Swing -> [Mark.Sweep; Mark.Follow; Mark.End]
        | AnimationType.Shoot -> [Mark.Release; Mark.End]
        | AnimationType.Grab -> [Mark.Impact; Mark.End]
        | AnimationType.Sprawl -> [Mark.Release; Mark.End]
        | AnimationType.Hurt | AnimationType.Wound -> [Mark.End]
        | _ -> []

    let [<Test>] ``Every character has a puppet that plays every animation the game has it play, with the marks it times by.`` () =
        Assert.AreEqual (Characters.Length, Data.Puppets.Count)
        for characterType in Characters do
            let puppet = Data.Puppets[characterType]
            for animationType in getPlayedAnimationTypes characterType do
                match puppet.Animations.TryFind animationType with
                | Some animation ->
                    let marks = animation.Segments |> List.ofArray |> List.collect (fun segment -> segment.Marks) |> List.map fst
                    let required = getRequiredMarks animationType
                    Assert.AreEqual (required, marks |> List.filter (fun mark -> List.contains mark required), scstring characterType + "'s " + scstring animationType)
                | None -> Assert.Fail (scstring characterType + " has no " + scstring animationType + " animation.")

    /// Whether a vector is a real place, every part of it finite.
    let private isReal (v : Vector3) =
        Single.IsFinite v.X && Single.IsFinite v.Y && Single.IsFinite v.Z

    let [<Test>] ``Every puppet poses through every animation it plays, every part placed somewhere real.`` () =
        for characterType in Characters do
            let puppet = Data.Puppets[characterType]
            let animations =
                [IdleAnimation; RelaxAnimation; WalkAnimation; WalkBackAnimation; AimAnimation 1.0f; AimAnimation -1.0f; ShootAnimation 0.0f
                 ReadyAnimation 1.0f; ReadyAnimation -1.0f; SwingAnimation; HurtAnimation; WoundAnimation; GrabAnimation; CrouchAnimation; TackleAnimation; SprawlAnimation] |>
                List.filter (fun animation -> List.contains (Puppet.getAnimationType animation) (getPlayedAnimationTypes characterType))
            for animation in animations do
                for step in 0 .. 50 do
                    let time = single step * 0.1f
                    let pose = Puppet.poseOf time time puppet v3Back (characterType <> Player) animation
                    for part in Puppet.place time puppet pose v3Zero 0.0f do
                        Assert.IsTrue (isReal part.PartPosition && isReal part.PartScale, scstring characterType + " " + scstring animation)

    (* Scenes *)

    let [<Test>] ``Every area is one scene, resolved the same way every time, watched by a fixed eye.`` () =
        Assert.AreEqual ((cases<HospitalAreaType> ()).Length + (cases<DormAreaType> ()).Length + (cases<PlantAreaType> ()).Length, Data.Areas.Count)
        for area in Data.Areas.Values do
            Assert.IsNotEmpty (area.Cameras, scstring area.AreaType + " has no Camera.")
            Assert.AreEqual (Resolver.dump (Resolver.resolve Constants.Gameplay.FixedGameSeed Data.Scenes[area.Scene] Data.AppearancesLibrary), Resolver.dump area, scstring area.AreaType + " resolves differently a second time.")

    let [<Test>] ``Every door has its partner of the same name in the area it leads to, alike and leading back.`` () =
        for area in Data.Areas.Values do
            for (door, enter) in getDoors area.AreaType do
                match getDoors enter.To |> List.tryFind (fun (other, _) -> other.PropName = door.PropName) with
                | Some (_, partner) ->
                    Assert.AreEqual (area.AreaType, partner.To, door.PropName + " leads to a partner that leads elsewhere.")
                    Assert.AreEqual (enter.Style, partner.Style, door.PropName + " looks one way and its partner another.")
                | None -> Assert.Fail (scstring area.AreaType + "'s " + door.PropName + " has no partner of its name in " + scstring enter.To + ".")

    let [<Test>] ``Every door's stand, every other prop interacted with and every creature of an area is within its walls.`` () =
        for area in Data.Areas.Values do
            for prop in area.Props do
                match (prop.EnterOpt, prop.PropInteractOpt) with
                | (Some _, _) -> Assert.IsTrue (area.IsOpen (fst (getDoorStand prop)), prop.PropName)
                | (None, Some _) -> Assert.IsTrue (area.IsOpen (Collider.toFloor prop.PropSpot), prop.PropName)
                | (None, None) -> ()
            for enemy in area.Enemies do
                Assert.IsTrue (area.IsOpen (Collider.toFloor enemy.EnemyPosition), enemy.EnemyName)

    let [<Test>] ``Every prop interacted with that has a solid body is interacted with at a spot within it.`` () =
        for area in Data.Areas.Values do
            for prop in area.Props do
                if prop.PropInteractOpt.IsSome && prop.EnterOpt.IsNone && not (Array.isEmpty prop.PropColliders) then
                    let spot = Collider.toFloor prop.PropSpot
                    Assert.IsTrue (prop.PropColliders |> Array.exists (Collider.contains spot), scstring area.AreaType + "'s " + prop.PropName + " is interacted with outside its body, so one standing right at it turns away to act on it.")

    let [<Test>] ``Every pickup and creature's name is its own.`` () =
        let pickups = Data.Areas.Values |> List.ofSeq |> List.collect (fun area -> area.Props |> List.filter (fun prop -> prop.PickUpOpt.IsSome) |> List.map _.PropName)
        Assert.AreEqual (pickups.Length, (List.distinct pickups).Length, "Pickups share a name.")
        let enemies = Data.Areas.Values |> List.ofSeq |> List.collect (fun area -> area.Enemies |> List.map _.EnemyName)
        Assert.AreEqual (enemies.Length, (List.distinct enemies).Length, "Creatures share a name.")

    /// Every flag play can raise - each pickup's and each item given as one's taken, each enemy's killed, and enraged
    /// if it can be, each locked door's unlocked, and every flag a cue sets.
    let private RaisableFlags =
        Set.ofList
            [for area in Data.Areas.Values do
                for prop in area.Props do
                    if prop.PickUpOpt.IsSome then Taken prop.PropName
                    match prop.EnterOpt with
                    | Some { Key = Some _ } -> Unlocked prop.PropName
                    | Some _ | None -> ()
                    for interact in Option.toList prop.PropInteractOpt do
                        for cue in Cue.getAll interact.Cue do
                            match cue with
                            | SetFlag flag -> flag
                            | GiveAs (name, _, _) -> Taken name
                            | Fin | Say _ | If _ | Sequence _ | Give _ | Cue.Take _ | Enter _ | PickUp _ | Save | OpenBox | Keypad _ | Sound _ | Rouse | Cue.Travel _ | Escape -> ()
                for enemy in area.Enemies do
                    Killed enemy.EnemyName
                    if Data.Creatures[enemy.EnemyCreatureType].EnrageAt > 0.0f then Enraged enemy.EnemyName]

    let [<Test>] ``Every flag a scene looks for or a chapter assumes is one play can raise.`` () =
        let lookedFor =
            [for area in Data.Areas.Values do
                let name = scstring area.AreaType
                for prop in area.Props do
                    let conditions = Option.toList prop.PropConditionOpt @ (prop.PropInteractOpt |> Option.toList |> List.collect (fun interact -> Cue.getConditions interact.Cue))
                    for condition in conditions do
                        for flag in CueCondition.getFlags condition -> (name + "'s " + prop.PropName, flag)
                for enemy in area.Enemies do
                    for condition in Option.toList enemy.EnemyConditionOpt do
                        for flag in CueCondition.getFlags condition -> (name + "'s " + enemy.EnemyName, flag)
                for song in area.Songs do
                    for condition in Option.toList song.When do
                        for flag in CueCondition.getFlags condition -> (name + "'s songs", flag)
             for scenario in Scenario.All do
                for flag in scenario.ScenarioFlags -> (scenario.ScenarioName, flag)]
        let unraisable = lookedFor |> List.filter (fun (_, flag) -> not (RaisableFlags.Contains flag))
        Assert.IsEmpty (unraisable, "Flags looked for that nothing raises: " + String.Join (", ", unraisable |> List.map (fun (where, flag) -> where + " " + scstring flag)) + ".")

    let [<Test>] ``Every line read on screen - said, tried at a locked door, or captioned travelling - fits without being clipped.`` () =
        let lines =
            [for area in Data.Areas.Values do
                for prop in area.Props do
                    for interact in Option.toList prop.PropInteractOpt do
                        for cue in Cue.getAll interact.Cue do
                            let where = scstring area.AreaType + "'s " + prop.PropName
                            match cue with
                            | Say says -> for line in says -> (where, line)
                            | Enter enter -> yield (where, enter.LockedLine)
                            | Cue.Travel travel -> yield (where, travel.Caption)
                            | Fin | SetFlag _ | If _ | Sequence _ | Give _ | GiveAs _ | Cue.Take _ | PickUp _ | Save | OpenBox | Keypad _ | Sound _ | Rouse | Escape -> ()]
        let clipped = lines |> List.filter (fun (_, line) -> line.Length > Constants.Gui.LineLengthMax)
        Assert.IsEmpty (clipped, "Lines longer than " + string Constants.Gui.LineLengthMax + " characters: " + String.Join (" ", clipped |> List.map (fun (where, line) -> where + " (" + string line.Length + "): " + line)))

    let [<Test>] ``Every area's last song plays with no condition, and every keypad's code can be entered on its keys.`` () =
        for area in Data.Areas.Values do
            match List.tryLast area.Songs with
            | Some song -> Assert.IsTrue (song.When.IsNone, scstring area.AreaType + "'s last song has a condition, so as play stands no song might play.")
            | None -> Assert.Fail (scstring area.AreaType + " has no songs.")
            for prop in area.Props do
                for interact in Option.toList prop.PropInteractOpt do
                    for cue in Cue.getAll interact.Cue do
                        match cue with
                        | Keypad keypad ->
                            Assert.IsNotEmpty (keypad.Code, prop.PropName + "'s keypad has no code.")
                            Assert.IsTrue (keypad.Code |> Seq.forall (fun digit -> Char.IsDigit digit && Array.contains (string digit) Constants.Gui.KeypadKeys), prop.PropName + "'s keypad code " + keypad.Code + " can't be entered on its keys.")
                        | Fin | Say _ | SetFlag _ | If _ | Sequence _ | Give _ | GiveAs _ | Cue.Take _ | Enter _ | PickUp _ | Save | OpenBox | Sound _ | Rouse | Cue.Travel _ | Escape -> ()
