// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.IO
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// The game played headless, through the player's intent - from a new game to the handcar rolling off and the credits,
/// and the rules of play along the way: locked doors, the keypad, saving and loading, the dead staying dead, and dying.
module PlayTests =

    let private play (test : Bot -> unit) =
        let world = makeWorld ()
        let bot = Bot.make world
        try test bot
        finally endWorld world

    let private door area name =
        getDoors area |> List.find (fun (door, _) -> door.PropName = name)

    (* The Scenarios *)

    let private getScenario number =
        Scenario.All |> List.item (dec number)

    /// Play a scenario from its start, as the editor starts it, fighting off whatever's there.
    let private playScenario number (playOn : Bot -> unit) =
        let scenario = getScenario number
        play (fun bot ->
            startEditMode scenario.ScenarioName bot.World
            Assert.AreEqual (scenario.ScenarioArea, Simulants.Gameplay.GetAreaType bot.World)
            Bot.fight bot
            playOn bot
            TestContext.WriteLine
                (scenario.ScenarioName + ": " + string (Simulants.Gameplay.GetPlayTime bot.World) + " s of play, " + string bot.HurtTaken + " hurt taken, " +
                 string bot.Healed + " healed, shots " + scstring bot.Shots + "."))

    /// Check play's left what the next scenario assumes - everything it's had done, and every weapon and key item it
    /// carries, carried or in the item box.
    let private leadsTo number (bot : Bot) =
        let next = getScenario number
        let world = bot.World
        let missing = Set.difference next.ScenarioFlags (Simulants.Gameplay.GetAdvents world)
        Assert.IsEmpty (missing, next.ScenarioName + " assumes what play hasn't done: " + scstring missing + ".")
        let inventory = Simulants.GameplayPlayer.GetInventory world
        for slot in next.ScenarioInventory.Slots do
            match slot with
            | Some slot when (match Data.Items[slot.SlotItem].ItemKind with Weapon _ | KeyItem -> true | _ -> false) ->
                let held = Inventory.has slot.SlotItem inventory || Map.containsKey slot.SlotItem inventory.Box
                Assert.IsTrue (held, next.ScenarioName + " assumes a " + scstring slot.SlotItem + " play didn't leave.")
            | _ -> ()

    let [<Test>] ``Scenario 1 - from a new game, the guard's gun, a save, and the records drawer's key.`` () =
        playScenario 1 (fun bot ->
            let world = bot.World
            Bot.examine "Entrance" bot
            Bot.take "GuardHandgun" bot
            Bot.take "GuardBullets" bot
            Bot.equip Handgun bot
            Bot.goTo (Hospital NurseStation) bot
            Bot.take "StationRibbons" bot
            Bot.take "StationAntiseptic" bot
            Bot.examine "Whiteboard" bot
            Bot.save bot
            Bot.goTo (Hospital WestHall) bot
            Bot.take "WestBullets" bot
            Bot.take "WestAntiseptic" bot
            Bot.goTo (Hospital Records) bot
            Bot.take "RecordsBullets" bot
            Bot.enterCode "Desk" (getCode (Hospital Records) "Desk") bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Examined "RecordsDrawer") world)
            Assert.IsTrue (Simulants.GameplayPlayer.Carries PharmacyKey world, "The drawer didn't give the pharmacy key.")
            leadsTo 2 bot)

    let [<Test>] ``Scenario 2 - into the pharmacy with its key, and out with the morgue's.`` () =
        playScenario 2 (fun bot ->
            let world = bot.World
            Bot.goTo (Hospital Pharmacy) bot
            Assert.IsFalse (Simulants.GameplayPlayer.Carries PharmacyKey world, "The pharmacy key wasn't used up opening the pharmacy.")
            Bot.take "PharmacyMorgueKey" bot
            Bot.take "PharmacyCoagulant" bot
            Bot.take "PharmacyAntiseptic" bot
            Bot.take "PharmacyBullets" bot
            leadsTo 3 bot)

    let [<Test>] ``Scenario 3 - the morgue's fuse, and out before its dead are on their feet.`` () =
        playScenario 3 (fun bot ->
            let world = bot.World
            Bot.goTo (Hospital Morgue) bot
            Bot.snatch "MorgueFuse" bot
            runUntil (fun world -> Simulants.Gameplay.HasTranspired (Examined "MorgueDrawer") world) 1.0 "banged the morgue's drawer open" world
            Bot.runFor (Hospital EastHall) bot
            leadsTo 4 bot)

    let [<Test>] ``Scenario 4 - the power restored, the shotgun, and a save.`` () =
        playScenario 4 (fun bot ->
            let world = bot.World
            Bot.goTo (Hospital Generator) bot
            Bot.examine "FusePanel" bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired PowerRestored world, "The fuse didn't restore the power.")
            Bot.take "GeneratorShells" bot
            Bot.goTo (Hospital Security) bot
            Bot.take "SecurityShotgun" bot
            Bot.take "SecurityShells" bot
            Bot.take "SecurityBullets" bot
            Bot.take "SecuritySpray" bot
            Bot.goTo (Hospital NurseStation) bot
            Bot.heal bot
            Bot.save bot
            leadsTo 5 bot)

    let [<Test>] ``Scenario 5 - the surgeon dead, its keys, and the ambulance driving off to the dorm.`` () =
        playScenario 5 (fun bot ->
            let world = bot.World
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed "Surgeon") world, "The surgeon wasn't killed.")
            Bot.take "SurgeonKeys" bot
            Bot.examine "Ambulance" bot
            Bot.travel (Dorm Lot) bot
            leadsTo 6 bot)

    let [<Test>] ``Scenario 6 - the dorm's ground floor, a student ID in a dryer, and the stairwell's card reader.`` () =
        playScenario 6 (fun bot ->
            let world = bot.World
            Bot.take "LotBullets" bot
            Bot.examine "Ambulance" bot
            Bot.goTo (Dorm Lobby) bot
            Bot.examine "SignInSheet" bot
            Bot.examine "Plaque" bot
            Bot.take "LobbyAntiseptic" bot
            Bot.goTo (Dorm Lounge) bot
            Bot.take "LoungeRibbons" bot
            Bot.take "LoungeAntiseptic" bot
            Bot.save bot
            Bot.goTo (Dorm EastCorridor) bot
            Bot.take "CorridorBullets" bot
            Bot.goTo (Dorm Room104) bot
            Bot.examine "Note" bot
            Bot.take "Room104Antiseptic" bot
            Bot.goTo (Dorm Laundry) bot
            Bot.ensureRoom bot
            Bot.examine "DryerThree" bot
            Assert.IsTrue (Simulants.GameplayPlayer.Carries StudentId world, "Dryer 3 didn't give the student ID.")
            Bot.take "LaundryBullets" bot
            Bot.goTo (Dorm Showers) bot
            Bot.take "ShowersCoagulant" bot
            Bot.take "ShowersBullets" bot
            Bot.goTo (Dorm Stairwell) bot
            Assert.IsFalse (Simulants.GameplayPlayer.Carries StudentId world, "The student ID wasn't used up opening the stairwell.")
            leadsTo 7 bot)

    let [<Test>] ``Scenario 7 - the upper floor, the RA's keypad, the magnum and the master key.`` () =
        playScenario 7 (fun bot ->
            let world = bot.World
            Bot.goTo (Dorm UpperCorridor) bot
            Bot.enterCode "RaKeypad" (getCode (Dorm UpperCorridor) "RaKeypad") bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Examined "RaKeypad") world, "The RA's keypad didn't take the code.")
            Bot.goTo (Dorm RaOffice) bot
            Bot.take "RaMagnum" bot
            Bot.take "RaMasterKey" bot
            Bot.take "RaRounds" bot
            Bot.take "RaRibbon" bot
            Bot.goTo (Dorm Room212) bot
            Bot.examine "StickyNote" bot
            Bot.take "Room212Rounds" bot
            leadsTo 8 bot)

    let [<Test>] ``Scenario 8 - the linebacker dead, the van's keys, and the van driving off to the plant.`` () =
        playScenario 8 (fun bot ->
            let world = bot.World
            Bot.goTo (Dorm Gym) bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed "Linebacker") world, "The linebacker wasn't killed.")
            Bot.take "GymVanKeys" bot
            Bot.goTo (Dorm Lot) bot
            Bot.examine "Van" bot
            Bot.travel (Plant Yard) bot
            leadsTo 9 bot)

    let [<Test>] ``Scenario 9 - the plant's front half, the smokehouse's board, and Ray's bolt cutters.`` () =
        playScenario 9 (fun bot ->
            let world = bot.World
            Bot.take "YardShells" bot
            Bot.goTo (Plant Dock) bot
            Bot.take "DockAntiseptic" bot
            Bot.goTo (Plant Office) bot
            Bot.take "OfficeRibbons" bot
            Bot.take "OfficeAntiseptic" bot
            Bot.examine "Radio" bot
            Bot.examine "Memo" bot
            Bot.save bot
            Bot.goTo (Plant KillFloor) bot
            Bot.take "KillAntiseptic" bot
            Bot.goTo (Plant Lockers) bot
            Bot.examine "LockerNote" bot
            Bot.take "LockersShells" bot
            Bot.goTo (Plant Grinder) bot
            Bot.take "GrinderRounds" bot
            Bot.goTo (Plant Smokehouse) bot
            Bot.examine "Whiteboard" bot
            Bot.take "SmokeAntiseptic" bot
            Bot.take "SmokeShells" bot
            Bot.goTo (Plant Lockers) bot
            Bot.enterCode "RayLocker" (getCode (Plant Lockers) "RayLocker") bot
            Assert.IsTrue (Simulants.GameplayPlayer.Carries BoltCutters world, "Ray's locker didn't give the bolt cutters.")
            leadsTo 10 bot)

    let [<Test>] ``Scenario 10 - the defrost valve, the foreman's keycard, and the Guardsman's launcher.`` () =
        playScenario 10 (fun bot ->
            let world = bot.World
            Bot.goTo (Plant Boiler) bot
            Bot.take "BoilerAntiseptic" bot
            Bot.take "BoilerShells" bot
            Bot.examine "BoilerValve" bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Examined "BoilerValve") world, "The defrost valve didn't open.")
            Bot.goTo (Plant ColdStorage) bot
            Bot.ensureRoom bot
            Bot.examine "FrozenForeman" bot
            Assert.IsTrue (Simulants.GameplayPlayer.Carries Keycard world, "The foreman didn't give his keycard.")
            Bot.take "ColdSpray" bot
            Bot.take "ColdRounds" bot
            Bot.goTo (Plant ControlRoom) bot
            Assert.IsFalse (Simulants.GameplayPlayer.Carries Keycard world, "The keycard wasn't used up opening the control room.")
            Bot.examine "Guardsman" bot
            Bot.take "ControlLauncher" bot
            Bot.take "ControlGrenades" bot
            Bot.examine "LineSwitch" bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Examined "LineSwitch") world, "The line's switch wasn't thrown.")
            leadsTo 11 bot)

    let [<Test>] ``Scenario 11 - the rendering room's chain cut, the sow dead, and the freight elevator's key.`` () =
        playScenario 11 (fun bot ->
            let world = bot.World
            Bot.goTo (Plant Rendering) bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed "Sow") world, "The sow wasn't killed.")
            Bot.take "RenderingElevatorKey" bot
            Bot.take "RenderingGrenades" bot
            leadsTo 12 bot)

    let [<Test>] ``Scenario 12 - the meat king raging and dead, the handcar rolling off, and the credits.`` () =
        playScenario 12 (fun bot ->
            let world = bot.World
            Bot.goTo (Plant Pit) bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Enraged "MeatKing") world, "The meat king never raged.")
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed "MeatKing") world, "The meat king wasn't killed.")
            Bot.goTo (Plant Platform) bot
            Bot.examine "Handcar" bot
            match Simulants.Gameplay.GetMode world with
            | Escaped _ -> ()
            | mode -> Bot.fail ("Didn't escape, but " + scstring mode + ".") bot
            runUntil (fun world -> match Game.GetGameState world with Credits _ -> true | _ -> false) 10.0 "came to the credits" world
            runUntil (fun world -> match Game.GetGameState world with Ending _ -> true | _ -> false) 120.0 "rolled the credits through to the ending" world)

    (* The Rules *)

    let [<Test>] ``A locked door stays shut without its key, and the area stays the area.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital EastHall) (Set.singleton (Killed "EastOrderly")) Armed world
            let (morgueDoors, enter) = door (Hospital EastHall) "MorgueDoors"
            let (stand, yaw) = getDoorStand morgueDoors
            Bot.placeAt stand yaw bot
            Bot.act bot
            match Simulants.Gameplay.GetMode world with
            | Reading reading -> Assert.AreEqual (enter.LockedLine, reading.Lines.Head)
            | mode -> Assert.Fail ("The locked door gave " + scstring mode + ".")
            Bot.readThrough bot
            runFor 0.5 world
            Assert.AreEqual (Hospital EastHall, Simulants.Gameplay.GetAreaType world)
            Assert.AreEqual (Exploring, Simulants.Gameplay.GetMode world))

    let [<Test>] ``A door that needs the power stays shut until it's back.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital EastHall) (Set.singleton (Killed "EastOrderly")) Armed world
            let (securityDoor, _) = getDoors (Hospital EastHall) |> List.find (fun (_, enter) -> enter.To = Hospital Security)
            let (stand, yaw) = getDoorStand securityDoor
            Bot.placeAt stand yaw bot
            Bot.act bot
            Bot.readThrough bot
            runFor 0.5 world
            Assert.AreEqual (Hospital EastHall, Simulants.Gameplay.GetAreaType world))

    let [<Test>] ``A wrong code doesn't open the records drawer, and the right one does.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital Records) (Set.singleton (Killed "RecordsPatient")) Armed world
            Bot.examine "Desk" bot
            for digit in "1234" do
                let key = match Simulants.Gameplay.GetMode world with Keypadding _ -> (Simulants.GameplayKeypad.GetKeypad world).Key | _ -> 0
                let target = Array.IndexOf (Constants.Gui.KeypadKeys, string digit)
                for _ in 1 .. abs (target % 3 - key % 3) do Bot.press { Intent.empty with Move = v2i (sign (target % 3 - key % 3)) 0 } bot
                for _ in 1 .. abs (target / 3 - key / 3) do Bot.press { Intent.empty with Move = v2i 0 (sign (target / 3 - key / 3)) } bot
                Bot.act bot
            match Simulants.Gameplay.GetMode world with
            | Keypadding _ -> Assert.AreEqual ("1234", (Simulants.GameplayKeypad.GetKeypad world).Entered)
            | mode -> Assert.Fail ("The keypad went away, to " + scstring mode + ".")
            Bot.press { Intent.empty with Back = true } bot
            Assert.IsFalse (Simulants.Gameplay.HasTranspired (Examined "RecordsDrawer") world)
            Bot.enterCode "Desk" "1234" bot
            Assert.IsFalse (Simulants.Gameplay.HasTranspired (Examined "RecordsDrawer") world, "A wrong code opened the drawer.")
            Bot.press { Intent.empty with Back = true } bot
            Bot.enterCode "Desk" (getCode (Hospital Records) "Desk") bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Examined "RecordsDrawer") world)
            Assert.IsTrue (Simulants.GameplayPlayer.Carries PharmacyKey world))

    let [<Test>] ``A save at the typewriter comes back as it was saved.`` () =
        if Save.getExists () then File.Delete Constants.Gameplay.SaveFilePath
        let saved =
            let mutable saved = Unchecked.defaultof<Save>
            play (fun bot ->
                let world = bot.World
                startIn (Hospital NurseStation) (Set.singleton (Taken "GuardHandgun")) Armed world
                Bot.take "StationRibbons" bot
                Bot.save bot
                Assert.IsTrue (Save.getExists (), "No save was written.")
                saved <- Save.read ()
                Assert.AreEqual (Hospital NurseStation, saved.SaveArea)
                Assert.AreEqual (1, saved.SaveCount)
                Assert.AreEqual (2, Inventory.count InkRibbon saved.SaveInventory, "The ribbon spent saving wasn't spent.")
                Assert.IsTrue (saved.SaveFlags.Contains (Taken "StationRibbons")))
            saved
        play (fun bot ->
            let world = bot.World
            startFrom saved world
            Assert.AreEqual (Hospital NurseStation, Simulants.Gameplay.GetAreaType world)
            Assert.AreEqual (saved.SaveInventory, Simulants.GameplayPlayer.GetInventory world)
            Assert.AreEqual (saved.SaveFlags, Simulants.Gameplay.GetAdvents world)
            Assert.IsFalse ((Simulants.GameplayArea (Hospital NurseStation).Name / "StationRibbons").GetExists world, "Ribbons taken before the save came back.")
            Assert.AreEqual (float saved.SavePlayTime, float (Simulants.Gameplay.GetPlayTime world), 0.5))
        File.Delete Constants.Gameplay.SaveFilePath

    let [<Test>] ``The dead stay dead, and what's taken stays taken, from area to area.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty Armed world
            Bot.fight bot
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed "WestPatient") world)
            Bot.take "WestBullets" bot
            Bot.goTo (Hospital Reception) bot
            Bot.goTo (Hospital WestHall) bot
            Assert.IsFalse ((Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient").GetExists world, "A creature killed came back.")
            Assert.IsFalse ((Simulants.GameplayArea (Hospital WestHall).Name / "WestBullets").GetExists world, "A pickup taken came back."))

    /// Fire at a creature, its last hit point left, raising the gun aimed up or level and firing once it's raised, turned
    /// toward it each frame, until it's down or dead.
    let private shootDown (creature : Entity) pitch world =
        creature.SetHitPoints 1 world
        let start = world.UnscaledTime
        while (match creature.GetCreatureState world with Dead _ | Downed _ -> false | _ -> true) do
            if since start world > 30.0 then Assert.Fail ("Never shot " + creature.Name + " down.")
            let toCreature = Collider.toFloor (creature.GetPosition world) - Collider.toFloor (Simulants.GameplayPlayer.GetPosition world)
            Simulants.GameplayPlayer.SetYaw (Math.YawOf toCreature) world
            let aiming = match Simulants.GameplayPlayer.GetAction world with Aiming _ -> true | _ -> false
            intend { Intent.empty with Aim = true; Act = aiming; Forward = pitch } world
            advance world
        intend Intent.empty world

    let [<Test>] ``A shambler shot dead in the head has its head burst, and doesn't rise.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty Armed world
            let patient = Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient"
            patient.SetWillRise true world
            shootDown patient 1.0f world
            Assert.IsTrue ((match patient.GetCreatureState world with Dead _ -> true | _ -> false), "A shambler shot in the head went down to rise.")
            Assert.IsTrue ((patient.GetHeadBurstOpt world).IsSome, "A shambler shot dead in the head kept its head.")
            Assert.IsTrue (Simulants.Gameplay.HasTranspired (Killed patient.Name) world)
            runFor 1.0 world
            Assert.IsTrue ((patient / "Puppet").GetHeadless world, "The headless shambler's body still drew its head.")
            Assert.IsTrue ((patient / "HeadBurst").GetExists world, "No burst where the head was.")
            runFor 5.0 world
            Assert.IsTrue ((match patient.GetCreatureState world with Dead _ -> true | _ -> false), "A shambler without its head got up again."))

    let [<Test>] ``A shambler shot down in the body keeps its head, and rises if it's fated to.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty Armed world
            let patient = Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient"
            patient.SetWillRise true world
            shootDown patient 0.0f world
            Assert.IsTrue ((match patient.GetCreatureState world with Downed _ -> true | _ -> false), "A shambler fated to rise was killed outright.")
            Assert.IsTrue ((patient.GetHeadBurstOpt world).IsNone, "A shambler shot in the body lost its head.")
            Assert.IsFalse ((patient / "Puppet").GetHeadless world))

    /// Stand some metres off a creature, toward where the player stood, and fire one shell at it aimed up, the gun raised
    /// first.
    let private shellAt (creature : Entity) distance world =
        let creatureFloor = Collider.toFloor (creature.GetPosition world)
        let away = Vector2.Normalize (Collider.toFloor (Simulants.GameplayPlayer.GetPosition world) - creatureFloor)
        let spot = creatureFloor + away * distance
        Simulants.GameplayPlayer.SetPosition (v3 spot.X 0.0f spot.Y) world
        Simulants.GameplayPlayer.SetYaw (Math.YawOf -away) world
        let shells = Inventory.count ShotgunShells (Simulants.GameplayPlayer.GetInventory world)
        let start = world.UnscaledTime
        while Inventory.count ShotgunShells (Simulants.GameplayPlayer.GetInventory world) = shells do
            if since start world > 5.0 then Assert.Fail ("Never fired at " + creature.Name + ".")
            let aiming = match Simulants.GameplayPlayer.GetAction world with Aiming _ -> true | _ -> false
            intend { Intent.empty with Aim = true; Act = aiming; Forward = 1.0f } world
            advance world
        intend Intent.empty world
        advance world

    /// An inventory with the shotgun in hand and shells for it.
    let private ShotgunArmed =
        Inventory.Initial |> Inventory.add Shotgun 1 |> fst |> Inventory.add ShotgunShells 10 |> fst |> Inventory.equip Shotgun

    let [<Test>] ``A shotgun shell to a shambler's head from up close bursts it.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty ShotgunArmed world
            let patient = Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient"
            shellAt patient 1.0f world
            Assert.IsTrue ((match patient.GetCreatureState world with Dead _ -> true | _ -> false), "A shell to the head from up close didn't kill.")
            Assert.IsTrue ((patient.GetHeadBurstOpt world).IsSome, "A shell to the head from up close left the head on."))

    let [<Test>] ``A shotgun shell to a shambler's head from afar doesn't kill it.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty ShotgunArmed world
            let patient = Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient"
            shellAt patient 3.5f world
            Assert.IsTrue (patient.GetAlive world, "A shell to the head from afar killed outright.")
            Assert.Less (patient.GetHitPoints world, Data.Creatures[patient.GetCreatureType world].HitPoints, "A shell from afar missed."))

    let [<Test>] ``A player left to the dead falls, and the game's over.`` () =
        play (fun bot ->
            let world = bot.World
            startIn (Hospital WestHall) Set.empty Inventory.Initial world
            Simulants.GameplayPlayer.SetHealth 20 world
            // walk up to the patient down the hall, then stand there and take it
            let patient = Simulants.GameplayArea (Hospital WestHall).Name / "WestPatient"
            let start = world.UnscaledTime
            while Simulants.GameplayPlayer.GetAction world = Free && (Collider.toFloor (patient.GetPosition world) - Collider.toFloor (Simulants.GameplayPlayer.GetPosition world)).Length () > 1.0f do
                if since start world > 30.0 then Assert.Fail "Never came up to the patient."
                let toPatient = Collider.toFloor (patient.GetPosition world) - Collider.toFloor (Simulants.GameplayPlayer.GetPosition world)
                Simulants.GameplayPlayer.SetYaw (Math.YawOf toPatient) world
                intend { Intent.empty with Forward = 1.0f } world
                advance world
            intend Intent.empty world
            runUntil (fun world -> match Simulants.Gameplay.GetMode world with Fallen _ -> true | _ -> false) 60.0 "fell" world
            runUntil (fun world -> Game.GetGameState world = GameOver) 10.0 "came to game over" world)
