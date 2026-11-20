// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open Prime
open Nu

/// An individual play scenario in the game - where play picks up, as the scenario before it leaves it: the area,
/// what's been done, and what the player carries. The editor starts play at each, and the tests play each from its
/// start, checking the scenario before it leaves what it assumes.
type Scenario =
    { ScenarioName : string
      ScenarioArea : AreaType
      ScenarioFlags : Advent Set
      ScenarioInventory : Inventory }

    /// The pickups taken, by name.
    static let taken names =
        names |> List.map Taken

    /// What's been done by the time the records drawer's opened.
    static let DrawerAdvents =
        taken ["GuardHandgun"; "GuardBullets"; "StationRibbons"; "StationAntiseptic"; "WestBullets"; "WestAntiseptic"; "RecordsBullets"; "RecordsPharmacyKey"] @
        [Examined "RecordsDrawer"]

    /// What's been done by the time the pharmacy's been searched.
    static let PharmacyAdvents =
        DrawerAdvents @ [Unlocked "PharmacyDoor"] @ taken ["PharmacyMorgueKey"; "PharmacyCoagulant"; "PharmacyAntiseptic"; "PharmacyBullets"]

    /// What's been done by the time the morgue's fuse is taken.
    static let MorgueAdvents =
        PharmacyAdvents @ [Unlocked "MorgueDoors"; Taken "MorgueFuse"; Examined "MorgueDrawer"]

    /// What's been done by the time the power's restored and the security room searched.
    static let PowerAdvents =
        MorgueAdvents @ [PowerRestored] @ taken ["GeneratorShells"; "SecurityShotgun"; "SecurityShells"; "SecurityBullets"; "SecuritySpray"]

    /// What's been done by the time the ambulance has driven off to the dorm.
    static let HospitalAdvents =
        PowerAdvents @ [Killed "Surgeon"; Examined "Ambulance"] @ taken ["SurgeonKeys"]

    /// What's been done by the time the dorm's ground floor's searched and the stairwell's open.
    static let GroundFloorAdvents =
        HospitalAdvents @ [Unlocked "StairwellDoor"] @
        taken ["LotBullets"; "LobbyAntiseptic"; "LoungeRibbons"; "LoungeAntiseptic"; "CorridorBullets"; "Room104Antiseptic"; "LaundryId"; "LaundryBullets"; "ShowersCoagulant"; "ShowersBullets"]

    /// What's been done by the time the upper floor's searched, the magnum and the master key found.
    static let UpperFloorAdvents =
        GroundFloorAdvents @ [Examined "RaKeypad"] @ taken ["RaMagnum"; "RaMasterKey"; "RaRounds"; "RaRibbon"; "Room212Rounds"]

    /// What's been done by the time the van has driven off to the packing plant.
    static let DormAdvents =
        UpperFloorAdvents @ [Unlocked "GymDoors"; Killed "Linebacker"; Examined "Van"] @ taken ["GymVanKeys"]

    /// What's been done by the time the plant's front half is searched and Ray's bolt cutters are found.
    static let CuttersAdvents =
        DormAdvents @ [Examined "RayLocker"] @
        taken ["YardShells"; "DockAntiseptic"; "OfficeRibbons"; "OfficeAntiseptic"; "KillAntiseptic"; "LockersShells"; "GrinderRounds"; "SmokeAntiseptic"; "SmokeShells"; "LockerCutters"]

    /// What's been done by the time cold storage is thawed, the control room opened and the line switched on.
    static let LineAdvents =
        CuttersAdvents @ [Examined "BoilerValve"; Unlocked "BoilerControlDoor"; Examined "LineSwitch"] @
        taken ["BoilerAntiseptic"; "BoilerShells"; "ForemanKeycard"; "ColdRounds"; "ColdSpray"; "ControlLauncher"; "ControlGrenades"]

    /// What's been done by the time the sow's dead and the freight elevator's key found.
    static let SowAdvents =
        LineAdvents @ [Unlocked "RenderingDoor"; Killed "Sow"] @ taken ["RenderingElevatorKey"; "RenderingGrenades"]

    /// The game's scenarios, in order.
    static member All =
        [{ ScenarioName = "Scenario 01 (Reception)"; ScenarioArea = Hospital Reception; ScenarioFlags = Set.empty; ScenarioInventory = Inventory.Initial }
         { ScenarioName = "Scenario 02 (Pharmacy)"; ScenarioArea = Hospital Records; ScenarioFlags = Set.ofList DrawerAdvents; ScenarioInventory = Inventory.carrying [(Knife, 1); (Handgun, 1); (HandgunBullets, 30); (Antiseptic, 1); (InkRibbon, 2); (PharmacyKey, 1)] |> Inventory.equip Handgun }
         { ScenarioName = "Scenario 03 (Morgue)"; ScenarioArea = Hospital EastHall; ScenarioFlags = Set.ofList PharmacyAdvents; ScenarioInventory = Inventory.carrying [(Knife, 1); (Handgun, 1); (HandgunBullets, 40); (Antiseptic, 1); (Coagulant, 1); (MorgueKey, 1)] |> Inventory.equip Handgun }
         { ScenarioName = "Scenario 04 (Power)"; ScenarioArea = Hospital EastHall; ScenarioFlags = Set.ofList MorgueAdvents; ScenarioInventory = Inventory.carrying [(Knife, 1); (Handgun, 1); (HandgunBullets, 30); (Antiseptic, 1); (InkRibbon, 1); (Fuse, 1)] |> Inventory.equip Handgun }
         { ScenarioName = "Scenario 05 (Surgeon)"; ScenarioArea = Hospital AmbulanceBay; ScenarioFlags = Set.ofList PowerAdvents; ScenarioInventory = Inventory.carrying [(Handgun, 1); (HandgunBullets, 30); (Shotgun, 1); (ShotgunShells, 18); (FirstAidSpray, 1); (Antiseptic, 1)] |> Inventory.equip Shotgun }
         { ScenarioName = "Scenario 06 (Dorm)"; ScenarioArea = Dorm Lot; ScenarioFlags = Set.ofList HospitalAdvents; ScenarioInventory = Inventory.carrying [(Handgun, 1); (HandgunBullets, 30); (Shotgun, 1); (ShotgunShells, 10); (Antiseptic, 1)] |> Inventory.equip Handgun }
         { ScenarioName = "Scenario 07 (Upper Floor)"; ScenarioArea = Dorm Stairwell; ScenarioFlags = Set.ofList GroundFloorAdvents; ScenarioInventory = Inventory.carrying [(Handgun, 1); (HandgunBullets, 40); (Shotgun, 1); (ShotgunShells, 10); (Antiseptic, 1)] |> Inventory.equip Handgun }
         { ScenarioName = "Scenario 08 (Linebacker)"; ScenarioArea = Dorm Lobby; ScenarioFlags = Set.ofList UpperFloorAdvents; ScenarioInventory = Inventory.carrying [(Magnum, 1); (MagnumRounds, 12); (Shotgun, 1); (ShotgunShells, 10); (MasterKey, 1); (Antiseptic, 1)] |> Inventory.equip Magnum }
         { ScenarioName = "Scenario 09 (Plant)"; ScenarioArea = Plant Yard; ScenarioFlags = Set.ofList DormAdvents; ScenarioInventory = Inventory.carrying [(Magnum, 1); (MagnumRounds, 8); (Shotgun, 1); (ShotgunShells, 10); (Antiseptic, 1)] |> Inventory.equip Magnum }
         { ScenarioName = "Scenario 10 (Cold Storage)"; ScenarioArea = Plant Grinder; ScenarioFlags = Set.ofList CuttersAdvents; ScenarioInventory = Inventory.carrying [(Magnum, 1); (MagnumRounds, 12); (Shotgun, 1); (ShotgunShells, 16); (BoltCutters, 1); (Antiseptic, 1)] |> Inventory.equip Shotgun }
         { ScenarioName = "Scenario 11 (Sow)"; ScenarioArea = Plant KillFloor; ScenarioFlags = Set.ofList LineAdvents; ScenarioInventory = Inventory.carrying [(GrenadeLauncher, 1); (Grenades, 6); (Magnum, 1); (MagnumRounds, 14); (BoltCutters, 1); (FirstAidSpray, 1)] |> Inventory.equip Magnum }
         { ScenarioName = "Scenario 12 (Meat King)"; ScenarioArea = Plant Rendering; ScenarioFlags = Set.ofList SowAdvents; ScenarioInventory = Inventory.carrying [(GrenadeLauncher, 1); (Grenades, 7); (Magnum, 1); (MagnumRounds, 14); (ElevatorKey, 1); (FirstAidSpray, 1)] |> Inventory.equip GrenadeLauncher }]

    /// The start a scenario makes, with the fixed game seed.
    static member getStart scenario =
        Save.makeIn scenario.ScenarioArea scenario.ScenarioFlags scenario.ScenarioInventory

/// The game's plugin - the editor's edit modes, starting play at the title, in any area, at a boss, or at the start of
/// any scenario, and the packages loaded at start.
type ZombieGamePlugin () =
    inherit NuPlugin ()

    /// Start play in an area for the editor, with the fixed game seed, and whatever it needs to be there.
    static let editPlay area flags inventory world =
        Simulants.Gameplay.Start (Save.makeIn area flags inventory) world
        Game.SetGameState Playing world

    override this.EditModes =
        let armed = Inventory.Initial |> Inventory.add Handgun 1 |> fst |> Inventory.add HandgunBullets 45 |> fst |> Inventory.equip Handgun
        let boss = armed |> Inventory.add Shotgun 1 |> fst |> Inventory.add ShotgunShells 18 |> fst |> Inventory.equip Shotgun
        let dorm = Inventory.carrying [(Magnum, 1); (MagnumRounds, 12); (Shotgun, 1); (ShotgunShells, 18); (Antiseptic, 1)] |> Inventory.equip Magnum
        let plant = Inventory.carrying [(GrenadeLauncher, 1); (Grenades, 8); (Magnum, 1); (MagnumRounds, 18); (FirstAidSpray, 1); (Antiseptic, 1)] |> Inventory.equip GrenadeLauncher
        Map.ofList
            [("Title", fun world -> Game.SetGameState Title world)
             ("Play", fun world -> editPlay (Hospital Reception) Set.empty Inventory.Initial world)
             for area in [NurseStation; WestHall; Records; Pharmacy; EastHall; Morgue; Generator; Security] do
                ("Play " + scstring area, fun world -> editPlay (Hospital area) Set.empty armed world)
             ("Play Powered", fun world -> editPlay (Hospital EastHall) (Set.ofList [PowerRestored]) armed world)
             ("Play Boss", fun world -> editPlay (Hospital AmbulanceBay) (Set.ofList [PowerRestored]) boss world)
             for area in [Lot; Lobby; Lounge; EastCorridor; Laundry; Room104; Showers; Stairwell; UpperCorridor; RaOffice; Room212] do
                ("Play Dorm " + scstring area, fun world -> editPlay (Dorm area) Set.empty armed world)
             ("Play Linebacker", fun world -> editPlay (Dorm Gym) Set.empty dorm world)
             for area in [Yard; PlantAreaType.Dock; Office; KillFloor; Lockers; Grinder; Smokehouse; Boiler; ColdStorage; ControlRoom; Platform] do
                ("Play Plant " + scstring area, fun world -> editPlay (Plant area) Set.empty dorm world)
             ("Play Sow", fun world -> editPlay (Plant Rendering) Set.empty plant world)
             ("Play Meat King", fun world -> editPlay (Plant Pit) Set.empty plant world)
             for scenario in Scenario.All do
                (scenario.ScenarioName, fun world -> Simulants.Gameplay.Start (Scenario.getStart scenario) world; Game.SetGameState Playing world)]

    override this.InitialPackages =
        [Assets.Gameplay.PackageName]
