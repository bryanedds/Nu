// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] Teammate =
    { TeamIndex : int // key
      PartyIndexOpt : int option
      ArchetypeType : ArchetypeType
      CharacterType : CharacterType
      HitPoints : int
      TechPoints : int
      ExpPoints : int
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list }

    member this.Name = CharacterType.getName this.CharacterType
    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt Map.empty this.ArchetypeType this.Level // no statuses outside battle
    member this.Magic = Algorithms.magic this.WeaponOpt Map.empty this.ArchetypeType this.Level // no statuses outside battle
    member this.Shield effectType = Algorithms.shield effectType this.Accessories Map.empty this.ArchetypeType this.Level // no statuses outside battle
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member equipWeaponOpt weaponTypeOpt (teammate : Teammate) =
        { teammate with WeaponOpt = weaponTypeOpt }

    static member equipArmorOpt armorTypeOpt (teammate : Teammate) =
        let teammate = { teammate with ArmorOpt = armorTypeOpt }
        let teammate = { teammate with HitPoints = min teammate.HitPoints teammate.HitPointsMax; TechPoints = min teammate.TechPoints teammate.HitPointsMax }
        teammate

    static member equipAccessory1Opt accessoryTypeOpt (teammate : Teammate) =
        { teammate with Accessories = Option.toList accessoryTypeOpt }

    static member canUseItem itemType teammate =
        match Map.tryFind teammate.CharacterType Data.Value.Characters with
        | Some characterData ->
            match Map.tryFind characterData.ArchetypeType Data.Value.Archetypes with
            | Some archetypeData ->
                match itemType with
                | Consumable itemType ->
                    match itemType with
                    | GreenHerb | RedHerb | GoldHerb | Remedy -> teammate.HitPoints > 0 && teammate.HitPoints < teammate.HitPointsMax
                    | Ether | HighEther | TurboEther -> teammate.TechPoints < teammate.TechPointsMax
                    | Revive -> teammate.HitPoints = 0
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType ->
                        match Map.tryFind weaponType Data.Value.Weapons with
                        | Some weaponData -> weaponData.WeaponSubtype = archetypeData.WeaponSubtype
                        | None -> false
                    | ArmorType armorType ->
                        match Map.tryFind armorType Data.Value.Armors with
                        | Some armorData -> armorData.ArmorSubtype = archetypeData.ArmorSubtype
                        | None -> false
                    | AccessoryType _ -> true
                | KeyItem _ -> false
                | Stash _ -> false
            | None -> false
        | None -> false

    static member tryUseItem itemType teammate =
        if Teammate.canUseItem itemType teammate then
            match itemType with
            | Consumable consumableType ->
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumableData) ->
                    match consumableType with
                    | GreenHerb | RedHerb | GoldHerb | Remedy ->
                        let teammate = { teammate with HitPoints = min teammate.HitPointsMax (teammate.HitPoints + int consumableData.Scalar) }
                        (true, None, teammate)
                    | Ether | HighEther | TurboEther ->
                        let teammate = { teammate with TechPoints = min teammate.TechPointsMax (teammate.TechPoints + int consumableData.Scalar) }
                        (true, None, teammate)
                    | Revive ->
                        let teammate = { teammate with HitPoints = min teammate.HitPointsMax (int consumableData.Scalar) }
                        (true, None, teammate)
                | (false, _) -> (false, None, teammate)
            | Equipment equipmentType ->
                match equipmentType with
                | WeaponType weaponType -> (true, Option.map (Equipment << WeaponType) teammate.WeaponOpt, Teammate.equipWeaponOpt (Some weaponType) teammate)
                | ArmorType armorType -> (true, Option.map (Equipment << ArmorType) teammate.ArmorOpt, Teammate.equipArmorOpt (Some armorType) teammate)
                | AccessoryType accessoryType -> (true, Option.map (Equipment << AccessoryType) (List.tryHead teammate.Accessories), Teammate.equipAccessory1Opt (Some accessoryType) teammate)
            | KeyItem _ -> (false, None, teammate)
            | Stash _ -> (false, None, teammate)
        else (false, None, teammate)

    static member restore teammate =
        { teammate with
            HitPoints = teammate.HitPointsMax
            TechPoints = teammate.TechPointsMax }

    static member make index allyType =
        let characterType = Ally allyType
        let character = Map.find characterType Data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = character.WeaponOpt
        let armorOpt = character.ArmorOpt
        let accessories = character.Accessories
        { ArchetypeType = archetypeType
          TeamIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
          Accessories = accessories }

    // for debugging
    static member makeAtLevel level index allyType =
        { Teammate.make index allyType with ExpPoints = Algorithms.levelToExpPoints level }