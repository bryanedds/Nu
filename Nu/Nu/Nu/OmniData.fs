namespace Nu
open OpenTK
module OmniData =

    type [<StructuralEquality; StructuralComparisonAttribute>] Element =
        | Fire
        | Ice
        | Lightning
        | Dark
        | Holy

    type [<StructuralEquality; StructuralComparisonAttribute>] Status =
        | Strong
        | Gifted
        | Shell
        | Haste
        | Slow
        | Poison
        | Numb
        | Mute
        | Sleep

    type [<StructuralEquality; NoComparison>] WeaponType =
        | LightSword
        | HeavySword
        | Arrow
        | Staff

    type [<StructuralEquality; NoComparison>] ArmorType =
        | Heavy // better protection against physical damage
        | Incanted // better protection against magical damage

    type [<StructuralEquality; NoComparison>] RelicType =
        | GoldBrace // +50 HPMax
        | RubyRing // 2x SP gain
        | SilverLeaf // +2 Agility
        | GemStone // Start Battle w/ Full SP
        | RedCrystal // Double Damage of Offensive Items
        | EmeraldEarring // Can Participate in Combo with Half AT
        | HawkFeather // Anti-Counter
        | SpiritBox // Counter-Counter
        | SapphireOrb // +5 MDef
        | Gauntlet // +5 PDef
        | IronCross // 1.5x HPMax
        | SilverCross // Anti-Ailment

    type [<StructuralEquality; NoComparison>] EquipmentType =
        | Weapon of WeaponType
        | Armor of ArmorType
        | Relic of RelicType

    type [<StructuralEquality; NoComparison>] ConsumableType = // consumables only work in battle in OmniBlade
        | GreenHerb // Cure 30 HP
        | RedHerb // Cure 50 HP
        | BlueHerb // Cure 100 HP
        | Remedy // Remove Status Ailment (except perhaps Numb)
        | FireBall // Cast Fire
        | IceBall // Cast Ice
        | LitBall // Cast Lightning
        | FireCrystal // Cast Flare
        | IceCrystal // Cast Blizzard
        | LitCrystal // Cast T-Storm
        | SmellingSalts // Revive Ally
        | PaleAle // +10 SP, but inflicts Numb
        | NillaAle // +30 SP, but inflicts Numb
        | ChocoAle // +50 SP, but inflicts Numb

    type [<StructuralEquality; NoComparison>] SpecialType =
        | GuardCounter // defend + counter
        | PreCounter
        | HolySlash // directed line of holy dmg
        | HolyShell // Physical def up
        | DoubleGreenHerb // uses a green herb at 2x healing
        | DoubleRedHerb
        | DoubleBlueHerb
        | GrayBlades // both holy and dark physical damage
        | DualCounter // if enemy attacks either ally, both allies counter
        | FireCounter
        | DeathCounter
        | GreenTonic // Double Green Herb All
        | RedTonic // Double Red Herb All
        | BlueTonic // Double Blue Herb All
        | SuperSalts // super salts restores life and fill HP to max
        | TriCounter
        | HolyWind // shell all
        | LightningBlades
        | CrossBlades // holy dmg in both horizontal and vertical line (a cross)

    type [<StructuralEquality; NoComparison>] EffectType =
        | Physical
        | Magical

    type [<StructuralEquality; NoComparison>] DualSpecialType =
        | JakkAndLana
        | JakkAndMeryl
        | JakkAndKane
        | LanaAndMaryl
        | LanaAndKane
        | MaerylAndKane
        //| GoblinAndTurkey

    type [<StructuralEquality; NoComparison>] TripleSpecialType =
        | JakkAndLanaAndMaryl
        | JakkAndMarylAndKane
        | JakkAndLanaAndKane
        | LanaAndMarylAndKane
        //| GoblinAndTurkeyAndFrog

    type [<StructuralEquality; NoComparison>] SpecialAbilityType =
        | SingleSpecial
        | DualSpecial of DualSpecialType
        | TripleSpecial of TripleSpecialType

    type [<StructuralEquality; NoComparison>] Targetype =
        | Single
        | DirectedLine
        | HorizontalLine
        | VerticalLine
        | Cross
        | SmallRadius
        | MediumRadius
        | LargeRadius
        | All

    type [<StructuralEquality; NoComparison>] MoveType =
        | Attack
        | Defend
        | Consumable of ConsumableType
        | Special of SpecialType

    type [<StructuralEquality; NoComparison>] TargetingInputType =
        | AttackInput of Id
        | ItemInput of Id // for now, just uses GreenHerb
        | SpecialInput of Id // for now, just uses DefendCounter?

     type [<StructuralEquality; NoComparison>] BattleInputState =
        | GoInput of Id Set
        | CrossInput of Id * Id Set
        | TargetableInput of TargetingInputType
        | TargetInput of TargetingInputType

    type [<StructuralEquality; NoComparison>] BattleState =
        | BattleBegin
        | BattleAdvance of BattleInputState
        | BattleInput of BattleInputState
        | BattleEnding of bool

    type [<StructuralEquality; NoComparison>] Weapon =
        { Type : WeaponType
          PhysicalPower : int
          MagicalPower : int
          Elements : Element Set }

    type [<StructuralEquality; NoComparison>] Armor =
        { Type : ArmorType
          PhysicalDefense : int
          MagicalDefense : int
          Elements : Element Set }

    type [<StructuralEquality; NoComparison>] Move =
        { Name : string
          Type : MoveType
          SpecialAbilityType : SpecialAbilityType
          EffectType : EffectType
          SpecialPoints : int
          PowerMultiplier : single // multiplier to the relevant powers queried from the move's participants
          SuccessRate : single
          Curative : bool
          Percentive : bool
          Defending : bool
          Countering : bool
          Element : Element
          AddStatus : Status Set
          RemoveStatus : Status Set
          Targetype : Targetype
          Revive : bool }

    type [<StructuralEquality; NoComparison>] CharacterType =
        | Player
        | Enemy

    type [<StructuralEquality; NoComparison>] CharacterData =
        { Name : string
          Type : CharacterType
          Moves : Move list // usable moves are queried
          EquipWeaponType : WeaponType
          EquipArmorType : ArmorType
          PhysicalRating : int // physical power is queried
          MagicRating : int // magic power is queried
          StaminaRating : int // hp max is queried
          AgilityRating : int // tt gain is queried
          WillRating : int // sp max is queried
          StartingExperience : int // this is the only experience for enemies
          ExperienceEarned : int // only used with enemies
          GoldEarned : int } // only used with enemies

    type [<StructuralEquality; NoComparison>] Character =
        { Data : CharacterData
          Hp : int // hp max is queried
          Sp : int // sp max is queried
          Tt : int // turn ticks
          EquippedWeapon : WeaponType option
          EquippedArmor : ArmorType option
          EquippedRelics : RelicType list
          Experience : int } // level is queried

    type [<StructuralEquality; NoComparison>] ItemType =
        | Equipment of EquipmentType
        | Consumable of ConsumableType

    type [<StructuralEquality; NoComparison>] Item =
        { Name : string
          Type : ItemType
          OptMove : Move option }

    type [<StructuralEquality; NoComparison>] FieldData =
        { Id : Id
          GroupModelFileName : string }

    type [<StructuralEquality; NoComparison>] BattleData =
        { Id : Id
          Name : string
          BossEnemy : CharacterData
          BossEnemyDefeatEndsBattle : bool
          MinorEnemies : CharacterData list
          MinorEnemySpawnsMax : int
          AwardType : ItemType }

    type [<StructuralEquality; NoComparison>] Battle =
        { Data : BattleData
          Enemies : Character option list
          MinorEnemySpawns : int }

    type [<StructuralEquality; NoComparison>] Player =
        { FieldId : Id
          FieldPosition : Vector2
          OptBattleDataId : Id option
          Allies : Character option list
          Gold : int }

    type [<StructuralEquality; NoComparison>] MoveUsage =
        { Move : Move
          Characters : Character list } // characters involved in the move