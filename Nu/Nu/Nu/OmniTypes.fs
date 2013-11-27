namespace Nu
open OpenTK

type [<StructuralEquality; StructuralComparisonAttribute>] OmniElementType =
    | OmniFire
    | OmniIce
    | OmniLightning
    | OmniDark
    | OmniHoly

type [<StructuralEquality; StructuralComparisonAttribute>] OmniStatusType =
    | OmniStrong
    | OmniGifted
    | OmniShell
    | OmniHaste
    | OmniSlow
    | OmniPoison
    | OmniNumb
    | OmniMute
    | OmniSleep

type [<StructuralEquality; NoComparison>] OmniWeaponType =
    | OmniLightSword
    | OmniHeavySword
    | OmniArrow
    | OmniStaff

type [<StructuralEquality; NoComparison>] OmniArmorType =
    | OmniHeavy // better protection against physical damage
    | OmniIncanted // better protection against magical damage

type [<StructuralEquality; NoComparison>] OmniRelicType =
    | OmniGoldBrace // +50 HPMax
    | OmniRubyRing // 2x SP gain
    | OmniSilverLeaf // +2 Agility
    | OmniGemStone // Start Battle w/ Full SP
    | OmniRedCrystal // Double Damage of Offensive Items
    | OmniEmeraldEarring // Can Participate in Combo with Half AT
    | OmniHawkFeather // Anti-Counter
    | OmniSpiritBox // Counter-Counter
    | OmniSapphireOrb // +5 MDef
    | OmniGauntlet // +5 PDef
    | OmniIronCross // 1.5x HPMax
    | OmniSilverCross // Anti-Ailment

type [<StructuralEquality; NoComparison>] OmniEquipmentType =
    | OmniWeapon of OmniWeaponType
    | OmniArmor of OmniArmorType
    | OmniRelic of OmniRelicType

type [<StructuralEquality; NoComparison>] OmniConsumableType = // consumables only work in battle in OmniBlade
    | OmniGreenHerb // Cure 30 HP
    | OmniRedHerb // Cure 50 HP
    | OmniBlueHerb // Cure 100 HP
    | OmniRemedy // Remove Status Ailment (except perhaps Numb)
    | OmniFireBall // Cast Fire
    | OmniIceBall // Cast Ice
    | OmniLitBall // Cast Lightning
    | OmniFireCrystal // Cast Flare
    | OmniIceCrystal // Cast Blizzard
    | OmniLitCrystal // Cast T-Storm
    | OmniSmellingSalts // Revive Ally
    | OmniPaleAle // +10 SP, but inflicts Numb
    | OmniNillaAle // +30 SP, but inflicts Numb
    | OmniChocoAle // +50 SP, but inflicts Numb

type [<StructuralEquality; NoComparison>] OmniItemType =
    | OmniEquipment of OmniEquipmentType
    | OmniConsumable of OmniConsumableType

type [<StructuralEquality; NoComparison>] OmniSpecialType =
    | OmniGuardCounter // defend + counter
    | OmniPreCounter
    | OmniHolySlash // directed line of holy dmg
    | OmniHolyShell // Physical def up
    | OmniDoubleGreenHerb // uses a green herb at 2x healing
    | OmniDoubleRedHerb
    | OmniDoubleBlueHerb
    | OmniGrayBlades // both holy and dark physical damage
    | OmniDualCounter // if enemy attacks either ally, both allies counter
    | OmniFireCounter
    | OmniDeathCounter
    | OmniGreenTonic // Double Green Herb All
    | OmniRedTonic // Double Red Herb All
    | OmniBlueTonic // Double Blue Herb All
    | OmniSuperSalts // super salts restores life and fill HP to max
    | OmniTriCounter
    | OmniHolyWind // shell all
    | OmniLightningBlades
    | OmniCrossBlades // holy dmg in both horizontal and vertical line (a cross)

type [<StructuralEquality; NoComparison>] OmniEffectType =
    | OmniPhysical
    | OmniMagical

type [<StructuralEquality; NoComparison>] OmniDualSpecialType =
    | OmniJakkAndLana
    | OmniJakkAndMeryl
    | OmniJakkAndKane
    | OmniLanaAndMaryl
    | OmniLanaAndKane
    | OmniMaerylAndKane
    //| OmniGoblinAndTurkey

type [<StructuralEquality; NoComparison>] OmniTripleSpecialType =
    | OmniJakkAndLanaAndMaryl
    | OmniJakkAndMarylAndKane
    | OmniJakkAndLanaAndKane
    | OmniLanaAndMarylAndKane
    //| OmniGoblinAndTurkeyAndFrog

type [<StructuralEquality; NoComparison>] OmniSpecialAbilityType =
    | OmniSingleSpecial
    | OmniDualSpecial of OmniDualSpecialType
    | OmniTripleSpecial of OmniTripleSpecialType

type [<StructuralEquality; NoComparison>] OmniTargetType =
    | OmniSingle
    | OmniDirectedLine
    | OmniHorizontalLine
    | OmniVerticalLine
    | OmniCross
    | OmniSmallRadius
    | OmniMediumRadius
    | OmniLargeRadius
    | OmniAll

type [<StructuralEquality; NoComparison>] OmniMoveType =
    | OmniAttack
    | OmniDefend
    | OmniConsumable of OmniConsumableType
    | OmniSpecial of OmniSpecialType

type [<StructuralEquality; NoComparison>] OmniTargetingInputType =
    | OmniAttackInput of Id
    | OmniItemInput of Id // for now, just uses GreenHerb
    | OmniSpecialInput of Id // for now, just uses DefendCounter?

type [<StructuralEquality; NoComparison>] OmniMortalityType =
    | OmniRevive
    | OmniDeath
    | OmniMortalityNone

type [<StructuralEquality; NoComparison>] OmniCharacterType =
    | OmniAlly
    | OmniEnemy