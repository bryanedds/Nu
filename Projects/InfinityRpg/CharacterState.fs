namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

type ElementType =
    | Fire // beats nothing; strongest
    | Water // beats fire, lightning; weakest
    | Lightning // beats water

type StatusType =
    | Defending // also implies countering
    | Poisoned
    | Muted
    | Sleeping

type EquipmentType =
    | Weapon
    | Armor
    | Relic

type ConsumableType =
    | Herb

type KeyItemType =
    | Feather // jump
    | Gills // swim in shallow water
    | Hammer // smash obstacles
    | SteelSoles // walk over spikes
    | Wings // fly in overworld
    | Raft // navigate over deep water

type ItemType =
    | Equipment of EquipmentType
    | Consumable of ConsumableType
    | KeyItem of KeyItemType

type TargetType =
    | SingleTarget of Distance : int * SelfOnly : bool
    | RangeTarget of Distance : int * SelfOnly : bool * Range : int
    | LineTarget of Distance : int
    | QuarterScreenTarget // in one of four cardinal directions like <, >, ^, or v
    | FullScreenTarget

type EffectType =
    | Physical
    | Magical

type SpecialType =
    | Spark
    | Bomb
    | DragonBreath
    | Plasma
    | Volt
    | PowerUp
    | PowerDown
    | ShieldUp
    | ShieldDown
    | MindUp
    | MindDown
    | CounterUp
    | CounterDown
    | Sleep
    | Poison
    | Immunity // immunizes against status changes

type ActionType =
    | Attack
    | Defend // auto counters at rate of counter stat
    | Consume of ConsumableType
    | Special of SpecialType
    | Interact // general interaction such as talking to NPCs

type WeaponType =
    | BentSword
    | WindCutter
    | BFS

type WeaponSubtype =
    | Melee
    | Sword
    | Axe
    | Bow
    | Staff
    | Rod

type WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int }

type ArmorType =
    | HoboRags
    | LeatherHide
    | ChainMail

type ArmorSubtype =
    | Robe
    | Vest
    | Mail

type ArmorData =
    { ArmorType : ArmorType // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      SpecialPointsBase : int }

type RelicType =
    | LeatherBoots
    | BlingRing
    | MagicPorkRinds

type RelicData =
    { RelicType : RelicType // key
      ShieldBase : int
      CounterBase : int }

type AllyType =
    | Player

type EnemyType =
    | Goblin
    | Snake
    | Zommie

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

type DrainData =
    { EffectType : EffectType
      Percentage : single }

type ActionData =
    { ActionType : ActionType
      EffectType : EffectType
      SpecialPointCost : int
      SuccessRate : single
      Curative : bool
      ElementType : ElementType
      AddStatusType : StatusType Set
      RemoveStatusType : StatusType Set
      TargetType : TargetType }

type ConsumableData =
    { ConsumableData : unit }

type KeyItemData =
    { KeyItemData : unit }

type ItemData =
    { ItemType : ItemType // key
      Description : string }

type RewardData =
    { Gold : int }

type CharacterData =
    { CharacterType : CharacterType // key
      CharacterName : string
      BaseActions : ActionData list // base actions for all instances of character
      Reward : RewardData }

type ControlType =
    | PlayerControlled
    | Chaos
    | Uncontrolled

type [<CustomEquality; NoComparison>] NavigationNode =
    { PositionM : Vector2i
      mutable Neighbors : NavigationNode list } // OPTIMIZATION: has to be mutable to be efficiently populated.

    interface NavigationNode IHasNeighbors with
        member this.Neighbors = this.Neighbors :> _ seq

    interface NavigationNode IEquatable with
        member this.Equals that =
            this.PositionM = that.PositionM

    override this.Equals that =
        match that with
        | :? NavigationNode as that -> this.PositionM = that.PositionM
        | _ -> false

    override this.GetHashCode () =
        this.PositionM.GetHashCode ()

type WalkState =
    | WalkFinished
    | WalkContinuing

type WalkDescriptor =
    { WalkDirection : Direction
      WalkOriginM : Vector2i }

    static member nextPositionM walkDescriptor =
        walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection

type [<StructuralEquality; NoComparison>] NavigationDescriptor =
    { WalkDescriptor : WalkDescriptor
      NavigationPathOpt : NavigationNode list option }

    static member nextPositionM navigationDescriptor =
        WalkDescriptor.nextPositionM navigationDescriptor.WalkDescriptor

    static member nextPositionI navigationDescriptor =
        navigationDescriptor |> NavigationDescriptor.nextPositionM |> vmtovi

    static member nextPosition navigationDescriptor =
        navigationDescriptor |> NavigationDescriptor.nextPositionI |> vitovf

type [<StructuralEquality; NoComparison>] ActionDescriptor =
    { ActionTicks : int64 // an arbitrary number to show a hacky action animation
      ActionTargetPositionMOpt : Vector2i option
      ActionDataName : string }

    static member getActionDirection currentPosition currentDirection actionDescriptor =
        match actionDescriptor.ActionTargetPositionMOpt with
        | Some targetPositionM -> targetPositionM - vftovm currentPosition |> vmtod
        | None -> currentDirection
        
type [<StructuralEquality; NoComparison>] ActivityState =
    | Action of ActionDescriptor
    | Navigation of NavigationDescriptor
    | NoActivity

    static member isActing activity =
        match activity with
        | Action _ -> true
        | Navigation _ | NoActivity -> false

    static member isNotActing activity =
        not (ActivityState.isActing activity)

    static member isNavigating activity =
        match activity with
        | Action _ | NoActivity -> false
        | Navigation _ -> true

    static member isNotNavigating activity =
        not (ActivityState.isNavigating activity)

    static member isNavigatingPath activity =
        match activity with
        | Navigation navigationDescriptor -> Option.isSome navigationDescriptor.NavigationPathOpt
        | Action _ | NoActivity -> false

type [<StructuralEquality; NoComparison>] Turn =
    | ActionTurn of ActionDescriptor
    | NavigationTurn of NavigationDescriptor
    | CancelTurn
    | NoTurn