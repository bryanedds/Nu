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

// NOTE: this seems to be a wierd type; I can't specify the Consume type without also specifying
// a ConsumableType, for example. Whether this is a real problem remains to be seen.
type ActionType =
    | Attack
    | Defend // auto counters at rate of counter stat
    | Consume of ConsumableType
    | Special of SpecialType
    | Interact // general interaction such as talking to NPCs

type WeaponType =
    | BentSword
    | WindCutter
    | BigMeany

type ArmorType =
    | HobosClobos
    | LeatherHide
    | ChainMail

type RelicType =
    | WussInBoots
    | BlingRing
    | MagicPorkRinds

type CharacterType =
    | Player
    | Goopy
    | Batsy
    | Zommie

type SpecialData =
    { SpecialType : SpecialType
      EffectType : EffectType
      SpecialPointCost : int
      SuccessRate : single
      Curative : bool
      ElementType : ElementType
      AddStatusType : StatusType Set
      RemoveStatusType : StatusType Set
      TargetType : TargetType }

type EquipmentRatingData =
    { PhysicalRating : single // physical power = Level * PhysicalRating
      MagicalRating : single } // magical power = Level * MagicalRating

type WeaponData =
    { EquipmentRating : EquipmentRatingData }

type ArmorData =
    { EquipmentRating : EquipmentRatingData }
       
type RelicData =
    { EquipmentRating : EquipmentRatingData }

type EquipmentData =
    | WeaponData of WeaponData
    | ArmorData of ArmorData
    | RelicData of RelicData

type ConsumableData =
    { ConsumableData : unit }

type KeyItemData =
    { KeyItemData : unit }

type BasicItemData =
    | EquipmentData of EquipmentData
    | ConsumableData of ConsumableData
    | KeyItemData of KeyItemData

type ItemData =
    { Name : string
      Description : string
      BasicData : BasicItemData }

type ActionData =
    { Name : string
      ActionType : ActionType }

type CharacterRatingData =
    { PhysicalRating : int // physical power is calculated based on level
      MagicRating : int // magic power is calculated based on level
      StaminaRating : int // hp max is calculated based on level
      WillRating : int } // sp max is calculated based on level

type RewardData =
    { Gold : int }

type CharacterData =
    { Name : string
      CharacterType : CharacterType
      CharacterRating : CharacterRatingData
      BaseActions : ActionData list // base actions for all instances of character
      Reward : RewardData }

type ControlType =
    | Player
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
        not ^ ActivityState.isActing activity

    static member isNavigating activity =
        match activity with
        | Action _ | NoActivity -> false
        | Navigation _ -> true

    static member isNotNavigating activity =
        not ^ ActivityState.isNavigating activity

    static member isNavigatingPath activity =
        match activity with
        | Navigation navigationDescriptor -> Option.isSome navigationDescriptor.NavigationPathOpt
        | Action _ | NoActivity -> false

type [<StructuralEquality; NoComparison>] Turn =
    | ActionTurn of ActionDescriptor
    | NavigationTurn of NavigationDescriptor
    | CancelTurn
    | NoTurn