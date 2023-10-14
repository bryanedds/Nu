namespace Tactics
open System
open System.Numerics
open Prime
open Nu

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

    member this.Opposite =
        match this with
        | Upward -> Downward
        | Rightward -> Leftward
        | Downward -> Upward
        | Leftward -> Rightward

    static member ofVector3 (v3 : Vector3) =
        let angle = double (atan2 v3.Y v3.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.75 || angle <= Math.PI * 0.25 then  Rightward
            elif    angle > Math.PI * 0.75 && angle <= Math.PI * 1.25 then  Leftward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then  Upward
            else                                                            Downward
        direction

    static member ofVector3Biased (v3 : Vector3) =
        let angle = double (atan2 v3.Y v3.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.74997 || angle <= Math.PI * 0.25003 then    Rightward
            elif    angle > Math.PI * 0.74997 && angle <= Math.PI * 1.25003 then    Leftward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then          Upward
            else                                                                    Downward
        direction

    static member toVector3 direction =
        match direction with
        | Upward -> v3Up
        | Rightward -> v3Right
        | Downward -> v3Down
        | Leftward -> v3Left

type SaveSlot =
    | Slot1
    | Slot2
    | Slot3

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

    member this.Ally =
        match this with
        | AllyIndex _ -> true
        | EnemyIndex _ -> false

    member this.Enemy =
        not this.Ally

    static member friendly index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

    static member unfriendly index index2 =
        not (CharacterIndex.friendly index index2)

    static member toEntityName index =
        match index with
        | AllyIndex i -> "Ally+" + scstring i
        | EnemyIndex i -> "Enemy+" + scstring i

type AllyType =
    | Jinn
    | Shade
    | Mael
    | Riain
    | Peric

type EnemyType =
    | DebugGoblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

    static member getName characterType =
        match characterType with
        | Ally ty -> string ty
        | Enemy ty -> string ty

type AnimationType =
    | LoopedWithDirection
    | LoopedWithoutDirection
    | SaturatedWithDirection
    | SaturatedWithoutDirection

type PoiseType =
    | Poising
    | Defending
    | Charging

type CharacterAnimationType =
    | WalkAnimation
    | CelebrateAnimation
    | ReadyAnimation
    | PoiseAnimation of PoiseType
    | AttackAnimation
    | WoundAnimation
    | SpinAnimation
    | DamageAnimation
    | IdleAnimation
    | CastAnimation
    | Cast2Animation
    | SlashAnimation
    | WhirlAnimation

type [<SymbolicExpansion>] CharacterAnimationState =
    { StartTime : int64
      AnimationSheet : Image AssetTag
      CharacterAnimationType : CharacterAnimationType
      Direction : Direction }

    static member face direction (state : CharacterAnimationState) =
        { state with Direction = direction }

    static member setCharacterAnimationType timeOpt characterAnimationType state =
        if state.CharacterAnimationType <> characterAnimationType then
            match timeOpt with
            | Some time -> { state with StartTime = time; CharacterAnimationType = characterAnimationType }
            | None -> { state with CharacterAnimationType = characterAnimationType }
        else state

    static member directionToInt direction =
        match direction with
        | Upward -> 0
        | Rightward -> 1
        | Downward -> 2
        | Leftward -> 3

    static member localTime time state =
        time - state.StartTime

    static member indexCel delay time state =
        let localTime = CharacterAnimationState.localTime time state
        int (localTime / delay)

    static member indexLooped run delay time state =
        CharacterAnimationState.indexCel delay time state % run

    static member indexSaturated run delay time state =
        let cel = CharacterAnimationState.indexCel delay time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run delay time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run delay offset time state =
        let position = CharacterAnimationState.indexLooped run delay time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run delay time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match state.CharacterAnimationType with
        | WalkAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 15L (v2i 0 0) time state
        | CelebrateAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 10L (v2i 0 1) time state
        | ReadyAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15L (v2i 0 5) time state
        | PoiseAnimation poiseType ->
            match poiseType with
            | Poising -> CharacterAnimationState.indexLoopedWithDirection 4 15L (v2i 0 3) time state
            | Defending -> CharacterAnimationState.indexLoopedWithDirection 1 10L (v2i 0 9) time state
            | Charging -> CharacterAnimationState.indexLoopedWithDirection 4 10L (v2i 0 2) time state
        | AttackAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15L (v2i 0 6) time state
        | WoundAnimation -> CharacterAnimationState.indexLoopedWithDirection 1 10L (v2i 0 11) time state
        | SpinAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 10L (v2i 0 10) time state
        | DamageAnimation -> CharacterAnimationState.indexSaturatedWithDirection 1 10L (v2i 0 8) time state
        | IdleAnimation -> CharacterAnimationState.indexLoopedWithDirection 1 10L (v2i 0 10) time state
        | CastAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 5L (v2i 0 2) time state
        | Cast2Animation -> CharacterAnimationState.indexLoopedWithDirection 2 10L (v2i 0 7) time state
        | SlashAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15L (v2i 0 6) time state
        | WhirlAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 3L (v2i 0 12) time state

    static member inset time (celSize : Vector2) state =
        let index = CharacterAnimationState.index time state
        let offset = v2 (single index.X) (single index.Y) * celSize
        let inset = box2 offset celSize
        inset

    static member progressOpt time state =
        let localTime = CharacterAnimationState.localTime time state
        let lengthOpt =
            match state.CharacterAnimationType with
            | WalkAnimation -> None
            | CelebrateAnimation -> None
            | ReadyAnimation -> Some 60
            | PoiseAnimation poiseType ->
                match poiseType with
                | Poising -> None
                | Defending -> None
                | Charging -> None
            | AttackAnimation -> Some 60
            | WoundAnimation -> Some 60
            | SpinAnimation -> Some 40
            | DamageAnimation -> Some 40
            | IdleAnimation -> None
            | CastAnimation -> None
            | Cast2Animation -> None
            | SlashAnimation -> Some 120
            | WhirlAnimation -> None
        match lengthOpt with
        | Some length -> Some (min 1.0f (single localTime / single length))
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> true

    static member empty =
        { StartTime = 0L
          AnimationSheet = asset "Field" "Jinn"
          CharacterAnimationType = IdleAnimation
          Direction = Downward }

    static member initial =
        { CharacterAnimationState.empty with Direction = Upward }