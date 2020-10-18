namespace InfinityRpg

type CharacterAnimationType =
    | CharacterAnimationFacing
    | CharacterAnimationActing
    | CharacterAnimationDefending
    | CharacterAnimationSpecial // works for jump, cast magic, being healed, and perhaps others!
    | CharacterAnimationSlain

type CharacterAnimationState =
    { StartTime : int64
      AnimationType : CharacterAnimationType
      Direction : Direction }
    
    static member initial =
        { StartTime = 0L
          AnimationType = CharacterAnimationFacing
          Direction = Upward }

    member this.UpdateDirection direction =
        { this with Direction = direction }

    member this.Slain =
        { this with AnimationType = CharacterAnimationSlain }

    member this.Facing time =
        { this with StartTime = time; AnimationType = CharacterAnimationFacing }

    static member makeAction time direction =
        { StartTime = time
          AnimationType = CharacterAnimationActing
          Direction = direction }