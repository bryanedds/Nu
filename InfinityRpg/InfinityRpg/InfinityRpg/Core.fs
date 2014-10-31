namespace InfinityRpg

[<AutoOpen>]
module DirectionModule =

    type Direction =
        | North
        | East
        | South
        | West

[<AutoOpen>]
module AnimationModule =

    type AnimationData =
        { FrameCount : int
          FrameStutter : int }