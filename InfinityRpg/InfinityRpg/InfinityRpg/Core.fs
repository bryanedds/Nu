namespace InfinityRpg
open System
open System.ComponentModel
open Prime
open Nu

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