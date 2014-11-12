namespace InfinityRpg
open System
open System.ComponentModel
open MathNet.Numerics.Random
open Nu

[<AutoOpen>]
module RandModule =

    type [<NoEquality; NoComparison>] Rand =
        private
            { RandSource : RandomSource }
        static member make (seed : int) =
            { RandSource = MersenneTwister seed }
        static member next rand =
            (rand.RandSource.Next (), { RandSource = rand.RandSource })
        static member next2 max rand =
            (rand.RandSource.Next max, { RandSource = rand.RandSource })

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