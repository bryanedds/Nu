namespace InfinityRpg
open System
open System.ComponentModel
open MathNet.Numerics.Random
open Nu

[<AutoOpen>]
module RandModule =

    /// Implements an immutable random number generator using a MersenneTwister.
    type [<NoEquality; NoComparison>] Rand =
        private
            { RandSource : RandomSource
              mutable Expired : bool }
        
        static member private make rand =
            rand.Expired <- true
            { RandSource = rand.RandSource; Expired = false }
        
        static member make (seed : int) =
            let randSource = MersenneTwister seed
            { RandSource = randSource; Expired = false }
        
        static member private withRand fn rand =
            if not rand.Expired
            then (fn rand.RandSource, Rand.make rand)
            else failwith "Expired Rand value."
        
        static member next rand =
            Rand.withRand (fun randSource -> randSource.Next ()) rand
        
        static member next2 max rand =
            Rand.withRand (fun randSource -> randSource.Next max) rand

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