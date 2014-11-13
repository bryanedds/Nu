namespace InfinityRpg
open System
open System.ComponentModel
open Prime
open Nu

[<AutoOpen>]
module RandModule =

    /// Implements an immutable random number generator using the xorshift* algorithm.
    type [<NoEquality; NoComparison>] Rand =
        { Current : uint64 }

        static member private int (ui64 : uint64) =
            let i = int ui64
            if i = Int32.MaxValue then i - 1
            elif i < 0 then i + Int32.MaxValue
            else i

        static member make (seed : int) =
            if seed = 0 then failwith "Seed for Rand may not be zero."
            { Current = uint64 seed }

        static member make () =
            // NOTE: number generated via http://www.random.org/bytes/
            Rand.make 0x69c6cb1b

        static member advance rand =
            let c = rand.Current
            let c = c ^^^ (c >>> 12)
            let c = c ^^^ (c <<< 25)
            let c = c ^^^ (c >>> 27)
            { Current = c }

        static member private sample rand =
            rand.Current * 2685821657736338717UL

        static member next rand =
            let rand = Rand.advance rand
            let number = Rand.int <| Rand.sample rand
            (number, rand)

        static member next2 max rand =
            let rand = Rand.advance rand
            let number = int (Rand.sample rand % uint64 max)
            (number, rand)
        
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