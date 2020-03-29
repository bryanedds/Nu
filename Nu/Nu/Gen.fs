// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime

[<AutoOpen>]
module Gen =

    let mutable private Counter = -1L
    let private CounterLock = obj ()

    /// Generates engine-specific values on-demand.
    type Gen =
        private | Gen of unit

        /// Generate a unique counter.
        static member counter =
            lock CounterLock (fun () -> Counter <- inc Counter; Counter)

        /// The prefix of a generated name
        static member namePrefix =
            "@"

        /// Generate a unique name.
        static member name =
            Gen.namePrefix + scstring Gen.counter

        /// Generate a unique name if given none.
        static member nameIf nameOpt =
            match nameOpt with
            | Some name -> name
            | None -> Gen.name

        /// Check that a name is generated.
        static member isName (name : string) =
            name.StartsWith Gen.namePrefix

        /// Generate a unique id.
        static member id =
            Guid.NewGuid ()
        
        /// Generate an id from a couple of ints.
        /// It is the user's responsibility to ensure uniqueness when using the resulting ids.
        static member idFromInts m n =
            let bytes = Array.create<byte> 8 (byte 0)
            Guid (m, int16 (n >>> 16), int16 n, bytes)

        /// Generate an id deterministically.
        /// HACK: this is an ugly hack to create a deterministic sequance of guids.
        /// Limited to creating 65,536 guids.
        static member idDeterministic offset (guid : Guid) =
            let arr = guid.ToByteArray ()
            if arr.[15] + byte offset < arr.[15] then arr.[14] <- arr.[14] + byte 1
            arr.[15] <- arr.[15] + byte offset                    
            Guid arr

        /// Derive a unique id and name if given none.
        static member idAndNameIf nameOpt =
            let id = Gen.id
            let name = Gen.nameIf nameOpt
            (id, name)

/// Generates engine-specific values on-demand.
type Gen = Gen.Gen