// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module Gen =

    let private Lock = obj ()
    let private Random = Random ()
    let mutable private Id32 = 0u
    let mutable private Id64 = 0UL
    let mutable private IdForEditor = 0UL

    /// The prefix of a generated name
    let NamePrefix = "@"

    /// The separator of a generated name
    let NameSeparator = "-"

    /// Generates engine-specific values on-demand.
    type Gen =
        private | Gen of unit

        /// Get the next random number integer.
        static member random =
            lock Lock (fun () -> Random.Next ())

        /// Get the next random boolean.
        static member randomb =
            lock Lock (fun () -> Random.Next () < Int32.MaxValue / 2)

        /// Get the next random byte.
        static member randomy =
            lock Lock (fun () -> byte (Random.Next ()))

        /// Get the next random unsigned.
        static member randomu =
            lock Lock (fun () -> uint (Random.Next ()))

        /// Get the next random long.
        static member randoml =
            lock Lock (fun () -> int64 (Random.Next () <<< 32 ||| Random.Next ()))

        /// Get the next random unsigned long.
        static member randomul =
            lock Lock (fun () -> uint64 (Random.Next () <<< 32 ||| Random.Next ()))

        /// Get the next random single >= 0.0f and < 1.0f.
        static member randomf =
            lock Lock (fun () -> single (Random.NextDouble ()))

        /// Get the next random double >= 0.0 and < 1.0.
        static member randomd =
            lock Lock (fun () -> Random.NextDouble ())
            
        /// Get the next random number integer below ceiling.
        static member random1 ceiling =
            lock Lock (fun () -> Random.Next ceiling)
            
        /// Get the next random number single below ceiling.
        static member randomy1 (ceiling : byte) =
            lock Lock (fun () -> byte (Random.Next (int ceiling)))
            
        /// Get the next random number single below ceiling.
        static member randomf1 ceiling =
            lock Lock (fun () -> single (Random.NextDouble ()) * ceiling)
            
        /// Get the next random number single below ceiling.
        static member randomd1 ceiling =
            lock Lock (fun () -> Random.NextDouble () * ceiling)

        /// Get the next random number integer GTE minValue and LT ceiling.
        static member random2 minValue ceiling =
            lock Lock (fun () -> Random.Next (minValue, ceiling))

        /// Get a random element from a sequence if there are any elements or None.
        /// If seq is large, this may allocate to the LOH.
        static member randomItemOpt seq =
            let arr = Seq.toArray seq
            if Array.notEmpty arr
            then lock Lock (fun () -> Some arr.[Gen.random1 arr.Length])
            else None

        /// Get a random element from a sequence or a default if sequence is empty.
        static member randomItemOrDefault default_ seq =
            match Gen.randomItemOpt seq with
            | Some item -> item
            | None -> default_

        /// Get a random element from a sequence, throwing if the sequence is empty.
        static member randomItem seq =
            if Seq.isEmpty seq then failwith "Cannot get a random item from an empty sequence."
            Gen.randomItemOpt seq |> Option.get

        /// Get a random key if there are any or None.
        static member randomKeyOpt (dict : IDictionary<'k, 'v>) =
            Gen.randomItemOpt dict.Keys

        /// Get a random value if there are any or None.
        static member randomValueOpt (dict : IDictionary<'k, 'v>) =
            Gen.randomItemOpt dict.Values

        /// Randomly shuffle a sequence.
        /// If seq is large, this may allocate to the LOH.
        static member shuffle (seq : 'a seq) =
            seq |>
            Array.ofSeq |>
            Array.map (fun a -> (Gen.random, a)) |>
            Array.sortBy fst |>
            Array.map snd |>
            Array.toSeq

        /// Generate a unique name based on a 64-bit id.
        static member name =
            NamePrefix + string Gen.id64

        /// Check that a name is generated from a 64-bit id.
        static member isNameGenerated (name : string) =
            let mutable p = 0UL
            name.StartsWith NamePrefix &&
            UInt64.TryParse (String.skip 1 name, &p)

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

        /// Generate a unique non-zero 64-bit id.
        static member id32 =
            lock Lock (fun () ->
                if Id32 = UInt32.MaxValue then failwith "Overflowed Gen.Id32."
                Id32 <- inc Id32; Id32)

        /// Generate a unique non-zero 64-bit id.
        static member id64 =
            lock Lock (fun () -> Id64 <- inc Id64; Id64)

        /// Derive a unique id and name if given none.
        static member id64AndNameIf nameOpt =
            let (id, name) =
                match nameOpt with
                | Some name ->
                    let id =
                        if Gen.isNameGenerated name then
                            match UInt64.TryParse (String.skip 1 name) with
                            | (true, id) -> id
                            | (false, _) -> Gen.id64
                        else Gen.id64
                    (id, name)
                | None ->
                    let id = Gen.id64
                    let name = NamePrefix + string id
                    (id, name)
            (id, name)

        /// Derive a unique id and surnames if given none.
        static member id64AndSurnamesIf surnamesOpt =
            match surnamesOpt with
            | Some surnames ->
                if Array.length surnames = 0 then failwith "Entity must have at least one surname."
                let id =
                    match surnames with
                    | [||] -> Gen.id64
                    | _ ->
                        let name = Array.last surnames
                        if Gen.isNameGenerated name then
                            match UInt64.TryParse (String.skip 1 name) with
                            | (true, id) -> id
                            | (false, _) -> Gen.id64
                        else Gen.id64
                (id, surnames)
            | None ->
                let id = Gen.id64
                let name = NamePrefix + string id
                (id, [|name|])

        /// Generate a unique non-zero 64-bit id for use in editor naming.
        static member idForEditor =
            lock Lock (fun () -> IdForEditor <- inc IdForEditor; IdForEditor)

        /// Generate a unique name for use in an editor.
        static member nameForEditor (dispatcherName : string) =
            let id = Gen.idForEditor
            let truncatedName = dispatcherName.Replace ("Dispatcher", "")
            truncatedName + NameSeparator + id.ToString "D4"

/// Generates engine-specific values on-demand.
type Gen = Gen.Gen