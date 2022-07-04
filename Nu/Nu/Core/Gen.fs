// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Text
open Prime

[<AutoOpen>]
module Gen =

    let private Lock = obj ()
    let private Random = Random ()
    let private Cids = dictPlus<string, Guid> StringComparer.Ordinal []
    let private CidBytes = Array.zeroCreate 16 // TODO: P1: use stack-based allocation via NativePtr.stackalloc and Span - https://bartoszsypytkowski.com/writing-high-performance-f-code/
    let private CnameBytes = Array.zeroCreate 16 // TODO: P1: use stack-based allocation via NativePtr.stackalloc and Span - https://bartoszsypytkowski.com/writing-high-performance-f-code/
    let mutable private IdForEditor = 0UL
    let mutable private Id64 = 0UL
    let mutable private Id32 = 0u

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

        /// The prefix of a generated name
        static member namePrefix = "@"

        /// The separator of a generated name
        static member nameSeparator = "-"

        /// Generate a unique name.
        static member name =
            Gen.namePrefix + string Gen.id

        /// Check that a name is generated.
        static member isName (name : string) =
            name.StartsWith Gen.namePrefix

        /// Generate an empty id.
        static member idEmpty =
            Guid.Empty

        /// Generate a unique id.
        static member id =
            Guid.NewGuid ()

        /// Generate a unique id that is guaranteed to be convertible to a valid UTF-16 string of 8 characters.
        static member cid =
            let id = Guid.NewGuid ()
            let str = id.ToByteArray () |> UnicodeEncoding.Unicode.GetString
            if str.Length = 8 then
                let id2 = UnicodeEncoding.Unicode.GetBytes str |> Guid
                if id.Equals id2 then id else Gen.cid
            else Gen.cid

        /// Generate a unique name that is byte-convertible to a valid cid if given none.
        static member cnameIf cnameOpt =
            match cnameOpt with
            | Some cname -> cname
            | None ->
                let cid = Gen.cid
                let cname = cid.ToByteArray () |> UnicodeEncoding.Unicode.GetString
                "@" + cname

        /// Check that a name is directly correlatable.
        static member isCname (name : string) =
            name.Length = 9 && name.[0] = '@'

        /// Correlate a name to a cid, caching a unique cid for it if it is not directly correlatable.
        static member correlate (name : string) =
            if Gen.isCname name then
                lock Lock $ fun () ->
                    Encoding.Unicode.GetBytes (name, 1, 8, CnameBytes, 0) |> ignore<int>
                    Array.Copy (CnameBytes, 0, CidBytes, 0, 16)
                    Guid CidBytes
            else
                match Cids.TryGetValue name with
                | (false, _) -> let cid = Gen.cid in Cids.Add (name, cid); cid
                | (true, cid) -> cid

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
            let name =
                match nameOpt with
                | Some name -> name
                | None -> Gen.namePrefix + string id
            (id, name)

        /// Derive a unique id and surnames if given none.
        static member idAndSurnamesIf surnamesOpt =
            match surnamesOpt with
            | Some surnames ->
                if Array.length surnames = 0 then failwith "Entity must have at least one surname."
                (Gen.id, surnames)
            | None ->
                let id = Gen.id
                let name = Gen.namePrefix + string id
                (id, [|name|])

        /// Generate a unique non-zero 64-bit id for use in editor naming.
        static member idForEditor =
            lock Lock (fun () -> IdForEditor <- inc IdForEditor; IdForEditor)

        /// Generate a unique name for use in an editor.
        static member nameForEditor (dispatcherName : string) =
            let id = Gen.idForEditor
            let truncatedName = dispatcherName.Replace ("Dispatcher", "")
            truncatedName + Gen.nameSeparator + id.ToString "D4"

        /// Generate a unique non-zero 64-bit id.
        static member id64 =
            lock Lock (fun () -> Id64 <- inc Id64; Id64)

        /// Generate a unique non-zero 64-bit id.
        static member id32 =
            lock Lock (fun () ->
                if Id32 = UInt32.MaxValue then failwith "Overflowed Gen.Id32."
                Id32 <- inc Id32; Id32)

/// Generates engine-specific values on-demand.
type Gen = Gen.Gen