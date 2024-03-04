namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open Prime

/// A dictionary with access-age-based eviction.
/// TODO: once this is well-tested, let's consider moving into Prime.
/// NOTE: not supported by SymbolicConverter.
type ForgetfulDictionary<'k, 'v when 'k : equality> (capacity : int, comparer : 'k IEqualityComparer) =

    let entries = Dictionary<'k, 'v> (capacity, comparer)
    let accesses = Dictionary<'k, int64> (capacity, comparer)
    let evictions = List<'k> () // OPTIMIZATION: cached to avoid thrashing LOH.

    new (capacity) =
        ForgetfulDictionary (capacity, HashIdentity.Structural)

    new (comparer) =
        ForgetfulDictionary (0, comparer)

    member this.IsReadOnly =
        false

    member this.Count =
        entries.Count

    member this.Keys =
        let now = Stopwatch.GetTimestamp ()
        for entry in accesses do accesses.[entry.Key] <- now
        entries.Keys

    member this.Values =
        let now = Stopwatch.GetTimestamp ()
        for entry in accesses do accesses.[entry.Key] <- now
        entries.Values

    member this.ContainsKey key =
        if entries.ContainsKey key then
            accesses.[key] <- Stopwatch.GetTimestamp ()
            true
        else false

    member this.TryGetValue (key, value: 'v outref) =
        if entries.TryGetValue (key, &value) then
            accesses.[key] <- Stopwatch.GetTimestamp ()
            true
        else false

    member this.Item
        with get key = 
            let mutable v = Unchecked.defaultof<'v>
            if this.TryGetValue (key, &v) then v
            else raise (KeyNotFoundException("The key was not found in the dictionary."))
        and set key value =
            entries.[key] <- value
            accesses.[key] <- Stopwatch.GetTimestamp ()

    member this.Add (key, value) =
        entries.Add (key, value)
        accesses.Add (key, Stopwatch.GetTimestamp ())

    member this.Remove key =
        entries.Remove key |> ignore<bool>
        accesses.Remove key

    member this.Clear() =
        entries.Clear ()
        accesses.Clear ()

    member this.GetEnumeratorGeneralized () =
        let now = Stopwatch.GetTimestamp ()
        for entry in accesses do accesses.[entry.Key] <- now
        entries.GetEnumerator () :> IEnumerator

    member this.GetEnumerator () =
        let now = Stopwatch.GetTimestamp ()
        for entry in accesses do accesses.[entry.Key] <- now
        entries.GetEnumerator () :> IEnumerator<KeyValuePair<'k, 'v>>

    member this.CopyTo (array, index) =
        let now = Stopwatch.GetTimestamp ()
        for entry in accesses do accesses.[entry.Key] <- now // TODO: don't update access time of entries before 'index'.
        (entries :> ICollection).CopyTo (array, index)

    member this.Evict (accessAgeTicks : int64) =
        let time = Stopwatch.GetTimestamp ()
        let cutoff = time - accessAgeTicks
        evictions.Clear ()
        for accessEntry in accesses do
            if accessEntry.Value < cutoff then
                evictions.Add accessEntry.Key
        for key in evictions do
            entries.Remove key |> ignore<bool>
            accesses.Remove key |> ignore<bool>

    member this.Evict (accessAgeSeconds : double) =
        this.Evict (accessAgeSeconds * double Stopwatch.Frequency |> int64)

    interface IDictionary<'k, 'v> with
        member this.IsReadOnly = this.IsReadOnly
        member this.Count = this.Count
        member this.Keys = this.Keys
        member this.Values = this.Values
        member this.Item with get value = this.[value] and set key value = this.[key] <- value
        member this.Contains kvp = match this.TryGetValue kvp.Key with (true, value) -> objEq kvp.Value value | (false, _) -> false
        member this.ContainsKey key = this.ContainsKey key
        member this.TryGetValue (key, value) = this.TryGetValue (key, &value)
        member this.Add kvp = this.Add (kvp.Key, kvp.Value)
        member this.Add (key, value) = this.Add (key, value)
        member this.Remove (kvp : KeyValuePair<'k, 'v>) = match this.TryGetValue kvp.Key with (true, value) -> (if objEq kvp.Value value then this.Remove kvp.Key else false) | (false, _) -> false
        member this.Remove key = this.Remove key
        member this.Clear () = this.Clear ()
        member this.CopyTo (array, index) = this.CopyTo (array, index)
        member this.GetEnumerator () = this.GetEnumeratorGeneralized ()
        member this.GetEnumerator () = this.GetEnumerator ()