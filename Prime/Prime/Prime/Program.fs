// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open FSharpx.Collections
open Prime
open Prime.Tests
module Program =

    let rand = Random 1
    let entries = [|for _ in 0 .. 65535 do yield (let n = rand.Next () in (string n, (string n, string n)))|]

    for _ in 0 .. 15 do
        let map = Array.fold (fun map (k, v) -> Vmap.add k v map) (Vmap.makeEmpty (KeyEq strEq) 5) entries
        GC.Collect ()
        let watch = Stopwatch.StartNew ()
        Array.iter (fun (k, _) -> ignore ^ Vmap.find k map) entries
        watch.Stop ()
        printfn "%A" watch.Elapsed
        ignore map

    printfn "%s" "NEXT"

    for _ in 0 .. 15 do
        let map = Array.fold (fun map (k, v) -> Map.add k v map) Map.empty entries
        GC.Collect ()
        let watch = Stopwatch.StartNew ()
        Array.iter (fun (k, _) -> ignore ^ Map.find k map) entries
        watch.Stop ()
        printfn "%A" watch.Elapsed
        ignore map

    printfn "%s" "NEXT"

    for _ in 0 .. 15 do
        let map = Dictionary<string, string * string> ()
        Array.iter (fun (k, v) -> if not ^ map.ContainsKey k then map.Add (k, v)) entries
        GC.Collect ()
        let watch = Stopwatch.StartNew ()
        Array.iter (fun (k, _) -> ignore ^ map.[k]) entries
        watch.Stop ()
        printfn "%A" watch.Elapsed
        ignore map

    // apparently a side-effect is needed to avoid the empty program warning
    Console.Write "Running Prime.exe"