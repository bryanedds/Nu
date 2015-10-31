// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics
open Prime
open Prime.Tests
module Program =

    (*let rand = Random 1
    let entries = [|for _ in 0 .. 65535 do yield (let n = rand.Next () in (n, string n))|]

    for _ in 0 .. 15 do
        let watch = Stopwatch.StartNew ()
        let map = Array.fold (fun map (k, v) -> Vmap.add k v map) (Vmap.makeEmpty 6) entries
        watch.Stop ()
        printfn "%A" watch.Elapsed
        ignore map

    printfn "%s" "NEXT"

    for _ in 0 .. 15 do
        let watch = Stopwatch.StartNew ()
        let map = Array.fold (fun map (k, v) -> Map.add k v map) Map.empty entries
        watch.Stop ()
        printfn "%A" watch.Elapsed
        ignore map*)

    // apparently a side-effect is needed to avoid the empty program warning
    Console.Write "Running Prime.exe"