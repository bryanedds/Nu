// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open Prime
open Prime.Tests
module Program =

    /// Performs some ad-hoc tests to compare performance of maps.
    let runTimings make lookup name =
        printfn "%s timings..." name
        let rand = Random 1
        let entries = [|for _ in 0 .. 524280 do yield (let n = rand.Next () in (string n, (string n, string n)))|]
        for _ in 0 .. 4 do
            GC.Collect ()
            let watch = Stopwatch.StartNew ()
            let made = make entries
            watch.Stop ()
            GC.Collect ()
            let watch2 = Stopwatch.StartNew ()
            lookup entries made
            watch2.Stop ()
            printfn "Make time: %A\tLookup time: %A" watch.Elapsed watch2.Elapsed

    // run map timings
    runTimings
        (fun entries -> Array.fold (fun map (k, v) -> Map.add k v map) Map.empty entries)
        (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Map.find k map) entries)
        "F# Map"

    // run vmap timings
    runTimings
        (fun entries -> Array.fold (fun map (k, v) -> Vmap.add k v map) (Vmap.makeEmpty ()) entries)
        (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Vmap.find k map) entries)
        "Vmap"

    // run tmap timings with computation expressions
    runTimings
        (fun entries -> Array.fold (fun map (k, v) -> Tmap.add k v map) (Tmap.makeEmpty None) entries)
        (fun entries map -> entries |> Array.iter (fun (k, _) -> ignore ^ Tmap.find k map))
        "Tmap"

    // run tmap timings without computation expressions
    runTimings
        (fun entries -> Array.fold (fun map (k, v) -> Umap.add k v map) (Umap.makeEmpty None) entries)
        (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Umap.find k map) entries)
        "Umap"

    // run dictionary timings
    let dic = Dictionary<string, string * string> ()
    runTimings
        (fun entries -> Array.iter (fun (k, v) -> if not ^ dic.ContainsKey k then dic.Add (k, v)) entries)
        (fun entries () -> Array.iter (fun (k, _) -> ignore ^ dic.[k]) entries)
        ".NET Dictionary"