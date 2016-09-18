// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open Prime
module Program =

    /// The number of samples taken for each timing.
    let [<Literal>] Samples = 3

    /// Performs some ad-hoc tests to compare performance of fns.
    let runTimings fn name =
        printfn "%s timings..." name
        for _ in 1 .. Samples do
            let watch = Stopwatch.StartNew ()
            fn () |> ignore
            watch.Stop ()
            printfn "Run time: %A" watch.Elapsed

    /// Performs some ad-hoc tests to compare performance of maps.
    let runMapTimings make lookup name =
        printfn "%s timings..." name
        let rand = Random 1
        let entries = [|for _ in 0 .. 524280 do yield let n = rand.Next () in (string n, (string n, string n))|]
        for _ in 1 .. Samples do
            GC.Collect ()
            let watch = Stopwatch.StartNew ()
            let made = make entries
            watch.Stop ()
            GC.Collect ()
            let watch2 = Stopwatch.StartNew ()
            lookup entries made
            watch2.Stop ()
            printfn "Make time: %A\tLookup time: %A\tRun time: %A" watch.Elapsed watch2.Elapsed (watch.Elapsed + watch2.Elapsed)

    /// Run timings.
    /// NOTE: even if this timing functionality is cleared out, the main entry point must remain in tact due to -
    /// https://github.com/Microsoft/visualfsharp/issues/1371#issuecomment-235101700
    let [<EntryPoint; STAThread>] main _ =

        // run array timings
        let array = [|0 .. 1000000|]
        runTimings (fun () -> array |> Array.map (fun x -> x + x * 13)) "Array Compute"

        // run list timings
        let list = [0 .. 1000000]
        runTimings (fun () -> list |> List.map (fun x -> x + x * 13)) "List Compute"
        
        // run map timings
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> Map.add k v map) Map.empty entries)
            (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Map.find k map) entries)
            "F# Map"
        
        // run vmap timings
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> Vmap.add k v map) (Vmap.makeEmpty ()) entries)
            (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Vmap.find k map) entries)
            "Vmap"
        
        // run tmap timings with computation expressions
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> Tmap.add k v map) (Tmap.makeEmpty None) entries)
            (fun entries map -> entries |> Array.iter (fun (k, _) -> ignore ^ Tmap.find k map))
            "Tmap"
        
        // run umap timings without computation expressions
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> Umap.add k v map) (Umap.makeEmpty None) entries)
            (fun entries map -> Array.iter (fun (k, _) -> ignore ^ Umap.find k map) entries)
            "Umap"
        
        // run dictionary timings
        let dic = Dictionary<string, string * string> ()
        runMapTimings
            (fun entries -> Array.iter (fun (k, v) -> if not ^ dic.ContainsKey k then dic.Add (k, v)) entries)
            (fun entries () -> Array.iter (fun (k, _) -> ignore ^ dic.[k]) entries)
            ".NET Dictionary"

        // success
        0