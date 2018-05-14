// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

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
            GC.Collect ()
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
        let array = [|0 .. 10000000|]
        runTimings (fun () -> array |> Array.rev |> Array.sort |> Array.map (fun x -> x * 13) |> Array.filter (fun x -> x % 2 = 0)) "Array Compute"

        // run ulist timings
        let ulist = UList.makeFromSeq Functional [|0 .. 10000000|]
        runTimings (fun () -> ulist |> UList.rev |> UList.sort |> UList.map (fun x -> x * 13) |> UList.filter (fun x -> x % 2 = 0)) "UList Compute"

        // run ulist imperative timings
        let ulist = UList.makeFromSeq Imperative [|0 .. 10000000|]
        runTimings (fun () -> ulist |> UList.rev |> UList.sort |> UList.map (fun x -> x * 13) |> UList.filter (fun x -> x % 2 = 0)) "UList Imperative Compute"

        // run list timings
        let list = [0 .. 10000000]
        runTimings (fun () -> list |> List.rev |> List.sort |> List.map (fun x -> x * 13) |> List.filter (fun x -> x % 2 = 0)) "F# List Compute"
        
        // run map timings
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> Map.add k v map) Map.empty entries)
            (fun entries map -> Array.iter (fun (k, _) -> Map.find k map |> ignore) entries)
            "F# Map"
        
        // run hmap timings
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> HMap.add k v map) (HMap.makeEmpty ()) entries)
            (fun entries map -> Array.iter (fun (k, _) -> HMap.find k map |> ignore) entries)
            "HMap"
        
        // run tmap timings with computation expressions
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> TMap.add k v map) (TMap.makeEmpty Functional) entries)
            (fun entries map -> entries |> Array.iter (fun (k, _) -> TMap.find k map |> ignore))
            "TMap"
        
        // run umap timings without computation expressions
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> UMap.add k v map) (UMap.makeEmpty Functional) entries)
            (fun entries map -> Array.iter (fun (k, _) -> UMap.find k map |> ignore) entries)
            "UMap"
        
        // run umap imperative timings without computation expressions
        runMapTimings
            (fun entries -> Array.fold (fun map (k, v) -> UMap.add k v map) (UMap.makeEmpty Imperative) entries)
            (fun entries map -> Array.iter (fun (k, _) -> UMap.find k map |> ignore) entries)
            "UMap Imperative"
        
        // run dictionary timings
        let dic = Dictionary<string, string * string> ()
        runMapTimings
            (fun entries -> Array.iter (fun (k, v) -> if not (dic.ContainsKey k) then dic.Add (k, v)) entries)
            (fun entries () -> Array.iter (fun (k, _) -> dic.[k] |> ignore) entries)
            ".NET Dictionary"

        // success
        0
