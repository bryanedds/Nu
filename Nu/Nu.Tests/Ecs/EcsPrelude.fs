module Nu.Tests

open System.IO
open NUnit.Framework
open Nu.Ecs

[<SetUp>]
let Setup () = ()

let currentDirectory = Directory.GetCurrentDirectory()

[<StructuralEquality; NoComparison; Struct>]
type Test =
    { mutable Active: bool
      mutable X: int
      mutable Y: int }

    interface Test Component with
        member this.Active
            with get () = this.Active
            and set value = this.Active <- value

/// <summary>
/// Store should write and load exact same data from disk
/// </summary>
[<Test>]
let ``Store.Read: write and load to file`` () =
    let store: Test Store = Store "Name"
    let testingFileName = (currentDirectory, "ent.bin") |> Path.Combine

    let fact = Array.init 256 (fun i -> { Active = i % 2 = 0; X = i; Y = i * 2 })
    fact |> Array.iteri store.SetItem



    let fs = testingFileName |> File.OpenWrite
    store.Write fs

    let fso = testingFileName |> File.OpenRead
    store.Read 0 255 fso

    let test =
        [| for i = 0 to 255 do
               store.Item i |]
        
    fso.Close()
    File.Delete testingFileName
    Assert.AreEqual(test, fact)


/// <summary>
/// Store should write and load exact same data from disk
/// </summary>
[<Test>]
let ``Store.ReadFrom: write and load to file`` () =
    let store: Test Store = Store "Name"
    let testingFileName = (currentDirectory, "ent2.bin") |> Path.Combine

    let fact = Array.init 256 (fun i -> { Active = i % 2 = 0; X = i; Y = i * 2 })
    fact |> Array.iteri store.SetItem



    let fs =  File.OpenWrite testingFileName
    store.Write fs

    let fso = File.OpenRead testingFileName
    store.ReadFrom 0 255 fso

    let test =
        [| for i = 0 to 255 do
               store.Item i |]

    fso.Close()
    File.Delete testingFileName
    Assert.AreEqual(test, fact)
