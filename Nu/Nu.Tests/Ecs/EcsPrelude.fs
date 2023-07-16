module Nu.Tests

open System
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
    let mutable store: Test Store = Store "Name"
    let testingFileName = (currentDirectory, "ent.bin") |> Path.Combine

    let fact = Array.init 256 (fun i -> { Active = i % 2 = 0; X = i; Y = i * 2 })
    fact |> Array.iteri store.SetItem

    let fs = testingFileName |> File.OpenWrite
    store.Write 0 256 fs

    store <- Store "Name"

    let fso = testingFileName |> File.OpenRead
    store.Read 0 256 fso

    let test =
        [| for i = 0 to 255 do
               store.Item i |]

    fso.Close()
    File.Delete testingFileName
    Assert.AreEqual(fact, test)

/// <summary>
/// Should throw IndexOutOfRangeException if count is greater that number of entities stored
/// </summary>
[<Test>]
let ``Store.Write: Should throw IndexOutOfRangeException`` () =
    try
        let store: Test Store = Store "Name"
        store.Write 257 0 null
    with :? IndexOutOfRangeException as e ->
        Assert.Pass()

    Assert.Fail()


/// <summary>
/// Should throw ArgumentOutOfRangeException if count is greater than number of entities stored
/// </summary>
[<Test>]
let ``Store.Write: Should throw ArgumentOutOfRangeException`` () =
    try
        let store: Test Store = Store "Name"
        store.Write 0 257 null
    with :? ArgumentOutOfRangeException as e ->
        Assert.Pass()

    Assert.Fail()

/// <summary>
/// Should throw EndOfStreamException if filestream did not read requested amount of entities
/// </summary>
[<Test>]
let ``Store.Read: Should throw EndOfStreamException`` () =
    let temp = (currentDirectory, "throwtest.bin") |> Path.Combine
    use fs = File.OpenWrite temp
    
    fs.Flush()
    fs.Close()
    use fs = File.OpenRead temp
    try
        let store: Test Store = Store "Name"
        store.Read 0 2 fs
    with :? EndOfStreamException as e ->
        fs.Close()
        File.Delete temp
        Assert.Pass()
        
    fs.Close()    
    File.Delete temp
    Assert.Fail()
