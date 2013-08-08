//module Program = let [<EntryPoint>] main = fun _ -> 0

// NOTE: the code below is just expirementing with F# serialization...

module Program
open System
open System.IO
open System.Text  
open System.Xml
open System.Xml.Serialization
open System.Runtime.Serialization
open System.Reflection
open Microsoft.FSharp.Reflection

let getUnionTypes<'a> () =
    let nestedTypes = typedefof<'a>.GetNestedTypes (BindingFlags.Public ||| BindingFlags.NonPublic) 
    Array.filter FSharpType.IsUnion nestedTypes

type Alpha =
    { X : int * int
      Y : Alpha option }
      
type [<KnownType "GetTypes">] Beta =
    | A of int * Alpha
    | B of Beta option
    | C of Map<int, Beta>
    static member GetTypes () = getUnionTypes<Beta> ()

let [<EntryPoint>] main _ =
    let alpha = { X = (0, 0); Y = Some { X = (1, 1); Y = None }}
    let betaA = A (0, alpha)
    let betaB = B (Some betaA)
    let betaC = C (Map.singleton 0 betaB)
    let sb = new StringBuilder()
    let xmlSerializer = DataContractSerializer(typeof<Beta>); 
    xmlSerializer.WriteObject(new XmlTextWriter(new StringWriter(sb)), betaC)
    let sr = sb.ToString()
    printfn "%A" sr
    0