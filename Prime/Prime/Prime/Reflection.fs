namespace Prime
open System
open System.Reflection

[<RequireQualifiedAccess>]
module Reflection =

    /// Try to find a type by its name from all the loaded assemblies. Time-intensive.
    let tryFindType typeName =
        match Type.GetType typeName with
        | null ->
            let allAssemblies = AppDomain.CurrentDomain.GetAssemblies ()
            let types =
                Seq.choose
                    (fun (assembly : Assembly) -> match assembly.GetType typeName with null -> None | aType -> Some aType)
                    allAssemblies
            Seq.tryFind (fun _ -> true) types 
        | aType -> Some aType

    /// Find a type by its name from all the loaded assemblies. Time-intensive.
    let findType typeName =
        match tryFindType typeName with
        | Some aType -> aType
        | None -> failwith <| "Could not find type with name '" + typeName + "'."