namespace Prime
open System
open System.Reflection

module Type =

    /// Try to get an existing type with the given unqualified name. Time-intensive.
    let TryGetTypeUnqualified name =
        match Type.GetType name with
        | null ->
            let allAssemblies = AppDomain.CurrentDomain.GetAssemblies ()
            let types = Seq.choose (fun (assembly : Assembly) -> Option.denull <| assembly.GetType name) allAssemblies
            Seq.tryFind (fun _ -> true) types
        | aType -> Some aType

    /// Get an existing type with the given unqualified name. Time-intensive.
    let GetTypeUnqualified name =
        match TryGetTypeUnqualified name with
        | Some aType -> aType
        | None -> failwith <| "Could not find type with unqualified name '" + name + "'."