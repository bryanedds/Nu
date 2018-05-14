namespace Prime
open System
open System.Net
open System.Collections.Generic
open FSharp.Reflection

[<RequireQualifiedAccess>]
module Molder =

    type Path =
        string * string

    type [<NoEquality; NoComparison>] Rule =
        | Constant of obj
        | Variable of (unit -> obj)

    type Rules =
        Map<Path option, Rule>

    type Primitive =
        | Unit
        | Boolean
        | Byte
        | Char
        | String
        | Int32
        | Int64
        | Single
        | Double
        | Decimal
        | Enum of string * string list
        | DateTime
        | IPAddress
        | Guid

    /// Describes an F# type in a normalized fashion.
    type Mold =
        | Primitive of Path option * Primitive
        | Tuple of Path option * string * Mold list
        | Record of Path option * string * (string * Mold) list
        | Union of Path option * string * (string * Mold list) list
        | Option of Path option * Mold
        | List of Path option * Mold
        | Set of Path option * Mold
        | Map of Path option * Mold * Mold
        | Array of Path option * Mold
        | Unidentified of Path option

    /// Automatically converts F# types, molds, and instances as customized by user-defined rules.
    type [<NoEquality; NoComparison>] Molder =
        private
            { ForceIdentification : bool
              DefaultAsRandom : bool
              Random : Random
              Rules : Rules
              Cache : Dictionary<Path option * Type, Mold ref> }

    let getForceIdentification molder =
        Map.containsKey molder.ForceIdentification

    let getRandomization molder =
        Map.containsKey molder.DefaultAsRandom

    let getPathOpt mold (_ : Molder) =
        match mold with
        | Primitive (pathOpt, _)
        | Tuple (pathOpt, _, _)
        | Record (pathOpt, _, _)
        | Union (pathOpt, _, _)
        | Option (pathOpt, _)
        | List (pathOpt, _)
        | Set (pathOpt, _)
        | Map (pathOpt, _, _)
        | Array (pathOpt, _)
        | Unidentified pathOpt -> pathOpt

    let primitiveToInstance primitive molder =
        match primitive with
        | Unit -> () :> obj
        | Boolean -> if molder.DefaultAsRandom then (if molder.Random.Next () % 2 = 0 then false else true) :> obj else false :> obj
        | Byte -> if molder.DefaultAsRandom then byte (molder.Random.Next ()) :> obj else byte 0 :> obj
        | Char -> if molder.DefaultAsRandom then char (molder.Random.Next ()) :> obj else char 0 :> obj
        | String -> if molder.DefaultAsRandom then string (Guid.NewGuid ()) :> obj else "" :> obj
        | Int32 -> if molder.DefaultAsRandom then int (molder.Random.Next ()) :> obj else int 0 :> obj
        | Int64 -> if molder.DefaultAsRandom then int64 (molder.Random.Next ()) + int64 (molder.Random.Next () <<< 32) :> obj else int64 0 :> obj
        | Single -> if molder.DefaultAsRandom then single (molder.Random.NextDouble ()) :> obj else single 0 :> obj
        | Double -> if molder.DefaultAsRandom then double (molder.Random.NextDouble ()) :> obj else double 0 :> obj
        | Decimal -> if molder.DefaultAsRandom then decimal (molder.Random.NextDouble ()) :> obj else decimal 0 :> obj
        | Enum (name, cases) -> let ty = Type.GetType name in if molder.DefaultAsRandom then Enum.Parse (ty, cases.[molder.Random.Next cases.Length]) else Activator.CreateInstance ty
        | DateTime -> if molder.DefaultAsRandom then DateTime.UtcNow :> obj else DateTime.MinValue :> obj
        | IPAddress -> if molder.DefaultAsRandom then new IPAddress (int64 (molder.Random.Next ())) :> obj else Guid.Empty :> obj
        | Guid -> if molder.DefaultAsRandom then Guid.NewGuid () :> obj else Guid.Empty :> obj

    let rec private typeToMold3 pathOpt (ty : Type) molder =
        let key = (pathOpt, ty)
        match molder.Cache.TryGetValue key with
        | (false, _) ->
            let moldRef = ref (Unidentified None)
            molder.Cache.Add ((pathOpt, ty), moldRef)
            if ty = typeof<FSharp.Core.Unit> then moldRef := Primitive (pathOpt, Unit)
            elif ty = typeof<System.Boolean> then moldRef := Primitive (pathOpt, Boolean)
            elif ty = typeof<System.Byte> then moldRef := Primitive (pathOpt, Byte)
            elif ty = typeof<System.Char> then moldRef := Primitive (pathOpt, Char)
            elif ty = typeof<System.String> then moldRef := Primitive (pathOpt, String)
            elif ty = typeof<System.Int32> then moldRef := Primitive (pathOpt, Int32)
            elif ty = typeof<System.Int64> then moldRef := Primitive (pathOpt, Int64)
            elif ty = typeof<System.Single> then moldRef := Primitive (pathOpt, Single)
            elif ty = typeof<System.Double> then moldRef := Primitive (pathOpt, Double)
            elif ty = typeof<System.Decimal> then moldRef := Primitive (pathOpt, Decimal)
            elif ty.IsEnum then moldRef := Primitive (pathOpt, Enum (ty.AssemblyQualifiedName, Array.toList (Enum.GetNames ty)))
            elif ty = typeof<System.DateTime> then moldRef := Primitive (pathOpt, DateTime)
            elif ty = typeof<System.Net.IPAddress> then moldRef := Primitive (pathOpt, IPAddress)
            elif ty = typeof<System.Guid> then moldRef := Primitive (pathOpt, Guid)
            elif FSharpType.IsTuple ty then
                let name = (ty.GetGenericTypeDefinition ()).AssemblyQualifiedName
                let elementMolds =
                    FSharpType.GetTupleElements ty |>
                    Array.map (fun ty' -> typeToMold3 None ty' molder) |>
                    Array.toList
                moldRef := Tuple (pathOpt, name, elementMolds)
            elif FSharpType.IsRecord ty then
                let name = ty.AssemblyQualifiedName
                let fieldMolds =
                    FSharpType.GetRecordFields ty |>
                    Array.map (fun propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)) |>
                    Array.map (fun (name', ty') -> (name', typeToMold3 (Some (name, name')) ty' molder)) |>
                    Array.toList
                moldRef := Record (pathOpt, name, fieldMolds)
            elif FSharpType.IsUnion ty then
                let name = ty.AssemblyQualifiedName
                let cases = FSharpType.GetUnionCases ty
                let caseMolds =
                    cases |>
                    Array.map (fun case ->
                        let fieldMolds = case.GetFields () |> Array.map (fun field -> typeToMold3 (Some (name, field.Name)) field.PropertyType molder) |> Array.toList
                        (case.Name, fieldMolds)) |>
                    Array.toList
                moldRef := Union (pathOpt, name, caseMolds)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ option> then
                let containedType = ty.GenericTypeArguments.[0]
                let containedMold = typeToMold3 None containedType molder
                moldRef := Option (pathOpt, containedMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ list> then
                let elementType = ty.GenericTypeArguments.[0]
                let elementMold = typeToMold3 None elementType molder
                moldRef := List (pathOpt, elementMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ Set> then
                let elementType = ty.GenericTypeArguments.[0]
                let elementMold = typeToMold3 None elementType molder
                moldRef := List (pathOpt, elementMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<Map<_, _>> then
                let keyType = ty.GenericTypeArguments.[0]
                let keyMold = typeToMold3 None keyType molder
                let valueType = ty.GenericTypeArguments.[1]
                let valueMold = typeToMold3 None valueType molder
                moldRef := Map (pathOpt, keyMold, valueMold)
            elif ty.IsArray then
                let elementType = ty.GetElementType ()
                let elementMold = typeToMold3 None elementType molder
                moldRef := List (pathOpt, elementMold)
            else
                if molder.ForceIdentification
                then failwith "Could not identity a type for molding."
                else moldRef := Unidentified pathOpt
            moldRef.Value
        | (true, moldRef) -> moldRef.Value

    let typeToMold ty molder =
        typeToMold3 None ty molder

    let moldToPrimitive primitive (_ : Molder) =
        match primitive with
        | Unit -> typeof<FSharp.Core.Unit>
        | Boolean -> typeof<System.Boolean>
        | Byte -> typeof<System.Byte>
        | Char -> typeof<System.Char>
        | String -> typeof<System.String>
        | Int32 -> typeof<System.Int32>
        | Int64 -> typeof<System.Int64>
        | Single -> typeof<System.Single>
        | Double -> typeof<System.Double>
        | Decimal -> typeof<System.Decimal>
        | Enum (name, _) -> Type.GetType name
        | DateTime -> typeof<System.DateTime>
        | IPAddress -> typeof<System.Net.IPAddress>
        | Guid -> typeof<System.Guid>

    let rec moldToType mold molder =
        match mold with
        | Primitive (_, primitive) -> moldToPrimitive primitive molder
        | Tuple (_, _, molds) -> let types = molds |> List.map (fun mold' -> moldToType mold' molder) |> List.toArray in FSharpType.MakeTupleType types
        | Record (_, name, _) -> Type.GetType name
        | Union (_, name, _) -> Type.GetType name
        | Option (_, mold) -> let containedType = moldToType mold molder in typedefof<_ option>.MakeGenericType containedType
        | List (_, mold) -> let elementType = moldToType mold molder in typedefof<_ list>.MakeGenericType elementType
        | Set (_, mold) -> let elementType = moldToType mold molder in typedefof<_ Set>.MakeGenericType elementType
        | Map (_, keyMold, valueMold) -> typedefof<Map<_, _>>.MakeGenericType [|moldToType keyMold molder; moldToType valueMold molder|]
        | Array (_, mold) -> let elementType = moldToType mold molder in elementType.MakeArrayType ()
        | Unidentified _ -> typeof<obj>

    let rec moldToInstance mold molder =
        let pathOpt = getPathOpt mold molder
        match Map.tryFind pathOpt molder.Rules with
        | Some rule ->
            match rule with
            | Constant value -> value
            | Variable fn -> fn ()
        | None ->
            match mold with
            | Primitive (_, primitive) ->
                primitiveToInstance primitive molder
            | Tuple (_, name, elementMolds) ->
                let ty = Type.GetType name
                let elements = List.map (fun ty' -> moldToInstance ty' molder) elementMolds |> List.toArray
                FSharpValue.MakeTuple (elements, ty)
            | Record (_, name, fieldMolds) ->
                let ty = Type.GetType name
                let fields = List.map (snd >> fun ty' -> moldToInstance ty' molder) fieldMolds |> List.toArray
                FSharpValue.MakeRecord (ty, fields)
            | Union (_, name, caseMolds) ->
                let ty = Type.GetType name
                let caseInfos = FSharpType.GetUnionCases ty
                let caseInfo = caseInfos.[0]
                let (_, caseFields) = caseMolds.[0]
                let caseFieldInstances = List.map (fun ty' -> moldToInstance ty' molder) caseFields |> List.toArray
                FSharpValue.MakeUnion (caseInfo, caseFieldInstances)
            | Option (_, _) ->
                None :> obj
            | List (_, mold) ->
                let elementType = moldToType mold molder
                let ty = typedefof<_ list>.MakeGenericType [|elementType|]
                Reflection.objsToList ty []
            | Set (_, mold) ->
                let elementType = moldToType mold molder
                let ty = typedefof<_ Set>.MakeGenericType [|elementType|]
                Reflection.objsToSet ty []
            | Map (_, keyMold, valueMold) ->
                let keyType = moldToType keyMold molder
                let valueType = moldToType valueMold molder
                let ty = typedefof<Map<_, _>>.MakeGenericType [|keyType; valueType|]
                Reflection.pairsToMap ty []
            | Array (_, mold) ->
                let elementType = moldToType mold molder
                let ty = elementType.MakeArrayType ()
                Reflection.objsToArray ty []
            | Unidentified _ ->
                null :> obj

    let instanceToMold instance molder =
        let ty = instance.GetType ()
        typeToMold ty molder

    let typeToInstance (ty : Type) molder =
        let mold = typeToMold ty molder
        let instance = moldToInstance mold molder
        instance

    let instanceToType instance molder =
        let mold = instanceToMold instance molder
        let ty = moldToType mold molder
        ty

    let make forceIdentification defaultAsRandom seed rules =
        { ForceIdentification = forceIdentification
          DefaultAsRandom = defaultAsRandom
          Random = Random seed 
          Rules = rules
          Cache = Dictionary<Path option * Type, Mold ref> () }

    let makeEmpty () =
        make false false 0 Map.empty

/// Automatically converts F# types, molds, and instances as customized by user-defined rules.
type Molder = Molder.Molder
