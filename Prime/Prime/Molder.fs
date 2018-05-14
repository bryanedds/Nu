namespace Prime
open System
open System.Net
open System.Collections.Generic
open FSharp.Reflection

type MemberPath =
    { TypeName : string
      MemberName : string }

type [<NoEquality; NoComparison>] InstantiationRule =
    | Constant of obj
    | Variable of (unit -> obj)

type InstantiationRules =
    Map<MemberPath option, InstantiationRule>

[<RequireQualifiedAccess>]
module Molder =

    // A primitive mold type.
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
        | Enum of string * string array
        | DateTime
        | IPAddress
        | Guid

    /// Describes an F# type in a normalized fashion.
    type Mold =
        | Primitive of MemberPath option * Primitive
        | Tuple of MemberPath option * string * Mold array
        | Record of MemberPath option * string * (string * Mold) array
        | Union of MemberPath option * string * (string * Mold array) array
        | Option of MemberPath option * Mold
        | List of MemberPath option * Mold
        | Set of MemberPath option * Mold
        | Map of MemberPath option * Mold * Mold
        | Array of MemberPath option * Mold
        | Unidentified of MemberPath option

        static member getMemberPathOpt mold =
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

    /// Automatically converts F# types, molds, and instances as customized by user-defined rules.
    type [<NoEquality; NoComparison>] Molder =
        private
            { Randomizing : bool
              RequireIdentification : bool
              Random : Random
              InstantiationRules : InstantiationRules
              Cache : Dictionary<MemberPath option * Type, Mold ref> }

    let getRandomizing molder =
        molder.Randomizing

    let setRandomizing randomizing molder =
        { molder with Randomizing = randomizing }

    let getRequireIdentification molder =
        molder.RequireIdentification

    let setRequireIdentification requireIdentification molder =
        { molder with RequireIdentification = requireIdentification }

    let getInstantiationRules molder =
        molder.InstantiationRules

    let setInstantiationRules instantiationRules molder =
        { molder with InstantiationRules = instantiationRules }

    let addInstantiationRule<'t> memberName rule molder =
        let key = Some { TypeName = typeof<'t>.AssemblyQualifiedName; MemberName = memberName }
        let value = rule
        let rules = Map.add key value molder.InstantiationRules
        { molder with InstantiationRules = rules }

    let removeInstantiationRule<'t> memberName molder =
        let key = Some { TypeName = typeof<'t>.AssemblyQualifiedName; MemberName = memberName }
        let rules = Map.remove key molder.InstantiationRules
        { molder with InstantiationRules = rules }

    let private primitiveToInstance primitive molder =
        match primitive with
        | Unit -> () :> obj
        | Boolean -> if molder.Randomizing then (if molder.Random.Next () % 2 = 0 then false else true) :> obj else false :> obj
        | Byte -> if molder.Randomizing then byte (molder.Random.Next ()) :> obj else byte 0 :> obj
        | Char -> if molder.Randomizing then char (molder.Random.Next ()) :> obj else char 0 :> obj
        | String -> if molder.Randomizing then string (Guid.NewGuid ()) :> obj else "" :> obj
        | Int32 -> if molder.Randomizing then int (molder.Random.Next ()) :> obj else int 0 :> obj
        | Int64 -> if molder.Randomizing then int64 (molder.Random.Next ()) + int64 (molder.Random.Next () <<< 32) :> obj else int64 0 :> obj
        | Single -> if molder.Randomizing then single (molder.Random.NextDouble ()) :> obj else single 0 :> obj
        | Double -> if molder.Randomizing then double (molder.Random.NextDouble ()) :> obj else double 0 :> obj
        | Decimal -> if molder.Randomizing then decimal (molder.Random.NextDouble ()) :> obj else decimal 0 :> obj
        | Enum (name, cases) -> let ty = Type.GetType name in if molder.Randomizing then Enum.Parse (ty, cases.[molder.Random.Next cases.Length]) else Activator.CreateInstance ty
        | DateTime -> if molder.Randomizing then DateTime.UtcNow :> obj else DateTime.MinValue :> obj
        | IPAddress -> if molder.Randomizing then new IPAddress (int64 (molder.Random.Next ())) :> obj else Guid.Empty :> obj
        | Guid -> if molder.Randomizing then Guid.NewGuid () :> obj else Guid.Empty :> obj

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
            elif ty.IsEnum then moldRef := Primitive (pathOpt, Enum (ty.AssemblyQualifiedName, Enum.GetNames ty))
            elif ty = typeof<System.DateTime> then moldRef := Primitive (pathOpt, DateTime)
            elif ty = typeof<System.Net.IPAddress> then moldRef := Primitive (pathOpt, IPAddress)
            elif ty = typeof<System.Guid> then moldRef := Primitive (pathOpt, Guid)
            elif FSharpType.IsTuple ty then
                let name = (ty.GetGenericTypeDefinition ()).AssemblyQualifiedName
                let elementMolds =
                    FSharpType.GetTupleElements ty |>
                    Array.map (fun ty' -> typeToMold3 None ty' molder)
                moldRef := Tuple (pathOpt, name, elementMolds)
            elif FSharpType.IsRecord ty then
                let name = ty.AssemblyQualifiedName
                let fieldMolds =
                    FSharpType.GetRecordFields ty |>
                    Array.map (fun propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)) |>
                    Array.map (fun (name', ty') -> (name', typeToMold3 (Some { TypeName = name; MemberName = name' }) ty' molder))
                moldRef := Record (pathOpt, name, fieldMolds)
            elif FSharpType.IsUnion ty then
                let name = ty.AssemblyQualifiedName
                let cases = FSharpType.GetUnionCases ty
                let caseMolds =
                    Array.map (fun (case : UnionCaseInfo) ->
                        let fieldMolds =
                            case.GetFields () |>
                            Array.map (fun field -> typeToMold3 (Some { TypeName = name; MemberName = field.Name }) field.PropertyType molder)
                        (case.Name, fieldMolds))
                        cases
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
                if molder.RequireIdentification
                then failwith "Could not identity a type for molding."
                else moldRef := Unidentified pathOpt
            moldRef.Value
        | (true, moldRef) -> moldRef.Value

    let private moldToPrimitive primitive (_ : Molder) =
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

    let typeToMold ty molder =
        typeToMold3 None ty molder

    let rec moldToType mold molder =
        match mold with
        | Primitive (_, primitive) -> moldToPrimitive primitive molder
        | Tuple (_, _, molds) -> let types = molds |> Array.map (fun mold' -> moldToType mold' molder) in FSharpType.MakeTupleType types
        | Record (_, name, _) -> Type.GetType name
        | Union (_, name, _) -> Type.GetType name
        | Option (_, mold) -> let containedType = moldToType mold molder in typedefof<_ option>.MakeGenericType containedType
        | List (_, mold) -> let elementType = moldToType mold molder in typedefof<_ list>.MakeGenericType elementType
        | Set (_, mold) -> let elementType = moldToType mold molder in typedefof<_ Set>.MakeGenericType elementType
        | Map (_, keyMold, valueMold) -> typedefof<Map<_, _>>.MakeGenericType [|moldToType keyMold molder; moldToType valueMold molder|]
        | Array (_, mold) -> let elementType = moldToType mold molder in elementType.MakeArrayType ()
        | Unidentified _ -> typeof<obj>

    let rec moldToInstance mold molder =
        let pathOpt = Mold.getMemberPathOpt mold
        match Map.tryFind pathOpt molder.InstantiationRules with
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
                let elements = Array.map (fun ty' -> moldToInstance ty' molder) elementMolds
                FSharpValue.MakeTuple (elements, ty)
            | Record (_, name, fieldMolds) ->
                let ty = Type.GetType name
                let fields = Array.map (snd >> fun ty' -> moldToInstance ty' molder) fieldMolds
                FSharpValue.MakeRecord (ty, fields)
            | Union (_, name, caseMolds) ->
                let ty = Type.GetType name
                let caseInfos = FSharpType.GetUnionCases ty
                let caseInfo = caseInfos.[0]
                let (_, caseFields) = caseMolds.[0]
                let caseFieldInstances = Array.map (fun ty' -> moldToInstance ty' molder) caseFields
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

    let make randomizing requireIdentification randomizingSeed instantiationRules =
        { Randomizing = randomizing
          RequireIdentification = requireIdentification
          Random = Random randomizingSeed 
          InstantiationRules = instantiationRules
          Cache = Dictionary<MemberPath option * Type, Mold ref> () }

    let makeEmpty () =
        make false false 0 Map.empty

/// Automatically converts F# types, molds, and instances as customized by user-defined rules.
type Molder = Molder.Molder
