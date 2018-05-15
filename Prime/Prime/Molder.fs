namespace Prime
open System
open System.Net
open System.Collections.Generic
open FSharp.Reflection

[<RequireQualifiedAccess>]
module Molder =

    /// Automatically converts F# types, molds, and instances as specified by user-defined rules.
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
        let key = Some { Mold.TypeName = typeof<'t>.AssemblyQualifiedName; Mold.MemberName = memberName }
        let value = rule
        let rules = Map.add key value molder.InstantiationRules
        { molder with InstantiationRules = rules }

    let removeInstantiationRule<'t> memberName molder =
        let key = Some { Mold.TypeName = typeof<'t>.AssemblyQualifiedName; Mold.MemberName = memberName }
        let rules = Map.remove key molder.InstantiationRules
        { molder with InstantiationRules = rules }

    let private primitiveToInstance primitive molder =
        match primitive with
        | Mold.Unit -> () :> obj
        | Mold.Boolean -> if molder.Randomizing then (if molder.Random.Next () % 2 = 0 then false else true) :> obj else false :> obj
        | Mold.Byte -> if molder.Randomizing then byte (molder.Random.Next ()) :> obj else byte 0 :> obj
        | Mold.Char -> if molder.Randomizing then char (molder.Random.Next ()) :> obj else char 0 :> obj
        | Mold.String -> if molder.Randomizing then string (Guid.NewGuid ()) :> obj else "" :> obj
        | Mold.Int32 -> if molder.Randomizing then int (molder.Random.Next ()) :> obj else int 0 :> obj
        | Mold.Int64 -> if molder.Randomizing then int64 (molder.Random.Next ()) + int64 (molder.Random.Next () <<< 32) :> obj else int64 0 :> obj
        | Mold.Single -> if molder.Randomizing then single (molder.Random.NextDouble ()) :> obj else single 0 :> obj
        | Mold.Double -> if molder.Randomizing then double (molder.Random.NextDouble ()) :> obj else double 0 :> obj
        | Mold.Decimal -> if molder.Randomizing then decimal (molder.Random.NextDouble ()) :> obj else decimal 0 :> obj
        | Mold.Enum (name, cases) -> let ty = Type.GetType name in if molder.Randomizing then Enum.Parse (ty, cases.[molder.Random.Next cases.Length]) else Activator.CreateInstance ty
        | Mold.DateTime -> if molder.Randomizing then DateTime.UtcNow :> obj else DateTime.MinValue :> obj
        | Mold.IPAddress -> if molder.Randomizing then new IPAddress (int64 (molder.Random.Next ())) :> obj else Guid.Empty :> obj
        | Mold.Guid -> if molder.Randomizing then Guid.NewGuid () :> obj else Guid.Empty :> obj

    let rec private typeToMold3 pathOpt (ty : Type) molder =
        let key = (pathOpt, ty)
        match molder.Cache.TryGetValue key with
        | (false, _) ->
            let moldRef = ref (Mold.Unidentified None)
            molder.Cache.Add ((pathOpt, ty), moldRef)
            if ty = typeof<FSharp.Core.Unit> then moldRef := Mold.Primitive (pathOpt, Mold.Unit)
            elif ty = typeof<System.Boolean> then moldRef := Mold.Primitive (pathOpt, Mold.Boolean)
            elif ty = typeof<System.Byte> then moldRef := Mold.Primitive (pathOpt, Mold.Byte)
            elif ty = typeof<System.Char> then moldRef := Mold.Primitive (pathOpt, Mold.Char)
            elif ty = typeof<System.String> then moldRef := Mold.Primitive (pathOpt, Mold.String)
            elif ty = typeof<System.Int32> then moldRef := Mold.Primitive (pathOpt, Mold.Int32)
            elif ty = typeof<System.Int64> then moldRef := Mold.Primitive (pathOpt, Mold.Int64)
            elif ty = typeof<System.Single> then moldRef := Mold.Primitive (pathOpt, Mold.Single)
            elif ty = typeof<System.Double> then moldRef := Mold.Primitive (pathOpt, Mold.Double)
            elif ty = typeof<System.Decimal> then moldRef := Mold.Primitive (pathOpt, Mold.Decimal)
            elif ty.IsEnum then moldRef := Mold.Primitive (pathOpt, Mold.Enum (ty.AssemblyQualifiedName, Enum.GetNames ty))
            elif ty = typeof<System.DateTime> then moldRef := Mold.Primitive (pathOpt, Mold.DateTime)
            elif ty = typeof<System.Net.IPAddress> then moldRef := Mold.Primitive (pathOpt, Mold.IPAddress)
            elif ty = typeof<System.Guid> then moldRef := Mold.Primitive (pathOpt, Mold.Guid)
            elif FSharpType.IsTuple ty then
                let name = (ty.GetGenericTypeDefinition ()).AssemblyQualifiedName
                let elementMolds =
                    FSharpType.GetTupleElements ty |>
                    Array.map (fun ty' -> typeToMold3 None ty' molder)
                moldRef := Mold.Tuple (pathOpt, name, elementMolds)
            elif FSharpType.IsRecord ty then
                let name = ty.AssemblyQualifiedName
                let fieldMolds =
                    FSharpType.GetRecordFields ty |>
                    Array.map (fun propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)) |>
                    Array.map (fun (name', ty') -> (name', typeToMold3 (Some { TypeName = name; MemberName = name' }) ty' molder))
                moldRef := Mold.Record (pathOpt, name, fieldMolds)
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
                moldRef := Mold.Union (pathOpt, name, caseMolds)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ option> then
                let containedType = ty.GenericTypeArguments.[0]
                let containedMold = typeToMold3 None containedType molder
                moldRef := Mold.Option (pathOpt, containedMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ list> then
                let elementType = ty.GenericTypeArguments.[0]
                let elementMold = typeToMold3 None elementType molder
                moldRef := Mold.List (pathOpt, elementMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ Set> then
                let elementType = ty.GenericTypeArguments.[0]
                let elementMold = typeToMold3 None elementType molder
                moldRef := Mold.List (pathOpt, elementMold)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<Map<_, _>> then
                let keyType = ty.GenericTypeArguments.[0]
                let keyMold = typeToMold3 None keyType molder
                let valueType = ty.GenericTypeArguments.[1]
                let valueMold = typeToMold3 None valueType molder
                moldRef := Mold.Map (pathOpt, keyMold, valueMold)
            elif ty.IsArray then
                let elementType = ty.GetElementType ()
                let elementMold = typeToMold3 None elementType molder
                moldRef := Mold.List (pathOpt, elementMold)
            else
                if molder.RequireIdentification
                then failwith "Could not identity a type for molding."
                else moldRef := Mold.Unidentified pathOpt
            moldRef.Value
        | (true, moldRef) -> moldRef.Value

    let private moldToPrimitive primitive (_ : Molder) =
        match primitive with
        | Mold.Unit -> typeof<FSharp.Core.Unit>
        | Mold.Boolean -> typeof<System.Boolean>
        | Mold.Byte -> typeof<System.Byte>
        | Mold.Char -> typeof<System.Char>
        | Mold.String -> typeof<System.String>
        | Mold.Int32 -> typeof<System.Int32>
        | Mold.Int64 -> typeof<System.Int64>
        | Mold.Single -> typeof<System.Single>
        | Mold.Double -> typeof<System.Double>
        | Mold.Decimal -> typeof<System.Decimal>
        | Mold.Enum (name, _) -> Type.GetType name
        | Mold.DateTime -> typeof<System.DateTime>
        | Mold.IPAddress -> typeof<System.Net.IPAddress>
        | Mold.Guid -> typeof<System.Guid>

    let typeToMold2 ty molder =
        typeToMold3 None ty molder

    let typeToMold<'t> molder =
        typeToMold3 None typeof<'t> molder

    let rec moldToType mold molder =
        match mold with
        | Mold.Primitive (_, primitive) -> moldToPrimitive primitive molder
        | Mold.Tuple (_, _, molds) -> let types = molds |> Array.map (fun mold' -> moldToType mold' molder) in FSharpType.MakeTupleType types
        | Mold.Record (_, name, _) -> Type.GetType name
        | Mold.Union (_, name, _) -> Type.GetType name
        | Mold.Option (_, mold) -> let containedType = moldToType mold molder in typedefof<_ option>.MakeGenericType containedType
        | Mold.List (_, mold) -> let elementType = moldToType mold molder in typedefof<_ list>.MakeGenericType elementType
        | Mold.Set (_, mold) -> let elementType = moldToType mold molder in typedefof<_ Set>.MakeGenericType elementType
        | Mold.Map (_, keyMold, valueMold) -> typedefof<Map<_, _>>.MakeGenericType [|moldToType keyMold molder; moldToType valueMold molder|]
        | Mold.Array (_, mold) -> let elementType = moldToType mold molder in elementType.MakeArrayType ()
        | Mold.Unidentified _ -> typeof<obj>

    let rec moldToInstance mold molder =
        let pathOpt = Mold.getMemberPathOpt mold
        match Map.tryFind pathOpt molder.InstantiationRules with
        | Some rule ->
            match rule with
            | Mold.Constant value -> value
            | Mold.Variable fn -> fn ()
        | None ->
            match mold with
            | Mold.Primitive (_, primitive) ->
                primitiveToInstance primitive molder
            | Mold.Tuple (_, name, elementMolds) ->
                let ty = Type.GetType name
                let elements = Array.map (fun ty' -> moldToInstance ty' molder) elementMolds
                FSharpValue.MakeTuple (elements, ty)
            | Mold.Record (_, name, fieldMolds) ->
                let ty = Type.GetType name
                let fields = Array.map (snd >> fun ty' -> moldToInstance ty' molder) fieldMolds
                FSharpValue.MakeRecord (ty, fields)
            | Mold.Union (_, name, caseMolds) ->
                let ty = Type.GetType name
                let caseInfos = FSharpType.GetUnionCases ty
                let caseInfo = caseInfos.[0]
                let (_, caseFields) = caseMolds.[0]
                let caseFieldInstances = Array.map (fun ty' -> moldToInstance ty' molder) caseFields
                FSharpValue.MakeUnion (caseInfo, caseFieldInstances)
            | Mold.Option (_, _) ->
                None :> obj
            | Mold.List (_, mold) ->
                let elementType = moldToType mold molder
                let ty = typedefof<_ list>.MakeGenericType [|elementType|]
                Reflection.objsToList ty []
            | Mold.Set (_, mold) ->
                let elementType = moldToType mold molder
                let ty = typedefof<_ Set>.MakeGenericType [|elementType|]
                Reflection.objsToSet ty []
            | Mold.Map (_, keyMold, valueMold) ->
                let keyType = moldToType keyMold molder
                let valueType = moldToType valueMold molder
                let ty = typedefof<Map<_, _>>.MakeGenericType [|keyType; valueType|]
                Reflection.pairsToMap ty []
            | Mold.Array (_, mold) ->
                let elementType = moldToType mold molder
                let ty = elementType.MakeArrayType ()
                Reflection.objsToArray ty []
            | Mold.Unidentified _ ->
                null :> obj

    let instanceToMold instance molder =
        let ty = instance.GetType ()
        typeToMold2 ty molder

    let typeToInstance2 (ty : Type) molder =
        let mold = typeToMold2 ty molder
        let instance = moldToInstance mold molder
        instance

    let typeToInstance<'t> molder =
        typeToInstance2 typeof<'t> molder :?> 't

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
