namespace Prime
open System
open FSharp.Reflection
open Prime

[<RequireQualifiedAccess>]
module Mold =

    let private rand = Random ()

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
        | DateTime
        | Guid

    type Mold =
        | Primitive of Primitive
        | Enum of string * string list
        | Tuple of string * Mold list
        | Record of string * (string * Mold) list
        | Union of string * (string * Mold list) list
        | Option of Mold
        | List of Mold
        | Set of Mold
        | Array of Mold
        | Unidentified

    let rec typeToMold (ty : Type) =
        if ty = typeof<FSharp.Core.Unit> then Primitive Unit
        elif ty = typeof<System.Boolean> then Primitive Boolean
        elif ty = typeof<System.Byte> then Primitive Byte
        elif ty = typeof<System.Char> then Primitive Char
        elif ty = typeof<System.String> then Primitive String
        elif ty = typeof<System.Int32> then Primitive Int32
        elif ty = typeof<System.Int64> then Primitive Int64
        elif ty = typeof<System.Single> then Primitive Single
        elif ty = typeof<System.Double> then Primitive Double
        elif ty = typeof<System.Decimal> then Primitive Decimal
        elif ty = typeof<System.DateTime> then Primitive DateTime
        elif ty = typeof<System.Guid> then Primitive Guid
        elif ty.IsEnum then Enum (ty.AssemblyQualifiedName, Array.toList (Enum.GetNames ty))
        elif FSharpType.IsTuple ty then
            let name = (ty.GetGenericTypeDefinition ()).AssemblyQualifiedName
            let elementMolds =
                FSharpType.GetTupleElements ty |>
                Array.map typeToMold |>
                Array.toList
            Tuple (name, elementMolds)
        elif FSharpType.IsRecord ty then
            let name = ty.AssemblyQualifiedName
            let fieldMolds =
                FSharpType.GetRecordFields ty |>
                Array.map (fun propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)) |>
                Array.map (fun (name, ty) -> (name, typeToMold ty)) |>
                Array.toList
            Record (name, fieldMolds)
        elif FSharpType.IsUnion ty then
            let name = ty.AssemblyQualifiedName
            let cases = FSharpType.GetUnionCases ty
            let caseMolds =
                cases |>
                Array.map (fun case ->
                    let fieldMolds = case.GetFields () |> Array.map (fun field -> typeToMold field.PropertyType) |> Array.toList
                    (case.Name, fieldMolds)) |>
                Array.toList
            Union (name, caseMolds)
        elif ty.GetGenericTypeDefinition () = typedefof<_ option> then
            let containedType = ty.GenericTypeArguments.[0]
            let containedMold = typeToMold containedType
            Option containedMold
        elif ty.GetGenericTypeDefinition () = typedefof<_ list> then
            let elementType = ty.GenericTypeArguments.[0]
            let elementMold = typeToMold elementType
            List elementMold
        elif ty.GetGenericTypeDefinition () = typedefof<_ Set> then
            let elementType = ty.GenericTypeArguments.[0]
            let elementMold = typeToMold elementType
            List elementMold
        elif ty.IsArray then
            let elementType = ty.GetElementType ()
            let elementMold = typeToMold elementType
            List elementMold
        else Unidentified

    let moldToPrimitive primitive =
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
        | DateTime -> typeof<System.DateTime>
        | Guid -> typeof<System.Guid>

    let rec moldToType mold =
        match mold with
        | Primitive primitive -> moldToPrimitive primitive
        | Enum (name, _) -> Type.GetType name
        | Tuple (_, molds) -> let types = molds |> List.map moldToType |> List.toArray in FSharpType.MakeTupleType types
        | Record (name, _) -> Type.GetType name
        | Union (name, _) -> Type.GetType name
        | Option mold -> let containedType = moldToType mold in typedefof<_ option>.MakeGenericType containedType
        | List mold -> let elementType = moldToType mold in typedefof<_ list>.MakeGenericType elementType
        | Set mold -> let elementType = moldToType mold in typedefof<_ Set>.MakeGenericType elementType
        | Array mold -> let elementType = moldToType mold in elementType.MakeArrayType ()
        | Unidentified -> typeof<obj>

    let primitiveToInstance randomize primitive =
        match primitive with
        | Unit -> () :> obj
        | Boolean -> if randomize then (if rand.Next () % 2 = 0 then false else true) :> obj else false :> obj
        | Byte -> if randomize then byte (rand.Next ()) :> obj else byte 0 :> obj
        | Char -> if randomize then char (rand.Next ()) :> obj else char 0 :> obj
        | String -> if randomize then string (Guid.NewGuid ()) :> obj else "" :> obj
        | Int32 -> if randomize then int (rand.Next ()) :> obj else int 0 :> obj
        | Int64 -> if randomize then int64 (rand.Next ()) + int64 (rand.Next () <<< 32) :> obj else int64 0 :> obj
        | Single -> if randomize then single (rand.NextDouble ()) :> obj else single 0 :> obj
        | Double -> if randomize then double (rand.NextDouble ()) :> obj else double 0 :> obj
        | Decimal -> if randomize then decimal (rand.NextDouble ()) :> obj else decimal 0 :> obj
        | DateTime -> if randomize then DateTime.UtcNow :> obj else DateTime.MinValue :> obj
        | Guid -> if randomize then Guid.NewGuid () :> obj else Guid.Empty :> obj

    let rec moldToInstance randomize mold =
        match mold with
        | Primitive primitive ->
            primitiveToInstance randomize primitive
        | Enum (name, cases) ->
            let ty = Type.GetType name
            if randomize
            then Enum.Parse (ty, cases.[rand.Next cases.Length])
            else Activator.CreateInstance ty
        | Tuple (name, elementMolds) ->
            let ty = Type.GetType name
            let elements = List.map (moldToInstance randomize) elementMolds |> List.toArray
            FSharpValue.MakeTuple (elements, ty)
        | Record (name, fieldMolds) ->
            let ty = Type.GetType name
            let fields = List.map (snd >> moldToInstance randomize) fieldMolds |> List.toArray
            FSharpValue.MakeRecord (ty, fields)
        | Union (name, caseMolds) ->
            let ty = Type.GetType name
            let caseInfos = FSharpType.GetUnionCases ty
            let caseInfo = caseInfos.[0]
            let (_, caseFields) = caseMolds.[0]
            let caseFieldInstances = List.map (moldToInstance randomize) caseFields |> List.toArray
            FSharpValue.MakeUnion (caseInfo, caseFieldInstances)
        | Option mold ->
            let containedType = moldToType mold
            let ty = typedefof<_ option>.MakeGenericType [|containedType|]
            if randomize then
                if rand.Next 2 = 0
                then None :> obj
                else Activator.CreateInstance (ty, [|moldToInstance randomize mold|])
            else None :> obj
        | List mold ->
            let elementType = moldToType mold
            let ty = typedefof<_ list>.MakeGenericType [|elementType|]
            Reflection.objsToList ty []
        | Set mold ->
            let elementType = moldToType mold
            let ty = typedefof<_ Set>.MakeGenericType [|elementType|]
            Reflection.objsToSet ty []
        | Array mold ->
            let elementType = moldToType mold
            let ty = elementType.MakeArrayType ()
            Reflection.objsToArray ty []
        | Unidentified ->
            null :> obj

    let instanceToMold instance =
        let ty = instance.GetType ()
        typeToMold ty

    let typeToInstance randomize (ty : Type) =
        let mold = typeToMold ty
        let instance = moldToInstance randomize mold
        instance

    let instanceToType instance =
        let mold = instanceToMold instance
        let ty = moldToType mold
        ty

type Primitive = Mold.Primitive

type Mold = Mold.Mold
