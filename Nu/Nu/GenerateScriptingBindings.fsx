// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

#I __SOURCE_DIRECTORY__
#load "Interactive.fsx"
open System
open System.Reflection
open Prime
open Nu

type ParameterConversion =
    | WorldParameter
    | NormalParameterConversion
    | RelationToEntity
    | RelationToLayer
    | RelationToScreen
    | RelationToSimulant
    | LambdaToTasklet
    | ListToSeq of ParameterConversion

type ReturnConversion =
    | WorldReturn
    | TupleReturn of ReturnConversion array
    | NormalReturnConversion
    | EntityToRelation
    | LayerToRelation
    | ScreenToRelation
    | SimulantToRelation
    | SeqToList of ReturnConversion

type MethodBinding =
    { MethodName : string
      MethodParameters : (ParameterConversion * Type) array
      MethodReturn : ReturnConversion * Type }

let modules =
    [|WorldModule.ModuleType
      WorldModuleGame.ModuleType
      WorldModuleScreen.ModuleType
      WorldModuleLayer.ModuleType
      WorldModuleEntity.ModuleType
      WorldEntityModule.ModuleType
      WorldLayerModule.ModuleType
      WorldScreenModule.ModuleType
      WorldGameModule.ModuleType
      WorldSimulantModule.ModuleType
      WorldInputModule.ModuleType
      WorldPhysicsModule.ModuleType
      WorldRenderModule.ModuleType
      WorldAudioModule.ModuleType
      WorldModule2.ModuleType|]

let methods =
    modules |>
    Array.map (fun (ty : Type) -> ty.GetMethods ()) |>
    Array.concat |>
    Array.filter (fun (mi : MethodInfo) -> mi.IsPublic) |>
    Array.filter (fun (mi : MethodInfo) -> not mi.IsGenericMethod)

let tryGetParameterConversion parCount parIndex (ty : Type) =
    match ty.Name with
    | "World" ->
        // no engine function that deals with multiple worlds should be exposed
        if parIndex = parCount - 1
        then Some WorldParameter
        else None
    | "Entity" -> Some RelationToEntity
    | "Layer" -> Some RelationToLayer
    | "Screen" -> Some RelationToScreen
    | "Game" -> None // no engine function should deal with the Game type directly
    | "Simulant" -> Some RelationToSimulant
    | _ -> Some NormalParameterConversion

let rec tryGetReturnConversion level (ty : Type) : ReturnConversion option =
    match ty.Name with
    | "World" ->
        if level = 0
        then Some WorldReturn
        else None // no engine function that deals with multiple worlds should be exposed
    | "Tuple" ->
        if level = 0 then
            let gargs = ty.GetGenericArguments ()
            let gargLast = Array.last gargs
            if gargLast.Name = "World" then
                let subconversionOpts = Array.map (tryGetReturnConversion (inc level)) gargs
                match Array.definitizePlus subconversionOpts with
                | (true, subconversions) -> Some (TupleReturn subconversions)
                | (false, _) -> None
            else None // no engine function that deals with multiple worlds should be exposed
        else
            let gargs = ty.GetGenericArguments ()
            let subconversionOpts = Array.map (tryGetReturnConversion (inc level)) gargs
            match Array.definitizePlus subconversionOpts with
            | (true, subconversions) -> Some (TupleReturn subconversions)
            | (false, _) -> None
    | "Address<Entity>" -> Some EntityToRelation
    | "Address<Layer>" -> Some LayerToRelation
    | "Address<Screen>" -> Some ScreenToRelation
    | "Address<Game>" -> None // no engine function should deal with the Game type directly
    | "Address<Simulant>" -> Some SimulantToRelation
    | _ -> Some NormalReturnConversion

let tryGenerateBinding (method : MethodInfo) =
    let pars = Array.map getType (method.GetParameters ())
    let conversionOpts = Array.mapi (tryGetParameterConversion pars.Length) pars
    match Array.definitizePlus conversionOpts with
    | (true, conversions) ->
        let returnType = method.ReturnType
        match tryGetReturnConversion 0 returnType with
        | Some returnConversion ->
            Some
                { MethodName = method.Name
                  MethodParameters = Array.zip conversions pars
                  MethodReturn = (returnConversion, returnType) }
        | None -> None
    | (false, _) -> None

let tryGenerateBindingCode binding =
    let header =
        "   let " + binding.MethodName + " =\n" +
        "\n"
    Some header

let tryGenerateBindingsCode bindings =

    let header =
        "// Nu Game Engine.\n" +
        "// Copyright (C) Bryan Edds, 2013-2017.\n" +
        "\n" +
        "namespace Nu\n" +
        "open System\n" +
        "open System.Collections.Generic\n" +
        "open OpenTK\n" +
        "open Prime\n" +
        "open Prime.Scripting\n" +
        "open Nu\n" +
        "\n" +
        "module WorldScriptingBindings =\n" +
        "\n" +
        ""

    Some header

Array.map (fun (mi : MethodInfo) -> mi.Name) methods