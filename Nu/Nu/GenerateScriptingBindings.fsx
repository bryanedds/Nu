// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

#I __SOURCE_DIRECTORY__
#load "Interactive.fsx"
open System
open System.IO
open System.Reflection
open Prime
open global.Nu

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

type FunctionBinding =
    { FunctionName : string
      FunctionParameters : (string * ParameterConversion * Type) array
      FunctionReturn : ReturnConversion * Type }

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
    let pars = method.GetParameters ()
    let parTypes = Array.map getType pars
    let parNames = Array.map (fun (par : ParameterInfo) -> par.Name) pars
    let conversionOpts = Array.mapi (tryGetParameterConversion pars.Length) parTypes
    match Array.definitizePlus conversionOpts with
    | (true, conversions) ->
        let returnType = method.ReturnType
        match tryGetReturnConversion 0 returnType with
        | Some returnConversion ->
            Some
                { FunctionName = method.Name.Replace("World.", "").Replace(".Static", "")
                  FunctionParameters = Array.zip3 parNames conversions parTypes
                  FunctionReturn = (returnConversion, returnType) }
        | None -> None
    | (false, _) -> None

let generateParameterList functionParameters =
    let parNames = Array.map Triple.fst functionParameters : string array
    String.Join (" ", parNames)

//let tryGenerateParameterConversion

let tryGenerateBindingCode binding =
    let header =
        "   let " + binding.FunctionName + " " + generateParameterList binding.FunctionParameters + " =\n" +
        "       world\n"
    Some header

let tryGenerateBindingsCode bindings =
    let bindingCodes =
        bindings |>
        Array.map tryGenerateBindingCode |>
        Array.definitize |> // TODO: error output
        fun strs -> String.Join ("\n", strs)
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
        "open global.Nu\n" +
        "#nowarn \"1182\"\n" +
        "\n" +
        "module WorldScriptingBindings =\n" +
        "\n" +
        bindingCodes
    Some header

let types =
    AppDomain.CurrentDomain.GetAssemblies () |>
    Array.filter (fun asm -> (asm.GetName ()).Name = "Nu") |>
    Array.head |>
    fun asm -> asm.GetTypes () |> Array.filter (fun ty -> isNotNull (ty.GetCustomAttribute<ModuleBindingAttribute> ()))

let bindings =
    types |>
    Array.map (fun (ty : Type) -> ty.GetMethods ()) |>
    Array.concat |>
    Array.filter (fun mi -> isNotNull (mi. GetCustomAttribute<FunctionBindingAttribute> ())) |>
    Array.map tryGenerateBinding |>
    Array.definitize // TODO: error output

let code =
    tryGenerateBindingsCode bindings |>
    Option.get |> // TODO: error output
    fun code -> File.WriteAllText ("../../ScriptingBindings.fs", code)