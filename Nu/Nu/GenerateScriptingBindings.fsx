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
    | ListToSeq

type ReturnConversion =
    | WorldReturn
    | SingleReturnConversion
    | PairReturnConversion
    | EntityToRelation
    | LayerToRelation
    | ScreenToRelation
    | SimulantToRelation
    | SeqToList

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

let rec tryGetReturnConversion (ty : Type) : ReturnConversion option =
    match ty.GetGenericName () with
    | "World" -> Some WorldReturn
    | "Tuple`2" ->
        let gargs = ty.GetGenericArguments ()
        match gargs with
        | [|garg0; garg1|] when garg1.Name = "World" ->
            let subconversionOpts = Array.map tryGetReturnConversion gargs
            match Array.definitizePlus subconversionOpts with
            | (true, subconversions) -> Some PairReturnConversion
            | (false, _) -> None
        | _ -> None
    | "Address<Entity>" -> Some EntityToRelation
    | "Address<Layer>" -> Some LayerToRelation
    | "Address<Screen>" -> Some ScreenToRelation
    | "Address<Game>" -> None // no engine function should deal with the Game type directly
    | "Address<Simulant>" -> Some SimulantToRelation
    | _ -> Some SingleReturnConversion

let tryGenerateBinding (method : MethodInfo) =
    let pars = method.GetParameters ()
    let parTypes = Array.map (fun (pi : ParameterInfo) -> pi.ParameterType) pars
    let parNames = Array.map (fun (par : ParameterInfo) -> par.Name) pars
    let conversionOpts = Array.mapi (tryGetParameterConversion pars.Length) parTypes
    match Array.definitizePlus conversionOpts with
    | (true, conversions) ->
        let returnType = method.ReturnType
        match tryGetReturnConversion returnType with
        | Some returnConversion ->
            Some
                { FunctionName = method.Name.Replace(".Static", "").Replace("World.", "")
                  FunctionParameters = Array.zip3 parNames conversions parTypes
                  FunctionReturn = (returnConversion, returnType) }
        | None -> None
    | (false, _) -> None

let generateParameterList functionParameters =
    let parNames = Array.map Triple.fst functionParameters : string array
    String.Join (" ", parNames)

let tryGenerateParameterConversion (par : string) conversion (ty : Type) =
    match conversion with
    | WorldParameter ->
        None
    | NormalParameterConversion ->
        Some
            ("            let " + par + " = ScriptingWorld.tryExport (" + par + ".GetType ()) " + par + " world |> Option.get :?> " + ty.GetGenericName () + "\n")
    | RelationToEntity ->
        Some
            ("            let struct (" + par + ", world) =\n" +
             "                let context = World.getScriptContext world\n" +
             "                match World.evalInternal " + par + " world with\n" +
             "                | struct (Scripting.String str, world)\n" +
             "                | struct (Scripting.Keyword str, world) ->\n" +
             "                    let relation = Relation.makeFromString str\n" +
             "                    let address = Relation.resolve context.SimulantAddress relation\n" +
             "                    struct (Entity address, world)\n" +
             "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
             "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n")
    | RelationToLayer ->
        Some
            ("            let struct (" + par + ", world) =\n" +
             "                let context = World.getScriptContext world\n" +
             "                match World.evalInternal " + par + " world with\n" +
             "                | struct (Scripting.String str, world)\n" +
             "                | struct (Scripting.Keyword str, world) ->\n" +
             "                    let relation = Relation.makeFromString str\n" +
             "                    let address = Relation.resolve context.SimulantAddress relation\n" +
             "                    struct (Layer address, world)\n" +
             "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
             "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n")
    | RelationToScreen ->
        Some
            ("            let struct (" + par + ", world) =\n" +
             "                let context = World.getScriptContext world\n" +
             "                match World.evalInternal " + par + " world with\n" +
             "                | struct (Scripting.String str, world)\n" +
             "                | struct (Scripting.Keyword str, world) ->\n" +
             "                    let relation = Relation.makeFromString str\n" +
             "                    let address = Relation.resolve context.SimulantAddress relation\n" +
             "                    struct (Screen address, world)\n" +
             "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
             "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n")
    | RelationToSimulant ->
        Some
            ("            let struct (" + par + ", world) =\n" +
             "                let context = World.getScriptContext world\n" +
             "                match World.evalInternal " + par + " world with\n" +
             "                | struct (Scripting.String str, world)\n" +
             "                | struct (Scripting.Keyword str, world) ->\n" +
             "                    let relation = Relation.makeFromString str\n" +
             "                    let address = Relation.resolve context.SimulantAddress relation\n" +
             "                    struct (World.deriveSimulant address, world)\n" +
             "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
             "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n")
    | ListToSeq _ ->
        None

let generateBindingFunction binding =
    let functionAndExceptionHeader =
        "    let " + binding.FunctionName + " " + generateParameterList binding.FunctionParameters + " =\n" +
        "        let oldWorld = world\n" +
        "        try\n"
    let conversions =
        Array.map (fun (par, conversion, ty) -> tryGenerateParameterConversion par conversion ty) binding.FunctionParameters |>
        Array.definitize |> // TODO: error output
        fun conversions -> String.Join ("", conversions)
    let returnConversion =
        let (returnConversion, returnType) = binding.FunctionReturn
        (match returnConversion with
         | WorldReturn ->
            Some "            struct (Scripting.Unit, result)\n"
         | SingleReturnConversion ->
            Some
                ("            let value = result\n" +
                 "            let value = ScriptingWorld.tryImport typeof<" + returnType.GetGenericName() + "> value world |> Option.get\n" +
                 "            struct (value, world)\n")
         | PairReturnConversion ->
            Some
                ("            let (value, world) = result\n" +
                 "            let value = ScriptingWorld.tryImport typeof<" + returnType.GetGenericArguments().[0].GetGenericName() + "> value world |> Option.get\n" +
                 "            struct (value, world)\n")
         | EntityToRelation ->
            None
         | LayerToRelation ->
            None
         | ScreenToRelation ->
            None
         | SimulantToRelation ->
            None
         | SeqToList _ ->
           None) |>
        Option.get // TODO: error output
    let invocation =
        "            let result = World." + binding.FunctionName + " " + generateParameterList binding.FunctionParameters + "\n"
    let exceptionHandler =
        "        with exn ->\n" +
        "            let violation = Scripting.Violation ([\"InvalidBindingInvocation\"], \"Could not invoke binding '" + binding.FunctionName + "' due to: \" + scstring exn, None)\n" +
        "            struct (violation, World.choose oldWorld)\n"
    functionAndExceptionHeader +
    conversions +
    invocation +
    returnConversion +
    exceptionHandler

let generateBindingMatcher binding =
    let args = binding.FunctionParameters |> Array.allButLast |> Array.map Triple.fst
    let argArray = "[|" + String.Join ("; ", args) + "|]"
    "        | \"" + binding.FunctionName + "\" ->\n" +
    "            match World.evalManyInternal exprs world with\n" +
    "            | struct (" + argArray + " as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->\n" +
    "                " + binding.FunctionName + " " + String.Join (" ", args) + " world\n" +
    "            | _ ->\n" +
    "                let violation = Scripting.Violation ([\"InvalidBindingInvocation\"], \"Incorrect number of arguments for binding '" + binding.FunctionName + "' at:\\n\" + SymbolOrigin.tryPrint originOpt, None)\n" +
    "                struct (violation, world)\n"

let generateBindingsMatcher bindings =
    let bindingMatchers =
        bindings |>
        Array.map generateBindingMatcher |>
        fun bindings -> String.Join ("", bindings)
    let matcher =
        "    let bindings fnName exprs originOpt world =\n" +
        "        match fnName with\n" +
        bindingMatchers +
        "        | _ ->\n" +
        "            let violation = Scripting.Violation ([\"InvalidBindingInvocation\"], \"No binding exists for '\" + " + "fnName" + " + \"' at:\\n\" + SymbolOrigin.tryPrint originOpt, None)\n" +
        "            struct (violation, world)\n"
    matcher

let generateBindingsCode bindings =
    let header =
        "// Nu Game Engine.\n" +
        "// Copyright (C) Bryan Edds, 2013-2017.\n" +
        "\n" +
        "namespace Nu\n" +
        "open System\n" +
        "open System.Collections.Generic\n" +
        "open OpenTK\n" +
        "open Prime\n" +
        "open global.Nu\n" +
        "\n" +
        "[<RequireQualifiedAccess>]\n" +
        "module WorldScriptingBindings =\n" +
        "\n"
    let bindingCodes =
        bindings |>
        Array.map generateBindingFunction |>
        fun strs -> String.Join ("\n", strs) + "\n"
    let bindingsMatcher =
        generateBindingsMatcher bindings
    header +
    bindingCodes +
    bindingsMatcher

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
    generateBindingsCode bindings |>
    fun code -> File.WriteAllText ("../../ScriptingBindings.fs", code)