// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

#I __SOURCE_DIRECTORY__
#load "Interactive.fsx"
open System
open System.IO
open System.Reflection
open Prime
open global.Nu

type ParameterConversionDetails =
    | NormalParameter of Type
    | RelationToEntity
    | RelationToLayer
    | RelationToScreen
    | RelationToGame
    | RelationToSimulant
    | ListToSeq of Type

type ParameterConversion =
    | ValueParameter of ParameterConversionDetails
    | WorldParameter

type ReturnConversionDetails =
    | NormalReturn of Type
    | SimulantToRelation
    | SeqToList of Type

type ReturnConversion =
    | PureReturn of ReturnConversionDetails
    | MixedReturn of ReturnConversionDetails
    | ImpureReturn

type FunctionBinding =
    { FunctionName : string
      FunctionParameters : (string * ParameterConversion) array
      FunctionReturn : ReturnConversion }

let tryGetParameterConversion parCount parIndex (ty : Type) =
    match ty.Name with
    | "World" ->
        // no engine function that deals with multiple worlds should be exposed
        if parIndex = parCount - 1
        then Some WorldParameter
        else None
    | "Entity" -> Some (ValueParameter RelationToEntity)
    | "Layer" -> Some (ValueParameter RelationToLayer)
    | "Screen" -> Some (ValueParameter RelationToScreen)
    | "Game" -> Some (ValueParameter RelationToGame)
    | "Simulant" -> Some (ValueParameter RelationToSimulant)
    | _ -> Some (ValueParameter (NormalParameter ty))

let rec tryGetReturnConversion (ty : Type) : ReturnConversion option =
    match ty.GetGenericName () with
    | "World" -> Some ImpureReturn
    | "Tuple`2" ->
        let gargs = ty.GetGenericArguments ()
        match gargs with
        | [|garg; garg2|] when garg2.Name = "World" ->
            match
                (match garg.GetGenericName () with
                 | "Entity" | "Layer" | "Screen" | "Game" | "Simulant" -> Some SimulantToRelation
                 | _ -> Some (NormalReturn garg)) with
            | Some conversion ->
                let subconversionOpts = Array.map tryGetReturnConversion gargs
                match Array.definitizePlus subconversionOpts with
                | (true, subconversions) ->
                    match (tryGetReturnConversion garg, tryGetReturnConversion garg2) with
                    | (Some rc, Some rc2) -> Some (MixedReturn conversion)
                    | (_, _) -> None
                | (false, _) -> None
            | None -> None
        | _ -> None
    | "Entity" | "Layer" | "Screen" | "Game" | "Simulant" -> Some (PureReturn SimulantToRelation)
    | _ -> Some (PureReturn (NormalReturn ty))

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
                  FunctionParameters = Array.zip parNames conversions
                  FunctionReturn = returnConversion }
        | None -> None
    | (false, _) -> None

let generateParameterList functionParameters =
    let parNames = Array.map fst functionParameters : string array
    String.Join (" ", parNames)

let rec tryGenerateParameterConversion (par : string) conversion =
    match conversion with
    | ValueParameter rc ->
        match rc with
        | NormalParameter ty ->
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
        | RelationToGame ->
            Some
                ("            let struct (" + par + ", world) =\n" +
                 "                let context = World.getScriptContext world\n" +
                 "                match World.evalInternal " + par + " world with\n" +
                 "                | struct (Scripting.String str, world)\n" +
                 "                | struct (Scripting.Keyword str, world) ->\n" +
                 "                    let relation = Relation.makeFromString str\n" +
                 "                    let address = Relation.resolve context.SimulantAddress relation\n" +
                 "                    struct (Game address, world)\n" +
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
    | WorldParameter ->
        None

let generateBindingFunction binding =
    let functionAndExceptionHeader =
        "    let " + binding.FunctionName + " " + generateParameterList binding.FunctionParameters + " =\n" +
        "        let oldWorld = world\n" +
        "        try\n"
    let conversions =
        Array.map (fun (par, conversion) -> tryGenerateParameterConversion par conversion) binding.FunctionParameters |>
        Array.definitize |> // TODO: error output
        fun conversions -> String.Join ("", conversions)
    let returnConversion =
        (match binding.FunctionReturn with
         | ImpureReturn ->
            Some "            struct (Scripting.Unit, result)\n"
         | MixedReturn rc ->
            match rc with
            | NormalReturn ty ->
                Some
                    ("            let (value, world) = result\n" +
                     "            let value = ScriptingWorld.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                     "            struct (value, world)\n")
            | SimulantToRelation ->
                Some
                    ("            let (value, world) = result\n" +
                     "            let value = Scripting.String (scstring value)\n" +
                     "            struct (value, world)\n")
            | SeqToList _ -> None
         | PureReturn rc ->
            match rc with
            | NormalReturn ty ->
                Some
                    ("            let value = result\n" +
                     "            let value = ScriptingWorld.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                     "            struct (value, world)\n")
            | SimulantToRelation ->
                Some
                    ("            let value = result\n" +
                     "            let value = Scripting.String (scstring value)\n" +
                     "            struct (value, world)\n")
            | SeqToList _ -> None) |>
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
    let args = binding.FunctionParameters |> Array.allButLast |> Array.map fst
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