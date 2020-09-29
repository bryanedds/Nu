// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#load "Interactive.fsx"
open System
open System.IO
open System.Reflection
open Prime
open Nu

type ParameterConversionDetails =
    | NormalParameter of Type
    | NormalSeqParameter of Type
    | RelationParameter of Type
    | RelationSeqParameter of Type
    | AddressParameter of Type
    | AddressSeqParameter of Type

type ParameterConversion =
    | ValueParameter of ParameterConversionDetails
    | WorldParameter

type ReturnConversionDetails =
    | NormalReturn of Type
    | NormalListReturn of Type
    | SimulantReturn
    | SimulantHashSetReturn

type ReturnConversion =
    | PureReturn of ReturnConversionDetails
    | MixedReturn of ReturnConversionDetails
    | ImpureReturn

type FunctionBinding =
    { FunctionName : string
      FunctionBindingName : string
      FunctionParameters : (string * ParameterConversion) array
      FunctionReturn : ReturnConversion }

let getExtrinsicKeywords () =
    "v2 v4 v2i v4i color get getAsStream set setAsStream update streamEvent stream bind " +
    "self parent grandparent game toData monitor"

let getParameterConversion (ty : Type) =
    match ty.Name with
    | "World" -> WorldParameter
    | "Entity" | "Layer" | "Screen" | "Game" | "Simulant" -> ValueParameter (RelationParameter ty)
    | "Address`1" -> ValueParameter (AddressParameter ty)
    | "IEnumerable`1" | "FSharpList`1" ->
        let itemType = (ty.GetGenericArguments ()).[0]
        if not (typeof<Simulant>.IsAssignableFrom itemType) then ValueParameter (NormalSeqParameter ty)
        elif itemType.Name = "Address`1" then ValueParameter (AddressSeqParameter ty)
        else ValueParameter (RelationSeqParameter ty)
    | _ -> ValueParameter (NormalParameter ty)

let rec tryGetReturnConversion (ty : Type) : ReturnConversion option =
    match ty.Name with
    | "World" -> Some ImpureReturn
    | "Tuple`2" ->
        let gargs = ty.GetGenericArguments ()
        match gargs with
        | [|garg; garg2|] when garg2.Name = "World" ->
            match
                (match garg.Name with
                 | "Entity" | "Layer" | "Screen" | "Game" | "Simulant" -> Some SimulantReturn
                 | "IEnumerable`1" | "FSharpList`1"| "HashSet`1"  ->
                     let itemType = (garg.GetGenericArguments ()).[0]
                     if typeof<Simulant>.IsAssignableFrom itemType
                     then Some SimulantHashSetReturn
                     else Some (NormalListReturn garg)
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
    | "IEnumerable`1" | "FSharpList`1" | "HashSet`1" ->
        let itemType = (ty.GetGenericArguments ()).[0]
        if not (typeof<Simulant>.IsAssignableFrom itemType)
        then Some (PureReturn (NormalListReturn ty))
        else Some (PureReturn SimulantHashSetReturn)
    | "Entity" | "Layer" | "Screen" | "Game" | "Simulant" -> Some (PureReturn SimulantReturn)
    | _ -> Some (PureReturn (NormalReturn ty))

let tryGenerateBinding (method : MethodInfo) =
    let functionName = method.Name.Replace(".Static", "").Replace("World.", "")
    let functionBindingName =
        match (method.GetCustomAttribute<FunctionBindingAttribute> ()).BindingName with
        | "" -> functionName
        | bindingName -> bindingName
    let pars = method.GetParameters ()
    let parTypes = Array.map (fun (pi : ParameterInfo) -> pi.ParameterType) pars
    let parNames = Array.map (fun (par : ParameterInfo) -> par.Name) pars
    let conversions = Array.map getParameterConversion parTypes
    let returnType = method.ReturnType
    match tryGetReturnConversion returnType with
    | Some returnConversion ->
        Some
            { FunctionName = functionName
              FunctionBindingName = functionBindingName
              FunctionParameters = Array.zip parNames conversions
              FunctionReturn = returnConversion }
    | None -> None

let generateParameterList (functionParameters : (string * ParameterConversion) array) =
    let parNames = Array.map fst functionParameters
    String.Join (" ", parNames)

let generateNormalParameterConversion (par : string) (ty : Type) =
    "            let " + par + " =\n" +
    "                match ScriptingSystem.tryExport typeof<" + ty.GetGenericName () + "> " + par + " world with\n" +
    "                | Some value -> value :?> " + ty.GetGenericName () + "\n" +
    "                | None -> failwith \"Invalid argument type for '" + par + "'; expecting a value convertable to " + ty.Name + ".\"\n"

let generateNormalListParameterConversion (par : string) (ty : Type) =
    "            let struct (" + par + ", world) =\n" +
    "                match World.evalInternal " + par + " world with\n" +
    "                | struct (Scripting.List list, world) ->\n" +
    "                    Seq.fold (fun struct (values, world) value ->\n" +
    "                        let value =\n" +
    "                            match ScriptingSystem.tryExport typeof<" + ty.GetGenericName () + "> value world with\n" +
    "                            | Some value -> value :?> " + ty.GetGenericName () + "\n" +
    "                            | None -> failwith \"Invalid argument type for '" + par + "'; expecting a value convertable to " + ty.Name + ".\"\n" +
    "                        struct (value :: values, world))\n" +
    "                        struct ([], world)\n" +
    "                        list\n" +
    "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n"

let generateRelationParameterConversion (par : string) (ty : Type) =
    let addressToSimulant = if ty.Name = "Simulant" then "World.derive" else ty.Name
    "            let struct (" + par + ", world) =\n" +
    "                let context = World.getScriptContext world\n" +
    "                match World.evalInternal " + par + " world with\n" +
    "                | struct (Scripting.String str, world)\n" +
    "                | struct (Scripting.Keyword str, world) ->\n" +
    "                    let relation = Relation.makeFromString str\n" +
    "                    let address = Relation.resolve context.SimulantAddress relation\n" +
    "                    struct (" + addressToSimulant + " address, world)\n" +
    "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n"

let generateRelationListParameterConversion (par : string) (ty : Type) =
    let addressToSimulant = if ty.Name = "Simulant" then "World.derive" else ty.Name
    "            let struct (" + par + ", world) =\n" +
    "                let context = World.getScriptContext world\n" +
    "                match World.evalInternal " + par + " world with\n" +
    "                | struct (Scripting.List simulants, world) ->\n" +
    "                    List.fold (fun struct (simulants, world) simulant ->\n" +
    "                        match simulant with\n" +
    "                        | Scripting.String str\n" +
    "                        | Scripting.Keyword str ->\n" +
    "                            let relation = Relation.makeFromString str\n" +
    "                            let address = Relation.resolve context.SimulantAddress relation\n" +
    "                            struct (" + addressToSimulant + " address :: simulants, world)\n" +
    "                        | Scripting.Violation (_, error, _) -> failwith error\n" +
    "                        | _ -> failwith \"Relation must be either a String or Keyword.\")\n" +
    "                        struct ([], world)\n" +
    "                        simulants\n" +
    "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                | struct (_, _) -> failwith \"Expecting a list of relations.\"\n"

let generateAddressParameterConversion (par : string) (ty : Type) =
    "            let struct (" + par + ", world) =\n" +
    "                let context = World.getScriptContext world\n" +
    "                match World.evalInternal " + par + " world with\n" +
    "                | struct (Scripting.String str, world)\n" +
    "                | struct (Scripting.Keyword str, world) ->\n" +
    "                    let relation = Relation.makeFromString str\n" +
    "                    let address = Relation.resolve context.SimulantAddress relation\n" +
    "                    struct (address, world)\n" +
    "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                | struct (_, _) -> failwith \"Address must be either a String or Keyword.\"\n"

let generateAddressListParameterConversion (par : string) (ty : Type) =
    "            let struct (" + par + ", world) =\n" +
    "                match World.evalInternal " + par + " world with\n" +
    "                | struct (Scripting.List list, world) ->\n" +
    "                    Seq.fold (fun struct (values, world) value ->\n" +
    "                       let struct (value, world) =\n" +
    "                           let context = World.getScriptContext world\n" +
    "                           match World.evalInternal " + par + " world with\n" +
    "                           | struct (Scripting.String str, world)\n" +
    "                           | struct (Scripting.Keyword str, world) ->\n" +
    "                               let relation = Relation.makeFromString str\n" +
    "                               let address = Relation.resolve context.SimulantAddress relation\n" +
    "                               struct (address, world)\n" +
    "                           | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                           | struct (_, _) -> failwith \"Address must be either a String or Keyword.\"\n" +
    "                        struct (value :: values, world))\n" +
    "                        struct ([], world)\n" +
    "                        list\n" +
    "                | struct (Scripting.Violation (_, error, _), _) -> failwith error\n" +
    "                | struct (_, _) -> failwith \"Relation must be either a String or Keyword.\"\n"

let rec generateParameterConversionOpt (par : string) conversion =
    match conversion with
    | ValueParameter rc ->
        match rc with
        | NormalParameter ty -> Some (generateNormalParameterConversion par ty)
        | NormalSeqParameter ty -> Some (generateNormalListParameterConversion par (ty.GetGenericArguments ()).[0])
        | RelationParameter ty -> Some (generateRelationParameterConversion par ty)
        | RelationSeqParameter ty -> Some (generateRelationListParameterConversion par (ty.GetGenericArguments ()).[0])
        | AddressParameter ty -> Some (generateAddressParameterConversion par ty)
        | AddressSeqParameter ty -> Some (generateAddressListParameterConversion par (ty.GetGenericArguments ()).[0])
    | WorldParameter -> None

let generateBindingFunction binding =
    
    let functionAndExceptionHeader =
        "    let " + binding.FunctionBindingName + " " + generateParameterList binding.FunctionParameters + " =\n" +
        "        let oldWorld = world\n" +
        "        try\n"
    
    let conversions =
        Array.map (fun (par, conversion) -> generateParameterConversionOpt par conversion) binding.FunctionParameters |>
        Array.definitize |>
        fun conversions -> String.Join ("", conversions)
    
    let returnConversion =
        match binding.FunctionReturn with
        | ImpureReturn -> "            struct (Scripting.Unit, result)\n"
        | MixedReturn rc ->
            match rc with
            | NormalReturn ty ->
                "            let (value, world) = result\n" +
                "            let value = ScriptingSystem.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                "            struct (value, world)\n"
            | NormalListReturn ty ->
                "            let (value, world) = result\n" +
                "            let value = ScriptingSystem.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                "            struct (value, world)\n"
            | SimulantReturn ->
                "            let (value, world) = result\n" +
                "            let value = let str = scstring value in if Symbol.shouldBeExplicit str then Scripting.String str else Scripting.Keyword str\n" +
                "            struct (value, world)\n"
            | SimulantHashSetReturn ->
                "            let (value, world) = result\n" +
                "            let value = Scripting.Ring (Set.ofSeq (Seq.map (fun value -> let str = scstring value in if Symbol.shouldBeExplicit str then Scripting.String str else Scripting.Keyword str) value))\n" +
                "            struct (value, world)\n"
        | PureReturn rc ->
            match rc with
            | NormalReturn ty ->
                "            let value = result\n" +
                "            let value = ScriptingSystem.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                "            struct (value, world)\n"
            | NormalListReturn ty ->
                "            let value = result\n" +
                "            let value = ScriptingSystem.tryImport typeof<" + ty.GetGenericName () + "> value world |> Option.get\n" +
                "            struct (value, world)\n"
            | SimulantReturn ->
                "            let value = result\n" +
                "            let value = let str = scstring value in if Symbol.shouldBeExplicit str then Scripting.String str else Scripting.Keyword str\n" +
                "            struct (value, world)\n"
            | SimulantHashSetReturn ->
                "            let value = result\n" +
                "            let value = Scripting.Ring (Set.ofSeq (Seq.map (fun value -> let str = scstring value in if Symbol.shouldBeExplicit str then Scripting.String str else Scripting.Keyword str) value))\n" +
                "            struct (value, world)\n"
    
    let invocation =
        "            let result = World." + binding.FunctionName + " " + generateParameterList binding.FunctionParameters + "\n"
    
    let exceptionHandler =
        "        with exn ->\n" +
        "            let violation = Scripting.Violation ([\"InvalidBindingInvocation\"], \"Could not invoke binding '" + binding.FunctionBindingName + "' due to: \" + scstring exn, None)\n" +
        "            struct (violation, World.choose oldWorld)\n"
    
    functionAndExceptionHeader +
    conversions +
    invocation +
    returnConversion +
    exceptionHandler

let generateBindingFunction' binding =
    
    let args =
        binding.FunctionParameters |>
        Array.filter (function (_, WorldParameter) -> false | _ -> true) |>
        Array.map fst

    let argsStr =
        if args.Length <> 0 then String.Join (" ", args) + " " else ""
    
    let argArray = "[|" + String.Join ("; ", args) + "|]"
    
    "    let eval" + String.capitalize binding.FunctionBindingName + "Binding fnName exprs originOpt world =\n" +
    "        let struct (evaleds, world) = World.evalManyInternal exprs world\n" +
    "        match Array.tryFind (function Scripting.Violation _ -> true | _ -> false) evaleds with\n" +
    "        | None ->\n" +
    "            match evaleds with\n" +
    "            | " + argArray + " -> " + binding.FunctionBindingName + " " + argsStr + "world\n" +
    "            | _ ->\n" +
    "                let violation = Scripting.Violation ([\"InvalidBindingInvocation\"], \"Incorrect number of arguments for binding '\" + fnName + \"' at:\\n\" + SymbolOrigin.tryPrint originOpt, None)\n" +
    "                struct (violation, world)\n" +
    "        | Some violation -> struct (violation, world)\n"

let generateTryGetBinding () =
    "    let tryGetBinding fnName =\n" +
    "        match WorldScripting.Bindings.TryGetValue fnName with\n" +
    "        | (true, binding) -> Some binding\n" +
    "        | (false, _) -> None\n"

let generateInitBindings bindings =

    let bindingDispatchers =
        bindings |>
        Array.map (fun binding ->
            let pars =
                binding.FunctionParameters |>
                Array.filter (snd >> function ValueParameter _ -> true | WorldParameter -> false) |>
                Array.map (fst >> String.surround "\"") 
            let pars = String.Join ("; ", pars)
            let name = binding.FunctionBindingName
            let nameCap = String.capitalize name
            "             (\"" + name + "\", { Fn = eval" + nameCap + "Binding; Pars = [|" + pars + "|]; DocOpt = None })\n") |>
        fun dispatchers -> String.Join ("", dispatchers)
    "    let initBindings () =\n" +
    "        let bindings =\n" +
    "            [\n" +
    bindingDispatchers +
    "            ] |>\n" +
    "            dictPlus\n" +
    "        WorldScripting.Bindings <- bindings\n"

let generateBindingSyntax bindings =

    let extrinsicKeywords =
        getExtrinsicKeywords ()

    let bindingLists =
        bindings |>
        Array.map (fun binding -> binding.FunctionBindingName) |>
        (fun arr -> Array.splitInto ((Array.length arr) / 4) arr) |>
        Array.map (fun bindingList -> "        \"" + String.Join (" ", bindingList)) |>
        (fun bindingLists -> String.Join (" \" +\n", bindingLists) + "\"\n")

    "    let [<Literal>] BindingKeywords =\n        \"" + extrinsicKeywords + " \" +\n" + bindingLists

let generateBindingsCode bindings =

    let header =
        "// Nu Game Engine.\n" +
        "// Copyright (C) Bryan Edds, 2013-2020.\n" +
        "\n" +
        "//*********************************************************************************************//\n" +
        "//                                                                                             //\n" +
        "// NOTE: This code is GENERATED by 'GenerateWorldBindings.fsx'! Do NOT edit this code by hand! //\n" +
        "//                                                                                             //\n" +
        "//*********************************************************************************************//\n" +
        "\n" +
        "namespace Nu\n" +
        "open System\n" +
        "open Prime\n" +
        "open Nu\n" +
        "\n" +
        "[<RequireQualifiedAccess>]\n" +
        "module WorldBindings =\n" +
        "\n"

    let bindingSyntax =
        generateBindingSyntax bindings + "\n"

    let bindingFunctions =
        bindings |>
        Array.map generateBindingFunction |>
        fun functions -> String.Join ("\n", functions) + "\n"

    let bindingFunctions' =
        bindings |>
        Array.map generateBindingFunction' |>
        fun functions -> String.Join ("\n", functions) + "\n"

    let tryGetBinding =
        generateTryGetBinding () + "\n"

    let initBindings =
        generateInitBindings bindings

    header +
    bindingSyntax +
    bindingFunctions +
    bindingFunctions' +
    tryGetBinding +
    initBindings

let types =
    AppDomain.CurrentDomain.GetAssemblies () |>
    Array.filter (fun asm -> (asm.GetName ()).Name = "Nu") |>
    Array.head |>
    fun asm -> asm.GetTypes () |> Array.filter (fun ty -> notNull (ty.GetCustomAttribute<ModuleBindingAttribute> ()))

let bindings =
    types |>
    Array.map (fun (ty : Type) -> ty.GetMethods ()) |>
    Array.concat |>
    Array.filter (fun mi -> notNull (mi.GetCustomAttribute<FunctionBindingAttribute> ())) |>
    Array.map tryGenerateBinding |>
    Array.definitize // TODO: error output

do
    generateBindingsCode bindings |>
    fun code -> File.WriteAllText ("../../WorldBindings.fs", code)