// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime
open Prime.Scripting
open Prime.ScriptingUnary
open Prime.ScriptingBinary
open Prime.ScriptingMarshalling
module ScriptingPrimitives =

    let evalSinglet fn fnName originOpt evaledArgs world =
        match evaledArgs with
        | [evaledArg] -> fn fnName originOpt evaledArg world
        | _ -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 1 argument.", originOpt), world)

    let evalDoublet fn fnName originOpt evaledArgs world =
        match evaledArgs with
        | [evaledArg; evaledArg2] -> fn fnName originOpt evaledArg evaledArg2 world
        | _ -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 2 arguments.", originOpt), world)

    let evalTriplet fn fnName originOpt evaledArgs world =
        match evaledArgs with
        | [evaledArg; evaledArg2; evaledArg3] -> fn fnName originOpt evaledArg evaledArg2 evaledArg3 world
        | _ -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 3 arguments.", originOpt), world)

    let evalQuadlet fn fnName originOpt evaledArgs world =
        match evaledArgs with
        | [evaledArg; evaledArg2; evaledArg3; evaledArg4] -> fn fnName originOpt evaledArg evaledArg2 evaledArg3 evaledArg4 world
        | _ -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 4 arguments.", originOpt), world)

    let evalQuintet fn fnName originOpt evaledArgs world =
        match evaledArgs with
        | [evaledArg; evaledArg2; evaledArg3; evaledArg4; evaledArg5] -> fn fnName originOpt evaledArg evaledArg2 evaledArg3 evaledArg4 evaledArg5 world
        | _ -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 5 arguments.", originOpt), world)

    let evalDereference fnName originOpt evaledArg world =
        match evaledArg with
        | Option opt ->
            match opt with
            | Some value -> (value, world)
            | None -> (Violation (["InvalidDereference"; String.capitalize fnName], "Function '" + fnName + "' requires a some value.", originOpt), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a referent value.", originOpt), world)

    let evalKeyname fnName originOpt evaledArg world =
        match evaledArg with
        | Keyphrase (name, _) -> (String name, world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a keyphrase value.", originOpt), world)

    let evalKeyfields fnName originOpt evaledArg world =
        match evaledArg with
        | Keyphrase (_, fields) -> (Tuple fields, world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a keyphrase value.", originOpt), world)

    let evalTuple _ _ evaledArgs world =
        (Tuple (List.toArray evaledArgs), world)

    let evalPair _ (_ : string) evaledArg evaledArg2 world =
        (Tuple [|evaledArg; evaledArg2|], world)

    let evalNth5 index fnName originOpt evaledArg world =
        match evaledArg with
        | Tuple arr ->
            if index >= 0 && index < Array.length arr
            then (arr.[index], world)
            else (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Tuple does not contain element at index " + string index + ".", originOpt), world)
        | Keyphrase (_, arr) ->
            if index >= 0 && index < Array.length arr
            then (arr.[index], world)
            else (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Keyphrase does not contain element at index " + string index + ".", originOpt), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-indexed value.", originOpt), world)

    let evalNthAs5 index fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (value, Tuple arr) ->
            if index >= 0 && index < Array.length arr then
                let arr = Array.copy arr
                arr.[index] <- value
                (Tuple arr, world)
            else (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Tuple does not contain element at index " + string index + ".", originOpt), world)
        | (value, Keyphrase (_, arr)) ->
            if index >= 0 && index < Array.length arr then
                let arr = Array.copy arr
                arr.[index] <- value
                (Tuple arr, world)
            else (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Keyphrase does not contain element at index " + string index + ".", originOpt), world)
        | (Violation _ as violation, _) -> (violation, world)
        | (_, (Violation _ as violation)) -> (violation, world)
        | (_, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-indexed value.", originOpt), world)

    let evalNth fnName originOpt evaledArg evaledArg2 world =
        match evaledArg with
        | Int int -> evalNth5 int fnName originOpt evaledArg2 world
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an int for the first argument.", originOpt), world)

    let evalNthAs fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match evaledArg with
        | Int int -> evalNthAs5 int fnName originOpt evaledArg2 evaledArg3 world
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an int for the first argument.", originOpt), world)

    let evalSome _ _ evaledArg world =
        (Option (Some evaledArg), world)
    
    let evalIsNone fnName originOpt evaledArg world =
        match evaledArg with
        | Option evaled -> (Bool (Option.isNone evaled), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-option.", originOpt), world)
    
    let evalIsSome fnName originOpt evaledArg world =
        match evaledArg with
        | Option evaled -> (Bool (Option.isSome evaled), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-option.", originOpt), world)

    let evalCodata fnName originOpt evaledArg evaledArg2 world =
        match evaledArg with
        | Binding _ as binding -> (Codata (Unfold (binding, evaledArg2)), world) // evaled expr to binding implies built-in function
        | Fun _ as fn -> (Codata (Unfold (fn, evaledArg2)), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " must be a function.", originOpt), world)

    let rec evalCodataTryUncons evalApply fnName originOpt codata world =
        match codata with
        | Empty -> Right (Left world)
        | Add (left, right) ->
            match evalCodataTryUncons evalApply fnName originOpt left world with
            | Right (Right (_, _, _)) as success -> success
            | Right (Left world) -> evalCodataTryUncons evalApply fnName originOpt right world
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | (Option (Some state), world) -> Right (Right (state, Unfold (unfolder, state), world))
            | (Option None, world) -> Right (Left world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion (head :: []) -> Right (Right (head, Empty, world))
        | Conversion (head :: tail) -> Right (Right (head, Conversion tail, world))
        | Conversion [] -> Right (Left world)

    let rec evalCodataIsEmpty evalApply fnName originOpt codata world =
        match evalCodataTryUncons evalApply fnName originOpt codata world with
        | Right (Right (_, _, world)) -> Right (false, world)
        | Right (Left world) -> Right (true, world)
        | Left error -> Left error

    let evalIsEmpty evalApply fnName originOpt evaledArg world =
        match evaledArg with
        | Bool bool -> (Bool (not bool), world)
        | Int int -> (Bool (int = 0), world)
        | Int64 int64 -> (Bool (int64 = 0L), world)
        | Single single -> (Bool (single = 0.0f), world)
        | Double double -> (Bool (double = 0.0), world)
        | String str -> (Bool (String.isEmpty str), world)
        | Option opt -> (Bool (Option.isNone opt), world)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right (empty, world) -> (Bool empty, world)
            | Left error -> error
        | List list -> (Bool (List.isEmpty list), world)
        | Ring set -> (Bool (Set.isEmpty set), world)
        | Table map -> (Bool (Map.isEmpty map), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalNotEmpty evalApply fnName originOpt evaledArg world =
        match evaledArg with
        | Bool bool -> (Bool bool, world)
        | Int int -> (Bool (int <> 0), world)
        | Int64 int64 -> (Bool (int64 <> 0L), world)
        | Single single -> (Bool (single <> 0.0f), world)
        | Double double -> (Bool (double <> 0.0), world)
        | String str -> (Bool (String.notEmpty str), world)
        | Option opt -> (Bool (Option.isSome opt), world)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right (empty, world) -> (Bool (not empty), world)
            | Left error -> error
        | List list -> (Bool (List.notEmpty list), world)
        | Ring set -> (Bool (Set.notEmpty set), world)
        | Table map -> (Bool (Map.notEmpty map), world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryUnconsInner evalApply fnName originOpt evaledArg world =
        match evaledArg with
        | String str ->
            if String.notEmpty str
            then Right (Right (String (string str.[0]), String (str.Substring 1), world))
            else Right (Left world)
        | Option opt ->
            match opt with
            | Some value -> Right (Right (value, NoneValue, world))
            | None -> Right (Left world)
        | Codata codata ->
            match evalCodataTryUncons evalApply fnName originOpt codata world with
            | Right (Right (head, tail, world)) -> Right (Right (head, Codata tail, world))
            | Right (Left world) -> Right (Left world)
            | Left error -> Left error
        | List list ->
            match list with
            | [] -> Right (Left world)
            | head :: tail -> Right (Right (head, List tail, world))
        | Ring set ->
            match Seq.tryHead set with
            | Some head -> Right (Right (head, Ring (Set.remove head set), world))
            | None -> Right (Left world)
        | Table map ->
            match Seq.tryHead map with
            | Some kvp -> Right (Right (Tuple [|kvp.Key; kvp.Value|], Table (Map.remove kvp.Key map), world))
            | None -> Right (Left world)
        | Violation _ as violation -> Left (violation, world)
        | _ -> Left (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryUncons evalApply fnName originOpt evaledArg world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArg world with
        | Right (Right (head, tail, world)) -> (Option (Some (Tuple [|head; tail|])), world)
        | Right (Left world) -> (Option None, world)
        | Left error -> error

    let evalUncons evalApply fnName originOpt evaledArg world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArg world with
        | Right (Right (head, tail, world)) -> (Tuple [|head; tail|], world)
        | Right (Left world) -> (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to an empty container.", originOpt), world)
        | Left error -> error

    let evalCons fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (evaledArg, String str) ->
            match evaledArg with
            | String head when String.length head = 1 -> (String (head + str), world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 string arguments required where the first is of length 1.", originOpt), world)
        | (evaledArg, Option opt) ->
            match opt with
            | Some _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot cons onto a some value.", originOpt), world)
            | None -> (Option (Some evaledArg), world)
        | (evaledArg, List list) ->
            (List (evaledArg :: list), world)
        | (evaledArg, Codata codata) ->
            match codata with
            | Empty -> (Codata (Conversion [evaledArg]), world)
            | Add _ -> (Codata (Add (Conversion [evaledArg], codata)), world)
            | Unfold _ -> (Codata (Add (Conversion [evaledArg], codata)), world)
            | Conversion list -> (Codata (Conversion (evaledArg :: list)), world)
        | (evaledArg, Ring set) ->
            (Ring (Set.add evaledArg set), world)
        | (evaledArg, Table map) ->
            match evaledArg with
            | Tuple arr when Array.length arr = 2 -> (Table (Map.add arr.[0] arr.[1] map), world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Table entry must consist of a pair.", originOpt), world)
        | (Violation _ as violation, _) -> (violation, world)
        | (_, (Violation _ as violation)) -> (violation, world)
        | (_, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-list.", originOpt), world)

    let evalCommit fnName originOpt evaledArg world =
        match evaledArg with
        | Option _ -> (evaledArg, world)
        | Codata _ -> (evaledArg, world)
        | List list -> (List (List.rev list), world)
        | Ring _ -> (evaledArg, world)
        | Table _ -> (evaledArg, world)
        | Violation _ as violation -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryHead evalApply fnName originOpt evaledArgs world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArgs world with
        | Right (Right (head, _, world)) -> (head, world)
        | Right (Left world) -> (Option None, world)
        | Left error -> error

    let evalHead evalApply fnName originOpt evaledArgs world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArgs world with
        | Right (Right (head, _, world)) -> (head, world)
        | Right (Left world) -> (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt), world)
        | Left error -> error

    let evalTryTail evalApply fnName originOpt evaledArgs world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArgs world with
        | Right (Right (_, tail, world)) -> (tail, world)
        | Right (Left world) -> (Option None, world)
        | Left error -> error

    let evalTail evalApply fnName originOpt evaledArgs world =
        match evalTryUnconsInner evalApply fnName originOpt evaledArgs world with
        | Right (Right (_, tail, world)) -> (tail, world)
        | Right (Left world) -> (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt), world)
        | Left error -> error

    let rec evalScanWhileCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right (state, [], world)
        | Add (left, right) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state left world with
            | Right (state, statesLeft, world) ->
                match evalScanWhileCodata evalApply fnName originOpt scanner state right world with
                | Right (state, statesRight, world) -> Right (state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | (Option (Some state), world) -> evalScanWhileCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world)
            | (Option None, world) -> Right (state, [], world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (state, states, world) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | (Option (Some state), world) -> (Right (state, state :: states, world))
                | (Option None, world) -> Left (List states, world)
                | (Violation _, _) as error -> Left error
                | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                (Right (state, [], world))
                list

    let rec evalScaniCodata evalApply fnName originOpt i scanner state codata world =
        match codata with
        | Empty ->
            Right (i, state, [], world)
        | Add (left, right) ->
            match evalScaniCodata evalApply fnName originOpt (inc i) scanner state left world with
            | Right (i, state, statesLeft, world) ->
                match evalScaniCodata evalApply fnName originOpt (inc i) scanner state right world with
                | Right (i, state, statesRight, world) -> Right (i, state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|scanner; Int i; state; costate|] originOpt world with
                | (Violation _, _) as error -> Left error
                | (state, world) -> evalScaniCodata evalApply fnName originOpt (inc i) scanner state (Unfold (unfolder, costate)) world
            | (Option None, world) -> Right (i, state, [], world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (i, state, states, world) elem ->
                match evalApply [|scanner; Int i; state; elem|] originOpt world with
                | (Option (Some state), world) -> (Right (inc i, state, state :: states, world))
                | (Option None, world) -> Left (List states, world)
                | (Violation _, _) as error -> Left error
                | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                (Right (i, state, [], world))
                list

    let rec evalScanCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right (state, [], world)
        | Add (left, right) ->
            match evalScanCodata evalApply fnName originOpt scanner state left world with
            | Right (state, statesLeft, world) ->
                match evalScanCodata evalApply fnName originOpt scanner state right world with
                | Right (state, statesRight, world) -> Right (state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | (Violation _, _) as error -> Left error
                | (state, world) -> evalScanCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
            | (Option None, world) -> Right (state, [], world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (state, states, world) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | (Option (Some state), world) -> (Right (state, state :: states, world))
                | (Option None, world) -> Left (List states, world)
                | (Violation _, _) as error -> Left error
                | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                (Right (state, [], world))
                list

    let evalScanWhile evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (scanner, state, String str) ->
            match
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [|scanner; state; String (string elem)|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                    (Right (state, [], world))
                    str with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Codata codata) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state codata world with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            match
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                    (Right (state, [], world))
                    list with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Ring set) ->
            match
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                    (Right (state, [], world))
                    set with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Table map) ->
            match
                Seq.foldWhileRight (fun (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|scanner; state; entry|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an option.", originOpt), world))
                    (Right (state, [], world))
                    (Map.toList map) with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (Violation _ as error, _, _) -> (error, world)
        | (_, (Violation _ as error), _) -> (error, world)
        | (_, _, (Violation _ as error)) -> (error, world)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalScani evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (scanner, state, String str) ->
            let (_, states, world) =
                Seq.foldi (fun i (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; Int i; state; String (string elem)|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    str
            (List (List.rev states), world)
        | (scanner, state, Codata codata) ->
            match evalScaniCodata evalApply fnName originOpt 0 scanner state codata world with
            | Right (_, _, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            let (_, states, world) =
                Seq.foldi (fun i (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; Int i; state; elem|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    list
            (List (List.rev states), world)
        | (scanner, state, Ring set) ->
            let (_, states, world) =
                Seq.foldi (fun i (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; Int i; state; elem|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    set
            (List (List.rev states), world)
        | (scanner, state, Table map) ->
            let (_, states, world) =
                Seq.foldi (fun i (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let (state, world) = evalApply [|scanner; Int i; state; entry|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    (Map.toList map)
            (List (List.rev states), world)
        | (Violation _ as error, _, _) -> (error, world)
        | (_, (Violation _ as error), _) -> (error, world)
        | (_, _, (Violation _ as error)) -> (error, world)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalScan evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (scanner, state, String str) ->
            let (_, states, world) =
                Seq.fold (fun (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; state; String (string elem)|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    str
            (List (List.rev states), world)
        | (scanner, state, Codata codata) ->
            match evalScanCodata evalApply fnName originOpt scanner state codata world with
            | Right (_, states, world) -> (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            let (_, states, world) =
                Seq.fold (fun (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; state; elem|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    list
            (List (List.rev states), world)
        | (scanner, state, Ring set) ->
            let (_, states, world) =
                Seq.fold (fun (state, states, world) elem ->
                    let (state, world) = evalApply [|scanner; state; elem|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    set
            (List (List.rev states), world)
        | (scanner, state, Table map) ->
            let (_, states, world) =
                Seq.fold (fun (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let (state, world) = evalApply [|scanner; state; entry|] originOpt world
                    (state, state :: states, world))
                    (state, [], world)
                    (Map.toList map)
            (List (List.rev states), world)
        | (Violation _ as error, _, _) -> (error, world)
        | (_, (Violation _ as error), _) -> (error, world)
        | (_, _, (Violation _ as error)) -> (error, world)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalFoldWhileCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right (state, world)
        | Add (left, right) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state left world with
            | Right (state, world) -> evalFoldWhileCodata evalApply fnName originOpt folder state right world
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | (Option (Some state), world) -> evalFoldWhileCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world)
            | (Option None, world) -> Right (state, world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (state, world) elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | (Option (Some state), world) -> Right (state, world)
                | (Option None, world) -> Left (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                (Right (state, world))
                list

    let rec evalFoldiCodata evalApply fnName originOpt i folder state codata world =
        match codata with
        | Empty ->
            Right (i, state, world)
        | Add (left, right) ->
            match evalFoldiCodata evalApply fnName originOpt (inc i) folder state left world with
            | Right (i, state, world) ->
                match evalFoldiCodata evalApply fnName originOpt (inc i) folder state right world with
                | Right (i, state, world) -> Right (i, state, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|folder; Int i; state; costate|] originOpt world with
                | (Violation _, _) as error -> Left error
                | (state, world) -> evalFoldiCodata evalApply fnName originOpt (inc i) folder state (Unfold (unfolder, costate)) world
            | (Option None, world) -> Right (i, state, world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (i, state, world) elem ->
                match evalApply [|folder; Int i; state; elem|] originOpt world with
                | (Option (Some state), world) -> (Right (inc i, state, world))
                | (Option None, world) -> Left (state, world)
                | (Violation _, _) as error -> Left error
                | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                (Right (i, state, world))
                list

    let rec evalFoldCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right (state, world)
        | Add (left, right) ->
            match evalFoldCodata evalApply fnName originOpt folder state left world with
            | Right (state, world) ->
                match evalFoldCodata evalApply fnName originOpt folder state right world with
                | Right (state, world) -> Right (state, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | (Option (Some costate), world) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | (Violation _, _) as error -> Left error
                | (state, world) -> evalFoldCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
            | (Option None, world) -> Right (state, world)
            | (Violation _, _) as error -> Left error
            | (_, world) -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun (state, world) elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | (Option (Some state), world) -> (Right (state, world))
                | (Option None, world) -> Left (state, world)
                | (Violation _, _) as error -> Left error
                | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                (Right (state, world))
                list

    let evalFoldWhile evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (folder, state, String str) ->
            let eir =
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [|folder; state; String (string elem)|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, world))
                    | (Option None, world) -> Left (state, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                    (Right (state, world))
                    str
            Either.amb eir
        | (folder, state, Codata codata) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state codata world with
            | Right success -> success
            | Left error -> error
        | (folder, state, List list) ->
            let eir =
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, world))
                    | (Option None, world) -> Left (state, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                    (Right (state, world))
                    list
            Either.amb eir
        | (folder, state, Ring set) ->
            let eir =
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, world))
                    | (Option None, world) -> Left (state, world)
                    | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                    (Right (state, world))
                    set
            Either.amb eir
        | (folder, state, Table map) ->
            let eir =
                Seq.foldWhileRight (fun (state, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|folder; state; entry|] originOpt world with
                    | (Option (Some state), world) -> (Right (state, world))
                    | (Option None, world) -> Left (state, world)
                    | _ -> Left (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an option.", originOpt), world))
                    (Right (state, world))
                    (Map.toList map)
            Either.amb eir
        | (Violation _ as error, _, _) -> (error, world)
        | (_, (Violation _ as error), _) -> (error, world)
        | (_, _, (Violation _ as error)) -> (error, world)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalFoldi evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (folder, state, String str) -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; String (string elem)|] originOpt world) (state, world) str
        | (folder, state, Codata codata) ->
            match evalFoldiCodata evalApply fnName originOpt 0 folder state codata world with
            | Right (_, state, world) -> (state, world)
            | Left error -> error
        | (folder, state, List list) -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; elem|] originOpt world) (state, world) list
        | (folder, state, Ring set) -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; elem|] originOpt world) (state, world) set
        | (folder, state, Table map) ->
            Seq.foldi (fun i (state, world) (key, value) ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; Int i; state; entry|] originOpt world)
                (state, world)
                (Map.toList map)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalFold evalApply fnName originOpt evaledArg evaledArg2 evaledArg3 world =
        match (evaledArg, evaledArg2, evaledArg3) with
        | (folder, state, String str) -> Seq.fold (fun (state, world) elem -> evalApply [|folder; state; String (string elem)|] originOpt world) (state, world) str
        | (folder, state, Codata codata) ->
            match evalFoldCodata evalApply fnName originOpt folder state codata world with
            | Right (state, world) -> (state, world)
            | Left error -> error
        | (folder, state, List list) -> List.fold (fun (state, world) elem -> evalApply [|folder; state; elem|] originOpt world) (state, world) list
        | (folder, state, Ring set) -> Set.fold (fun (state, world) elem -> evalApply [|folder; state; elem|] originOpt world) (state, world) set
        | (folder, state, Table map) ->
            Map.fold (fun (state, world) key value ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; state; entry|] originOpt world)
                (state, world)
                map
        | (Violation _ as error, _, _) -> (error, world)
        | (_, (Violation _ as error), _) -> (error, world)
        | (_, _, (Violation _ as error)) -> (error, world)
        | (_, _, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalMapCodata evalApply originOpt mapper codata (world : 'w) : Codata * 'w =
        match codata with
        | Empty ->
            (codata, world)
        | Add (left, right) ->
            let (leftMapped, world) = evalMapCodata evalApply originOpt mapper left world
            let (rightMapped, world) = evalMapCodata evalApply originOpt mapper right world
            (Add (leftMapped, rightMapped), world)
        | Unfold (unfolder, codata) ->
            let unfolder = Unfold (Fun ([|"state"|], 1, Apply ([|unfolder; Binding ("state", ref UncachedBinding, originOpt)|], originOpt), false, None, originOpt), codata)
            (unfolder, world)
        | Conversion list ->
            let (mapped, world) =
                List.fold (fun (elems, world) elem ->
                    let (elem, world) = evalApply [|mapper; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
                    list
            (Conversion (List.rev mapped), world)

    let evalMapi evalApply fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; Int 0; value|] originOpt world
            | None -> (option, world)
        | (mapper, String str) ->
            let (list, world) =
                str |>
                Seq.foldi (fun i (elems, world) elem ->
                    let elem = String (string elem)
                    let (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
            else (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a string of length 1.", originOpt), world)
        | (mapper, Codata codata) ->
            let (codata, world) = evalMapCodata evalApply originOpt mapper codata world
            (Codata codata, world)
        | (mapper, List list) ->
            let (list, world) =
                Seq.foldi (fun i (elems, world) elem ->
                    let (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
                    list
            (List (List.rev list), world)
        | (mapper, Ring set) ->
            let (set, world) =
                Seq.foldi (fun i (elems, world) elem ->
                    let (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    (Set.add elem elems, world))
                    (Set.empty, world)
                    set
            (Ring set, world)
        | (mapper, Table map) ->
            let (map, world) =
                Seq.foldi (fun i (elems, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let (entry, world) = evalApply [|mapper; Int i; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> ((Map.add elems'.[0] elems'.[1] elems), world)
                    | _ -> (elems, world))
                    (Map.empty, world)
                    (Map.toList map)
            (Table map, world)
        | (Violation _ as error, _) -> (error, world)
        | (_, (Violation _ as error)) -> (error, world)
        | (_, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalMap evalApply fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; value|] originOpt world
            | None -> (option, world)
        | (mapper, String str) ->
            let (list, world) =
                str |>
                Seq.fold (fun (elems, world) elem ->
                    let elem = String (string elem)
                    let (elem, world) = evalApply [|mapper; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
            else (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a string of length 1.", originOpt), world)
        | (mapper, Codata codata) ->
            let (codata, world) = evalMapCodata evalApply originOpt mapper codata world
            (Codata codata, world)
        | (mapper, List list) ->
            let (list, world) =
                List.fold (fun (elems, world) elem ->
                    let (elem, world) = evalApply [|mapper; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
                    list
            (List (List.rev list), world)
        | (mapper, Ring set) ->
            let (set, world) =
                Set.fold (fun (elems, world) elem ->
                    let (elem, world) = evalApply [|mapper; elem|] originOpt world
                    (Set.add elem elems, world))
                    (Set.empty, world)
                    set
            (Ring set, world)
        | (mapper, Table map) ->
            let (map, world) =
                Map.fold (fun (elems, world) key value ->
                    let entry = Tuple [|key; value|]
                    let (entry, world) = evalApply [|mapper; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> ((Map.add elems'.[0] elems'.[1] elems), world)
                    | _ -> (elems, world))
                    (Map.empty, world)
                    map
            (Table map, world)
        | (Violation _ as error, _) -> (error, world)
        | (_, (Violation _ as error)) -> (error, world)
        | (_, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalContainsCodata evalApply fnName originOpt evaledArg codata world =
        match codata with
        | Empty -> Right (false, world)
        | Add (left, right) ->
            match evalContainsCodata evalApply fnName originOpt evaledArg left world with
            | Right (false, world) -> evalContainsCodata evalApply fnName originOpt evaledArg right world
            | Right (true, _) as success -> success
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | (Option (Some state), world) ->
                if state <> evaledArg then
                    let codata = Unfold (unfolder, state)
                    evalContainsCodata evalApply fnName originOpt evaledArg codata world
                else Right (true, world)
            | (Option None, world) -> Right (false, world)
            | error -> Left error
        | Conversion list ->
            Right (List.contains evaledArg list, world)

    let evalContains evalApply fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (evaledArg, String str) ->
            match evaledArg with
            | String str' -> (Bool (str.Contains str'), world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " for a string must also be a string.", originOpt), world)
        | (evaledArg, Option opt) -> (Bool (match opt with Some value -> value = evaledArg | None -> false), world)
        | (evaledArg, Codata codata) ->
            match evalContainsCodata evalApply fnName originOpt evaledArg codata world with
            | Right (bool, world) -> (Bool bool, world)
            | Left error -> error
        | (evaledArg, List list) -> (Bool (List.contains evaledArg list), world)
        | (evaledArg, Ring set) -> (Bool (Set.contains evaledArg set), world)
        | (evaledArg, Table map) -> (Bool (Map.containsKey evaledArg map), world)
        | (Violation _ as error, _) -> (error, world)
        | (_, (Violation _ as error)) -> (error, world)
        | (_, _) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalToCodata fnName originOpt evaledArg world =
        match evaledArg with
        | Option opt -> (Codata (Conversion (match opt with Some value -> [value] | None -> [])), world)
        | Codata _ -> (evaledArg, world)
        | List list -> (Codata (Conversion list), world)
        | Ring set -> (Codata (Conversion (Set.toList set)), world)
        | Table map -> (Codata (Conversion (Map.toListBy (fun (key, value) -> Tuple [|key; value|]) map)), world)
        | Violation _ as error -> (error, world)
        | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Canot apply " + fnName + " to a non-container.", originOpt), world)

    let evalList _ _ evaledArgs world =
        (List evaledArgs, world)

    let evalRing _ _ evaledArgs world =
        (Ring (Set.ofList evaledArgs), world)

    let evalRemove fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (value, container) ->
            match container with
            | Ring set -> (Ring (Set.remove value set), world)
            | Table map -> (Table (Map.remove value map), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", originOpt), world)

    let evalTryFind fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (key, container) ->
            match container with
            | Table map -> (Option (Map.tryFind key map), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", originOpt), world)

    let evalFind fnName originOpt evaledArg evaledArg2 world =
        match (evaledArg, evaledArg2) with
        | (key, container) ->
            match container with
            | Table map ->
                match Map.tryFind key map with
                | Some value -> (value, world)
                | None -> (Violation (["InvalidKey"; String.capitalize fnName], "Key not found in table.", originOpt), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", originOpt), world)

    let evalTable fnName originOpt evaledArgs world =
        if List.forall (function Tuple arr when Array.length arr = 2 -> true | _ -> false) evaledArgs then
            let evaledPairs = List.map (function List [evaledFst; evaledSnd] -> (evaledFst, evaledSnd) | _ -> failwithumf ()) evaledArgs
            let evaledMap = Map.ofList evaledPairs
            (Table evaledMap, world)
        else (Violation (["InvalidEntries"; String.capitalize fnName], "Table entries must consist of 1 or more pairs.", originOpt), world)