// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Scripting
open Nu.WorldScriptingUnary.Scripting
open Nu.WorldScriptingBinary.Scripting
open Nu.WorldScriptingMarshalling.Scripting

[<AutoOpen>]
module WorldScriptingPrimitives =

    module Scripting =

        let evalSinglet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> fn fnOriginOpt fnName evaledArg world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 1 argument.", fnOriginOpt), world)

        let evalDoublet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; evaledArg2] -> fn fnOriginOpt fnName evaledArg evaledArg2 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 2 arguments.", fnOriginOpt), world)

        let evalTriplet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; evaledArg2; evaledArg3] -> fn fnOriginOpt fnName evaledArg evaledArg2 evaledArg3 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 3 arguments.", fnOriginOpt), world)

        let evalQuadlet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; evaledArg2; evaledArg3; evaledArg4] -> fn fnOriginOpt fnName evaledArg evaledArg2 evaledArg3 evaledArg4 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 4 arguments.", fnOriginOpt), world)

        let evalQuintet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; evaledArg2; evaledArg3; evaledArg4; evaledArg5] -> fn fnOriginOpt fnName evaledArg evaledArg2 evaledArg3 evaledArg4 evaledArg5 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 5 arguments.", fnOriginOpt), world)

        let evalDereference fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Option opt ->
                match opt with
                | Some value -> (value, world)
                | None -> (Violation ([!!"InvalidDereference"; !!"Referent"; !!"Option"; !!(String.capitalize fnName)], "Function '" + fnName + "' requires a some value.", fnOriginOpt), world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Referent"; !!(String.capitalize fnName)], "Function '" + fnName + "' requires a referent value.", fnOriginOpt), world)

        let evalV2 fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Single x; Single y] -> (Vector2 (OpenTK.Vector2 (x, y)), world)
            | [Violation _ as violation; _] -> (violation, world)
            | [_; Violation _ as violation] -> (violation, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"V2"; !!(String.capitalize fnName)], "Application of " + fnName + " requires a single for the both arguments.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"V2"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalTuple _ _ evaledArgs world =
            (Tuple (List.toArray evaledArgs), world)

        let evalPair fnOriginOpt (_ : string) evaledArgs world =
            match evaledArgs with
            | [_; _] -> (Tuple (List.toArray evaledArgs), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Pair"], "Incorrect number of arguments for creation of a pair; 2 arguments required.", fnOriginOpt), world)
    
        let evalNth5 index fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Tuple evaleds] ->
                if index >= 0 && index < Array.length evaleds
                then (evaleds.[index], world)
                else (Violation ([!!"OutOfRangeArgument"; !!"Indexed"; !!(String.capitalize fnName)], "Tuple does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [Keyphrase (_, evaleds)] ->
                if index >= 0 && index < Array.length evaleds
                then (evaleds.[index], world)
                else (Violation ([!!"OutOfRangeArgument"; !!"Indexed"; !!(String.capitalize fnName)], "Keyphrase does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [Violation _ as violation] -> (violation, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Indexed"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-indexed value.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Indexed"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
        
        let evalNth fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [head; foot] ->
                match head with
                | Int int -> evalNth5 int fnOriginOpt fnName [foot] world
                | Violation _ as violation -> (violation, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Indexed"; !!(String.capitalize fnName)], "Application of " + fnName + " requires an int for the first argument.", fnOriginOpt), world)
            | _ ->  (Violation ([!!"InvalidArgumentCount"; !!"Indexed"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)
            
        let evalSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> (Option (Some evaledArg), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsNone fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option evaled] -> (Bool (Option.isNone evaled), world)
            | [Violation _ as violation] -> (violation, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option evaled] -> (Bool (Option.isSome evaled), world)
            | [Violation _ as violation] -> (violation, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalCodata fnOriginOpt fnName evaledArg evaledArg2 world =
            match evaledArg with
            | Binding _ as binding -> (Codata (Unfold (binding, evaledArg2)), world) // evaled expr to binding implies built-in function
            | Fun _ as fn -> (Codata (Unfold (fn, evaledArg2)), world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Codata"; !!(String.capitalize fnName)], "First argument to " + fnName + " must be a function.", fnOriginOpt), world)

        let rec evalCodataTryUncons evalApply fnOriginOpt fnName codata world =
            match codata with
            | Empty -> Right (Left world)
            | Add (left, right) ->
                match evalCodataTryUncons evalApply fnOriginOpt fnName left world with
                | Right (Right (_, _, _)) as success -> success
                | Right (Left world) -> evalCodataTryUncons evalApply fnOriginOpt fnName right world
                | Left _ as error -> error
            | Unfold (unfolder, state) ->
                match evalApply [|unfolder; state|] fnOriginOpt world with
                | (Option (Some state), world) -> Right (Right (state, Unfold (unfolder, state), world))
                | (Option None, world) -> Right (Left world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Codata"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion (head :: []) -> Right (Right (head, Empty, world))
            | Conversion (head :: tail) -> Right (Right (head, Conversion tail, world))
            | Conversion [] -> Right (Left world)

        let rec evalCodataIsEmpty evalApply fnOriginOpt fnName codata world =
            match evalCodataTryUncons evalApply fnOriginOpt fnName codata world with
            | Right (Right (_, _, world)) -> Right (false, world)
            | Right (Left world) -> Right (true, world)
            | Left error -> Left error

        let evalIsEmpty evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Bool bool] -> (Bool (not bool), world)
            | [Int int] -> (Bool (int = 0), world)
            | [Int64 int64] -> (Bool (int64 = 0L), world)
            | [Single single] -> (Bool (single = 0.0f), world)
            | [Double double] -> (Bool (double = 0.0), world)
            | [Vector2 v2] -> (Bool (v2 = OpenTK.Vector2.Zero), world)
            | [String str] -> (Bool (String.isEmpty str), world)
            | [Option opt] -> (Bool (Option.isNone opt), world)
            | [Codata codata] ->
                match evalCodataIsEmpty evalApply fnOriginOpt fnName codata world with
                | Right (empty, world) -> (Bool empty, world)
                | Left error -> error
            | [List list] -> (Bool (List.isEmpty list), world)
            | [Ring set] -> (Bool (Set.isEmpty set), world)
            | [Table map] -> (Bool (Map.isEmpty map), world)
            | [Violation _ as violation] -> (violation, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalNotEmpty evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Bool bool] -> (Bool bool, world)
            | [Int int] -> (Bool (int <> 0), world)
            | [Int64 int64] -> (Bool (int64 <> 0L), world)
            | [Single single] -> (Bool (single <> 0.0f), world)
            | [Double double] -> (Bool (double <> 0.0), world)
            | [Vector2 v2] -> (Bool (v2 <> OpenTK.Vector2.Zero), world)
            | [String str] -> (Bool (String.notEmpty str), world)
            | [Option opt] -> (Bool (Option.isSome opt), world)
            | [Codata codata] ->
                match evalCodataIsEmpty evalApply fnOriginOpt fnName codata world with
                | Right (empty, world) -> (Bool (not empty), world)
                | Left error -> error
            | [List list] -> (Bool (List.notEmpty list), world)
            | [Ring set] -> (Bool (Set.notEmpty set), world)
            | [Table map] -> (Bool (Map.notEmpty map), world)
            | [Violation _ as violation] -> (violation, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [String str] ->
                if String.notEmpty str
                then Right (Right (String (string str.[0]), String (str.Substring 1), world))
                else Right (Left world)
            | [Option opt] ->
                match opt with
                | Some value -> Right (Right (value, NoneValue, world))
                | None -> Right (Left world)
            | [Codata codata] ->
                match evalCodataTryUncons evalApply fnOriginOpt fnName codata world with
                | Right (Right (head, tail, world)) -> Right (Right (head, Codata tail, world))
                | Right (Left world) -> Right (Left world)
                | Left error -> Left error
            | [List list] ->
                match list with
                | [] -> Right (Left world)
                | head :: tail -> Right (Right (head, List tail, world))
            | [Ring set] ->
                match Seq.tryHead set with
                | Some head -> Right (Right (head, Ring (Set.remove head set), world))
                | None -> Right (Left world)
            | [Table map] ->
                match Seq.tryHead map with
                | Some kvp -> Right (Right (Tuple [|kvp.Key; kvp.Value|], Table (Map.remove kvp.Key map), world))
                | None -> Right (Left world)
            | [Violation _ as violation] -> Left (violation, world)
            | [_] -> Left (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> Left (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTryUncons evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (head, tail, world)) -> (Option (Some (Tuple [|head; tail|])), world)
            | Right (Left world) -> (Option None, world)
            | Left error -> error

        let evalUncons evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (head, tail, world)) -> (Tuple [|head; tail|], world)
            | Right (Left world) -> (Violation ([!!"OutOfRangeArgument"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to an empty container.", fnOriginOpt), world)
            | Left error -> error

        let evalCons fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; String str] ->
                match evaledArg with
                | String head when String.length head = 1 -> (String (head + str), world)
                | Violation _ as violation -> (violation, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 string arguments required where the first is of length 1.", fnOriginOpt), world)
            | [evaledArg; Option opt] ->
                match opt with
                | Some _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot cons onto a some value.", fnOriginOpt), world)
                | None -> (Option (Some evaledArg), world)
            | [evaledArg; List list] ->
                (List (evaledArg :: list), world)
            | [evaledArg; Codata codata] ->
                (Codata (Add (Conversion [evaledArg], codata)), world)
            | [evaledArg; Ring set] ->
                (Ring (Set.add evaledArg set), world)
            | [evaledArg; Table map] ->
                match evaledArg with
                | Tuple arr when Array.length arr = 2 -> (Table (Map.add arr.[0] arr.[1] map), world)
                | Violation _ as violation -> (violation, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Table entry must consist of a pair.", fnOriginOpt), world)
            | [Violation _ as violation; _] -> (violation, world)
            | [_; Violation _ as violation] -> (violation, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalCommit fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Option _ -> (evaledArg, world)
            | Codata _ -> (evaledArg, world)
            | List list -> (List (List.rev list), world)
            | Ring _ -> (evaledArg, world)
            | Table _ -> (evaledArg, world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)

        let evalTryHead evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (head, _, world)) -> (head, world)
            | Right (Left world) -> (Option None, world)
            | Left error -> error

        let evalHead evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (head, _, world)) -> (head, world)
            | Right (Left world) -> (Violation ([!!"OutOfRangeArgument"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a container with no elements.", fnOriginOpt), world)
            | Left error -> error

        let evalTryTail evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (_, tail, world)) -> (tail, world)
            | Right (Left world) -> (Option None, world)
            | Left error -> error

        let evalTail evalApply fnOriginOpt fnName evaledArgs world =
            match evalTryUnconsInner evalApply fnOriginOpt fnName evaledArgs world with
            | Right (Right (_, tail, world)) -> (tail, world)
            | Right (Left world) -> (Violation ([!!"OutOfRangeArgument"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a container with no elements.", fnOriginOpt), world)
            | Left error -> error

        let rec evalScanWhileCodata evalApply fnOriginOpt fnName scanner state codata world =
            match codata with
            | Empty ->
                Right (state, [], world)
            | Add (left, right) ->
                match evalScanWhileCodata evalApply fnOriginOpt fnName scanner state left world with
                | Right (state, statesLeft, world) ->
                    match evalScanWhileCodata evalApply fnOriginOpt fnName scanner state right world with
                    | Right (state, statesRight, world) -> Right (state, statesRight @ statesLeft, world)
                    | error -> error
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|scanner; state; costate|] fnOriginOpt world with
                    | (Option (Some state), world) -> evalScanWhileCodata evalApply fnOriginOpt fnName scanner state (Unfold (unfolder, costate)) world
                    | (Option None, world) -> Right (state, [], world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world)
                | (Option None, world) -> Right (state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                    (Right (state, [], world))
                    list

        let rec evalScaniCodata evalApply fnOriginOpt fnName i scanner state codata world =
            match codata with
            | Empty ->
                Right (i, state, [], world)
            | Add (left, right) ->
                match evalScaniCodata evalApply fnOriginOpt fnName (inc i) scanner state left world with
                | Right (i, state, statesLeft, world) ->
                    match evalScaniCodata evalApply fnOriginOpt fnName (inc i) scanner state right world with
                    | Right (i, state, statesRight, world) -> Right (i, state, statesRight @ statesLeft, world)
                    | error -> error
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|scanner; Int i; state; costate|] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalScaniCodata evalApply fnOriginOpt fnName (inc i) scanner state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (i, state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (i, state, states, world) elem ->
                    match evalApply [|scanner; Int i; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> (Right (inc i, state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                    (Right (i, state, [], world))
                    list

        let rec evalScanCodata evalApply fnOriginOpt fnName scanner state codata world =
            match codata with
            | Empty ->
                Right (state, [], world)
            | Add (left, right) ->
                match evalScanCodata evalApply fnOriginOpt fnName scanner state left world with
                | Right (state, statesLeft, world) ->
                    match evalScanCodata evalApply fnOriginOpt fnName scanner state right world with
                    | Right (state, statesRight, world) -> Right (state, statesRight @ statesLeft, world)
                    | error -> error
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|scanner; state; costate|] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalScanCodata evalApply fnOriginOpt fnName scanner state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> (Right (state, state :: states, world))
                    | (Option None, world) -> Left (List states, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                    (Right (state, [], world))
                    list

        let evalScanWhile evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [scanner; state; String str] ->
                match
                    Seq.foldWhileRight (fun (state, states, world) elem ->
                        match evalApply [|scanner; state; String (string elem)|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, state :: states, world))
                        | (Option None, world) -> Left (List states, world)
                        | (Violation _, _) as error -> Left error
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                        (Right (state, [], world))
                        str with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; Codata codata] ->
                match evalScanWhileCodata evalApply fnOriginOpt fnName scanner state codata world with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; List list] ->
                match
                    Seq.foldWhileRight (fun (state, states, world) elem ->
                        match evalApply [|scanner; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, state :: states, world))
                        | (Option None, world) -> Left (List states, world)
                        | (Violation _, _) as error -> Left error
                        | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                        (Right (state, [], world))
                        list with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; Ring set] ->
                match
                    Seq.foldWhileRight (fun (state, states, world) elem ->
                        match evalApply [|scanner; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, state :: states, world))
                        | (Option None, world) -> Left (List states, world)
                        | (Violation _, _) as error -> Left error
                        | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                        (Right (state, [], world))
                        set with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; Table map] ->
                match
                    Seq.foldWhileRight (fun (state, states, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        match evalApply [|scanner; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, state :: states, world))
                        | (Option None, world) -> Left (List states, world)
                        | (Violation _, _) as error -> Left error
                        | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world))
                        (Right (state, [], world))
                        (Map.toList map) with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [Violation _ as error; _; _] -> (error, world)
            | [_; Violation _ as error; _] -> (error, world)
            | [_; _; Violation _ as error] -> (error, world)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let evalScani evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [scanner; state; String str] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; Int i; state; String (string elem)|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        str
                (List (List.rev states), world)
            | [scanner; state; Codata codata] ->
                match evalScaniCodata evalApply fnOriginOpt fnName 0 scanner state codata world with
                | Right (_, _, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; List list] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; Int i; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        list
                (List (List.rev states), world)
            | [scanner; state; Ring set] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; Int i; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        set
                (List (List.rev states), world)
            | [scanner; state; Table map] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (state, world) = evalApply [|scanner; Int i; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        (Map.toList map)
                (List (List.rev states), world)
            | [Violation _ as error; _; _] -> (error, world)
            | [_; Violation _ as error; _] -> (error, world)
            | [_; _; Violation _ as error] -> (error, world)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let evalScan evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [scanner; state; String str] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; state; String (string elem)|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        str
                (List (List.rev states), world)
            | [scanner; state; Codata codata] ->
                match evalScanCodata evalApply fnOriginOpt fnName scanner state codata world with
                | Right (_, states, world) -> (List (List.rev states), world)
                | Left error -> error
            | [scanner; state; List list] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        list
                (List (List.rev states), world)
            | [scanner; state; Ring set] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) elem ->
                        let (state, world) = evalApply [|scanner; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        set
                (List (List.rev states), world)
            | [scanner; state; Table map] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (state, world) = evalApply [|scanner; state; elem|] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        (Map.toList map)
                (List (List.rev states), world)
            | [Violation _ as error; _; _] -> (error, world)
            | [_; Violation _ as error; _] -> (error, world)
            | [_; _; Violation _ as error] -> (error, world)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let rec evalFoldWhileCodata evalApply fnOriginOpt fnName folder state codata world =
            match codata with
            | Empty ->
                Right (state, world)
            | Add (left, right) ->
                match evalFoldWhileCodata evalApply fnOriginOpt fnName folder state left world with
                | Right (state, world) -> evalFoldWhileCodata evalApply fnOriginOpt fnName folder state right world
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|folder; state; costate|] fnOriginOpt world with
                    | (Option (Some state), world) -> evalFoldWhileCodata evalApply fnOriginOpt fnName folder state (Unfold (unfolder, costate)) world
                    | (Option None, world) -> Right (state, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world)
                | (Option None, world) -> Right (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [|folder; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> Right (state, world)
                    | (Option None, world) -> Left (state, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                    (Right (state, world))
                    list

        let rec evalFoldiCodata evalApply fnOriginOpt fnName i folder state codata world =
            match codata with
            | Empty ->
                Right (i, state, world)
            | Add (left, right) ->
                match evalFoldiCodata evalApply fnOriginOpt fnName (inc i) folder state left world with
                | Right (i, state, world) ->
                    match evalFoldiCodata evalApply fnOriginOpt fnName (inc i) folder state right world with
                    | Right (i, state, world) -> Right (i, state, world)
                    | error -> error
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|folder; Int i; state; costate|] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalFoldiCodata evalApply fnOriginOpt fnName (inc i) folder state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (i, state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (i, state, world) elem ->
                    match evalApply [|folder; Int i; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> (Right (inc i, state, world))
                    | (Option None, world) -> Left (state, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                    (Right (i, state, world))
                    list

        let rec evalFoldCodata evalApply fnOriginOpt fnName folder state codata world =
            match codata with
            | Empty ->
                Right (state, world)
            | Add (left, right) ->
                match evalFoldCodata evalApply fnOriginOpt fnName folder state left world with
                | Right (state, world) ->
                    match evalFoldCodata evalApply fnOriginOpt fnName folder state right world with
                    | Right (state, world) -> Right (state, world)
                    | error -> error
                | error -> error
            | Unfold (unfolder, costate) ->
                match evalApply [|unfolder; costate|] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [|folder; state; costate|] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalFoldCodata evalApply fnOriginOpt fnName folder state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [|folder; state; elem|] fnOriginOpt world with
                    | (Option (Some state), world) -> (Right (state, world))
                    | (Option None, world) -> Left (state, world)
                    | (Violation _, _) as error -> Left error
                    | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                    (Right (state, world))
                    list

        let evalFoldWhile evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [folder; state; String str] ->
                let eir =
                    Seq.foldWhileRight (fun (state, world) elem ->
                        match evalApply [|folder; state; String (string elem)|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, world))
                        | (Option None, world) -> Left (state, world)
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                        (Right (state, world))
                        str
                Either.amb eir
            | [folder; state; Codata codata] ->
                match evalFoldWhileCodata evalApply fnOriginOpt fnName folder state codata world with
                | Right success -> success
                | Left error -> error
            | [folder; state; List list] ->
                let eir =
                    Seq.foldWhileRight (fun (state, world) elem ->
                        match evalApply [|folder; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, world))
                        | (Option None, world) -> Left (state, world)
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                        (Right (state, world))
                        list
                Either.amb eir
            | [folder; state; Ring set] ->
                let eir =
                    Seq.foldWhileRight (fun (state, world) elem ->
                        match evalApply [|folder; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, world))
                        | (Option None, world) -> Left (state, world)
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                        (Right (state, world))
                        set
                Either.amb eir
            | [folder; state; Table map] ->
                let eir =
                    Seq.foldWhileRight (fun (state, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        match evalApply [|folder; state; elem|] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, world))
                        | (Option None, world) -> Left (state, world)
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                        (Right (state, world))
                        (Map.toList map)
                Either.amb eir
            | [Violation _ as error; _; _] -> (error, world)
            | [_; Violation _ as error; _] -> (error, world)
            | [_; _; Violation _ as error] -> (error, world)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let evalFoldi evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [folder; state; String str] -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; String (string elem)|] fnOriginOpt world) (state, world) str
            | [folder; state; Codata codata] ->
                match evalFoldiCodata evalApply fnOriginOpt fnName 0 folder state codata world with
                | Right (_, state, world) -> (state, world)
                | Left error -> error
            | [folder; state; List list] -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; elem|] fnOriginOpt world) (state, world) list
            | [folder; state; Ring set] -> Seq.foldi (fun i (state, world) elem -> evalApply [|folder; Int i; state; elem|] fnOriginOpt world) (state, world) set
            | [folder; state; Table map] ->
                Seq.foldi (fun i (state, world) (key, value) ->
                    let elem = Tuple [|key; value|]
                    evalApply [|folder; Int i; state; elem|] fnOriginOpt world)
                    (state, world)
                    (Map.toList map)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let evalFold evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [folder; state; String str] -> Seq.fold (fun (state, world) elem -> evalApply [|folder; state; String (string elem)|] fnOriginOpt world) (state, world) str
            | [folder; state; Codata codata] ->
                match evalFoldCodata evalApply fnOriginOpt fnName folder state codata world with
                | Right (state, world) -> (state, world)
                | Left error -> error
            | [folder; state; List list] -> List.fold (fun (state, world) elem -> evalApply [|folder; state; elem|] fnOriginOpt world) (state, world) list
            | [folder; state; Ring set] -> Set.fold (fun (state, world) elem -> evalApply [|folder; state; elem|] fnOriginOpt world) (state, world) set
            | [folder; state; Table map] ->
                Map.fold (fun (state, world) key value ->
                    let elem = Tuple [|key; value|]
                    evalApply [|folder; state; elem|] fnOriginOpt world)
                    (state, world)
                    map
            | [Violation _ as error; _; _] -> (error, world)
            | [_; Violation _ as error; _] -> (error, world)
            | [_; _; Violation _ as error] -> (error, world)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let rec evalMapCodata evalApply fnOriginOpt mapper codata (world : World) : Codata * World =
            match codata with
            | Empty ->
                (codata, world)
            | Add (left, right) ->
                let (leftMapped, world) = evalMapCodata evalApply fnOriginOpt mapper left world
                let (rightMapped, world) = evalMapCodata evalApply fnOriginOpt mapper right world
                (Add (leftMapped, rightMapped), world)
            | Unfold (unfolder, codata) ->
                let unfolder = Unfold (Fun ([|"state"|], 1, Apply ([|unfolder; Binding ("state", ref UncachedBinding, fnOriginOpt)|], fnOriginOpt), false, None, fnOriginOpt), codata)
                (unfolder, world)
            | Conversion list ->
                let (mapped, world) =
                    List.fold (fun (elems, world) elem ->
                        let (elem, world) = evalApply [|mapper; elem|] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (Conversion (List.rev mapped), world)

        let evalMapi evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [mapper; Option opt as option] ->
                match opt with
                | Some value -> evalApply [|mapper; Int 0; value|] fnOriginOpt world
                | None -> (option, world)
            | [mapper; String str] ->
                let (list, world) =
                    str |>
                    Seq.foldi (fun i (elems, world) elem ->
                        let elem = String (string elem)
                        let (elem, world) = evalApply [|mapper; Int i; elem|] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
                then (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
                else (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + " applied to string's mapper must return a string of length 1.", fnOriginOpt), world)
            | [mapper; Codata codata] ->
                let (codata, world) = evalMapCodata evalApply fnOriginOpt mapper codata world
                (Codata codata, world)
            | [mapper; List list] ->
                let (list, world) =
                    Seq.foldi (fun i (elems, world) elem ->
                        let (elem, world) = evalApply [|mapper; Int i; elem|] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list), world)
            | [mapper; Ring set] ->
                let (set, world) =
                    Seq.foldi (fun i (elems, world) elem ->
                        let (elem, world) = evalApply [|mapper; Int i; elem|] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring set, world)
            | [mapper; Table map] ->
                let (map, world) =
                    Seq.foldi (fun i (elems, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (elem, world) = evalApply [|mapper; Int i; elem|] fnOriginOpt world
                        match elem with
                        | Tuple elems' when Array.length elems' = 2 -> ((Map.add elems'.[0] elems'.[1] elems), world)
                        | _ -> (elems, world))
                        (Map.empty, world)
                        (Map.toList map)
                (Table map, world)
            | [Violation _ as error; _] -> (error, world)
            | [_; Violation _ as error] -> (error, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalMap evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [mapper; Option opt as option] ->
                match opt with
                | Some value -> evalApply [|mapper; value|] fnOriginOpt world
                | None -> (option, world)
            | [mapper; String str] ->
                let (list, world) =
                    str |>
                    Seq.fold (fun (elems, world) elem ->
                        let elem = String (string elem)
                        let (elem, world) = evalApply [|mapper; elem|] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
                then (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
                else (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + " applied to string's mapper must return a string of length 1.", fnOriginOpt), world)
            | [mapper; Codata codata] ->
                let (codata, world) = evalMapCodata evalApply fnOriginOpt mapper codata world
                (Codata codata, world)
            | [mapper; List list] ->
                let (list, world) =
                    List.fold (fun (elems, world) elem ->
                        let (elem, world) = evalApply [|mapper; elem|] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list), world)
            | [mapper; Ring set] ->
                let (set, world) =
                    Set.fold (fun (elems, world) elem ->
                        let (elem, world) = evalApply [|mapper; elem|] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring set, world)
            | [mapper; Table map] ->
                let (map, world) =
                    Map.fold (fun (elems, world) key value ->
                        let elem = Tuple [|key; value|]
                        let (elem, world) = evalApply [|mapper; elem|] fnOriginOpt world
                        match elem with
                        | Tuple elems' when Array.length elems' = 2 -> ((Map.add elems'.[0] elems'.[1] elems), world)
                        | _ -> (elems, world))
                        (Map.empty, world)
                        map
                (Table map, world)
            | [Violation _ as error; _] -> (error, world)
            | [_; Violation _ as error] -> (error, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let rec evalContainsCodata evalApply fnOriginOpt fnName evaledArg codata world =
            match codata with
            | Empty -> Right (false, world)
            | Add (left, right) ->
                match evalContainsCodata evalApply fnOriginOpt fnName evaledArg left world with
                | Right (false, world) -> evalContainsCodata evalApply fnOriginOpt fnName evaledArg right world
                | Right (true, _) as success -> success
                | Left _ as error -> error
            | Unfold (unfolder, state) ->
                match evalApply [|unfolder; state|] fnOriginOpt world with
                | (Option (Some state), world) ->
                    if state <> evaledArg then
                        let codata = Unfold (unfolder, state)
                        evalContainsCodata evalApply fnOriginOpt fnName evaledArg codata world
                    else Right (true, world)
                | (Option None, world) -> Right (false, world)
                | error -> Left error
            | Conversion list ->
                Right (List.contains evaledArg list, world)

        let evalContains evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg; String str] ->
                match evaledArg with
                | String str' -> (Bool (str.Contains str'), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "First argument to " + fnName + " for a string must also be a string.", fnOriginOpt), world)
            | [evaledArg; Option opt] -> (Bool (match opt with Some value -> value = evaledArg | None -> false), world)
            | [evaledArg; Codata codata] ->
                match evalContainsCodata evalApply fnOriginOpt fnName evaledArg codata world with
                | Right (bool, world) -> (Bool bool, world)
                | Left error -> error
            | [evaledArg; List list] -> (Bool (List.contains evaledArg list), world)
            | [evaledArg; Ring set] -> (Bool (Set.contains evaledArg set), world)
            | [evaledArg; Table map] -> (Bool (Map.containsKey evaledArg map), world)
            | [Violation _ as error; _] -> (error, world)
            | [_; Violation _ as error] -> (error, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalToCodata fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Option opt -> (Codata (Conversion (match opt with Some value -> [value] | None -> [])), world)
            | Codata _ -> (evaledArg, world)
            | List list -> (Codata (Conversion list), world)
            | Ring set -> (Codata (Conversion (Set.toList set)), world)
            | Table map -> (Codata (Conversion (Map.toListBy (fun (key, value) -> Tuple [|key; value|]) map)), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Canot apply " + fnName + " to a non-container.", fnOriginOpt), world)

        let evalList _ _ evaledArgs world =
            (List evaledArgs, world)

        let evalRing _ (_ : string) evaledArgs world =
            (Ring (Set.ofList evaledArgs), world)

        let evalRemove fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring set -> (Ring (Set.remove value set), world)
                | Table map -> (Table (Map.remove value map), world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTryFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table map -> (Option (Map.tryFind key map), world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table map ->
                    match Map.tryFind key map with
                    | Some value -> (value, world)
                    | None -> (Violation ([!!"InvalidKey"; !!"Table"; !!(String.capitalize fnName)], "Key not found in table.", fnOriginOpt), world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTable fnOriginOpt fnName evaledArgs world =
            if List.forall (function Tuple arr when Array.length arr = 2 -> true | _ -> false) evaledArgs then
                let evaledPairs = List.map (function List [evaledFst; evaledSnd] -> (evaledFst, evaledSnd) | _ -> failwithumf ()) evaledArgs
                let evaledMap = Map.ofList evaledPairs
                (Table evaledMap, world)
            else (Violation ([!!"InvalidEntries"; !!"Table"; !!(String.capitalize fnName)], "Table entries must consist of 1 or more pairs.", fnOriginOpt), world)

        let evalSubscribe4 subscription (eventAddress : obj Address) subscriber world =
            EventWorld.subscribe<obj, Participant, Game, World> (fun evt world ->
                match World.tryGetSimulantScriptFrame subscriber world with
                | Some scriptFrame ->
                    match tryImport evt.Data evt.DataType with
                    | Some dataImported ->
                        let evtTuple =
                            Keyphrase
                                ("Event",
                                    [|dataImported
                                      String (scstring evt.Subscriber)
                                      String (scstring evt.Publisher)
                                      String (scstring evt.Address)|])
                        let application = Apply ([|subscription; evtTuple|], None)
                        World.evalWithLogging application scriptFrame subscriber world |> snd
                    | None -> Log.info "Property value could not be imported into scripting environment."; world
                | None -> world)
                eventAddress
                subscriber
                world

        let evalSubscribe fnOriginOpt fnName evaledArg evaledArg2 world =
            match evaledArg with
            | Binding _
            | Fun _ ->
                match evaledArg2 with
                | String str
                | Keyword str ->
                    let world = evalSubscribe4 evaledArg (Address.makeFromString str) (World.getScriptContext world) world
                    (Unit, world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation ([!!"InvalidArgumentType"], "Function '" + fnName + "' requires a relation for its 2nd argument.", fnOriginOpt), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation ([!!"InvalidArgumentType"], "Function '" + fnName + "' requires a function for its 1st argument.", fnOriginOpt), world)