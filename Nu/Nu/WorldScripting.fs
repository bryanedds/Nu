// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open OpenTK
open Prime
open Nu
open Nu.Scripting
open Nu.WorldScriptingMarshalling.Scripting
open Nu.WorldScriptingUnary.Scripting
open Nu.WorldScriptingBinary.Scripting
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScripting =

    type World with

        static member private getLocalDeclaration world =
            World.getScriptEnvBy EnvModule.Env.getLocalDeclaration world

        static member private setLocalDeclaration localDeclaration world =
            World.updateScriptEnv (EnvModule.Env.setLocalDeclaration localDeclaration) world

        static member private tryGetBinding name cachedBinding world =
            World.getScriptEnvBy (EnvModule.Env.tryGetBinding name cachedBinding) world

        static member private addDeclarationBinding name value world =
            World.updateScriptEnv (EnvModule.Env.addDeclarationBinding name value) world

        static member private addProceduralBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBinding appendType name value) world

        static member private addProceduralBindings appendType bindings world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBindings appendType bindings) world

        static member private addBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addBinding appendType name value) world

        static member private removeProceduralBindings world =
            World.updateScriptEnv EnvModule.Env.removeProceduralBindings world

        static member private getProceduralFrames world =
            World.getScriptEnvBy EnvModule.Env.getProceduralFrames world

        static member private setProceduralFrames proceduralFrames world =
            World.updateScriptEnv (EnvModule.Env.setProceduralFrames proceduralFrames) world

        static member internal getLocalFrame world =
            World.getScriptEnvBy EnvModule.Env.getLocalFrame world

        static member internal setLocalFrame localFrame world =
            World.updateScriptEnv (EnvModule.Env.setLocalFrame localFrame) world

    module Scripting =

        let combine originLeftOpt originRightOpt =
            match (originLeftOpt, originRightOpt) with
            | (Some originLeft, Some originRight) -> Some { Source = originLeft.Source; Start = originLeft.Start; Stop = originRight.Stop }
            | (_, _) -> None

        let evalBoolUnary fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] ->
                match evaledArg with
                | Bool bool -> (Bool (fn bool), world)
                | Violation _ as violation -> (violation, world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalUnaryInner (fns : UnaryFns) fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Bool boolValue -> (fns.Bool boolValue fnOriginOpt, world)
            | Int intValue -> (fns.Int intValue fnOriginOpt, world)
            | Int64 int64Value -> (fns.Int64 int64Value fnOriginOpt, world)
            | Single singleValue -> (fns.Single singleValue fnOriginOpt, world)
            | Double doubleValue -> (fns.Double doubleValue fnOriginOpt, world)
            | Vector2 vector2Value -> (fns.Vector2 vector2Value fnOriginOpt, world)
            | String stringValue -> (fns.String stringValue fnOriginOpt, world)
            | Keyword keywordValue -> (fns.Keyword keywordValue fnOriginOpt, world)
            | Tuple tupleValue -> (fns.Tuple tupleValue fnOriginOpt, world)
            | Keyphrase (nameValue, phraseValue) -> (fns.Keyphrase nameValue phraseValue fnOriginOpt, world)
            | Codata codataValue -> (fns.Codata codataValue fnOriginOpt, world)
            | List listValue -> (fns.List listValue fnOriginOpt, world)
            | Ring ringValue -> (fns.Ring ringValue fnOriginOpt, world)
            | Table tableValue -> (fns.Table tableValue fnOriginOpt, world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", fnOriginOpt), world)

        let evalUnary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> evalUnaryInner fns fnOriginOpt fnName evaledArg world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalBinaryInner (fns : BinaryFns) fnOriginOpt fnName evaledLeft evaledRight world =
            match (evaledLeft, evaledRight) with
            | (Bool boolLeft, Bool boolRight) -> (fns.Bool boolLeft boolRight fnOriginOpt, world)
            | (Int intLeft, Int intRight) -> (fns.Int intLeft intRight fnOriginOpt, world)
            | (Int64 int64Left, Int64 int64Right) -> (fns.Int64 int64Left int64Right fnOriginOpt, world)
            | (Single singleLeft, Single singleRight) -> (fns.Single singleLeft singleRight fnOriginOpt, world)
            | (Double doubleLeft, Double doubleRight) -> (fns.Double doubleLeft doubleRight fnOriginOpt, world)
            | (Vector2 vector2Left, Vector2 vector2Right) -> (fns.Vector2 vector2Left vector2Right fnOriginOpt, world)
            | (String stringLeft, String stringRight) -> (fns.String stringLeft stringRight fnOriginOpt, world)
            | (Keyword keywordLeft, Keyword keywordRight) -> (fns.String keywordLeft keywordRight fnOriginOpt, world)
            | (Tuple tupleLeft, Tuple tupleRight) -> (fns.Tuple tupleLeft tupleRight fnOriginOpt, world)
            | (Keyphrase (nameLeft, phraseLeft), Keyphrase (nameRight, phraseRight)) -> (fns.Keyphrase nameLeft phraseLeft nameRight phraseRight fnOriginOpt, world)
            | (Codata codataLeft, Codata codataRight) -> (fns.Codata codataLeft codataRight fnOriginOpt, world)
            | (List listLeft, List listRight) -> (fns.List listLeft listRight fnOriginOpt, world)
            | (Ring ringLeft, Ring ringRight) -> (fns.Ring ringLeft ringRight fnOriginOpt, world)
            | (Table tableLeft, Table tableRight) -> (fns.Table tableLeft tableRight fnOriginOpt, world)
            | (Violation _ as violation, _) -> (violation, world)
            | (_, (Violation _ as violation)) -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", fnOriginOpt), world)

        let evalBinary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledLeft; evaledRight] -> evalBinaryInner fns fnOriginOpt fnName evaledLeft evaledRight world                
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalDereference fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Option opt ->
                match opt with
                | Some value -> (value, world)
                | None -> (Violation ([!!"InvalidDereference"; !!"Referent"; !!"Option"; !!(String.capitalize fnName)], "Function '" + fnName + "' requires a some value.", fnOriginOpt), world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Referent"; !!(String.capitalize fnName)], "Function '" + fnName + "' requires a referent value.", fnOriginOpt), world)

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
                match evalApply [unfolder; state] fnOriginOpt world with
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [scanner; state; costate] fnOriginOpt world with
                    | (Option (Some state), world) -> evalScanWhileCodata evalApply fnOriginOpt fnName scanner state (Unfold (unfolder, costate)) world
                    | (Option None, world) -> Right (state, [], world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s scanner must return an option.", fnOriginOpt), world)
                | (Option None, world) -> Right (state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [scanner; state; elem] fnOriginOpt world with
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [scanner; Int i; state; costate] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalScaniCodata evalApply fnOriginOpt fnName (inc i) scanner state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (i, state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (i, state, states, world) elem ->
                    match evalApply [scanner; Int i; state; elem] fnOriginOpt world with
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [scanner; state; costate] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalScanCodata evalApply fnOriginOpt fnName scanner state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, [], world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, states, world) elem ->
                    match evalApply [scanner; state; elem] fnOriginOpt world with
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
                        match evalApply [scanner; state; String (string elem)] fnOriginOpt world with
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
                        match evalApply [scanner; state; elem] fnOriginOpt world with
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
                        match evalApply [scanner; state; elem] fnOriginOpt world with
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
                        match evalApply [scanner; state; elem] fnOriginOpt world with
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
                        let (state, world) = evalApply [scanner; Int i; state; String (string elem)] fnOriginOpt world
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
                        let (state, world) = evalApply [scanner; Int i; state; elem] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        list
                (List (List.rev states), world)
            | [scanner; state; Ring set] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) elem ->
                        let (state, world) = evalApply [scanner; Int i; state; elem] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        set
                (List (List.rev states), world)
            | [scanner; state; Table map] ->
                let (_, states, world) =
                    Seq.foldi (fun i (state, states, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (state, world) = evalApply [scanner; Int i; state; elem] fnOriginOpt world
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
                        let (state, world) = evalApply [scanner; state; String (string elem)] fnOriginOpt world
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
                        let (state, world) = evalApply [scanner; state; elem] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        list
                (List (List.rev states), world)
            | [scanner; state; Ring set] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) elem ->
                        let (state, world) = evalApply [scanner; state; elem] fnOriginOpt world
                        (state, state :: states, world))
                        (state, [], world)
                        set
                (List (List.rev states), world)
            | [scanner; state; Table map] ->
                let (_, states, world) =
                    Seq.fold (fun (state, states, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (state, world) = evalApply [scanner; state; elem] fnOriginOpt world
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [folder; state; costate] fnOriginOpt world with
                    | (Option (Some state), world) -> evalFoldWhileCodata evalApply fnOriginOpt fnName folder state (Unfold (unfolder, costate)) world
                    | (Option None, world) -> Right (state, world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world)
                | (Option None, world) -> Right (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [folder; state; elem] fnOriginOpt world with
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [folder; Int i; state; costate] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalFoldiCodata evalApply fnOriginOpt fnName (inc i) folder state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (i, state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (i, state, world) elem ->
                    match evalApply [folder; Int i; state; elem] fnOriginOpt world with
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
                match evalApply [unfolder; costate] fnOriginOpt world with
                | (Option (Some costate), world) ->
                    match evalApply [folder; state; costate] fnOriginOpt world with
                    | (Violation _, _) as error -> Left error
                    | (state, world) -> evalFoldCodata evalApply fnOriginOpt fnName folder state (Unfold (unfolder, costate)) world
                | (Option None, world) -> Right (state, world)
                | (Violation _, _) as error -> Left error
                | (_, world) -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s unfolder must return an option.", fnOriginOpt), world)
            | Conversion list ->
                Seq.foldWhileRight (fun (state, world) elem ->
                    match evalApply [folder; state; elem] fnOriginOpt world with
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
                        match evalApply [folder; state; String (string elem)] fnOriginOpt world with
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
                        match evalApply [folder; state; elem] fnOriginOpt world with
                        | (Option (Some state), world) -> (Right (state, world))
                        | (Option None, world) -> Left (state, world)
                        | _ -> Left (Violation ([!!"InvalidResult"; !!"Container"; !!(String.capitalize fnName)], "Function " + fnName + "'s folder must return an option.", fnOriginOpt), world))
                        (Right (state, world))
                        list
                Either.amb eir
            | [folder; state; Ring set] ->
                let eir =
                    Seq.foldWhileRight (fun (state, world) elem ->
                        match evalApply [folder; state; elem] fnOriginOpt world with
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
                        match evalApply [folder; state; elem] fnOriginOpt world with
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
            | [folder; state; String str] -> Seq.foldi (fun i (state, world) elem -> evalApply [folder; Int i; state; String (string elem)] fnOriginOpt world) (state, world) str
            | [folder; state; Codata codata] ->
                match evalFoldiCodata evalApply fnOriginOpt fnName 0 folder state codata world with
                | Right (_, state, world) -> (state, world)
                | Left error -> error
            | [folder; state; List list] -> Seq.foldi (fun i (state, world) elem -> evalApply [folder; Int i; state; elem] fnOriginOpt world) (state, world) list
            | [folder; state; Ring set] -> Seq.foldi (fun i (state, world) elem -> evalApply [folder; Int i; state; elem] fnOriginOpt world) (state, world) set
            | [folder; state; Table map] ->
                Seq.foldi (fun i (state, world) (key, value) ->
                    let elem = Tuple [|key; value|]
                    evalApply [folder; Int i; state; elem] fnOriginOpt world)
                    (state, world)
                    (Map.toList map)
            | [_; _; _] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 3 arguments required.", fnOriginOpt), world)

        let evalFold evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [folder; state; String str] -> Seq.fold (fun (state, world) elem -> evalApply [folder; state; String (string elem)] fnOriginOpt world) (state, world) str
            | [folder; state; Codata codata] ->
                match evalFoldCodata evalApply fnOriginOpt fnName folder state codata world with
                | Right (state, world) -> (state, world)
                | Left error -> error
            | [folder; state; List list] -> List.fold (fun (state, world) elem -> evalApply [folder; state; elem] fnOriginOpt world) (state, world) list
            | [folder; state; Ring set] -> Set.fold (fun (state, world) elem -> evalApply [folder; state; elem] fnOriginOpt world) (state, world) set
            | [folder; state; Table map] ->
                Map.fold (fun (state, world) key value ->
                    let elem = Tuple [|key; value|]
                    evalApply [folder; state; elem] fnOriginOpt world)
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
                let unfolder = Unfold (Fun (["state"], 1, Apply ([unfolder; Binding ("state", ref UncachedBinding, fnOriginOpt)], fnOriginOpt), false, None, fnOriginOpt), codata)
                (unfolder, world)
            | Conversion list ->
                let (mapped, world) =
                    List.fold (fun (elems, world) elem ->
                        let (elem, world) = evalApply [mapper; elem] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (Conversion (List.rev mapped), world)

        let evalMapi evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [mapper; Option opt as option] ->
                match opt with
                | Some value -> evalApply [mapper; Int 0; value] fnOriginOpt world
                | None -> (option, world)
            | [mapper; String str] ->
                let (list, world) =
                    str |>
                    Seq.foldi (fun i (elems, world) elem ->
                        let elem = String (string elem)
                        let (elem, world) = evalApply [mapper; Int i; elem] fnOriginOpt world
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
                        let (elem, world) = evalApply [mapper; Int i; elem] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list), world)
            | [mapper; Ring set] ->
                let (set, world) =
                    Seq.foldi (fun i (elems, world) elem ->
                        let (elem, world) = evalApply [mapper; Int i; elem] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring set, world)
            | [mapper; Table map] ->
                let (map, world) =
                    Seq.foldi (fun i (elems, world) (key, value) ->
                        let elem = Tuple [|key; value|]
                        let (elem, world) = evalApply [mapper; Int i; elem] fnOriginOpt world
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
                | Some value -> evalApply [mapper; value] fnOriginOpt world
                | None -> (option, world)
            | [mapper; String str] ->
                let (list, world) =
                    str |>
                    Seq.fold (fun (elems, world) elem ->
                        let elem = String (string elem)
                        let (elem, world) = evalApply [mapper; elem] fnOriginOpt world
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
                        let (elem, world) = evalApply [mapper; elem] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list), world)
            | [mapper; Ring set] ->
                let (set, world) =
                    Set.fold (fun (elems, world) elem ->
                        let (elem, world) = evalApply [mapper; elem] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring set, world)
            | [mapper; Table map] ->
                let (map, world) =
                    Map.fold (fun (elems, world) key value ->
                        let elem = Tuple [|key; value|]
                        let (elem, world) = evalApply [mapper; elem] fnOriginOpt world
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
                match evalApply [unfolder; state] fnOriginOpt world with
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
                        let application = Apply ([subscription; evtTuple], None)
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

        let rec Intrinsics =
            dictPlus
                [("=", evalBinary EqFns)
                 ("<>", evalBinary NotEqFns)
                 ("<", evalBinary LtFns)
                 (">", evalBinary GtFns)
                 ("<=", evalBinary LtEqFns)
                 (">=", evalBinary GtEqFns)
                 ("+", evalBinary AddFns)
                 ("-", evalBinary SubFns)
                 ("*", evalBinary MulFns)
                 ("/", evalBinary DivFns)
                 ("%", evalBinary ModFns)
                 ("!", evalSinglet evalDereference)
                 ("not", evalBoolUnary not)
                 ("toEmpty", evalUnary ToEmptyFns)
                 ("toIdentity", evalUnary ToIdentityFns)
                 ("toMin", evalUnary ToMinFns)
                 ("toMax", evalUnary ToMaxFns)
                 ("inc", evalUnary IncFns)
                 ("dec", evalUnary DecFns)
                 ("negate", evalUnary NegateFns)
                 ("hash", evalUnary HashFns)
                 ("pow", evalBinary PowFns)
                 ("root", evalBinary RootFns)
                 ("sqr", evalUnary SqrFns)
                 ("sqrt", evalUnary SqrtFns)
                 ("floor", evalUnary FloorFns)
                 ("ceiling", evalUnary CeilingFns)
                 ("truncate", evalUnary TruncateFns)
                 ("round", evalUnary RoundFns)
                 ("exp", evalUnary ExpFns)
                 ("log", evalUnary LogFns)
                 ("sin", evalUnary SinFns)
                 ("cos", evalUnary CosFns)
                 ("tan", evalUnary TanFns)
                 ("asin", evalUnary AsinFns)
                 ("acos", evalUnary AcosFns)
                 ("atan", evalUnary AtanFns)
                 ("length", evalUnary LengthFns)
                 ("normal", evalUnary NormalFns)
                 ("cross", evalBinary CrossFns)
                 ("dot", evalBinary DotFns)
                 ("bool", evalUnary BoolFns)
                 ("int", evalUnary IntFns)
                 ("int64", evalUnary Int64Fns)
                 ("single", evalUnary SingleFns)
                 ("double", evalUnary DoubleFns)
                 ("string", evalUnary StringFns)
                 ("v2", evalV2)
                 //("xOf", evalNOf 0) TODO
                 //("yOf", evalNOf 1) TODO
                 //("xAs", evalNAs 0) TODO
                 //("yAs", evalNAs 1) TODO
                 ("tuple", evalTuple)
                 ("pair", evalTuple)
                 ("fst", evalNth5 0)
                 ("snd", evalNth5 1)
                 ("thd", evalNth5 2)
                 ("fth", evalNth5 3)
                 ("fif", evalNth5 4)
                 ("nth", evalNth)
                 ("some", evalSome)
                 ("isNone", evalIsNone)
                 ("isSome", evalIsSome)
                 ("isEmpty", evalIsEmpty evalApply)
                 ("notEmpty", evalNotEmpty evalApply)
                 ("tryUncons", evalTryUncons evalApply)
                 ("uncons", evalUncons evalApply)
                 ("cons", evalCons)
                 ("commit", evalSinglet evalCommit)
                 ("tryHead", evalTryHead evalApply)
                 ("head", evalHead evalApply)
                 ("tryTail", evalTryTail evalApply)
                 ("tail", evalTail evalApply)
                 ("scanWhile", evalScanWhile evalApply)
                 ("scani", evalScani evalApply)
                 ("scan", evalScan evalApply)
                 ("foldWhile", evalFoldWhile evalApply)
                 ("foldi", evalFoldi evalApply)
                 ("fold", evalFold evalApply)
                 ("mapi", evalMapi evalApply)
                 ("map", evalMap evalApply)
                 //("filter", evalFilter evalApply) TODO
                 //("filteri", evalFilteri evalApply) TODO
                 ("contains", evalContains evalApply)
                 ("codata", evalDoublet evalCodata)
                 ("toCodata", evalSinglet evalToCodata)
                 ("list", evalList)
                 //("toList", evalToList) TODO
                 //("rev", evalRev) TODO
                 ("ring", evalRing)
                 //("toRing", evalToRing) TODO
                 ("add", evalCons)
                 ("remove", evalRemove)
                 ("table", evalTable)
                 //("toTable", evalToTable) TODO
                 ("tryFind", evalTryFind)
                 ("find", evalFind)
                 ("subscribe", evalDoublet evalSubscribe)
                 ("product", evalProduct)
                 ("entityExists", evalSinglet evalSimulantExists)
                 ("layerExists", evalSinglet evalSimulantExists)
                 ("screenExists", evalSinglet evalSimulantExists)
                 ("simulantExists", evalSinglet evalSimulantExists)]

        and isIntrinsic name =
            Intrinsics.ContainsKey name

        and evalIntrinsic originOpt name evaledArgs world =
            match Intrinsics.TryGetValue name with
            | (true, intrinsic) -> intrinsic originOpt name evaledArgs world
            | (false, _) -> (Violation ([!!"InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", originOpt), world)

        // TODO: ensure these origins are sensible
        and evalProduct originOpt name evaledArgs world =
            match evaledArgs with
            | [Stream (streamLeft, originLeftOpt); Stream (streamRight, originRightOpt)] ->
                match evalStream name streamLeft originLeftOpt world with
                | Right (streamLeft, world) ->
                    match evalStream name streamRight originRightOpt world with
                    | Right (streamRight, world) ->
                        let computedStream = Stream.product streamLeft streamRight
                        (Stream (ComputedStream computedStream, originOpt), world)
                    | Left violation -> (violation, world)
                | Left violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentTypes"; !!(String.capitalize name)], "Incorrect types of arguments for application of '" + name + "'; 1 relation and 1 stream required.", originOpt), world)

        and evalSimulantExists fnOriginOpt name evaledArg world =
            match evaledArg with
            | String str
            | Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDeriveSimulant address with
                | Some simulant -> (Bool (World.simulantExists simulant world), world)
                | None -> (Bool false, world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation ([!!"InvalidArgumentType"], "Function '" + name + "' requires 1 relation argument.", fnOriginOpt), world)

        and evalStream name stream originOpt world =
            match stream with
            | VariableStream variableName ->
                let context = World.getScriptContext world
                let variableAddress = ltoa [!!"Stream"; !!variableName; !!"Event"] ->>- context.SimulantAddress
                let variableStream = Stream.stream variableAddress
                Right (variableStream, world)
            | EventStream eventAddress ->
                let (eventAddressEvaled, world) = eval eventAddress world
                match eventAddressEvaled with
                | String eventAddressStr
                | Keyword eventAddressStr ->
                    try let eventAddress = Address.makeFromString eventAddressStr
                        let eventStream = Stream.stream eventAddress
                        Right (eventStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address '" + eventAddressStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address. Address must be either a string or a keyword", originOpt))
            | PropertyStream (propertyName, propertyRelation) ->
                let (propertyRelationEvaled, world) = eval propertyRelation world
                match propertyRelationEvaled with
                | String propertyRelationStr
                | Keyword propertyRelationStr ->
                    try let context = World.getScriptContext world
                        let propertyRelation = Relation.makeFromString propertyRelationStr
                        let propertyAddress = Relation.resolve context.SimulantAddress propertyRelation -<<- Address.makeFromName !!propertyName
                        let propertyStream = Stream.stream propertyAddress
                        Right (propertyStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation '" + propertyRelationStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation. Relation must be either a string or a keyword", originOpt))
            | PropertyStreamMany _ ->
                failwithnie ()
            | ComputedStream computedStream ->
                Right (computedStream :?> Prime.Stream<obj, Game, World>, world)

        and evalBinding expr name cachedBinding originOpt world =
            match World.tryGetBinding name cachedBinding world with
            | None ->
                if isIntrinsic name then (expr, world)
                else (Violation ([!!"NonexistentBinding"], "Non-existent binding '" + name + "' ", originOpt), world)
            | Some binding -> (binding, world)

        and evalApply exprs originOpt world =
            match evalMany exprs world with
            | (evaledHead :: evaledTail, world) ->
                match evaledHead with
                | Keyword keyword ->
                    let keyphrase = Keyphrase (keyword, List.toArray evaledTail)
                    (keyphrase, world)
                | Binding (name, _, originOpt) ->
                    // NOTE: we can infer we have an intrinsic when evaluation leads here
                    evalIntrinsic originOpt name evaledTail world
                | Fun (pars, parsCount, body, _, framesOpt, originOpt) ->
                    let (framesCurrentOpt, world) =
                        match framesOpt with
                        | Some frames ->
                            let framesCurrent =  World.getProceduralFrames world
                            let world = World.setProceduralFrames (frames :?> ProceduralFrame list) world
                            (Some framesCurrent, world)
                        | None -> (None, world)
                    let (evaled, world) =
                        let evaledArgs = evaledTail
                        if List.hasExactly parsCount evaledArgs then
                            let bindings = List.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                            let world = World.addProceduralBindings (AddToNewFrame parsCount) bindings world
                            let (evaled, world) = eval body world
                            (evaled, World.removeProceduralBindings world)
                        else (Violation ([!!"MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), world)
                    match framesCurrentOpt with
                    | Some framesCurrent ->
                        let world = World.setProceduralFrames framesCurrent world
                        (evaled, world)
                    | None -> (evaled, world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation ([!!"TODO: proper violation category."], "Cannot apply a non-binding.", originOpt), world)
            | ([], world) -> (Unit, world)

        and evalApplyAnd exprs originOpt world =
            match exprs with
            | [left; right] ->
                match eval left world with
                | (Bool false, world) -> (Bool false, world)
                | (Bool true, world) ->
                    match eval right world with
                    | (Bool _, _) as result -> result
                    | (Violation _, _) as error -> error
                    | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
                | (Violation _, _) as error -> error
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!"&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

        and evalApplyOr exprs originOpt world =
            match exprs with
            | [left; right] ->
                match eval left world with
                | (Bool true, world) -> (Bool true, world)
                | (Bool false, world) ->
                    match eval right world with
                    | (Bool _, _) as result -> result
                    | (Violation _, _) as error -> error
                    | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
                | (Violation _, _) as error -> error
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!"&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

        and evalLet4 binding body originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let (evaled, world) = eval body world
                    World.addProceduralBinding (AddToNewFrame 1) name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame 1) name fn world
            let (evaled, world) = eval body world
            (evaled, World.removeProceduralBindings world)

        and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
            let world =
                match bindingsHead with
                | VariableBinding (name, body) ->
                    let (bodyValue, world) = eval body world
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name fn world
            let world =
                List.foldi (fun i world binding ->
                    match binding with
                    | VariableBinding (name, body) ->
                        let (bodyValue, world) = eval body world
                        World.addProceduralBinding (AddToHeadFrame ^ inc i) name bodyValue world
                    | FunctionBinding (name, args, body) ->
                        let frames = World.getProceduralFrames world :> obj
                        let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                        World.addProceduralBinding (AddToHeadFrame ^ inc i) name fn world)
                    world
                    bindingsTail
            let (evaled, world) = eval body world
            (evaled, World.removeProceduralBindings world)
        
        and evalLet binding body originOpt world =
            evalLet4 binding body originOpt world
        
        and evalLetMany bindings body originOpt world =
            match bindings with
            | bindingsHead :: bindingsTail ->
                let bindingsCount = List.length bindingsTail + 1
                evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world
            | [] -> (Violation ([!!"MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)

        and evalFun fn pars parsCount body framesPushed framesOpt originOpt world =
            if not framesPushed then
                if Option.isNone framesOpt then
                    let frames = World.getProceduralFrames world :> obj
                    (Fun (pars, parsCount, body, true, Some frames, originOpt), world)
                else (Fun (pars, parsCount, body, true, framesOpt, originOpt), world)
            else (fn, world)

        and evalIf condition consequent alternative originOpt world =
            match eval condition world with
            | (Bool bool, world) -> if bool then eval consequent world else eval alternative world
            | (Violation _ as evaled, world) -> (evaled, world)
            | (_, world) -> (Violation ([!!"InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", originOpt), world)

        and evalMatch input (cases : (Expr * Expr) list) originOpt world =
            let (input, world) = eval input world
            let resultEir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    let (evaledInput, world) = eval condition world
                    match evalBinaryInner EqFns originOpt "=" input evaledInput world with
                    | (Bool true, world) -> Right (eval consequent world)
                    | (Bool false, world) -> Left world
                    | (Violation _, world) -> Right (evaledInput, world)
                    | _ -> failwithumf ())
                    (Left world)
                    cases
            match resultEir with
            | Right success -> success
            | Left world -> (Violation ([!!"InexhaustiveMatch"], "A match expression failed to meet any of its cases.", originOpt), world)

        and evalSelect exprPairs originOpt world =
            let resultEir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    match eval condition world with
                    | (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                    | (Violation _ as evaled, world) -> Right (evaled, world)
                    | (_, world) -> Right ((Violation ([!!"InvalidSelectCondition"], "Must provide an expression that evaluates to a bool in a case condition.", originOpt), world)))
                    (Left world)
                    exprPairs
            match resultEir with
            | Right success -> success
            | Left world -> (Violation ([!!"InexhaustiveSelect"], "A select expression failed to meet any of its cases.", originOpt), world)

        and evalTry body handlers _ world =
            match eval body world with
            | (Violation (categories, _, _) as evaled, world) ->
                match
                    List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                        let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                        if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                        (Left world)
                        handlers with
                | Right success -> success
                | Left world -> (evaled, world)
            | success -> success

        and evalDo exprs _ world =
            let evaledEir =
                List.foldWhileRight (fun (_, world) expr ->
                    match eval expr world with
                    | (Violation _, _) as error -> Left error
                    | success -> Right success)
                    (Right (Unit, world))
                    exprs
            Either.amb evaledEir

        and evalBreak expr world =
            // TODO: write all procedural bindings to console
            Debugger.Break ()
            eval expr world

        and evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match eval relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt), world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match tryImport propertyValue propertyType with
                    | Some propertyValue -> (propertyValue, world)
                    | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        and evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match eval relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt), world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    let (propertyValue, world) = eval propertyValueExpr world
                    match tryExport propertyValue propertyType with
                    | Some propertyValue ->
                        match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                        | (true, world) -> (Unit, world)
                        | (false, world) -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
                    | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        and evalDefine binding originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let (evaled, world) = eval body world
                    World.addDeclarationBinding name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addDeclarationBinding name fn world
            (Unit, world)

        and eval expr world : Expr * World =
            match expr with
            | Violation _ -> (expr, world)
            | Unit _ -> (expr, world)
            | Bool _ -> (expr, world)
            | Int _ -> (expr, world)
            | Int64 _ -> (expr, world)
            | Single _ -> (expr, world)
            | Double _ -> (expr, world)
            | Vector2 _ -> (expr, world)
            | String _ -> (expr, world)
            | Keyword _ -> (expr, world)
            | Tuple _ -> (expr, world)
            | Keyphrase _ -> (expr, world)
            | Option _ -> (expr, world)
            | Codata _ -> (expr, world)
            | List _ -> (expr, world)
            | Ring _ -> (expr, world)
            | Table _ -> (expr, world)
            | Stream _ -> (expr, world)
            | Binding (name, cachedBinding, originOpt) as expr -> evalBinding expr name cachedBinding originOpt world
            | Apply (exprs, originOpt) -> evalApply exprs originOpt world
            | ApplyAnd (exprs, originOpt) -> evalApplyAnd exprs originOpt world
            | ApplyOr (exprs, originOpt) -> evalApplyOr exprs originOpt world
            | Let (binding, body, originOpt) -> evalLet binding body originOpt world
            | LetMany (bindings, body, originOpt) -> evalLetMany bindings body originOpt world
            | Fun (pars, parsCount, body, framesPushed, framesOpt, originOpt) as fn -> evalFun fn pars parsCount body framesPushed framesOpt originOpt world
            | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt world
            | Match (input, cases, originOpt) -> evalMatch input cases originOpt world
            | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt world
            | Try (body, handlers, originOpt) -> evalTry body handlers originOpt world
            | Do (exprs, originOpt) -> evalDo exprs originOpt world
            | Break (expr, _) -> evalBreak expr world
            | Get (name, originOpt) -> evalGet name None originOpt world
            | GetFrom (name, expr, originOpt) -> evalGet name (Some expr) originOpt world
            | Set (name, expr, originOpt) -> evalSet name None expr originOpt world
            | SetTo (name, expr, expr2, originOpt) -> evalSet name (Some expr) expr2 originOpt world
            | Quote _ -> failwithnie ()
            | Define (binding, originOpt) -> evalDefine binding originOpt world

        and evalMany exprs world =
            let (evaledsRev, world) =
                List.fold
                    (fun (evaleds, world) expr ->
                        let (evaled, world) = eval expr world
                        (evaled :: evaleds, world))
                    ([], world)
                    exprs
            (List.rev evaledsRev, world)