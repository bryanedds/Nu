// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Aml
open System
open System.IO
open System.Collections.Generic
open FParsec.CharParsers
open Prime
open Aml.Ast
open Aml.AstModule
open Aml.AmlConstants
open Aml.Primitives
open Aml.Initial
open Aml.Writer
open Aml.Conversions
open Aml.Reader
open Aml.EvaluatorPrims

[<AutoOpen>]
module EvaluatorModule =

    /// The status of a conditional expression once evaluated.
    type [<ReferenceEquality>] ConditionalStatus =
        | CSViolation of Expr
        | CSNormal
        | CSNotBoolean

module Evaluator =

    /// Check a contract.
    let rec checkContract args argCount largs largCount contract env =
#if AML_OPTIMIZED
        let _ = (args, argCount, largs, largCount, contract, env)
        (true, None)
#else
        if isUnit contract then (true, None)
        else
            let result = applyLambda args argCount largs largCount contract tautology UnitValue UnitValue contract env
            match result.Value with
            | Violation _ as v -> (false, Some v)
            | Boolean boolean -> (boolean.BRValue, None)
            | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.contractSatisfied'."
#endif

    /// Intervene on a violation.
    and interveneOnViolation violation vioExpr env =
        let initialResultValue = vioExpr
        let interventionBranches = Seq.concat env.EnvInterventionBranchLists // OPTIMIZATION: lazy for speed
        // OPTIMIZATION: uses imperative code to exit processing early
        // TODO: consider factoring Seq.foldUntil out of this
        let mutable intervening = true
        let mutable branchEnumerator = interventionBranches.GetEnumerator ()
        let mutable currentResultValue = initialResultValue
        while branchEnumerator.MoveNext () && intervening do
            let branch = branchEnumerator.Current
            if isInCategory violation.VioCategory branch.IBCategory then
                let env =
                    match branch.IBEnv with
                    | Some env -> env
                    | None -> failwith "Unexpected match failure in 'Aml.Evaluator.interveneOnViolation'."
                let env = appendProceduralVariable (AppendToNewFrame 2) ProblemStr None vioExpr env
                let env = appendProceduralVariable (AppendToHeadFrame 1) DataStr None violation.VioData env
                if branch.IBHide then intervening <- false
                currentResultValue <- evalExprDropEnv branch.IBBody env
        makeEvalResult currentResultValue env

    /// Make a violation from a category and a message during evaluation, intervening if necessary.
    and makeEvalViolation category message env =
        let violation = makeViolationWithPositions category message env
        match violation with
        | Violation v -> interveneOnViolation v violation env
        | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.makeEvalViolation'."

    /// Make a first-class violation during evaluation.
    and makeEvalFirstClassViolation name env =
        makeEvalViolation ":v/eval/notFirstClass" ("Not a first-class value '" + name + "'.") env

    /// Make a an exception violation during evaluation.
    and makeEvalExceptionViolation (exn : Exception) env =
        makeEvalViolation ":v/exception" (xstring exn) env
    
    /// Forward a violation from a str during evaluation.
    and forwardEvalViolation violation env =
        makeEvalResult violation env

    /// Declare an equatable protocol.
    and declareEquatable env =
        let equatableProtocol =
            makeProtocolRecord
                EquatableStr
                AStr
                None
                (makeDoc "Enables testing for equality and inequality.")
                [makeSignature EqualityStr [makeArg AStr Concrete UnitValue; makeArg AStr Concrete UnitValue] (makeDoc "Query for semantic equality.")
                 makeSignature InequalityStr [makeArg AStr Concrete UnitValue; makeArg AStr Concrete UnitValue] (makeDoc "Query for semantic inequality.")]
                None
        let result = evalProtocol equatableProtocol env
        if isViolation result.Value then failwith "Could not declare equatable protocol."
        result.Env

    /// Instantiate a type under the equatable protocol.
    and instantiateEquatable typeName env =
        let xArg = makeArg XStr Concrete UnitValue
        let xSymbol = Symbol (makeSymbolRecord XStr (ref CEUncached) None)
        let xConstraint = makeConstraint typeName [XStr]
        let yArg = makeArg YStr Concrete UnitValue
        let ySymbol = Symbol (makeSymbolRecord YStr (ref CEUncached) None)
        let yConstraint = makeConstraint typeName [YStr]
        let equalSymbol = Symbol (makeSymbolRecord EqualStr (ref CEUncached) None)
        let equalBody = Series (makeSeriesRecord [equalSymbol; xSymbol; ySymbol] 3 None)
        let equalFunction = Function (makeFunctionRecord EqualityStr [xArg; yArg] 2 equalBody None None UnitValue UnitValue true None)
        let inequalSymbol = Symbol (makeSymbolRecord InequalStr (ref CEUncached) None)
        let inequalBody = Series (makeSeriesRecord [inequalSymbol; xSymbol; ySymbol] 3 None)
        let inequalFunction = Function (makeFunctionRecord InequalityStr [xArg; yArg] 2 inequalBody None None UnitValue UnitValue true None)
        let instance = makeInstanceRecord EquatableProtocolStr [XStr; YStr] [xConstraint; yConstraint] [equalFunction; inequalFunction] None
        let instanceExpr = Instance instance
        let result = evalInstance instance instanceExpr env
        if isViolation result.Value then failwith "Could not instantiate type over equatable protocol."
        result.Env

    /// Project a signature implementation to a concrete value.
    and projectSigImpl sigImpl env =
        match sigImpl with
        | Variable variable -> (variable.VarName, evalExprDropEnv variable.VarBody env)
        | Function fn -> (fn.FnName, Lambda (makeLambdaRecord false fn.FnName fn.FnArgs fn.FnArgCount fn.FnBody tautology fn.FnPre fn.FnPost fn.FnEmptyUnification fn.FnOptPositions (Some env)))
        | _ -> failwith "Unexpected match failure in Aml.Evaluator.Prims.sigImplToNamedExpr."

    /// Project a list of signature implementations to concrete values.
    and projectSigImpls sigImpls env =
        List.map (fun sigImpl -> projectSigImpl sigImpl env) sigImpls

    /// Apply an unary operator.
    and applyUnop exprToOptNumber numberToExpr unop (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedUnop" "Unop must have 1 argument." env
        else
            let argValue = evalExprDropEnv args.Head env
            let optNumber : 'a option = exprToOptNumber argValue
            match optNumber with
            | Some number -> makeEvalResult (numberToExpr (unop number)) env
            | None ->
                match argValue with
                | Violation _ as v -> forwardEvalViolation v env
                | _ -> makeEvalViolation ":v/contract/invalidUnopArgumentType" "Unop must have an argument of the correct type." env

    /// Apply a binary operator.
    and applyBinop allExprsToNumbers numberToExpr binop isZero checkForDivisionByZero (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedBinop" "Binop must have 2 arguments." env
        else
            let argValues = evalExprsDropEnv args env
            let numbers : 'a list = allExprsToNumbers argValues
            if numbers.IsEmpty then
                if anyViolations argValues then forwardEvalViolation (firstViolation argValues) env
                else makeEvalViolation ":v/contract/invalidBinopArgumentType" "Binop must have both arguments of the correct type." env
            else
                if not (checkForDivisionByZero && isZero numbers.[1]) then makeEvalResult (numberToExpr (List.reduce binop numbers)) env
                else makeEvalViolation ":v/contract/divByZero" "Division by zero." env

    /// Apply a comparator.
    and applyComparator comparator (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedComparator" "Comparator must have 2 arguments." env
        else
            let argValues = evalExprsDropEnv args env
            let comparables : IComparable list = allExprsToComparables argValues
            if comparables.IsEmpty then
                if anyViolations argValues then forwardEvalViolation (firstViolation argValues) env
                else makeEvalViolation ":v/eval/invalidComparatorForm" "Invalid comparator form." env
            else
                let compareResult = comparator comparables.[0] comparables.[1]
                makeEvalResult (Boolean (makeBooleanRecord compareResult None)) env

    /// Apply an and operator.
    and applyAnd (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedAndOperation" "And operation must have 2 arguments." env
        else
            let xValue = evalExprDropEnv args.[0] env
            match xValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Boolean xBoolean ->
                let xBool = xBoolean.BRValue
                if not xBool then makeEvalResult FalseValue env
                else
                    let yValue = evalExprDropEnv args.[1] env
                    match yValue with
                    | Violation _ as v -> forwardEvalViolation v env
                    | Boolean yBoolean ->
                        let yBool = yBoolean.BRValue
                        makeEvalResult (Boolean (makeBooleanRecord (xBool && yBool) None)) env // I would prefer that the short-circuiting && not be used...
                    | _ -> makeEvalViolation ":v/contract/invalidAndArgumentType" "And expression requires both argumements to be boolean." env
            | _ -> makeEvalViolation ":v/contract/invalidAndArgumentType" "And expression requires both argumements to be boolean." env

    /// Apply an or operator.
    and applyOr (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedOrOperation" "Or operation must have 2 arguments." env
        else
            let xValue = evalExprDropEnv args.[0] env
            match xValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Boolean xBoolean ->
                let xBool = xBoolean.BRValue
                if xBool then makeEvalResult TrueValue env
                else
                    let yValue = evalExprDropEnv args.[1] env
                    match yValue with
                    | Violation _ as v -> forwardEvalViolation v env
                    | Boolean yBoolean -> makeEvalResult (Boolean yBoolean) env
                    | _ -> makeEvalViolation ":v/contract/invalidOrArgumentType" "Or expression requires both argumements to be boolean." env
            | _ -> makeEvalViolation ":v/contract/invalidOrArgumentType" "Or expression requires both argumements to be boolean." env

    /// Apply an if operator.
    and applyIf (_ : string) (args : Expr list) argCount env =
        if argCount <> 3 then makeEvalViolation ":v/eval/malformedIfOperation" "If operation must have 3 arguments." env
        else
            let condition = evalExprDropEnv args.Head env
            match condition with
            | Violation _ as v -> forwardEvalViolation v env
            | Boolean boolean -> if boolean.BRValue then evalExpr args.[1] env else evalExpr args.[2] env
            | _ -> makeEvalViolation ":v/contract/invalidIfCondition" "Condition part of if operator must result in a boolean." env

    /// Apply the apply operator.
    and applyApply (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedApplyOperation" "Apply operation must have 2 arguments." env
        else
            let op = args.[0]
            let argList = evalExprDropEnv args.[1] env
            match argList with
            | List list -> let series = Series (makeSeriesRecord (op :: list.ListElements) (list.ListElements.Length + 1) None) in evalExpr series env
            | _ -> makeEvalViolation ":v/contract/invalidApplyArgument" "Apply requires a list as its argument." env

    /// Apply the doc operator.
    and applyDoc (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedDocOperation" "Doc operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | Keyword keyword ->
                let entryName =
                    if keyword.KRValue.StartsWith SimpleEntryPrefixStr
                    then keyword.KRValue.Substring SimpleEntryPrefixStr.Length
                    else keyword.KRValue
                let optEntry = tryFindEntry entryName env
                match optEntry with
                | Some entry ->
                    match entry with
                    | ValueEntry (_, doc) ->
                        match doc with
                        | Some str -> makeEvalResult (String (makeStringRecord str None)) env
                        | None -> makeEvalViolation ":v/eval/invalidDocOperation" "Documentation missing for entry." env
                    | DynamicEntry (_, doc) ->
                        match doc with
                        | Some str -> makeEvalResult (String (makeStringRecord str None)) env
                        | None -> makeEvalViolation ":v/eval/invalidDocOperation" "Documentation missing for entry." env
                    | TypeEntry (_, _, doc) ->
                        match doc with
                        | Some str -> makeEvalResult (String (makeStringRecord str None)) env
                        | None -> makeEvalViolation ":v/eval/invalidDocOperation" "Documentation missing for entry." env
                    | ProtocolEntry (_, _, doc, _) ->
                        match doc with
                        | Some str -> makeEvalResult (String (makeStringRecord str None)) env
                        | None -> makeEvalViolation ":v/eval/invalidDocOperation" "Documentation missing for entry." env
                | None -> makeEvalViolation ":v/eval/invalidDocOperation" "Entry not found for doc operation." env
            | _ -> makeEvalViolation ":v/eval/invalidDocParameter" "Doc operation requires a keyword argument." env

    /// Apply a type selector.
    and applyType (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedTypeOperation" "Type operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | _ -> let aType = getType value env in makeEvalResult aType env
        
    /// Apply a typeOf operator.
    and applyTypeOf (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedTypeOfOperation" "TypeOf operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | Keyword keyword ->
                let typeName = keyword.KRValue
                let optType = tryFindType typeName env
                match optType with
                | Some (_, aType, _) -> makeEvalResult aType env
                | None -> makeEvalViolation ":v/eval/nonexistantType" ("Could not find type '" + typeName + "'.") env
            | _ -> makeEvalViolation ":v/eval/invalidTypeOfArgumentType" "TypeOf operation requires a keyword argument." env

    /// Apply a structural equal query.
    and applyEqual (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            let secondValue = evalExprDropEnv args.[1] env
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation v env
            | (_, (Violation _ as v)) -> forwardEvalViolation v env
            | _ -> makeEvalResult (Boolean (makeBooleanRecord (firstValue = secondValue) None)) env
    
    /// Apply a structural inequal query.
    and applyInequal (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedInequalOperation" "Inequal operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            let secondValue = evalExprDropEnv args.[1] env
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation v env
            | (_, (Violation _ as v)) -> forwardEvalViolation v env
            | _ -> makeEvalResult (Boolean (makeBooleanRecord (firstValue <> secondValue) None)) env

    /// Apply a ref equality query.
    and applyRefEqual (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            let secondValue = evalExprDropEnv args.[1] env
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation v env
            | (_, (Violation _ as v)) -> forwardEvalViolation v env
            | (Ref _, Ref _) -> makeEvalResult (Boolean (makeBooleanRecord (firstValue === secondValue) None)) env
            | (Ref _, _) -> makeEvalViolation ":v/contract/refEqualityOnNonRef" "Second argument of reference equal operation must be a reference." env
            | (_, Ref _) -> makeEvalViolation ":v/contract/refEqualityOnNonRef" "First argument of reference equal operation must be a reference." env
            | _ -> makeEvalViolation ":v/contract/refEqualityOnNonRef" "Both arguments of reference equal operation must be references." env

    /// Apply a ref inequality query.
    and applyRefInequal (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            let secondValue = evalExprDropEnv args.[1] env
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation v env
            | (_, (Violation _ as v)) -> forwardEvalViolation v env
            | (Ref _, Ref _) -> makeEvalResult (Boolean (makeBooleanRecord (firstValue <<>> secondValue) None)) env
            | (Ref _, _) -> makeEvalViolation ":v/contract/refInequalityOnNonRef" "Second argument of reference inequal operation must be a reference." env
            | (_, Ref _) -> makeEvalViolation ":v/contract/refInequalityOnNonRef" "First argument of reference inequal operation must be a reference." env
            | _ -> makeEvalViolation ":v/contract/refInequalityOnNonRef" "Both arguments of reference inequal operation must be references." env
    
    /// Apply a cons operation.
    and applyCons (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedConsOperation" "Cons operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            match firstValue with
            | Violation _ as v -> forwardEvalViolation v env
            | _ ->
                let secondValue = evalExprDropEnv args.[1] env
                match secondValue with
                | Violation _ as v -> forwardEvalViolation v env
                | List list -> makeEvalResult (List (makeListRecord true (firstValue :: list.ListElements) None)) env
                | Series series when series.SerExprs.IsEmpty -> makeEvalResult (List (makeListRecord true [firstValue] None)) env
                | _ -> makeEvalViolation ":v/contract/consToNonList" "Cannot cons to a non-list." env
    
    /// Apply a head operation.
    and applyHead (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedHeadOperation" "Head operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | List list when list.ListElements.IsEmpty -> makeEvalViolation ":v/contract/headOfEmptyList" "Cannot take the head of an empty list." env
            | List list -> makeEvalResult list.ListElements.Head env
            | _ -> makeEvalViolation ":v/contract/headOfNonList" "Cannot take the head of a non-list." env
    
    /// Apply a tail operation.
    and applyTail (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedTailOperation" "Tail operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | List list when list.ListElements.IsEmpty -> makeEvalViolation ":v/contract/tailOfEmptyList" "Cannot take the tail of an empty list." env
            | List list -> makeEvalResult (List (makeListRecord true list.ListElements.Tail None)) env
            | _ -> makeEvalViolation ":v/contract/tailOfNonList" "Cannot take the tail of a non-list." env
    
    /// Apply a string length query.
    and applyStringLength (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedStringLengthOperation" "StringLength operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | String string -> makeEvalResult (Int (makeIntRecord string.SRValue.SVValue.Length None)) env
            | _ -> makeEvalViolation ":v/contract/stringLengthOfNonString" "Cannot get the string length of a non-string." env
    
    /// Apply a string append.
    and applyStringAppend (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedStringAppendOperation" "StringAppend operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            match firstValue with
            | Violation _ as v -> forwardEvalViolation v env
            | String string ->
                let secondValue = evalExprDropEnv args.[1] env
                match secondValue with
                | Violation _ as v2 -> forwardEvalViolation v2 env
                | String string2 -> makeEvalResult (String (makeStringRecord (makeLiteralStringValue (string.SRValue.SVValue + string2.SRValue.SVValue)) None)) env
                | _ -> makeEvalViolation ":v/contract/stringAppendOfNonString" "String appending requires 2 strings as arguments." env
            | _ -> makeEvalViolation ":v/contract/stringAppendOfNonString" "String appending requires 2 strings as arguments." env
    
    /// Apply a list length query.
    and applyListLength (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedListLengthOperation" "ListLength operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | List list -> makeEvalResult (Int (makeIntRecord list.ListElements.Length None)) env
            | _ -> makeEvalViolation ":v/contract/listLengthOfNonList" "Cannot get the list length of a non-list." env

    /// Apply a list append.
    and applyListAppend (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedListAppendOperation" "ListAppend operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            let secondValue = evalExprDropEnv args.[1] env
            match firstValue with
            | Violation _ as v -> forwardEvalViolation v env
            | List firstList ->
                match secondValue with
                | Violation _ as v -> forwardEvalViolation v env
                | List secondList -> makeEvalResult (List (makeListRecord true (firstList.ListElements @ secondList.ListElements) None)) env
                | Series secondSeries when secondSeries.SerExprs.IsEmpty -> makeEvalResult (List (makeListRecord true firstList.ListElements None)) env
                | _ -> makeEvalViolation ":v/contract/listAppendOfNonList" "Cannot append a list to a non-list." env
            | Series firstSeries when firstSeries.SerExprs.IsEmpty ->
                match secondValue with
                | Violation _ as v -> forwardEvalViolation v env
                | List secondList -> makeEvalResult (List (makeListRecord true secondList.ListElements None)) env
                | Series secondSeries when secondSeries.SerExprs.IsEmpty -> makeEvalUnit env
                | _ -> makeEvalViolation ":v/contract/listAppendOfNonList" "Cannot append a list to a non-list." env
            | _ -> makeEvalViolation ":v/contract/listAppendOfNonList" "Cannot append a non-list to a list." env

    /// Apply an array length query.
    and applyArrayLength (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedArrayLengthOperation" "ArrayLength operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | Array array -> makeEvalResult (Int (makeIntRecord array.ArrElements.Length None)) env
            | _ -> makeEvalViolation ":v/contract/arrayLengthOfNonArray" "Cannot get the array length of a non-array." env

    /// Apply an array append.
    and applyArrayAppend (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedArrayAppendOperation" "ArrayLength operation must have 2 arguments." env
        else
            let firstValue = evalExprDropEnv args.[0] env
            match firstValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Array array ->
                let secondValue = evalExprDropEnv args.[1] env
                match secondValue with
                | Violation _ as v -> forwardEvalViolation v env
                | Array array2 -> makeEvalResult (Array (makeArrayRecord true (Array.append array.ArrElements array2.ArrElements) None)) env
                | _ -> makeEvalViolation ":v/contract/arrayAppendOfNonArray" "Array appending requires two arrays as arguments." env
            | _ -> makeEvalViolation ":v/contract/arrayAppendOfNonArray" "Array appending requires two arrays as arguments." env

    /// Apply a steps operation.
    and applySteps (_ : string) args _ env =
        match args with
        | [] -> makeEvalViolation ":v/eval/malformedStepsOperation" "Steps operation must have at least one argument." env
        | _ -> let results = sequentiallyEvalExprs args false env in List.last results

    /// Apply a while operation.
    and applyWhile (_ : string) args _ env =
        match args with
        | [condition; statement] ->
            let mutable conditionResult = evalConditional condition env
            let mutable statementResult = makeEvalUnit env
            while fst conditionResult &&
                  snd conditionResult = CSNormal &&
                  not (isViolation statementResult.Value) do
                  statementResult <- evalExpr statement env
                  conditionResult <- evalConditional condition env
            match snd conditionResult with
            | CSViolation v -> forwardEvalViolation v env
            | CSNormal -> statementResult
            | CSNotBoolean -> makeEvalViolation ":v/contract/invalidWhileCondition" "The result of a while condition must be a boolean value." env
        | _ -> makeEvalViolation ":v/eval/malformedStepsOperation" "While operation must have two arguments." env

    /// Apply a type query with a predicate.
    and applyIsPred pred (_ : string) (args : Expr list) argCount env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedAstPredicateOperation" "AST predicate operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            match value with
            | Violation _ as v -> forwardEvalViolation v env
            | _ -> let resultValue = Boolean (makeBooleanRecord (pred value) None) in makeEvalResult resultValue env

    /// Apply a unit query.
    and applyIsUnit name (args : Expr list) argCount env = applyIsPred isUnit name args argCount env

    /// Apply a boolean query.
    and applyIsBoolean name (args : Expr list) argCount env = applyIsPred isBoolean name args argCount env

    /// Apply a character query.
    and applyIsCharacter name (args : Expr list) argCount env = applyIsPred isCharacter name args argCount env

    /// Apply a string query.
    and applyIsString name (args : Expr list) argCount env = applyIsPred isString name args argCount env

    /// Apply an int query.
    and applyIsInt name (args : Expr list) argCount env = applyIsPred isInt name args argCount env

    /// Apply an long query.
    and applyIsLong name (args : Expr list) argCount env = applyIsPred isLong name args argCount env

    /// Apply a float query.
    and applyIsFloat name (args : Expr list) argCount env = applyIsPred isFloat name args argCount env

    /// Apply a double query.
    and applyIsDouble name (args : Expr list) argCount env = applyIsPred isDouble name args argCount env

    /// Apply a keyword query.
    and applyIsKeyword name (args : Expr list) argCount env = applyIsPred isKeyword name args argCount env

    /// Apply a package query.
    and applyIsPackage name (args : Expr list) argCount env = applyIsPred isPackage name args argCount env

    /// Apply a lambda query.
    and applyIsLambda name (args : Expr list) argCount env = applyIsPred isLambda name args argCount env

    /// Apply a list query.
    and applyIsList name (args : Expr list) argCount env = applyIsPred isList name args argCount env

    /// Apply a array query.
    and applyIsArray name (args : Expr list) argCount env = applyIsPred isArray name args argCount env

    /// Apply a composite query.
    and applyIsComposite name (args : Expr list) argCount env = applyIsPred isComposite name args argCount env

    /// Apply a has type query.
    and applyHasType (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedHasTypeOperation" "HasType operation must have 2 arguments." env
        else
            let typeNameValue = evalExprDropEnv args.[0] env
            match typeNameValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Keyword keyword ->
                let typeName = keyword.KRValue
                let value = evalExprDropEnv args.[1] env
                match value with
                | Violation _ as v -> forwardEvalViolation v env
                | _ -> let resultValue = hasType typeName value env in makeEvalResult (Boolean (makeBooleanRecord resultValue None)) env
            | _ -> makeEvalViolation ":v/contract/typeLookupWithNonKeyword" "Types can only be looked up with a keyword." env

    /// Apply a has protocol query.
    and applyHasProtocol (_ : string) (args : Expr list) argCount env =
        if argCount <> 2 then makeEvalViolation ":v/eval/malformedHasProtocolOperation" "HasProtocol operation must have 2 arguments." env
        else
            let protocolNameValue = evalExprDropEnv args.[0] env
            match protocolNameValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Keyword keyword ->
                let protocolName = keyword.KRValue
                let value = evalExprDropEnv args.[1] env
                match value with
                | Violation _ as v -> forwardEvalViolation v env
                | _ -> let result = hasProtocol protocolName value env in makeEvalResult (Boolean (makeBooleanRecord result None)) env
            | _ -> makeEvalResult FalseValue env

    /// Apply a conversion from one primitive type to another.
    and applyConversion (converter : Expr -> Expr option) (args : Expr list) argCount typeName env =
        if argCount <> 1 then makeEvalViolation ":v/eval/malformedConversionOperation" "Conversion operation must have 1 argument." env
        else
            let value = evalExprDropEnv args.Head env
            let optConvertedValue = converter value
            match optConvertedValue with
            | Some convertedValue -> makeEvalResult convertedValue env
            | None ->
                match value with
                | Violation _ as v -> forwardEvalViolation v env
                | _ -> makeEvalViolation ":v/contract/invalidConversionType" ("Value must be of type '" + typeName + "' for this conversion.") env

    /// Apply a char to int conversion.
    and applyCharToInt (_ : string) (args : Expr list) argCount env = applyConversion (function | Character c -> Some (Int (makeIntRecord (int c.CRValue) None)) | _ -> None) args argCount CharacterStr env

    /// Apply an int to char conversion.
    and applyIntToChar (_ : string) (args : Expr list) argCount env = applyConversion (function | Int i -> Some (Character (makeCharacterRecord (char i.IRValue) None)) | _ -> None) args argCount IntStr env

    /// Apply an int to long conversion.
    and applyIntToLong (_ : string) (args : Expr list) argCount env = applyConversion (function | Int i -> Some (Long (makeLongRecord (int64 i.IRValue) None)) | _ -> None) args argCount IntStr env

    /// Apply a long to int conversion.
    and applyLongToInt (_ : string) (args : Expr list) argCount env = applyConversion (function | Long l -> Some (Int (makeIntRecord (int l.GRValue) None)) | _ -> None) args argCount LongStr env

    /// Apply a float to double conversion.
    and applyFloatToDouble (_ : string) (args : Expr list) argCount env = applyConversion (function | Float f -> Some (Double (makeDoubleRecord (float f.FRValue) None)) | _ -> None) args argCount FloatStr env

    /// Apply a double to float conversion.
    and applyDoubleToFloat (_ : string) (args : Expr list) argCount env = applyConversion (function | Double d -> Some (Float (makeFloatRecord (single d.DRValue) None)) | _ -> None) args argCount DoubleStr env

    /// Apply an int to float conversion.
    and applyIntToFloat (_ : string) (args : Expr list) argCount env = applyConversion (function | Int i -> Some (Float (makeFloatRecord (single i.IRValue) None)) | _ -> None) args argCount IntStr env

    /// Apply a float to int conversion.
    and applyFloatToInt (_ : string) (args : Expr list) argCount env = applyConversion (function | Float f -> Some (Int (makeIntRecord (int f.FRValue) None)) | _ -> None) args argCount FloatStr env

    /// Apply a long to double conversion.
    and applyLongToDouble (_ : string) (args : Expr list) argCount env = applyConversion (function | Long l -> Some (Double (makeDoubleRecord (float l.GRValue) None)) | _ -> None) args argCount LongStr env

    /// Apply a double to long conversion.
    and applyDoubleToLong (_ : string) (args : Expr list) argCount env = applyConversion (function | Double d -> Some (Long (makeLongRecord (int64 d.DRValue) None)) | _ -> None) args argCount DoubleStr env

    /// Apply a string to array conversion.
    and applyStringToArray (_ : string) (args : Expr list) argCount env = applyConversion (function | String s -> Some (stringToArray s.SRValue.SVValue env) | _ -> None) args argCount StringStr env

    /// Apply an array to string conversion.
    and applyArrayToString (_ : string) (args : Expr list) argCount env = applyConversion (function | Array a -> Some (arrayToString a env) | _ -> None) args argCount ArrayStr env

    /// Apply a list to array conversion.
    and applyListToArray (_ : string) (args : Expr list) argCount env = applyConversion (function | List l -> Some (listToArray l env) | _ -> None) args argCount ListStr env

    /// Apply an array to list conversion.
    and applyArrayToList (_ : string) (args : Expr list) argCount env = applyConversion (function | Array a -> Some (arrayToList a env) | _ -> None) args argCount ArrayStr env

    /// Apply an int unop.
    and applyIntUnop intUnop (_ : string) args argCount env = applyUnop exprToOptIntValue (fun i -> Int (makeIntRecord i None)) intUnop args argCount env

    /// Apply a long unop.
    and applyLongUnop longUnop (_ : string) args argCount env = applyUnop exprToOptLongValue (fun l -> Long (makeLongRecord l None)) longUnop args argCount env

    /// Apply a float unop.
    and applyFloatUnop floatUnop (_ : string) args argCount env = applyUnop exprToOptFloatValue (fun f -> Float (makeFloatRecord f None)) floatUnop args argCount env

    /// Apply a double unop.
    and applyDoubleUnop doubleUnop (_ : string) args argCount env = applyUnop exprToOptDoubleValue (fun d -> Double (makeDoubleRecord d None)) doubleUnop args argCount env

    /// Apply an int binop.
    and applyIntBinop intBinop opName args argCount env = applyBinop allExprsToIntValues (fun i -> Int (makeIntRecord i None)) intBinop (fun i -> i = 0) (usesDivisionOperation opName) args argCount env

    /// Apply a long binop.
    and applyLongBinop longBinop opName args argCount env = applyBinop allExprsToLongValues (fun l -> Long (makeLongRecord l None)) longBinop (fun l -> l = 0L) (usesDivisionOperation opName) args argCount env

    /// Apply a float binop.
    and applyFloatBinop floatBinop (_ : string) args argCount env = applyBinop allExprsToFloatValues (fun f -> Float (makeFloatRecord f None)) floatBinop absurdity false args argCount env

    /// Apply a double binop.
    and applyDoubleBinop doubleBinop (_ : string) args argCount env = applyBinop allExprsToDoubleValues (fun d -> Double (makeDoubleRecord d None)) doubleBinop absurdity false args argCount env

    /// Apply a special builtin operation.
    and applySpecialBuiltin name args argCount env =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.ApplySpecialBuiltin name args argCount env
        | None -> makeEvalViolation ":v/eval/invalidBuiltinOperator" "Built-in operator not found." env

    /// The appliable built-in lambdas.
    and appliableBuiltins : (string * (string -> Expr list -> int -> Env -> EvalResult)) list =
        [(FloatFloorStr, applyFloatUnop floatFloor)
         (FloatCeilingStr, applyFloatUnop floatCeiling)
         (FloatTruncateStr, applyFloatUnop floatTruncate)
         (FloatRoundStr, applyFloatUnop floatRound)
         (FloatExpStr, applyFloatUnop floatExp)
         (FloatLogStr, applyFloatUnop floatLog)
         (FloatSqrtStr, applyFloatUnop floatSqrt)
         (FloatSinStr, applyFloatUnop floatSin)
         (FloatCosStr, applyFloatUnop floatCos)
         (FloatTanStr, applyFloatUnop floatTan)
         (FloatAsinStr, applyFloatUnop floatAsin)
         (FloatAcosStr, applyFloatUnop floatAcos)
         (FloatAtanStr, applyFloatUnop floatAtan)
         (DoubleFloorStr, applyDoubleUnop Math.Floor)
         (DoubleCeilingStr, applyDoubleUnop Math.Ceiling)
         (DoubleTruncateStr, applyDoubleUnop Math.Truncate)
         (DoubleRoundStr, applyDoubleUnop Math.Round)
         (DoubleExpStr, applyDoubleUnop Math.Exp)
         (DoubleLogStr, applyDoubleUnop Math.Log)
         (DoubleSqrtStr, applyDoubleUnop Math.Sqrt)
         (DoubleSinStr, applyDoubleUnop Math.Sin)
         (DoubleCosStr, applyDoubleUnop Math.Cos)
         (DoubleTanStr, applyDoubleUnop Math.Tan)
         (DoubleAsinStr, applyDoubleUnop Math.Asin)
         (DoubleAcosStr, applyDoubleUnop Math.Acos)
         (DoubleAtanStr, applyDoubleUnop Math.Atan)
         (IntPlusStr, applyIntBinop (+))
         (IntMinusStr, applyIntBinop (-))
         (IntMultiplyStr, applyIntBinop (*))
         (IntDivideStr, applyIntBinop (/))
         (IntPowStr, applyIntBinop pown)
         (IntRemStr, applyIntBinop (%))
         (IntIncStr, applyIntUnop intInc)
         (IntDecStr, applyIntUnop intDec)
         (LongPlusStr, applyLongBinop (+))
         (LongMinusStr, applyLongBinop (-))
         (LongMultiplyStr, applyLongBinop (*))
         (LongDivideStr, applyLongBinop (/))
         (LongPowStr, applyLongBinop longPow)
         (LongRemStr, applyLongBinop (%))
         (LongIncStr, applyLongUnop longInc)
         (LongDecStr, applyLongUnop longDec)
         (FloatPlusStr, applyFloatBinop (+))
         (FloatMinusStr, applyFloatBinop (-))
         (FloatMultiplyStr, applyFloatBinop (*))
         (FloatDivideStr, applyFloatBinop (/))
         (FloatPowStr, applyFloatBinop ( ** ))
         (FloatRemStr, applyFloatBinop (%))
         (FloatLogNStr, applyFloatBinop floatLogN)
         (FloatRootStr, applyFloatBinop floatRoot)
         (DoublePlusStr, applyDoubleBinop (+))
         (DoubleMinusStr, applyDoubleBinop (-))
         (DoubleMultiplyStr, applyDoubleBinop (*))
         (DoubleDivideStr, applyDoubleBinop (/))
         (DoublePowStr, applyDoubleBinop ( ** ))
         (DoubleRemStr, applyDoubleBinop (%))
         (DoubleLogNStr, applyDoubleBinop doubleLogN)
         (DoubleRootStr, applyDoubleBinop doubleRoot)
         (CharEqualStr, applyComparator (=))
         (CharInequalStr, applyComparator (<>))
         (CharLessThanStr, applyComparator (<))
         (CharGreaterThanStr, applyComparator (>))
         (CharLessThanOrEqualStr, applyComparator (<=))
         (CharGreaterThanOrEqualStr, applyComparator (>=))
         (IntEqualStr, applyComparator (=))
         (IntInequalStr, applyComparator (<>))
         (IntLessThanStr, applyComparator (<))
         (IntGreaterThanStr, applyComparator (>))
         (IntLessThanOrEqualStr, applyComparator (<=))
         (IntGreaterThanOrEqualStr, applyComparator (>=))
         (LongEqualStr, applyComparator (=))
         (LongInequalStr, applyComparator (<>))
         (LongLessThanStr, applyComparator (<))
         (LongGreaterThanStr, applyComparator (>))
         (LongLessThanOrEqualStr, applyComparator (<=))
         (LongGreaterThanOrEqualStr, applyComparator (>=))
         (FloatEqualStr, applyComparator (=))
         (FloatInequalStr, applyComparator (<>))
         (FloatLessThanStr, applyComparator (<))
         (FloatGreaterThanStr, applyComparator (>))
         (FloatLessThanOrEqualStr, applyComparator (<=))
         (FloatGreaterThanOrEqualStr, applyComparator (>=))
         (DoubleEqualStr, applyComparator (=))
         (DoubleInequalStr, applyComparator (<>))
         (DoubleLessThanStr, applyComparator (<))
         (DoubleGreaterThanStr, applyComparator (>))
         (DoubleLessThanOrEqualStr, applyComparator (<=))
         (DoubleGreaterThanOrEqualStr, applyComparator (>=))
         (AndStr, applyAnd)
         (OrStr, applyOr)
         (DocStr, applyDoc)
         (IfStr, applyIf)
         (ApplyStr, applyApply)
         (TypeStr, applyType)
         (TypeOfStr, applyTypeOf)
         (EqualStr, applyEqual)
         (InequalStr, applyInequal)
         (RefEqualityStr, applyRefEqual)
         (RefInequalityStr, applyRefInequal)
         (ConsStr, applyCons)
         (HeadStr, applyHead)
         (TailStr, applyTail)
         (StringLengthStr, applyStringLength)
         (StringAppendStr, applyStringAppend)
         (ListLengthStr, applyListLength)
         (ListAppendStr, applyListAppend)
         (ArrayLengthStr, applyArrayLength)
         (ArrayAppendStr, applyArrayAppend)
         (StepsStr, applySteps)
         (WhileStr, applyWhile)
         (IsUnitStr, applyIsUnit)
         (IsBooleanStr, applyIsBoolean)
         (IsIntStr, applyIsInt)
         (IsLongStr, applyIsLong)
         (IsFloatStr, applyIsFloat)
         (IsDoubleStr, applyIsDouble)
         (IsCharacterStr, applyIsCharacter)
         (IsStringStr, applyIsString)
         (IsKeywordStr, applyIsKeyword)
         (IsPackageStr, applyIsPackage)
         (IsLambdaStr, applyIsLambda)
         (IsListStr, applyIsList)
         (IsArrayStr, applyIsArray)
         (IsCompositeStr, applyIsComposite)
         (HasTypeStr, applyHasType)
         (HasProtocolStr, applyHasProtocol)
         (CharToIntStr, applyCharToInt)
         (IntToCharStr, applyIntToChar)
         (IntToLongStr, applyIntToLong)
         (LongToIntStr, applyLongToInt)
         (FloatToDoubleStr, applyFloatToDouble)
         (DoubleToFloatStr, applyDoubleToFloat)
         (IntToFloatStr, applyIntToFloat)
         (FloatToIntStr, applyFloatToInt)
         (LongToDoubleStr, applyLongToDouble)
         (DoubleToLongStr, applyDoubleToLong)
         (StringToArrayStr, applyStringToArray)
         (ArrayToStringStr, applyArrayToString)
         (ListToArrayStr, applyListToArray)
         (ArrayToListStr, applyArrayToList)]

    /// The appliable built-in lambdas in dictionary form.
    and appliableBuiltinDictionary = 
        List.toDictionary appliableBuiltins

    /// Apply a built-in operator.
    and applyBuiltin name (args : Expr list) argCount env =
        let appliableBuiltin = ref Unchecked.defaultof<string -> Expr list -> int -> Env -> EvalResult>
        if appliableBuiltinDictionary.TryGetValue (name, appliableBuiltin)
        then !appliableBuiltin name args argCount env
        else applySpecialBuiltin name args argCount env

    /// Apply a built-in symbol.
    and applyBuiltinSymbol args argCount symbol env =
        // NOTE: only a built-in function can eval to a symbol expr
        let builtinResult = applyBuiltin symbol.SymName args argCount env
        makeEvalResult builtinResult.Value env

    /// Apply a lambda once its been pushed on the stack frame and its arguments have been unified.
    and applyLambdaPostPushAndUnification args argCount largs largCount cpre body pre post env =
#if AML_OPTIMIZED
        let _ = cpre
        let constraintPassed = true
#else
        let constraintPassed = cpre args
#endif
        if constraintPassed then
            let (preconditionPassed, optPreViolation) = checkContract args argCount largs largCount pre env
            if preconditionPassed then
                let localVars = List.map2 (fun arg larg -> (larg.ArgName, ValueEntry (arg, None))) args largs
                let resultValue =
                    let env = appendProceduralEntries (AppendToNewFrame argCount) localVars env
                    evalExprDropEnv body env
                let result = makeEvalResult resultValue env
                if isUnit post then result // OPTIMIZATION: explicitly checks for unit
                else
                    let postArgs = resultValue :: args
                    let postArgCount = argCount + 1
                    let postLargs = makeArg ResultStr Concrete UnitValue :: largs
                    let postLargCount = largCount + 1
                    let (postconditionPassed, optPostViolation) = checkContract postArgs postArgCount postLargs postLargCount post env
                    if postconditionPassed then result
                    else
                        match optPostViolation with
                        | Some postViolation -> forwardEvalViolation postViolation env
                        | None -> makeEvalViolation ":v/contract/postconditionFailed" "Contract postcondition failed." env
            else
                match optPreViolation with
                | None -> makeEvalViolation ":v/contract/preconditionFailed" "Contract precondition failed." env
                | Some preViolation -> forwardEvalViolation preViolation env
        elif anyViolations args then forwardEvalViolation (firstViolation args) env
        else makeEvalViolation ":v/contract/constraintFailed" "Constraint not satisfied." env

    /// Apply a lambda.
    and applyLambda (args : Expr list) argCount (largs : Arg list) largCount body cpre pre post lambdaExpr env =
        let pushedEnv = pushStackFrame lambdaExpr env
        let evalResult = applyLambdaPostPushAndUnification args argCount largs largCount cpre body pre post pushedEnv
        popEvalResultStackFrame evalResult

    /// Apply a dynamic dispatch as a lambda.
    and applyLambdaDispatch args argCount largs largCount lambda expr env =
        let overlaidEnv = overlayEnv lambda.LamEnv env
        let result = applyLambda args argCount largs largCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost expr overlaidEnv
        makeEvalResult result.Value env

    /// Apply a dynamic dispatch.
    /// TODO: see if this code could be made more bullet-proof and optimized (such as for when applyDispatch recurs).
    and applyDispatch args argCount dispatch skipArgEval env =
        if not (List.hasAtLeast (dispatch.DispContingentArg + 1) args)
        then makeEvalViolation ":v/eval/unresolvableDynamicDispatch" ("Cannot resolve dynamic dispatch for '" + dispatch.DispName + "' operation. It is missing the argument upon which function selection is contingent.") env
        else
            let argValues = if skipArgEval then args else evalExprsDropEnv args env
            let argValueContingent = argValues.[dispatch.DispContingentArg]
            let argValueContingentType = getType argValueContingent env
            match argValueContingentType with
            | Composite ctype ->
                let sigImpl = ref Unchecked.defaultof<Expr>
                if not (ctype.CompSigImpls.TryGetValue (dispatch.DispName, sigImpl))
                then makeEvalViolation ":v/eval/unresolvedDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName + "' operation.") env
                else
                    match !sigImpl with
                    | Symbol symbol -> applyBuiltinSymbol argValues argCount symbol env
                    | Lambda lambda as l -> applyLambdaDispatch argValues argCount lambda.LamArgs lambda.LamArgCount lambda l env
                    | Dispatch dispatch2 -> applyDispatch argValues argCount dispatch2 true env
                    | _ -> makeEvalViolation ":v/eval/invalidDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName + "' to either built-in function or lambda.") env
            | _ -> makeEvalViolation ":v/eval/unresolvedDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName + "' operation.") env
    
    /// Apply a string selector.
    and applyStringSelector key (str : string) env =
        let evaledKey = evalExprDropEnv key env
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation v env
        | Int int ->
            let index = int.IRValue
            if index >= 0 && index < str.Length then makeEvalResult (Character (makeCharacterRecord str.[index] None)) env
            else makeEvalViolation ":v/contract/selectorOutOfRange" "String index out of range." env
        | _ -> makeEvalViolation ":v/contract/invalidSelectorKeyType" "Only ints can select an element of a string." env

    /// Apply a special selector.
    and applySpecialSelector key target env =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.ApplySpecialSelector key target env
        | None -> makeEvalViolation ":v/contract/missingLanguageModule" "Cannot evaluate a special selector without a language module." env

    /// Apply a composite selector.
    and applyCompositeSelector key selectorType members env =
        let key = if selectorType = FunctionalSelector then evalExprDropEnv key env else key
        match key with
        | Violation _ as v -> forwardEvalViolation v env
        | Keyword keyword ->
            let name = keyword.KRValue
            if not (name.StartsWith MemberPrefixStr)
            then makeEvalViolation ":v/contract/invalidSelectorKeyType" ("Member selectors using keywords must start with member prefix '" + MemberPrefixStr + "'.") env
            else
                let memberName = name.Substring MemberPrefixStr.Length
                let mem = getMember memberName members env
                makeEvalResult mem env
        | Symbol symbol -> let memberName = symbol.SymName in makeEvalResult (getMember memberName members env) env
        | _ -> makeEvalViolation ":v/contract/invalidSelectorKeyType" ("Only keywords or dotted names can select a member of a struct or composite.") env
    
    /// Apply a list selector.
    and applyListSelector key (list : Expr list) env =
        let evaledKey = evalExprDropEnv key env
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation v env
        | Int int ->
            let index = int.IRValue
            if index >= 0 && List.hasAtLeast (index + 1) list then makeEvalResult list.[index] env
            else makeEvalViolation ":v/contract/selectorOutOfRange" "List index out of range." env
        | _ -> makeEvalViolation ":v/contract/invalidSelectorKeyType" "Only ints can select an element of an list." env
    
    /// Apply an array selector.
    and applyArraySelector key (elements : Expr array) env =
        let evaledKey = evalExprDropEnv key env
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation v env
        | Int int ->
            let index = int.IRValue
            if index >= 0 && index < elements.Length then makeEvalResult elements.[index] env
            else makeEvalViolation ":v/contract/selectorOutOfRange" "Array index out of range." env
        | _ -> makeEvalViolation ":v/contract/invalidSelectorKeyType" "Only ints can select an element of an array." env

    /// Evaluate an entry.
    and evalEntry name entry optPositions env =
        match entry with
        | ValueEntry (value, _) -> if isBuiltin value env then makeEvalResult value env else evalExpr value env
        | DynamicEntry (contingentArg, _) -> makeEvalResult (Dispatch (makeDispatchRecord name contingentArg optPositions)) env
        | TypeEntry _ -> makeEvalFirstClassViolation name env
        | ProtocolEntry _ -> makeEvalFirstClassViolation name env

    /// Evaluate a conditional.
    and evalConditional condition env =
        let resultValue = evalExprDropEnv condition env
        match resultValue with
        | Violation _ as v -> (false, CSViolation v)
        | Boolean boolean -> (boolean.BRValue, CSNormal)
        | _ -> (false, CSNotBoolean)
    
    /// Evaluate a symbol.
    /// TODO: clean up this function with extraction.
    and evalSymbol name cachedEntry optPositions env =
        match !cachedEntry with
        | CEUncached ->
            let optProceduralEntryPlus = tryFindProceduralEntry name env
            match optProceduralEntryPlus with
            | Some (proceduralEntry, offset, index) ->
                let newCachedEntry = CEProcedural (offset, index)
                cachedEntry := newCachedEntry
                evalEntry name proceduralEntry optPositions env
            | None ->
                let optDeclarationEntry = tryFindDeclarationEntry name env
                match optDeclarationEntry with
                | Some declarationEntry ->
                    if not env.EnvAllowRedeclaration then
                        let newCachedEntry = CEDeclaration declarationEntry
                        cachedEntry := newCachedEntry
                        env.EnvCachedDeclarationEntries.Add cachedEntry
                    evalEntry name declarationEntry optPositions env
                | None -> makeEvalViolation ":v/eval/nonexistentSymbol" ("Non-existent symbol '" + name + "'.") env
        | CEDeclaration entry ->
            evalEntry name entry optPositions env
        | CEProcedural (offset, index) ->
            let proceduralFrame = (List.skip offset env.EnvProceduralFrames).Head
            let (_, entry) = proceduralFrame.[index]
            evalEntry name entry optPositions env

    /// Evaluate a special value.
    and evalSpecialValue specialValue specialValueExpr env =
        match env.EnvOptLanguageModule with
        | Some lm when lm.Name = specialValue.SVLanguageName ->
            let specialObject = lm.SpecialValueToSpecialObject specialValueExpr env
            match specialObject with
            | Violation _ as v -> forwardEvalViolation v env
            | SpecialObject _ as s -> evalExpr s env
            | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalSpecialValue'."
        | Some _ -> makeEvalViolation ":v/languageModule/mismatchedLanguageModule" "Wrong language module for special value evaluation." env
        | None -> makeEvalViolation ":v/languageModule/missingLanguageModule" "Cannot evaluate a special value without a language module." env

    /// Evaluate a special object.
    and evalSpecialObject specialObject specialObjectExpr env =
        match env.EnvOptLanguageModule with
        | Some lm when lm.Guid = specialObject.SOLanguageGuid -> lm.EvalSpecialObject specialObjectExpr env
        | Some _ -> makeEvalViolation ":v/languageModule/mismatchedLanguageModule" "Wrong language module for special object evaluation." env
        | None -> makeEvalViolation ":v/languageModule/missingLanguageModule" "Cannot evaluate a special object without a language module." env

    /// Evaluate a violation.
    and evalViolation violation vioExpr env =
        if violation.VioEvaluated then makeEvalResult vioExpr env
        else
            let dataValue = evalExprDropEnv violation.VioData env
            let violation = { violation with VioData = dataValue }
            let violation = { violation with VioEvaluated = true }
            interveneOnViolation violation (Violation violation) env

    /// Evaluate a prefixed expressions.
    and evalPrefixed (_ : PrefixedRecord) pfxExpr env =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.EvalPrefixed pfxExpr env
        | _ -> makeEvalViolation ":v/languageModule/missingLanguageModule" "Cannot evaluate prefixed expressions without a language module." env

    /// Evaluate an operation.
    and evalOperation (exprs : Expr list) exprCount env =

        let opValue = evalExprDropEnv exprs.Head env
        let args = exprs.Tail
        let argCount = exprCount - 1

        match opValue with
        | Violation _ as v -> forwardEvalViolation v env
        | Symbol symbol -> applyBuiltinSymbol args argCount symbol env
        | Dispatch dispatch -> applyDispatch args argCount dispatch false env
        | Lambda lambda as l ->
        
            let optUnification = 
                if not lambda.LamEmptyUnification then tryUnifyArgs false args lambda.LamArgs
                else
                    if argCount = lambda.LamArgCount && List.fornone (fun arg -> match arg with Package _ -> true | _ -> false) args // TODO: consider checking for package args in reader
                    then Some (args, lambda.LamArgs)
                    else None

            match optUnification with
            | Some (unifiedArgs, unifiedLargs) ->

                let unifiedCount = unifiedArgs.Length
                let abstractedArgs =
                        List.map2
                            (fun arg larg ->
                                if larg.ArgType = Abstracting
                                then Lambda (makeLambdaRecord true String.Empty [] 0 arg tautology UnitValue UnitValue true None (Some env))
                                else arg)
                            unifiedArgs
                            unifiedLargs

                let argValues = evalExprsDropEnv abstractedArgs env
                let overlaidEnv = overlayEnv lambda.LamEnv env
                let lambdaResult = applyLambda argValues unifiedCount unifiedLargs unifiedCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost l overlaidEnv
                makeEvalResult lambdaResult.Value env

            | None -> makeEvalViolation ":v/eval/malformedLambdaInvocation" "Wrong number or type of arguments." env
        | _ -> makeEvalViolation ":v/eval/invalidOperator" ("Invalid operator '" + writeExpr opValue + "'.") env
    
    /// Evaluate a lambda.
    and evalLambda lambda lambdaExpr env =
        if lambda.LamEvaluated then makeEvalResult lambdaExpr env
        else
            let lambda =
                match lambda.LamEnv with
                | Some _ -> lambda
                | None -> { lambda with LamEnv = Some env }
            let lambda = { lambda with LamEvaluated = true }
            makeEvalResult (Lambda lambda) env

    /// Evaluate an attempt expression.
    and evalAttempt attemptRecord env =
        if attemptRecord.AttemptBranches.IsEmpty then makeEvalViolation ":v/eval/malformedAttemptOperation" "Attempt operation must have at least 1 branch." env
        else
            let bodyValue = evalExprDropEnv attemptRecord.AttemptBody env
            match bodyValue with
            | Violation violation as v ->
                let optBranch = List.tryFind (fun branch -> isInCategory violation.VioCategory branch.ABCategory) attemptRecord.AttemptBranches
                match optBranch with
                | Some branch ->
                    let dataValue = evalExprDropEnv violation.VioData env
                    let branchResult =
                        let env = appendProceduralVariable (AppendToNewFrame 1) DataStr None dataValue env
                        evalExprDropEnv branch.ABBody env
                    makeEvalResult branchResult env
                | None -> forwardEvalViolation v env
            | _ -> makeEvalResult bodyValue env

    /// Let evaluation helper function.
    and evalLet4 letRecord headBinding tailBindings env =
        let env =
            match headBinding with
            | LetVariable (name, body) ->
                let bodyValue = evalExprDropEnv body env
                appendProceduralVariable (AppendToNewFrame letRecord.LetBindingCount) name None bodyValue env
            | LetFunction (name, args, argCount, body, optConstraints, pre, post, emptyUnification) ->
                appendProceduralFunction (AppendToNewFrame letRecord.LetBindingCount) name args argCount body optConstraints None pre post emptyUnification letRecord.LetOptPositions env
        let mutable start = 1
        for binding in tailBindings do
            match binding with
            | LetVariable (name, body) ->
                let bodyValue = evalExprDropEnv body env
                ignore (appendProceduralVariable (AppendToHeadFrame start) name None bodyValue env)
            | LetFunction (name, args, argCount, body, optConstraints, pre, post, emptyUnification) ->
                ignore (appendProceduralFunction (AppendToHeadFrame start) name args argCount body optConstraints None pre post emptyUnification letRecord.LetOptPositions env)
            start <- start + 1
        evalExpr letRecord.LetBody env

    /// Evaluate a let expression.
    /// NOTE: the semantics for let will vary from the interpreter to a compiled-version in that premature access of
    /// entries in the interpreter will look-up shadowed entries while doing so in compiled code will return a violation.
    /// NOTE: this function is rather imperative and may have tricky operational implications. Use caution when modifying!
    and evalLet letRecord env =
        match letRecord.LetBindings with
        | [] -> makeEvalViolation ":v/eval/malformedLetOperation" "Let operation must have at least 1 binding." env
        | headBinding :: tailBindings ->
            let result = evalLet4 letRecord headBinding tailBindings env
            makeEvalResult result.Value env

    /// Evaluate an extend expression.
    and evalExtend extend env =
        if extend.ExtMembers.Count = 0 then makeEvalViolation ":v/eval/malformedExtendOperation" "Extend operation must have at least one member." env
        else
            let targetValue = evalExprDropEnv extend.ExtTarget env
            match targetValue with
            | Violation _ as v -> forwardEvalViolation v env
            | Composite composite when composite.CompType = CompositeType ->
                let (newCompositeMembers : MemberDictionary) =
                    let dictionary = Dictionary HashIdentity.Structural
                    dictionary.Consume composite.CompMembers
                    dictionary // TODO
                for extendMember in extend.ExtMembers do ignore (newCompositeMembers.ForceAdd (extendMember.Key, extendMember.Value))
                if newCompositeMembers.Count - composite.CompMembers.Count < extend.ExtMembers.Count
                then makeEvalViolation ":v/eval/malformedExtendOperation" "Extend operation may not duplicate composite members." env
                else
                    let newComposite = makeCompositeRecord composite.CompEvaluated String.Empty newCompositeMembers composite.CompType null null extend.ExtOptPositions
                    makeEvalResult (Composite newComposite) env
            | _ -> makeEvalViolation ":v/eval/malformedExtendOperation" "First argument of extend must be a composite (non-struct)." env

    /// Evaluate a case expression.
    and evalCase case env =
        let targetValue = evalExprDropEnv case.CaseTarget env
        match targetValue with
        | Violation _ as v -> forwardEvalViolation v env
        | _ ->
            let branchAndTestResults = Seq.map (fun branch -> (branch, evalExprDropEnv branch.TBTest env)) case.CaseBranches
            let optFirstPassingBranchAndTestResult = Seq.tryFind (fun (_, testResult) -> targetValue = testResult) branchAndTestResults
            match optFirstPassingBranchAndTestResult with
            | Some (firstPassingBranch, _) -> evalExpr firstPassingBranch.TBBody env
            | None ->
                let branchAndTestResultsList = Seq.toList branchAndTestResults
                let optFirstBranchAndViolation = List.tryFind (fun (_, testResult) -> isViolation testResult) branchAndTestResultsList
                match optFirstBranchAndViolation with
                | None -> makeEvalViolation ":v/eval/caseWithoutMatch" "A case expression did not find a match." env
                | Some (_, firstViolation) -> forwardEvalViolation firstViolation env

    /// Evaluate a condition expression.
    and evalCondition condition env =
        let branchAndTestResults = Seq.map (fun branch -> (branch, evalExprDropEnv branch.TBTest env)) condition.CondBranches
        let optFirstPassingBranchAndTestResult = Seq.tryFind (fun (_, testResult) -> isTrue testResult) branchAndTestResults // TODO: check for non-boolean values and return a violation when found
        match optFirstPassingBranchAndTestResult with
        | Some (firstPassingBranch, _) -> evalExpr firstPassingBranch.TBBody env
        | None ->
            let branchAndTestResultsList = Seq.toList branchAndTestResults
            let optFirstBranchAndViolation = List.tryFind (fun (_, testResult) -> isViolation testResult) branchAndTestResultsList
            match optFirstBranchAndViolation with
            | Some (_, firstViolation) -> forwardEvalViolation firstViolation env
            | None -> makeEvalViolation ":v/eval/condWithoutMatch" "A condition expression did not find a match." env

    /// Evaluate an intervene expression, intervening when necessary.
    and evalIntervene intervene env =
        let branches = intervene.ItvBranches
        if branches.IsEmpty then makeEvalViolation ":v/eval/malformedInterveneOperation" "Intervene operation must have at least 1 branch." env
        else
            let branchesWithEnv = List.map (fun branch -> { branch with IBEnv = Some env }) branches
            let resultValue =
                let env = pushInterventionBranchList branchesWithEnv env
                evalExprDropEnv intervene.ItvBody env
            makeEvalResult resultValue env
    
    /// Evaluate a ref operation.
    and evalRef reference refExpr env =
        if not reference.RefEvaluated then
            reference.RefExpr <- evalExprDropEnv reference.RefExpr env
            reference.RefEvaluated <- true
        makeEvalResult refExpr env

    /// Evaluate a get operation.
    and evalGet get env =
        let targetValue = evalExprDropEnv get.GetTarget env
        match targetValue with
        | Violation _ as v -> forwardEvalViolation v env
        | Ref reference -> makeEvalResult reference.RefExpr env
        | _ -> makeEvalViolation ":v/eval/invalidGetArgument" "Get argument must be a reference." env

    /// Evaluate a set operation.
    and evalSet set env =
        let targetValue = evalExprDropEnv set.SetTarget env
        match targetValue with
        | Violation _ as v -> forwardEvalViolation v env
        | Ref reference ->
            let injectionValue = evalExprDropEnv set.SetInjection env
            match injectionValue with
            | Violation _ as v -> forwardEvalViolation v env
            | _ ->
                reference.RefExpr <- injectionValue
                reference.RefEvaluated <- true
                makeEvalResult targetValue env
        | _ -> makeEvalViolation ":v/eval/invalidSetArgument" "Set target must be reference." env

    /// Evaluate a list.
    and evalList list listExpr env =
        if list.ListEvaluated then makeEvalResult listExpr env
        else
            let elementValues = evalExprsDropEnv list.ListElements env
            let list = List (makeListRecord true elementValues list.ListOptPositions)
            makeEvalResult list env

    /// Evaluate an array.
    and evalArray array arrExpr env =
        if array.ArrEvaluated then makeEvalResult arrExpr env
        else
            let elementValues = evalExprArrayDropEnv array.ArrElements env
            let array = Array (makeArrayRecord true elementValues array.ArrOptPositions)
            makeEvalResult array env

    /// Evaluate a composite member.
    and evalMember mem env =
        let value = evalExprDropEnv mem.MemExpr env
        makeMember mem.MemName value

    /// Evaluate composite members.
    and evalMembers members env =
        Dictionary.map (fun kvp -> evalMember kvp.Value env) members

    /// Evaluate a composite.
    and evalComposite composite compositeExpr env =
        if composite.CompEvaluated then makeEvalResult compositeExpr env
        else
            let membersWithValues = evalMembers composite.CompMembers env
            let composite = Composite (makeCompositeRecord true composite.CompName membersWithValues composite.CompType composite.CompSigImpls composite.CompProtocols composite.CompOptPositions)
            makeEvalResult composite env

    /// Evaluate a selector.
    and evalSelector selector env =
        let evaledTarget = evalExprDropEnv selector.SelTarget env
        match evaledTarget with
        | Violation _ as v -> forwardEvalViolation v env
        | String string -> applyStringSelector selector.SelKey string.SRValue.SVValue env
        | SpecialObject _ -> applySpecialSelector selector.SelKey evaledTarget env
        | Composite composite -> applyCompositeSelector selector.SelKey selector.SelType composite.CompMembers env
        | List list -> applyListSelector selector.SelKey list.ListElements env
        | Array array -> applyArraySelector selector.SelKey array.ArrElements env
        | _ -> makeEvalViolation ":v/eval/malformedSelector" "Cannot select elements from this value." env
    
    /// Evaluate a variable expression.
    and evalVariable variable env =
        let value = evalExprDropEnv variable.VarBody env
        let optEnv = tryAppendDeclarationVariable variable.VarName variable.VarDoc value env
        match optEnv with
        | Some env -> makeEvalUnit env
        | None -> makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The variable '" + variable.VarName + "' clashes names with an existing declaration.") env
    
    /// Evaluate a function expression.
    and evalFunction fn env =
        let optEnv = tryAppendDeclarationFunction fn.FnName fn.FnArgs fn.FnArgCount fn.FnBody fn.FnOptConstraints fn.FnDoc fn.FnPre fn.FnPost fn.FnEmptyUnification fn.FnOptPositions env
        match optEnv with
        | Some env -> makeEvalUnit env
        | None -> makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The function '" + fn.FnName + "' clashes names with an existing declaration.") env
    
    /// Evaluate a structure expression.
    and evalStructure structure env =
        if not env.EnvAllowRedeclaration && hasDeclarationEntry structure.StructName env
        then makeEvalViolation ":v/eval/structShadows" ("The struct '" + structure.StructName + "' shadows an existing entry.") env
        else
            let symbols = List.map (fun mem -> Symbol (makeSymbolRecord mem (ref CEUncached) structure.StructOptPositions)) structure.StructMemberNames
            let optEnv = tryAppendStructure structure.StructName structure.StructMemberNames structure.StructOptConstraints structure.StructDoc structure.StructReq structure.StructMemberNames symbols structure.StructOptPositions env
            match optEnv with
            | Some env -> let env = instantiateEquatable structure.StructName env in makeEvalUnit env
            | None -> makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The struct '" + structure.StructName + "' or one of its dependents clashes names with an existing declaration.") env

    /// Evaluate a protocol.
    and evalProtocol protocol env =
        if not env.EnvAllowRedeclaration && hasDeclarationEntry protocol.ProtoName env
        then makeEvalViolation ":v/eval/protocolShadows" ("The protocol '" + protocol.ProtoName + "' shadows an existing entry.") env
        else
            let arg = protocol.ProtoArg
            let optConstraints = protocol.ProtoOptConstraints
            let optConstraintsViolation = getOptConstraintsViolation [arg] optConstraints env
            match optConstraintsViolation with
            | Some constraintsViolation -> makeEvalResult constraintsViolation env
            | None ->
                let sigs = protocol.ProtoSignatures
                if sigs.IsEmpty then makeEvalViolation ":v/eval/malformedProtocol" "Protocols must have at least one signature." env
                elif not (doSigsHaveAllConcreteArgs sigs) then makeEvalViolation ":v/eval/malformedProtocolSignature" "Protocol signatures must use only normal arguments." env
                else
                    /// OPTIMIZATION: get matching entry lazily
                    let sigsMatchingEntry = Seq.filter (fun signature -> (tryFindDeclarationEntry signature.SigName env).IsSome) sigs
                    let optFirstSigMatchingEntry = Seq.tryHead sigsMatchingEntry
                    match optFirstSigMatchingEntry with
                    | Some firstSigMatchingEntry when not env.EnvAllowRedeclaration ->
                        makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The protocol signature '" + firstSigMatchingEntry.SigName + "' clashes names with an existing declaration.") env
                    | Some _
                    | None ->
                        let optSigsViolation = getSignaturesViolation arg sigs env
                        if optSigsViolation.IsSome then makeEvalResult optSigsViolation.Value env
                        else
                            let optEnv = tryAppendProtocol protocol.ProtoName arg optConstraints protocol.ProtoDoc sigs env
                            match optEnv with
                            | Some env -> makeEvalUnit env
                            | None -> makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The protocol '" + protocol.ProtoName + "' clashes names with an existing declaration.") env

    /// Evaluate an instance.
    /// TODO: optimize this generally.
    and evalInstance instance (_ : Expr) env =
        let optProtocol = tryFindProtocolEntry instance.InstProtocolName env
        match optProtocol with
        | Some (ProtocolEntry (parg, _, _, psigs)) ->
            let (protocolName, args, sigImpls) = (instance.InstProtocolName, instance.InstArgs, instance.InstFunctions)
            if args.IsEmpty then makeEvalViolation ":v/eval/malformedInstance" "Instances must have at least one argument." env
            elif not (List.areSameLength (List.distinct args) args) then makeEvalViolation ":v/eval/malformedInstance" "All arguments of an instance must be unique." env
            elif sigImpls.IsEmpty then makeEvalViolation ":v/eval/malformedInstance" "Instances must have at least one signature implementation." env
            elif List.exists (function | Variable _ -> false | Function fn -> fn.FnArgs.IsEmpty | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalInstance'.") sigImpls then makeEvalViolation ":v/eval/malformedInstance" "Instance functions must have one argument." env
            elif not (List.areSameLength psigs sigImpls) then makeEvalViolation ":v/eval/malformedInstance" "Instances must have the same number of signature implementations as its protocol has signatures." env
            elif not (List.hasAtLeast 1 args) then makeEvalViolation ":v/eval/malformedInstance" "Instances must have at least one argument." env
            else
                let constraints = instance.InstConstraints
                let optConstraintsViolation = getOptConstraintsViolation args (Some constraints) env
                if optConstraintsViolation.IsSome then makeEvalResult optConstraintsViolation.Value env
                else
                    let (constraintsSatisfied, constraintProjections) = projectConstraintsToProtocolArg constraints parg
                    if not constraintsSatisfied then makeEvalViolation ":v/eval/invalidInstanceConstraint" (writeConstraintFailures constraintProjections) env
                    else
                        let projectedImpls = projectSigImpls sigImpls env
                        if not (tryAppendInstance protocolName args constraints projectedImpls env)
                        then makeEvalViolation ":v/eval/invalidInstance" "Instances must be instantiated with an existing protocol and not over a protocol." env
                        else makeEvalUnit env
        | Some _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalInstance'."
        | None -> makeEvalViolation ":v/eval/missingProtocol" "Cannot declare an instance with a non-existent protocol." env

    /// Evaluate an affirmation expression.
    and evalAffirmation affirmation env =
        let (name, doc, body, optPositions) = (affirmation.AffName, affirmation.AffDoc, affirmation.AffBody, affirmation.AffOptPositions)
        let value = evalExprDropEnv body env
        match value with
        | Violation _ as v -> forwardEvalViolation v env
        | Boolean b when b.BRValue ->
            let optEnv = tryAppendAffirmationFunction name doc body optPositions env
            match optEnv with
            | Some env -> makeEvalUnit env
            | None -> makeEvalViolation ":v/eval/duplicateDeclarationEntry" ("The affirmation '" + name + "' clashes names with an existing declaration.") env
        | Boolean b when not b.BRValue -> makeEvalViolation ":v/affirmation/affirmationFailure" ("The affirmation '" + name + "' was determined to be false.") env
        | _ -> makeEvalViolation ":v/affirmation/invalidResultType" ("Expression for affirmation '" + name + "' must return a boolean value.") env

    /// Evaluate an Aml file.
    and evalUsingFile usingFile env =
        let fileName = Path.GetFileName usingFile.UFPath
        let directoryPath = getDirectoryRelativeToFile usingFile.UFPath env
        let absolutePath = Path.Combine (directoryPath, fileName)
        let usingFiles = if usingFile.UFReload then Set.empty else env.EnvUsingFiles
        let envUsing = { env with EnvPath = directoryPath; EnvUsingFiles = usingFiles }
        if envUsing.EnvUsingFiles.Contains absolutePath then makeEvalUnit env
        else
            try let exprs = runParserOnFile readExprsTillEnd () absolutePath System.Text.Encoding.Default
                let results = sequentiallyEvalReadResults exprs false envUsing
                if List.isEmpty results then makeEvalUnit env
                else
                    let lastResult = List.last results
                    let values = List.map (fun result -> result.Value) results
                    let anyViolationsInValues = anyViolations values
                    let usingFiles =
                        if usingFile.UFReload && not anyViolationsInValues
                        then Set.union envUsing.EnvUsingFiles env.EnvUsingFiles
                        else lastResult.Env.EnvUsingFiles
                    // NOTE: even if there are violations during file evaluation, and though we take no definitions from
                    // such a file, the file is still consider 'used'
                    let usingFiles = Set.add absolutePath usingFiles
                    let envEvaled = if anyViolationsInValues then env else lastResult.Env
                    let envEvaled = { envEvaled with EnvPath = env.EnvPath; EnvUsingFiles = usingFiles }
                    if anyViolationsInValues then
                        let violation = firstViolation values
                        forwardEvalViolation violation envEvaled
                    else makeEvalUnit envEvaled
            with exn -> makeEvalExceptionViolation exn env

    /// Evaluate an Aml language.
    and evalUsingLanguage usingLanguage env =
        match env.EnvOptLanguageModule with
        // TODO: consider making a violation if a different LM than a current one is loaded
        | Some _ -> makeEvalUnit env
        | None ->
            try let assembly = Reflection.Assembly.LoadFrom usingLanguage.ULPath
                let instance = assembly.CreateInstance usingLanguage.ULType
                if instance = null then makeEvalViolation ":v/languageModule/creationFailure" ("Could not make language module '" + usingLanguage.ULType + "'.") env
                else
                    let languageModule = instance :?> ILanguageModule
                    let envLm = { env with EnvOptLanguageModule = Some languageModule }
                    let optEnvLm = languageModule.TryInitialize envLm
                    match optEnvLm with
                    | Some envLm -> makeEvalUnit envLm
                    | None -> makeEvalViolation ":v/languageModule/creationFailure" ("Could not make language module '" + usingLanguage.ULType + "' due to duplicate declaration names.") env
            with exn -> makeEvalExceptionViolation exn env

    /// Evaluate a special series.
    and evalSpecialSeries specialSeries env =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.EvalSpecialSeries specialSeries env
        | _ -> makeEvalViolation ":v/languageModule/missingLanguageModule" "Cannot evaluate special series without a language module." env

    /// Evaluate an Aml expression structure.
    and evalExpr expr env =
        let pushedEnv = pushExpr expr env
        let result =
            match expr with
            | Violation violation -> evalViolation violation expr pushedEnv
            | Boolean _ -> makeEvalResult expr pushedEnv
            | Character _ -> makeEvalResult expr pushedEnv
            | String _ -> makeEvalResult expr pushedEnv
            | Int _ -> makeEvalResult expr pushedEnv
            | Long _ -> makeEvalResult expr pushedEnv
            | Float _ -> makeEvalResult expr pushedEnv
            | Double _ -> makeEvalResult expr pushedEnv
            | Keyword _ -> makeEvalResult expr pushedEnv
            | Symbol symbol -> let value = (evalSymbol symbol.SymName symbol.SymCachedEntry symbol.SymOptPositions pushedEnv).Value in makeEvalResult value pushedEnv
            | Package _ -> makeEvalResult expr pushedEnv
            | Prefixed prefixed -> evalPrefixed prefixed expr pushedEnv
            | Dispatch _ -> makeEvalResult expr pushedEnv
            | SpecialValue specialValue -> evalSpecialValue specialValue expr pushedEnv
            | SpecialObject specialObject -> evalSpecialObject specialObject expr pushedEnv
            | Series series -> if series.SerExprs.IsEmpty then makeEvalResult expr pushedEnv else evalOperation series.SerExprs series.SerExprCount pushedEnv
            | Lambda lambda -> evalLambda lambda expr pushedEnv
            | Attempt attemptRecord -> evalAttempt attemptRecord pushedEnv
            | Let letRecord -> evalLet letRecord pushedEnv
            | Extend extend -> evalExtend extend pushedEnv
            | Case case -> evalCase case pushedEnv
            | Condition condition -> evalCondition condition pushedEnv
            | Intervene intervene -> evalIntervene intervene pushedEnv
            | Ref reference -> evalRef reference expr pushedEnv
            | Get get -> evalGet get pushedEnv
            | Set set -> evalSet set pushedEnv
            | List list -> evalList list expr pushedEnv
            | Array array -> evalArray array expr pushedEnv
            | Composite composite -> evalComposite composite expr pushedEnv
            | Selector selector -> evalSelector selector pushedEnv
            | Variable variable -> evalVariable variable pushedEnv
            | Function fn -> evalFunction fn pushedEnv
            | Structure structure -> evalStructure structure pushedEnv
            | Protocol protocol -> evalProtocol protocol pushedEnv
            | Instance instance -> evalInstance instance expr pushedEnv
            | Affirmation affirmation -> evalAffirmation affirmation pushedEnv
            | UsingFile usingFile -> evalUsingFile usingFile pushedEnv
            | UsingLanguage usingLanguage -> evalUsingLanguage usingLanguage pushedEnv
            | SpecialSeries _ -> evalSpecialSeries expr pushedEnv
        popEvalResultExpr result

    /// Evaluate an expression but drop the environment.
    and evalExprDropEnv expr env =
        (evalExpr expr env).Value

    /// Evaluate an expression but drop the result.
    and evalExprDropResult expr env =
        (evalExpr expr env).Env

    /// Evaluate an Aml expression that has been read in.
    and evalReadResult readResult env =
        match readResult with
        | Success (expr, _, _) -> evalExpr expr env
        | Failure (message, _, _) -> makeEvalViolation ":v/reader/readFailure" ("Could not read an expression due to '" + message + "'.") env

    /// Evaluate multiple expressions.
    and evalExprs exprs env =
        List.map (fun expr -> evalExpr expr env) exprs
    
    /// Evaluate multiple exprs, dropping the environments they produce.
    and evalExprsDropEnv exprs env =
        List.map (fun expr -> evalExprDropEnv expr env) exprs
    
    /// Evaluate multiple exprs as an array, dropping the environments they produce.
    /// OPTIMIZATION: An optimization for evaluating arrays.
    and evalExprArrayDropEnv exprs env =
        Array.map (fun expr -> evalExprDropEnv expr env) exprs

    /// Evaluate an expr with it on the stack trace. 
    and evalExprWithExplicitTracing expr env =
        let pushedEnv = pushStackFrame expr env
        let result = evalExpr expr pushedEnv
        popEvalResultStackFrame result

    /// Evaluate multiple expressions sequentially.
    and sequentiallyEvalExprs exprs tillViolation env =
        let nextEnv evalResults currentEnv = if List.isEmpty evalResults then currentEnv else (List.head evalResults).Env
        let evalResults = List.fold (fun evalResults expr -> evalExprWithExplicitTracing expr (nextEnv evalResults env) :: evalResults) [] exprs
        let evalResultsRev = List.rev evalResults // TODO: see if we can avoid this reverse by using a Queue or List.foldBack
        if tillViolation then List.takeTillInclusive (fun evalResult -> isViolation evalResult.Value) evalResultsRev
        else evalResultsRev

    /// Evaluate Aml expressions that have been read in.
    and sequentiallyEvalReadResults readResults tillViolation env =
        match readResults with
        | Success (exprs, _, _) -> sequentiallyEvalExprs exprs tillViolation env
        | Failure (message, _, _) -> [makeEvalViolation ":v/reader/readFailure" ("Could not read expressions due to '" + message + "'.") env]