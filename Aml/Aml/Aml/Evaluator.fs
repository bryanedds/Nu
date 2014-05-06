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
    let rec checkContract env args argCount largs largCount contract =
#if AML_OPTIMIZED
        (true, None)
#else
        if isUnit contract then (true, None)
        else
            let (result : EvalResult) = applyLambda env args argCount largs largCount contract tautology UnitValue UnitValue true contract
            match result.Value with
            | Violation _ as v -> (false, Some v)
            | Boolean boolean -> (boolean.BRValue, None)
            | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.contractSatisfied'."
#endif

    /// Intervene on a violation.
    and interveneOnViolation env violation vioExpr =
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
                let env' =
                    match branch.IBEnv with
                    | None -> failwith "Unexpected match failure in 'Aml.Evaluator.interveneOnViolation'."
                    | Some env -> env
                let env'' = appendProceduralVariable env' (AppendToNewFrame 2) ProblemLun None vioExpr
                let env'3 = appendProceduralVariable env'' (AppendToHeadFrame 1) DataLun None violation.VioData
                if branch.IBHide then intervening <- false
                currentResultValue <- evalExprDropEnv env'3 branch.IBBody
        makeEvalResult env currentResultValue

    /// Make a violation from a category and a message during evaluation, intervening if necessary.
    and makeEvalViolation env category message =
        let violation = makeViolationWithPositions env category message
        match violation with
        | Violation v -> interveneOnViolation env v violation
        | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.makeEvalViolation'."

    /// Make a first-class violation during evaluation.
    and makeEvalFirstClassViolation env name =
        makeEvalViolation env ":v/eval/notFirstClass" ("Not a first-class value '" + name.LunStr + "'.")

    /// Make a an exception violation during evaluation.
    and makeEvalExceptionViolation env (exn : Exception) =
        makeEvalViolation env ":v/exception" (string exn)
    
    /// Forward a violation from a str during evaluation.
    and forwardEvalViolation env violation =
        makeEvalResult env violation

    /// Declare an equatable protocol.
    and declareEquatable env =
        let a = Lun.make AStr
        let equatable = Lun.make EquatableStr
        let equality = Lun.make EqualityStr
        let inequality = Lun.make InequalityStr
        let equatableProtocol =
            makeProtocolRecord
                equatable
                a
                None
                (makeDoc "Enables testing for equality and inequality.")
                [makeSignature equality [makeArg a Concrete UnitValue; makeArg a Concrete UnitValue] (makeDoc "Query for semantic equality.")
                 makeSignature inequality [makeArg a Concrete UnitValue; makeArg a Concrete UnitValue] (makeDoc "Query for semantic inequality.")]
                None
        let result : EvalResult = evalProtocol env equatableProtocol
        if isViolation result.Value then failwith "Could not declare equatable protocol."
        result.Env

    /// Instantiate a type under the equatable protocol.
    and instantiateEquatable env typeName =
        let x = Lun.make XStr
        let y = Lun.make YStr
        let equatableProtocol = Lun.make EquatableProtocolStr
        let equal = Lun.make EqualStr
        let inequal = Lun.make InequalStr
        let equality = Lun.make EqualityStr
        let inequality = Lun.make InequalityStr
        let xArg = makeArg x Concrete UnitValue
        let xSymbol = Symbol (makeSymbolRecord x (ref CEUncached) None)
        let xConstraint = makeConstraint typeName [x]
        let yArg = makeArg y Concrete UnitValue
        let ySymbol = Symbol (makeSymbolRecord y (ref CEUncached) None)
        let yConstraint = makeConstraint typeName [y]
        let equalSymbol = Symbol (makeSymbolRecord equal (ref CEUncached) None)
        let equalBody = Series (makeSeriesRecord [equalSymbol; xSymbol; ySymbol] 3 None)
        let equalFunction = Function (makeFunctionRecord equality [xArg; yArg] 2 equalBody None None UnitValue UnitValue true None)
        let inequalSymbol = Symbol (makeSymbolRecord inequal (ref CEUncached) None)
        let inequalBody = Series (makeSeriesRecord [inequalSymbol; xSymbol; ySymbol] 3 None)
        let inequalFunction = Function (makeFunctionRecord inequality [xArg; yArg] 2 inequalBody None None UnitValue UnitValue true None)
        let instance = makeInstanceRecord equatableProtocol [x; y] [xConstraint; yConstraint] [equalFunction; inequalFunction] None
        let instanceExpr = Instance instance
        let result : EvalResult = evalInstance env instance instanceExpr
        if isViolation result.Value then failwith "Could not instantiate type over equatable protocol."
        result.Env

    /// Project a signature implementation to a concrete value.
    and projectSigImpl env pname sigImpl =
        match sigImpl with
        | Variable variable -> (variable.VarName, evalExprDropEnv env variable.VarBody)
        | Function fn -> (fn.FnName, Lambda (makeLambdaRecord false fn.FnName fn.FnArgs fn.FnArgCount fn.FnBody tautology fn.FnPre fn.FnPost fn.FnEmptyUnification fn.FnOptPositions (Some env)))
        | _ -> failwith "Unexpected match failure in Aml.Evaluator.Prims.sigImplToNamedExpr."

    /// Project a list of signature implementations to concrete values.
    and projectSigImpls env pname sigImpls =
        List.map (fun sigImpl -> projectSigImpl env pname sigImpl) sigImpls

    /// Apply an unary operator.
    and applyUnop exprToOptNumber numberToExpr env unop (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedUnop" "Unop must have 1 argument."
        else
            let argValue = evalExprDropEnv env args.Head
            let optNumber : 'a option = exprToOptNumber argValue
            match optNumber with
            | None ->
                match argValue with
                | Violation _ as v -> forwardEvalViolation env v
                | _ -> makeEvalViolation env ":v/contract/invalidUnopArgumentType" "Unop must have an argument of the correct type."
            | Some number -> makeEvalResult env (numberToExpr (unop number))

    /// Apply a binary operator.
    and applyBinop allExprsToNumbers numberToExpr env binop isZero checkForDivisionByZero (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedBinop" "Binop must have 2 arguments."
        else
            let argValues = evalExprsDropEnv env args
            let numbers : 'a list = allExprsToNumbers argValues
            if numbers.IsEmpty then
                if anyViolations argValues then forwardEvalViolation env (firstViolation argValues)
                else makeEvalViolation env ":v/contract/invalidBinopArgumentType" "Binop must have both arguments of the correct type."
            else
                if not (checkForDivisionByZero && isZero numbers.[1]) then makeEvalResult env (numberToExpr (List.reduce binop numbers))
                else makeEvalViolation env ":v/contract/divByZero" "Division by zero."

    /// Apply a comparator.
    and applyComparator comparator env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedComparator" "Comparator must have 2 arguments."
        else
            let argValues = evalExprsDropEnv env args
            let comparables : IComparable list = allExprsToComparables argValues
            if comparables.IsEmpty then
                if anyViolations argValues then forwardEvalViolation env (firstViolation argValues)
                else makeEvalViolation env ":v/eval/invalidComparatorForm" "Invalid comparator form."
            else
                let compareResult = comparator comparables.[0] comparables.[1]
                makeEvalResult env (Boolean (makeBooleanRecord compareResult None))

    /// Apply an and operator.
    and applyAnd env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedAndOperation" "And operation must have 2 arguments."
        else
            let xValue = evalExprDropEnv env args.[0]
            match xValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Boolean xBoolean ->
                let xBool = xBoolean.BRValue
                if not xBool then makeEvalResult env FalseValue
                else
                    let yValue = evalExprDropEnv env args.[1]
                    match yValue with
                    | Violation _ as v -> forwardEvalViolation env v
                    | Boolean yBoolean ->
                        let yBool = yBoolean.BRValue
                        makeEvalResult env (Boolean (makeBooleanRecord (xBool && yBool) None)) // I would prefer that the short-circuiting && not be used...
                    | _ -> makeEvalViolation env ":v/contract/invalidAndArgumentType" "And expression requires both argumements to be boolean."
            | _ -> makeEvalViolation env ":v/contract/invalidAndArgumentType" "And expression requires both argumements to be boolean."

    /// Apply an or operator.
    and applyOr env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedOrOperation" "Or operation must have 2 arguments."
        else
            let xValue = evalExprDropEnv env args.[0]
            match xValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Boolean xBoolean ->
                let xBool = xBoolean.BRValue
                if xBool then makeEvalResult env TrueValue
                else
                    let yValue = evalExprDropEnv env args.[1]
                    match yValue with
                    | Violation _ as v -> forwardEvalViolation env v
                    | Boolean yBoolean -> makeEvalResult env (Boolean yBoolean)
                    | _ -> makeEvalViolation env ":v/contract/invalidOrArgumentType" "Or expression requires both argumements to be boolean."
            | _ -> makeEvalViolation env ":v/contract/invalidOrArgumentType" "Or expression requires both argumements to be boolean."

    /// Apply an if operator.
    and applyIf env name (args : Expr list) argCount =
        if argCount <> 3 then makeEvalViolation env ":v/eval/malformedIfOperation" "If operation must have 3 arguments."
        else
            let condition = evalExprDropEnv env args.Head
            match condition with
            | Violation _ as v -> forwardEvalViolation env v
            | Boolean boolean -> if boolean.BRValue then evalExpr env args.[1] else evalExpr env args.[2]
            | _ -> makeEvalViolation env ":v/contract/invalidIfCondition" "Condition part of if operator must result in a boolean."

    /// Apply the apply operator.
    and applyApply env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedApplyOperation" "Apply operation must have 2 arguments."
        else
            let op = args.[0]
            let argList = evalExprDropEnv env args.[1]
            match argList with
            | List list -> let series = Series (makeSeriesRecord (op :: list.ListElements) (list.ListElements.Length + 1) None) in evalExpr env series
            | _ -> makeEvalViolation env ":v/contract/invalidApplyArgument" "Apply requires a list as its argument."

    /// Apply the doc operator.
    and applyDoc env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedDocOperation" "Doc operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | Keyword keyword ->
                let entryNameStr = keyword.KRValue.LunStr
                let entryNameStr' = if entryNameStr.StartsWith SimpleEntryPrefixStr then entryNameStr.Substring SimpleEntryPrefixStr.Length else entryNameStr
                let entryName = Lun.make entryNameStr'
                let optEntry = tryFindEntry env entryName
                match optEntry with
                | None -> makeEvalViolation env ":v/eval/invalidDocOperation" "Entry not found for doc operation."
                | Some entry ->
                    match entry with
                    | ValueEntry (_, doc) -> match doc with None -> makeEvalViolation env ":v/eval/invalidDocOperation" "Documentation missing for entry." | Some str -> makeEvalResult env (String (makeStringRecord str None))
                    | DynamicEntry (_, doc) -> match doc with None -> makeEvalViolation env ":v/eval/invalidDocOperation" "Documentation missing for entry." | Some str -> makeEvalResult env (String (makeStringRecord str None))
                    | TypeEntry (_, _, doc) -> match doc with None -> makeEvalViolation env ":v/eval/invalidDocOperation" "Documentation missing for entry." | Some str -> makeEvalResult env (String (makeStringRecord str None))
                    | ProtocolEntry (_, _, doc, _) -> match doc with None -> makeEvalViolation env ":v/eval/invalidDocOperation" "Documentation missing for entry." | Some str -> makeEvalResult env (String (makeStringRecord str None))
            | _ -> makeEvalViolation env ":v/eval/invalidDocParameter" "Doc operation requires a keyword argument."

    /// Apply a type selector.
    and applyType env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedTypeOperation" "Type operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | _ -> let aType = getType env value in makeEvalResult env aType
        
    /// Apply a typeOf operator.
    and applyTypeOf env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedTypeOfOperation" "TypeOf operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | Keyword keyword ->
                let typeName = keyword.KRValue
                let optType = tryFindType env typeName
                match optType with
                | None -> makeEvalViolation env ":v/eval/nonexistantType" ("Could not find type '" + typeName.LunStr + "'.")
                | Some (_, aType, _) -> makeEvalResult env aType
            | _ -> makeEvalViolation env ":v/eval/invalidTypeOfArgumentType" "TypeOf operation requires a keyword argument."

    /// Apply a structural equal query.
    and applyEqual env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            let secondValue = evalExprDropEnv env args.[1]
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation env v
            | (_, (Violation _ as v)) -> forwardEvalViolation env v
            | _ -> makeEvalResult env (Boolean (makeBooleanRecord (firstValue = secondValue) None))
    
    /// Apply a structural inequal query.
    and applyInequal env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedInequalOperation" "Inequal operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            let secondValue = evalExprDropEnv env args.[1]
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation env v
            | (_, (Violation _ as v)) -> forwardEvalViolation env v
            | _ -> makeEvalResult env (Boolean (makeBooleanRecord (firstValue <> secondValue) None))

    /// Apply a ref equality query.
    and applyRefEqual env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            let secondValue = evalExprDropEnv env args.[1]
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation env v
            | (_, (Violation _ as v)) -> forwardEvalViolation env v
            | (Ref _, Ref _) -> makeEvalResult env (Boolean (makeBooleanRecord (firstValue === secondValue) None))
            | (Ref _, _) -> makeEvalViolation env ":v/contract/refEqualityOnNonRef" "Second argument of reference equal operation must be a reference."
            | (_, Ref _) -> makeEvalViolation env ":v/contract/refEqualityOnNonRef" "First argument of reference equal operation must be a reference."
            | _ -> makeEvalViolation env ":v/contract/refEqualityOnNonRef" "Both arguments of reference equal operation must be references."

    /// Apply a ref inequality query.
    and applyRefInequal env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedEqualOperation" "Equal operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            let secondValue = evalExprDropEnv env args.[1]
            match (firstValue, secondValue) with
            | (Violation _ as v, _) -> forwardEvalViolation env v
            | (_, (Violation _ as v)) -> forwardEvalViolation env v
            | (Ref _, Ref _) -> makeEvalResult env (Boolean (makeBooleanRecord (firstValue <<>> secondValue) None))
            | (Ref _, _) -> makeEvalViolation env ":v/contract/refInequalityOnNonRef" "Second argument of reference inequal operation must be a reference."
            | (_, Ref _) -> makeEvalViolation env ":v/contract/refInequalityOnNonRef" "First argument of reference inequal operation must be a reference."
            | _ -> makeEvalViolation env ":v/contract/refInequalityOnNonRef" "Both arguments of reference inequal operation must be references."
    
    /// Apply a cons operation.
    and applyCons env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedConsOperation" "Cons operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            match firstValue with
            | Violation _ as v -> forwardEvalViolation env v
            | _ ->
                let secondValue = evalExprDropEnv env args.[1]
                match secondValue with
                | Violation _ as v -> forwardEvalViolation env v
                | List list -> makeEvalResult env (List (makeListRecord true (firstValue :: list.ListElements) None))
                | Series series when series.SerExprs.IsEmpty -> makeEvalResult env (List (makeListRecord true [firstValue] None))
                | _ -> makeEvalViolation env ":v/contract/consToNonList" "Cannot cons to a non-list."
    
    /// Apply a head operation.
    and applyHead env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedHeadOperation" "Head operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | List list when list.ListElements.IsEmpty -> makeEvalViolation env ":v/contract/headOfEmptyList" "Cannot take the head of an empty list."
            | List list -> makeEvalResult env list.ListElements.Head
            | _ -> makeEvalViolation env ":v/contract/headOfNonList" "Cannot take the head of a non-list."
    
    /// Apply a tail operation.
    and applyTail env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedTailOperation" "Tail operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | List list when list.ListElements.IsEmpty -> makeEvalViolation env ":v/contract/tailOfEmptyList" "Cannot take the tail of an empty list."
            | List list -> makeEvalResult env (List (makeListRecord true list.ListElements.Tail None))
            | _ -> makeEvalViolation env ":v/contract/tailOfNonList" "Cannot take the tail of a non-list."
    
    /// Apply a string length query.
    and applyStringLength env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedStringLengthOperation" "StringLength operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | String string -> makeEvalResult env (Int (makeIntRecord string.SRValue.SVValue.Length None))
            | _ -> makeEvalViolation env ":v/contract/stringLengthOfNonString" "Cannot get the string length of a non-string."
    
    /// Apply a string append.
    and applyStringAppend env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedStringAppendOperation" "StringAppend operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            match firstValue with
            | Violation _ as v -> forwardEvalViolation env v
            | String string ->
                let secondValue = evalExprDropEnv env args.[1]
                match secondValue with
                | Violation _ as v2 -> forwardEvalViolation env v2
                | String string2 -> makeEvalResult env (String (makeStringRecord (makeLiteralStringValue (string.SRValue.SVValue + string2.SRValue.SVValue)) None))
                | _ -> makeEvalViolation env ":v/contract/stringAppendOfNonString" "String appending requires 2 strings as arguments."
            | _ -> makeEvalViolation env ":v/contract/stringAppendOfNonString" "String appending requires 2 strings as arguments."
    
    /// Apply a list length query.
    and applyListLength env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedListLengthOperation" "ListLength operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | List list -> makeEvalResult env (Int (makeIntRecord list.ListElements.Length None))
            | _ -> makeEvalViolation env ":v/contract/listLengthOfNonList" "Cannot get the list length of a non-list."

    /// Apply a list append.
    and applyListAppend env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedListAppendOperation" "ListAppend operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            let secondValue = evalExprDropEnv env args.[1]
            match firstValue with
            | Violation _ as v -> forwardEvalViolation env v
            | List firstList ->
                match secondValue with
                | Violation _ as v -> forwardEvalViolation env v
                | List secondList -> makeEvalResult env (List (makeListRecord true (firstList.ListElements @ secondList.ListElements) None))
                | Series secondSeries when secondSeries.SerExprs.IsEmpty -> makeEvalResult env (List (makeListRecord true firstList.ListElements None))
                | _ -> makeEvalViolation env ":v/contract/listAppendOfNonList" "Cannot append a list to a non-list."
            | Series firstSeries when firstSeries.SerExprs.IsEmpty ->
                match secondValue with
                | Violation _ as v -> forwardEvalViolation env v
                | List secondList -> makeEvalResult env (List (makeListRecord true secondList.ListElements None))
                | Series secondSeries when secondSeries.SerExprs.IsEmpty -> makeEvalUnit env
                | _ -> makeEvalViolation env ":v/contract/listAppendOfNonList" "Cannot append a list to a non-list."
            | _ -> makeEvalViolation env ":v/contract/listAppendOfNonList" "Cannot append a non-list to a list."

    /// Apply an array length query.
    and applyArrayLength env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedArrayLengthOperation" "ArrayLength operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | Array array -> makeEvalResult env (Int (makeIntRecord array.ArrElements.Length None))
            | _ -> makeEvalViolation env ":v/contract/arrayLengthOfNonArray" "Cannot get the array length of a non-array."

    /// Apply an array append.
    and applyArrayAppend env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedArrayAppendOperation" "ArrayLength operation must have 2 arguments."
        else
            let firstValue = evalExprDropEnv env args.[0]
            match firstValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Array array ->
                let secondValue = evalExprDropEnv env args.[1]
                match secondValue with
                | Violation _ as v -> forwardEvalViolation env v
                | Array array2 -> makeEvalResult env (Array (makeArrayRecord true (Array.append array.ArrElements array2.ArrElements) None))
                | _ -> makeEvalViolation env ":v/contract/arrayAppendOfNonArray" "Array appending requires two arrays as arguments."
            | _ -> makeEvalViolation env ":v/contract/arrayAppendOfNonArray" "Array appending requires two arrays as arguments."

    /// Apply a steps operation.
    and applySteps env name args _ =
        match args with
        | [] -> makeEvalViolation env ":v/eval/malformedStepsOperation" "Steps operation must have at least one argument."
        | _ -> let results = sequentiallyEvalExprs env args false in List.last results

    /// Apply a while operation.
    and applyWhile env name args _ =
        match args with
        | [condition; statement] ->
            let mutable conditionResult = evalConditional env condition
            let mutable statementResult = makeEvalUnit env
            while fst conditionResult &&
                  snd conditionResult = CSNormal &&
                  not (isViolation statementResult.Value) do
                  statementResult <- evalExpr env statement
                  conditionResult <- evalConditional env condition
            match snd conditionResult with
            | CSViolation v -> forwardEvalViolation env v
            | CSNormal -> statementResult
            | CSNotBoolean -> makeEvalViolation env ":v/contract/invalidWhileCondition" "The result of a while condition must be a boolean value."
        | _ -> makeEvalViolation env ":v/eval/malformedStepsOperation" "While operation must have two arguments."

    /// Apply a type query with a predicate.
    and applyIsPred pred env name (args : Expr list) argCount =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedAstPredicateOperation" "AST predicate operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            match value with
            | Violation _ as v -> forwardEvalViolation env v
            | _ -> let resultValue = Boolean (makeBooleanRecord (pred value) None) in makeEvalResult env resultValue

    /// Apply a unit query.
    and applyIsUnit env name (args : Expr list) argCount = applyIsPred isUnit env name args argCount

    /// Apply a boolean query.
    and applyIsBoolean env name (args : Expr list) argCount = applyIsPred isBoolean env name args argCount

    /// Apply a character query.
    and applyIsCharacter env name (args : Expr list) argCount = applyIsPred isCharacter env name args argCount

    /// Apply a string query.
    and applyIsString env name (args : Expr list) argCount = applyIsPred isString env name args argCount

    /// Apply an int query.
    and applyIsInt env name (args : Expr list) argCount = applyIsPred isInt env name args argCount

    /// Apply an long query.
    and applyIsLong env name (args : Expr list) argCount = applyIsPred isLong env name args argCount

    /// Apply a float query.
    and applyIsFloat env name (args : Expr list) argCount = applyIsPred isFloat env name args argCount

    /// Apply a double query.
    and applyIsDouble env name (args : Expr list) argCount = applyIsPred isDouble env name args argCount

    /// Apply a keyword query.
    and applyIsKeyword env name (args : Expr list) argCount = applyIsPred isKeyword env name args argCount

    /// Apply a package query.
    and applyIsPackage env name (args : Expr list) argCount = applyIsPred isPackage env name args argCount

    /// Apply a lambda query.
    and applyIsLambda env name (args : Expr list) argCount = applyIsPred isLambda env name args argCount

    /// Apply a list query.
    and applyIsList env name (args : Expr list) argCount = applyIsPred isList env name args argCount

    /// Apply a array query.
    and applyIsArray env name (args : Expr list) argCount = applyIsPred isArray env name args argCount

    /// Apply a composite query.
    and applyIsComposite env name (args : Expr list) argCount = applyIsPred isComposite env name args argCount

    /// Apply a has type query.
    and applyHasType env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedHasTypeOperation" "HasType operation must have 2 arguments."
        else
            let typeNameValue = evalExprDropEnv env args.[0]
            match typeNameValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Keyword keyword as typeKeyword ->
                let typeName = keyword.KRValue
                let value = evalExprDropEnv env args.[1]
                match value with
                | Violation _ as v -> forwardEvalViolation env v
                | _ -> let resultValue = hasType env typeName value in makeEvalResult env (Boolean (makeBooleanRecord resultValue None))
            | _ -> makeEvalViolation env ":v/contract/typeLookupWithNonKeyword" "Types can only be looked up with a keyword."

    /// Apply a has protocol query.
    and applyHasProtocol env name (args : Expr list) argCount =
        if argCount <> 2 then makeEvalViolation env ":v/eval/malformedHasProtocolOperation" "HasProtocol operation must have 2 arguments."
        else
            let protocolNameValue = evalExprDropEnv env args.[0]
            match protocolNameValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Keyword keyword ->
                let protocolName = keyword.KRValue
                let value = evalExprDropEnv env args.[1]
                match value with
                | Violation _ as v -> forwardEvalViolation env v
                | _ -> let result = hasProtocol env protocolName value in makeEvalResult env (Boolean (makeBooleanRecord result None))
            | _ -> makeEvalResult env FalseValue

    /// Apply a conversion from one primitive type to another.
    and applyConversion env (converter : Expr -> Expr option) (args : Expr list) argCount typeName =
        if argCount <> 1 then makeEvalViolation env ":v/eval/malformedConversionOperation" "Conversion operation must have 1 argument."
        else
            let value = evalExprDropEnv env args.Head
            let optConvertedValue = converter value
            match optConvertedValue with
            | None ->
                match value with
                | Violation _ as v -> forwardEvalViolation env v
                | _ -> makeEvalViolation env ":v/contract/invalidConversionType" ("Value must be of type '" + typeName + "' for this conversion.")
            | Some convertedValue -> makeEvalResult env convertedValue

    /// Apply a char to int conversion.
    and applyCharToInt env name (args : Expr list) argCount = applyConversion env (function | Character c -> Some (Int (makeIntRecord (int c.CRValue) None)) | _ -> None) args argCount CharacterStr

    /// Apply an int to char conversion.
    and applyIntToChar env name (args : Expr list) argCount = applyConversion env (function | Int i -> Some (Character (makeCharacterRecord (char i.IRValue) None)) | _ -> None) args argCount IntStr

    /// Apply an int to long conversion.
    and applyIntToLong env name (args : Expr list) argCount = applyConversion env (function | Int i -> Some (Long (makeLongRecord (int64 i.IRValue) None)) | _ -> None) args argCount IntStr

    /// Apply a long to int conversion.
    and applyLongToInt env name (args : Expr list) argCount = applyConversion env (function | Long l -> Some (Int (makeIntRecord (int l.GRValue) None)) | _ -> None) args argCount LongStr

    /// Apply a float to double conversion.
    and applyFloatToDouble env name (args : Expr list) argCount = applyConversion env (function | Float f -> Some (Double (makeDoubleRecord (float f.FRValue) None)) | _ -> None) args argCount FloatStr

    /// Apply a double to float conversion.
    and applyDoubleToFloat env name (args : Expr list) argCount = applyConversion env (function | Double d -> Some (Float (makeFloatRecord (single d.DRValue) None)) | _ -> None) args argCount DoubleStr

    /// Apply an int to float conversion.
    and applyIntToFloat env name (args : Expr list) argCount = applyConversion env (function | Int i -> Some (Float (makeFloatRecord (single i.IRValue) None)) | _ -> None) args argCount IntStr

    /// Apply a float to int conversion.
    and applyFloatToInt env name (args : Expr list) argCount = applyConversion env (function | Float f -> Some (Int (makeIntRecord (int f.FRValue) None)) | _ -> None) args argCount FloatStr

    /// Apply a long to double conversion.
    and applyLongToDouble env name (args : Expr list) argCount = applyConversion env (function | Long l -> Some (Double (makeDoubleRecord (float l.GRValue) None)) | _ -> None) args argCount LongStr

    /// Apply a double to long conversion.
    and applyDoubleToLong env name (args : Expr list) argCount = applyConversion env (function | Double d -> Some (Long (makeLongRecord (int64 d.DRValue) None)) | _ -> None) args argCount DoubleStr

    /// Apply a string to array conversion.
    and applyStringToArray env name (args : Expr list) argCount = applyConversion env (function | String s -> Some (stringToArray env s.SRValue.SVValue) | _ -> None) args argCount StringStr

    /// Apply an array to string conversion.
    and applyArrayToString env name (args : Expr list) argCount = applyConversion env (function | Array a -> Some (arrayToString env a) | _ -> None) args argCount ArrayStr

    /// Apply a list to array conversion.
    and applyListToArray env name (args : Expr list) argCount = applyConversion env (function | List l -> Some (listToArray env l) | _ -> None) args argCount ListStr

    /// Apply an array to list conversion.
    and applyArrayToList env name (args : Expr list) argCount = applyConversion env (function | Array a -> Some (arrayToList env a) | _ -> None) args argCount ArrayStr

    /// Apply an int unop.
    and applyIntUnop intUnop env opName args argCount = applyUnop exprToOptIntValue (fun i -> Int (makeIntRecord i None)) env intUnop args argCount

    /// Apply a long unop.
    and applyLongUnop longUnop env opName args argCount = applyUnop exprToOptLongValue (fun l -> Long (makeLongRecord l None)) env longUnop args argCount

    /// Apply a float unop.
    and applyFloatUnop floatUnop env opName args argCount = applyUnop exprToOptFloatValue (fun f -> Float (makeFloatRecord f None)) env floatUnop args argCount

    /// Apply a double unop.
    and applyDoubleUnop doubleUnop env opName args argCount = applyUnop exprToOptDoubleValue (fun d -> Double (makeDoubleRecord d None)) env doubleUnop args argCount

    /// Apply an int binop.
    and applyIntBinop intBinop env opName args argCount = applyBinop allExprsToIntValues (fun i -> Int (makeIntRecord i None)) env intBinop (fun i -> i = 0) (usesDivisionOperation opName) args argCount

    /// Apply a long binop.
    and applyLongBinop longBinop env opName args argCount = applyBinop allExprsToLongValues (fun l -> Long (makeLongRecord l None)) env longBinop (fun l -> l = 0L) (usesDivisionOperation opName) args argCount

    /// Apply a float binop.
    and applyFloatBinop floatBinop env opName args argCount = applyBinop allExprsToFloatValues (fun f -> Float (makeFloatRecord f None)) env floatBinop absurdity false args argCount

    /// Apply a double binop.
    and applyDoubleBinop doubleBinop env opName args argCount = applyBinop allExprsToDoubleValues (fun d -> Double (makeDoubleRecord d None)) env doubleBinop absurdity false args argCount

    /// Apply a special builtin operation.
    and applySpecialBuiltin env name args argCount =
        match env.EnvOptLanguageModule with
        | None -> makeEvalViolation env ":v/eval/invalidBuiltinOperator" "Built-in operator not found."
        | Some lm -> lm.ApplySpecialBuiltin env name args argCount

    /// The appliable built-in lambdas.
    and appliableBuiltins : (Lun * (Env -> Lun -> Expr list -> int -> EvalResult)) list =
        [(Lun.make FloatFloorStr, applyFloatUnop floatFloor)
         (Lun.make FloatCeilingStr, applyFloatUnop floatCeiling)
         (Lun.make FloatTruncateStr, applyFloatUnop floatTruncate)
         (Lun.make FloatRoundStr, applyFloatUnop floatRound)
         (Lun.make FloatExpStr, applyFloatUnop floatExp)
         (Lun.make FloatLogStr, applyFloatUnop floatLog)
         (Lun.make FloatSqrtStr, applyFloatUnop floatSqrt)
         (Lun.make FloatSinStr, applyFloatUnop floatSin)
         (Lun.make FloatCosStr, applyFloatUnop floatCos)
         (Lun.make FloatTanStr, applyFloatUnop floatTan)
         (Lun.make FloatAsinStr, applyFloatUnop floatAsin)
         (Lun.make FloatAcosStr, applyFloatUnop floatAcos)
         (Lun.make FloatAtanStr, applyFloatUnop floatAtan)
         (Lun.make DoubleFloorStr, applyDoubleUnop Math.Floor)
         (Lun.make DoubleCeilingStr, applyDoubleUnop Math.Ceiling)
         (Lun.make DoubleTruncateStr, applyDoubleUnop Math.Truncate)
         (Lun.make DoubleRoundStr, applyDoubleUnop Math.Round)
         (Lun.make DoubleExpStr, applyDoubleUnop Math.Exp)
         (Lun.make DoubleLogStr, applyDoubleUnop Math.Log)
         (Lun.make DoubleSqrtStr, applyDoubleUnop Math.Sqrt)
         (Lun.make DoubleSinStr, applyDoubleUnop Math.Sin)
         (Lun.make DoubleCosStr, applyDoubleUnop Math.Cos)
         (Lun.make DoubleTanStr, applyDoubleUnop Math.Tan)
         (Lun.make DoubleAsinStr, applyDoubleUnop Math.Asin)
         (Lun.make DoubleAcosStr, applyDoubleUnop Math.Acos)
         (Lun.make DoubleAtanStr, applyDoubleUnop Math.Atan)
         (Lun.make IntPlusStr, applyIntBinop (+))
         (Lun.make IntMinusStr, applyIntBinop (-))
         (Lun.make IntMultiplyStr, applyIntBinop (*))
         (Lun.make IntDivideStr, applyIntBinop (/))
         (Lun.make IntPowStr, applyIntBinop pown)
         (Lun.make IntRemStr, applyIntBinop (%))
         (Lun.make IntIncStr, applyIntUnop intInc)
         (Lun.make IntDecStr, applyIntUnop intDec)
         (Lun.make LongPlusStr, applyLongBinop (+))
         (Lun.make LongMinusStr, applyLongBinop (-))
         (Lun.make LongMultiplyStr, applyLongBinop (*))
         (Lun.make LongDivideStr, applyLongBinop (/))
         (Lun.make LongPowStr, applyLongBinop longPow)
         (Lun.make LongRemStr, applyLongBinop (%))
         (Lun.make LongIncStr, applyLongUnop longInc)
         (Lun.make LongDecStr, applyLongUnop longDec)
         (Lun.make FloatPlusStr, applyFloatBinop (+))
         (Lun.make FloatMinusStr, applyFloatBinop (-))
         (Lun.make FloatMultiplyStr, applyFloatBinop (*))
         (Lun.make FloatDivideStr, applyFloatBinop (/))
         (Lun.make FloatPowStr, applyFloatBinop ( ** ))
         (Lun.make FloatRemStr, applyFloatBinop (%))
         (Lun.make FloatLogNStr, applyFloatBinop floatLogN)
         (Lun.make FloatRootStr, applyFloatBinop floatRoot)
         (Lun.make DoublePlusStr, applyDoubleBinop (+))
         (Lun.make DoubleMinusStr, applyDoubleBinop (-))
         (Lun.make DoubleMultiplyStr, applyDoubleBinop (*))
         (Lun.make DoubleDivideStr, applyDoubleBinop (/))
         (Lun.make DoublePowStr, applyDoubleBinop ( ** ))
         (Lun.make DoubleRemStr, applyDoubleBinop (%))
         (Lun.make DoubleLogNStr, applyDoubleBinop doubleLogN)
         (Lun.make DoubleRootStr, applyDoubleBinop doubleRoot)
         (Lun.make CharEqualStr, applyComparator (=))
         (Lun.make CharInequalStr, applyComparator (<>))
         (Lun.make CharLessThanStr, applyComparator (<))
         (Lun.make CharGreaterThanStr, applyComparator (>))
         (Lun.make CharLessThanOrEqualStr, applyComparator (<=))
         (Lun.make CharGreaterThanOrEqualStr, applyComparator (>=))
         (Lun.make IntEqualStr, applyComparator (=))
         (Lun.make IntInequalStr, applyComparator (<>))
         (Lun.make IntLessThanStr, applyComparator (<))
         (Lun.make IntGreaterThanStr, applyComparator (>))
         (Lun.make IntLessThanOrEqualStr, applyComparator (<=))
         (Lun.make IntGreaterThanOrEqualStr, applyComparator (>=))
         (Lun.make LongEqualStr, applyComparator (=))
         (Lun.make LongInequalStr, applyComparator (<>))
         (Lun.make LongLessThanStr, applyComparator (<))
         (Lun.make LongGreaterThanStr, applyComparator (>))
         (Lun.make LongLessThanOrEqualStr, applyComparator (<=))
         (Lun.make LongGreaterThanOrEqualStr, applyComparator (>=))
         (Lun.make FloatEqualStr, applyComparator (=))
         (Lun.make FloatInequalStr, applyComparator (<>))
         (Lun.make FloatLessThanStr, applyComparator (<))
         (Lun.make FloatGreaterThanStr, applyComparator (>))
         (Lun.make FloatLessThanOrEqualStr, applyComparator (<=))
         (Lun.make FloatGreaterThanOrEqualStr, applyComparator (>=))
         (Lun.make DoubleEqualStr, applyComparator (=))
         (Lun.make DoubleInequalStr, applyComparator (<>))
         (Lun.make DoubleLessThanStr, applyComparator (<))
         (Lun.make DoubleGreaterThanStr, applyComparator (>))
         (Lun.make DoubleLessThanOrEqualStr, applyComparator (<=))
         (Lun.make DoubleGreaterThanOrEqualStr, applyComparator (>=))
         (Lun.make AndStr, applyAnd)
         (Lun.make OrStr, applyOr)
         (Lun.make DocStr, applyDoc)
         (Lun.make IfStr, applyIf)
         (Lun.make ApplyStr, applyApply)
         (Lun.make TypeStr, applyType)
         (Lun.make TypeOfStr, applyTypeOf)
         (Lun.make EqualStr, applyEqual)
         (Lun.make InequalStr, applyInequal)
         (Lun.make RefEqualityStr, applyRefEqual)
         (Lun.make RefInequalityStr, applyRefInequal)
         (Lun.make ConsStr, applyCons)
         (Lun.make HeadStr, applyHead)
         (Lun.make TailStr, applyTail)
         (Lun.make StringLengthStr, applyStringLength)
         (Lun.make StringAppendStr, applyStringAppend)
         (Lun.make ListLengthStr, applyListLength)
         (Lun.make ListAppendStr, applyListAppend)
         (Lun.make ArrayLengthStr, applyArrayLength)
         (Lun.make ArrayAppendStr, applyArrayAppend)
         (Lun.make StepsStr, applySteps)
         (Lun.make WhileStr, applyWhile)
         (Lun.make IsUnitStr, applyIsUnit)
         (Lun.make IsBooleanStr, applyIsBoolean)
         (Lun.make IsIntStr, applyIsInt)
         (Lun.make IsLongStr, applyIsLong)
         (Lun.make IsFloatStr, applyIsFloat)
         (Lun.make IsDoubleStr, applyIsDouble)
         (Lun.make IsCharacterStr, applyIsCharacter)
         (Lun.make IsStringStr, applyIsString)
         (Lun.make IsKeywordStr, applyIsKeyword)
         (Lun.make IsPackageStr, applyIsPackage)
         (Lun.make IsLambdaStr, applyIsLambda)
         (Lun.make IsListStr, applyIsList)
         (Lun.make IsArrayStr, applyIsArray)
         (Lun.make IsCompositeStr, applyIsComposite)
         (Lun.make HasTypeStr, applyHasType)
         (Lun.make HasProtocolStr, applyHasProtocol)
         (Lun.make CharToIntStr, applyCharToInt)
         (Lun.make IntToCharStr, applyIntToChar)
         (Lun.make IntToLongStr, applyIntToLong)
         (Lun.make LongToIntStr, applyLongToInt)
         (Lun.make FloatToDoubleStr, applyFloatToDouble)
         (Lun.make DoubleToFloatStr, applyDoubleToFloat)
         (Lun.make IntToFloatStr, applyIntToFloat)
         (Lun.make FloatToIntStr, applyFloatToInt)
         (Lun.make LongToDoubleStr, applyLongToDouble)
         (Lun.make DoubleToLongStr, applyDoubleToLong)
         (Lun.make StringToArrayStr, applyStringToArray)
         (Lun.make ArrayToStringStr, applyArrayToString)
         (Lun.make ListToArrayStr, applyListToArray)
         (Lun.make ArrayToListStr, applyArrayToList)]

    /// The appliable built-in lambdas in dictionary form.
    and appliableBuiltinDict = 
        List.toDictionary appliableBuiltins

    /// Apply a built-in operator.
    and applyBuiltin env name (args : Expr list) argCount =
        let appliableBuiltin = ref Unchecked.defaultof<Env -> Lun -> Expr list -> int -> EvalResult>
        if appliableBuiltinDict.TryGetValue (name, appliableBuiltin)
        then !appliableBuiltin env name args argCount
        else applySpecialBuiltin env name args argCount

    /// Apply a built-in symbol.
    and applyBuiltinSymbol env args argCount symbol =
        // NOTE: only a built-in function can eval to a symbol expr
        let builtinResult = applyBuiltin env symbol.SymName args argCount
        makeEvalResult env builtinResult.Value

    /// Apply a lambda once its been pushed on the stack frame and its arguments have been unified.
    and applyLambdaPostPushAndUnification env args argCount largs largCount cpre body pre post skipArgEval =
#if AML_OPTIMIZED
        let constraintPassed = true
#else
        let constraintPassed = cpre args
#endif
        if constraintPassed then
            let (preconditionPassed, optPreViolation) = checkContract env args argCount largs largCount pre
            if preconditionPassed then
                let localVars = List.map2 (fun arg larg -> (larg.ArgName, ValueEntry (arg, None))) args largs
                let env'' = appendProceduralEntries env (AppendToNewFrame argCount) localVars
                let resultValue = evalExprDropEnv env'' body
                let result = makeEvalResult env resultValue
                if isUnit post then result // OPTIMIZATION: explicitly checks for unit
                else
                    let postArgs = resultValue :: args
                    let postArgCount = argCount + 1
                    let postLargs = makeArg ResultLun Concrete UnitValue :: largs
                    let postLargCount = largCount + 1
                    let (postconditionPassed, optPostViolation) = checkContract env postArgs postArgCount postLargs postLargCount post
                    if postconditionPassed then result
                    else
                        match optPostViolation with
                        | None -> makeEvalViolation env ":v/contract/postconditionFailed" "Contract postcondition failed."
                        | Some postViolation -> forwardEvalViolation env postViolation
            else
                match optPreViolation with
                | None -> makeEvalViolation env ":v/contract/preconditionFailed" "Contract precondition failed."
                | Some preViolation -> forwardEvalViolation env preViolation
        elif anyViolations args then forwardEvalViolation env (firstViolation args)
        else makeEvalViolation env ":v/contract/constraintFailed" "Constraint not satisfied."

    /// Apply a lambda.
    and applyLambda env (args : Expr list) argCount (largs : Arg list) largCount body cpre pre post skipArgEval lambdaExpr =
        let pushedEnv = pushStackFrame env lambdaExpr
        let evalResult = applyLambdaPostPushAndUnification pushedEnv args argCount largs largCount cpre body pre post skipArgEval
        popEvalResultStackFrame evalResult

    /// Apply a dynamic dispatch as a lambda.
    and applyLambdaDispatch env args argCount largs largCount lambda expr =
        let overlaidEnv = overlayEnv lambda.LamEnv env
        let result = applyLambda overlaidEnv args argCount largs largCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost true expr
        makeEvalResult env result.Value

    /// Apply a dynamic dispatch.
    /// TODO: see if this code could be made more bullet-proof and optimized (such as for when applyDispatch recurs).
    and applyDispatch env args argCount dispatch skipArgEval =
        if not (List.hasAtLeast (dispatch.DispContingentArg + 1) args)
        then makeEvalViolation env ":v/eval/unresolvableDynamicDispatch" ("Cannot resolve dynamic dispatch for '" + dispatch.DispName.LunStr + "' operation. It is missing the argument upon which function selection is contingent.")
        else
            let argValues = if skipArgEval then args else evalExprsDropEnv env args
            let argValueContingent = argValues.[dispatch.DispContingentArg]
            let argValueContingentType = getType env argValueContingent
            match argValueContingentType with
            | Composite ctype ->
                let sigImpl = ref Unchecked.defaultof<Expr>
                if not (ctype.CompSigImpls.TryGetValue (dispatch.DispName, sigImpl))
                then makeEvalViolation env ":v/eval/unresolvedDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName.LunStr + "' operation.")
                else
                    match !sigImpl with
                    | Symbol symbol -> applyBuiltinSymbol env argValues argCount symbol
                    | Lambda lambda as l -> applyLambdaDispatch env argValues argCount lambda.LamArgs lambda.LamArgCount lambda l
                    | Dispatch dispatch2 -> applyDispatch env argValues argCount dispatch2 true
                    | _ -> makeEvalViolation env ":v/eval/invalidDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName.LunStr + "' to either built-in function or lambda.")
            | _ -> makeEvalViolation env ":v/eval/unresolvedDynamicDispatch" ("Could not resolve dynamic dispatch for '" + dispatch.DispName.LunStr + "' operation.")
    
    /// Apply a string selector.
    and applyStringSelector env key (str : string) =
        let evaledKey = evalExprDropEnv env key
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation env v
        | Int int ->
            let index = int.IRValue
            if index >= 0 && index < str.Length then makeEvalResult env (Character (makeCharacterRecord str.[index] None))
            else makeEvalViolation env ":v/contract/selectorOutOfRange" "String index out of range."
        | _ -> makeEvalViolation env ":v/contract/invalidSelectorKeyType" "Only ints can select an element of a string."

    /// Apply a special selector.
    and applySpecialSelector env key target =
        match env.EnvOptLanguageModule with
        | None -> makeEvalViolation env ":v/contract/missingLanguageModule" "Cannot evaluate a special selector without a language module."
        | Some lm -> lm.ApplySpecialSelector env key target

    /// Apply a composite selector.
    and applyCompositeSelector env key selectorType members =
        let key' = if selectorType = FunctionalSelector then evalExprDropEnv env key else key
        match key' with
        | Violation _ as v -> forwardEvalViolation env v
        | Keyword keyword ->
            let name = keyword.KRValue.LunStr
            if not (name.StartsWith MemberPrefixStr)
            then makeEvalViolation env ":v/contract/invalidSelectorKeyType" ("Member selectors using keywords must start with member prefix '" + MemberPrefixStr + "'.")
            else
                let memberNameStr = name.Substring MemberPrefixStr.Length
                let memberName = Lun.make memberNameStr
                let mem = getMember env memberName members
                makeEvalResult env mem
        | Symbol symbol -> let memberName = symbol.SymName in makeEvalResult env (getMember env memberName members)
        | _ -> makeEvalViolation env ":v/contract/invalidSelectorKeyType" ("Only keywords or dotted names can select a member of a struct or composite.")
    
    /// Apply a list selector.
    and applyListSelector env key (list : Expr list) =
        let evaledKey = evalExprDropEnv env key
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation env v
        | Int int ->
            let index = int.IRValue
            if index >= 0 && List.hasAtLeast (index + 1) list then makeEvalResult env list.[index]
            else makeEvalViolation env ":v/contract/selectorOutOfRange" "List index out of range."
        | _ -> makeEvalViolation env ":v/contract/invalidSelectorKeyType" "Only ints can select an element of an list."
    
    /// Apply an array selector.
    and applyArraySelector env key (elements : Expr array) =
        let evaledKey = evalExprDropEnv env key
        match evaledKey with
        | Violation _ as v -> forwardEvalViolation env v
        | Int int ->
            let index = int.IRValue
            if index >= 0 && index < elements.Length then makeEvalResult env elements.[index]
            else makeEvalViolation env ":v/contract/selectorOutOfRange" "Array index out of range."
        | _ -> makeEvalViolation env ":v/contract/invalidSelectorKeyType" "Only ints can select an element of an array."

    /// Evaluate an entry.
    and evalEntry env name entry optPositions =
        match entry with
        | ValueEntry (value, _) -> if isBuiltin env value then makeEvalResult env value else evalExpr env value
        | DynamicEntry (contingentArg, _) -> makeEvalResult env (Dispatch (makeDispatchRecord name contingentArg optPositions))
        | TypeEntry _ -> makeEvalFirstClassViolation env name
        | ProtocolEntry _ -> makeEvalFirstClassViolation env name

    /// Evaluate a conditional.
    and evalConditional env condition =
        let resultValue = evalExprDropEnv env condition
        match resultValue with
        | Violation _ as v -> (false, CSViolation v)
        | Boolean boolean -> (boolean.BRValue, CSNormal)
        | _ -> (false, CSNotBoolean)
    
    /// Evaluate a symbol.
    /// TODO: clean up this function with extraction.
    and evalSymbol env name cachedEntry optPositions =
        match !cachedEntry with
        | CEUncached ->
            let optProceduralEntryPlus = tryFindProceduralEntry env name
            match optProceduralEntryPlus with
            | None ->
                let optDeclarationEntry = tryFindDeclarationEntry env name
                match optDeclarationEntry with
                | None -> makeEvalViolation env ":v/eval/nonexistentSymbol" ("Non-existent symbol '" + name.LunStr + "'.")
                | Some declarationEntry ->
                    if not env.EnvAllowRedeclaration then
                        let newCachedEntry = CEDeclaration declarationEntry
                        cachedEntry := newCachedEntry
                        env.EnvCachedDeclarationEntries.Add cachedEntry
                    evalEntry env name declarationEntry optPositions
            | Some (proceduralEntry, offset, index) ->
                let newCachedEntry = CEProcedural (offset, index)
                cachedEntry := newCachedEntry
                evalEntry env name proceduralEntry optPositions
        | CEDeclaration entry ->
            evalEntry env name entry optPositions
        | CEProcedural (offset, index) ->
            let proceduralFrame = (List.skip offset env.EnvProceduralFrames).Head
            let (_, entry) = proceduralFrame.[index]
            evalEntry env name entry optPositions

    /// Evaluate a special value.
    and evalSpecialValue env specialValue specialValueExpr =
        match env.EnvOptLanguageModule with
        | None -> makeEvalViolation env ":v/languageModule/missingLanguageModule" "Cannot evaluate a special value without a language module."
        | Some lm when lm.Name = specialValue.SVLanguageName ->
            let specialObject = lm.SpecialValueToSpecialObject env specialValueExpr
            match specialObject with
            | Violation _ as v -> forwardEvalViolation env v
            | SpecialObject _ as s -> evalExpr env s
            | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalSpecialValue'."
        | Some lm -> makeEvalViolation env ":v/languageModule/mismatchedLanguageModule" "Wrong language module for special value evaluation."

    /// Evaluate a special object.
    and evalSpecialObject env specialObject specialObjectExpr =
        match env.EnvOptLanguageModule with
        | None -> makeEvalViolation env ":v/languageModule/missingLanguageModule" "Cannot evaluate a special object without a language module."
        | Some lm when lm.Guid = specialObject.SOLanguageGuid -> lm.EvalSpecialObject env specialObjectExpr
        | Some lm -> makeEvalViolation env ":v/languageModule/mismatchedLanguageModule" "Wrong language module for special object evaluation."

    /// Evaluate a violation.
    and evalViolation env violation vioExpr =
        if violation.VioEvaluated then makeEvalResult env vioExpr
        else
            let dataValue = evalExprDropEnv env violation.VioData
            let violation' = { violation with VioData = dataValue }
            let violation'' = { violation' with VioEvaluated = true }
            interveneOnViolation env violation'' (Violation violation'')

    /// Evaluate a prefixed expressions.
    and evalPrefixed env prefixed pfxExpr =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.EvalPrefixed env pfxExpr
        | _ -> makeEvalViolation env ":v/languageModule/missingLanguageModule" "Cannot evaluate prefixed expressions without a language module."

    /// Evaluate an operation.
    and evalOperation env (exprs : Expr list) exprCount optPositions =

        let opValue = evalExprDropEnv env exprs.Head
        let args = exprs.Tail
        let argCount = exprCount - 1

        match opValue with
        | Violation _ as v -> forwardEvalViolation env v
        | Symbol symbol -> applyBuiltinSymbol env args argCount symbol
        | Dispatch dispatch -> applyDispatch env args argCount dispatch false
        | Lambda lambda as l ->
        
            let optUnification = 
                if not lambda.LamEmptyUnification then tryUnifyArgs false args lambda.LamArgs
                else
                    if argCount = lambda.LamArgCount && List.fornone (fun arg -> match arg with Package _ -> true | _ -> false) args // TODO: consider checking for package args in reader
                    then Some (args, lambda.LamArgs)
                    else None

            match optUnification with
            | None -> makeEvalViolation env ":v/eval/malformedLambdaInvocation" "Wrong number or type of arguments."
            | Some (unifiedArgs, unifiedLargs) ->
                let unifiedCount = unifiedArgs.Length
        
                let abstractedArgs =
                        List.map2
                            (fun arg larg ->
                                if larg.ArgType = Abstracting
                                then Lambda (makeLambdaRecord true Lun.empty [] 0 arg tautology UnitValue UnitValue true None (Some env))
                                else arg)
                            unifiedArgs
                            unifiedLargs

                let argValues = evalExprsDropEnv env abstractedArgs
                let overlaidEnv = overlayEnv lambda.LamEnv env
                let lambdaResult = applyLambda overlaidEnv argValues unifiedCount unifiedLargs unifiedCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost true l
                makeEvalResult env lambdaResult.Value

        | _ -> makeEvalViolation env ":v/eval/invalidOperator" ("Invalid operator '" + writeExpr opValue + "'.")
    
    /// Evaluate a lambda.
    and evalLambda env lambda lambdaExpr =
        if lambda.LamEvaluated then makeEvalResult env lambdaExpr
        else
            let lambda' =
                match lambda.LamEnv with
                | None -> { lambda with LamEnv = Some env }
                | Some _ -> lambda
            let lambda'' = { lambda' with LamEvaluated = true }
            makeEvalResult env (Lambda lambda'')

    /// Evaluate an attempt expression.
    and evalAttempt env attemptRecord =
        if attemptRecord.AttemptBranches.IsEmpty then makeEvalViolation env ":v/eval/malformedAttemptOperation" "Attempt operation must have at least 1 branch."
        else
            let bodyValue = evalExprDropEnv env attemptRecord.AttemptBody
            match bodyValue with
            | Violation violation as v ->
                let optBranch = List.tryFind (fun branch -> isInCategory violation.VioCategory branch.ABCategory) attemptRecord.AttemptBranches
                match optBranch with
                | None -> forwardEvalViolation env v
                | Some branch ->
                    let dataValue = evalExprDropEnv env violation.VioData
                    let env' = appendProceduralVariable env (AppendToNewFrame 1) DataLun None dataValue
                    let branchResult = evalExprDropEnv env' branch.ABBody
                    makeEvalResult env branchResult
            | _ -> makeEvalResult env bodyValue

    /// Evaluate a let expression.
    /// NOTE: the semantics for let will vary from the interpreter to a compiled-version in that premature access of
    /// entries in the interpreter will look-up shadowed entries while doing so in compiled code will return a violation.
    /// NOTE: this function is rather imperative and may have tricky operational implications. Use caution when modifying!
    and evalLet env letRecord =
        match letRecord.LetBindings with
        | [] -> makeEvalViolation env ":v/eval/malformedLetOperation" "Let operation must have at least 1 binding."
        | headBinding :: tailBindings ->
            let env' =
                match headBinding with
                | LetVariable (name, body) ->
                    let bodyValue = evalExprDropEnv env body
                    appendProceduralVariable env (AppendToNewFrame letRecord.LetBindingCount) name None bodyValue
                | LetFunction (name, args, argCount, body, optConstraints, pre, post, emptyUnification) ->
                    appendProceduralFunction env (AppendToNewFrame letRecord.LetBindingCount) name args argCount body optConstraints None pre post emptyUnification letRecord.LetOptPositions
            let mutable start = 1
            for binding in tailBindings do
                match binding with
                | LetVariable (name, body) ->
                    let bodyValue = evalExprDropEnv env' body
                    ignore (appendProceduralVariable env' (AppendToHeadFrame start) name None bodyValue)
                | LetFunction (name, args, argCount, body, optConstraints, pre, post, emptyUnification) ->
                    ignore (appendProceduralFunction env' (AppendToHeadFrame start) name args argCount body optConstraints None pre post emptyUnification letRecord.LetOptPositions)
                start <- start + 1
            let result = evalExpr env' letRecord.LetBody
            makeEvalResult env result.Value

    /// Evaluate an extend expression.
    and evalExtend env extend =
        if extend.ExtMembers.Count = 0 then makeEvalViolation env ":v/eval/malformedExtendOperation" "Extend operation must have at least one member."
        else
            let targetValue = evalExprDropEnv env extend.ExtTarget
            match targetValue with
            | Violation _ as v -> forwardEvalViolation env v
            | Composite composite when composite.CompType = CompositeType ->
                let (newCompositeMembers : MemberDict) = Dictionary composite.CompMembers
                for extendMember in extend.ExtMembers do ignore (newCompositeMembers.ForceAdd (extendMember.Key, extendMember.Value))
                if newCompositeMembers.Count - composite.CompMembers.Count < extend.ExtMembers.Count
                then makeEvalViolation env ":v/eval/malformedExtendOperation" "Extend operation may not duplicate composite members."
                else
                    let newComposite = makeCompositeRecord composite.CompEvaluated Lun.empty newCompositeMembers composite.CompType null null extend.ExtOptPositions
                    makeEvalResult env (Composite newComposite)
            | _ -> makeEvalViolation env ":v/eval/malformedExtendOperation" "First argument of extend must be a composite (non-struct)."

    /// Evaluate a case expression.
    and evalCase env case =
        let targetValue = evalExprDropEnv env case.CaseTarget
        match targetValue with
        | Violation _ as v -> forwardEvalViolation env v
        | _ ->
            let branchAndTestResults = Seq.map (fun branch -> (branch, evalExprDropEnv env branch.TBTest)) case.CaseBranches
            let optFirstPassingBranchAndTestResult = Seq.tryFind (fun (_, testResult) -> targetValue = testResult) branchAndTestResults
            match optFirstPassingBranchAndTestResult with
            | None ->
                let branchAndTestResultsList = Seq.toList branchAndTestResults
                let optFirstBranchAndViolation = List.tryFind (fun (_, testResult) -> isViolation testResult) branchAndTestResultsList
                match optFirstBranchAndViolation with
                | None -> makeEvalViolation env ":v/eval/caseWithoutMatch" "A case expression did not find a match."
                | Some (_, firstViolation) -> forwardEvalViolation env firstViolation
            | Some (firstPassingBranch, _) -> evalExpr env firstPassingBranch.TBBody

    /// Evaluate a condition expression.
    and evalCondition env condition =
        let branchAndTestResults = Seq.map (fun branch -> (branch, evalExprDropEnv env branch.TBTest)) condition.CondBranches
        let optFirstPassingBranchAndTestResult = Seq.tryFind (fun (_, testResult) -> isTrue testResult) branchAndTestResults // TODO: check for non-boolean values and return a violation when found
        match optFirstPassingBranchAndTestResult with
        | None ->
            let branchAndTestResultsList = Seq.toList branchAndTestResults
            let optFirstBranchAndViolation = List.tryFind (fun (_, testResult) -> isViolation testResult) branchAndTestResultsList
            match optFirstBranchAndViolation with
            | None -> makeEvalViolation env ":v/eval/condWithoutMatch" "A condition expression did not find a match."
            | Some (_, firstViolation) -> forwardEvalViolation env firstViolation
        | Some (firstPassingBranch, _) -> evalExpr env firstPassingBranch.TBBody

    /// Evaluate an intervene expression, intervening when necessary.
    and evalIntervene env intervene =
        let branches = intervene.ItvBranches
        if branches.IsEmpty then makeEvalViolation env ":v/eval/malformedInterveneOperation" "Intervene operation must have at least 1 branch."
        else
            let branchesWithEnv = List.map (fun branch -> { branch with IBEnv = Some env }) branches
            let env' = pushInterventionBranchList env branchesWithEnv
            let resultValue = evalExprDropEnv env' intervene.ItvBody
            makeEvalResult env resultValue
    
    /// Evaluate a ref operation.
    and evalRef env reference refExpr =
        if not reference.RefEvaluated then
            reference.RefExpr <- evalExprDropEnv env reference.RefExpr
            reference.RefEvaluated <- true
        makeEvalResult env refExpr

    /// Evaluate a get operation.
    and evalGet env get =
        let targetValue = evalExprDropEnv env get.GetTarget
        match targetValue with
        | Violation _ as v -> forwardEvalViolation env v
        | Ref reference -> makeEvalResult env reference.RefExpr
        | _ -> makeEvalViolation env ":v/eval/invalidGetArgument" "Get argument must be a reference."

    /// Evaluate a set operation.
    and evalSet env set =
        let targetValue = evalExprDropEnv env set.SetTarget
        match targetValue with
        | Violation _ as v -> forwardEvalViolation env v
        | Ref reference ->
            let injectionValue = evalExprDropEnv env set.SetInjection
            match injectionValue with
            | Violation _ as v -> forwardEvalViolation env v
            | _ ->
                reference.RefExpr <- injectionValue
                reference.RefEvaluated <- true
                makeEvalResult env targetValue
        | _ -> makeEvalViolation env ":v/eval/invalidSetArgument" "Set target must be reference."

    /// Evaluate a list.
    and evalList env list listExpr =
        if list.ListEvaluated then makeEvalResult env listExpr
        else
            let elementValues = evalExprsDropEnv env list.ListElements
            let list = List (makeListRecord true elementValues list.ListOptPositions)
            makeEvalResult env list

    /// Evaluate an array.
    and evalArray env array arrExpr =
        if array.ArrEvaluated then makeEvalResult env arrExpr
        else
            let elementValues = evalExprArrayDropEnv env array.ArrElements
            let array = Array (makeArrayRecord true elementValues array.ArrOptPositions)
            makeEvalResult env array

    /// Evaluate a composite member.
    and evalMember env mem =
        let value = evalExprDropEnv env mem.MemExpr
        makeMember mem.MemName value

    /// Evaluate composite members.
    and evalMembers env members =
        Dictionary.map (fun kvp -> evalMember env kvp.Value) members

    /// Evaluate a composite.
    and evalComposite env composite compositeExpr =
        if composite.CompEvaluated then makeEvalResult env compositeExpr
        else
            let membersWithValues = evalMembers env composite.CompMembers
            let composite = Composite (makeCompositeRecord true composite.CompName membersWithValues composite.CompType composite.CompSigImpls composite.CompProtocols composite.CompOptPositions)
            makeEvalResult env composite

    /// Evaluate a selector.
    and evalSelector env selector =
        let evaledTarget = evalExprDropEnv env selector.SelTarget
        match evaledTarget with
        | Violation _ as v -> forwardEvalViolation env v
        | String string -> applyStringSelector env selector.SelKey string.SRValue.SVValue
        | SpecialObject specialObject -> applySpecialSelector env selector.SelKey evaledTarget
        | Composite composite -> applyCompositeSelector env selector.SelKey selector.SelType composite.CompMembers
        | List list -> applyListSelector env selector.SelKey list.ListElements
        | Array array -> applyArraySelector env selector.SelKey array.ArrElements
        | _ -> makeEvalViolation env ":v/eval/malformedSelector" "Cannot select elements from this value."
    
    /// Evaluate a variable expression.
    and evalVariable env variable =
        let value = evalExprDropEnv env variable.VarBody
        let optEnv' = tryAppendDeclarationVariable env variable.VarName variable.VarDoc value
        match optEnv' with
        | None -> makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The variable '" + variable.VarName.LunStr + "' clashes names with an existing declaration.")
        | Some env' -> makeEvalUnit env'
    
    /// Evaluate a function expression.
    and evalFunction env fn =
        let optEnv' = tryAppendDeclarationFunction env fn.FnName fn.FnArgs fn.FnArgCount fn.FnBody fn.FnOptConstraints fn.FnDoc fn.FnPre fn.FnPost fn.FnEmptyUnification fn.FnOptPositions
        match optEnv' with
        | None -> makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The function '" + fn.FnName.LunStr + "' clashes names with an existing declaration.")
        | Some env' -> makeEvalUnit env'
    
    /// Evaluate a structure expression.
    and evalStructure env structure =
        if not env.EnvAllowRedeclaration && hasDeclarationEntry env structure.StructName
        then makeEvalViolation env ":v/eval/structShadows" ("The struct '" + structure.StructName.LunStr + "' shadows an existing entry.")
        else
            let symbols = List.map (fun mem -> Symbol (makeSymbolRecord mem (ref CEUncached) structure.StructOptPositions)) structure.StructMemberNames
            let optEnv' = tryAppendStructure env structure.StructName structure.StructMemberNames structure.StructOptConstraints structure.StructDoc structure.StructReq structure.StructMemberNames symbols structure.StructOptPositions
            match optEnv' with
            | None -> makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The struct '" + structure.StructName.LunStr + "' or one of its dependents clashes names with an existing declaration.")
            | Some env' -> let env'' = instantiateEquatable env' structure.StructName in makeEvalUnit env''

    /// Evaluate a protocol.
    and evalProtocol env protocol =
        if not env.EnvAllowRedeclaration && hasDeclarationEntry env protocol.ProtoName
        then makeEvalViolation env ":v/eval/protocolShadows" ("The protocol '" + protocol.ProtoName.LunStr + "' shadows an existing entry.")
        else
            let arg = protocol.ProtoArg
            let optConstraints = protocol.ProtoOptConstraints
            let optConstraintsViolation = getOptConstraintsViolation env [arg] optConstraints
            match optConstraintsViolation with
            | None ->
                let sigs = protocol.ProtoSignatures
                if sigs.IsEmpty then makeEvalViolation env ":v/eval/malformedProtocol" "Protocols must have at least one signature."
                elif not (doSigsHaveAllConcreteArgs sigs) then makeEvalViolation env ":v/eval/malformedProtocolSignature" "Protocol signatures must use only normal arguments."
                else
                    /// OPTIMIZATION: get matching entry lazily
                    let sigsMatchingEntry = Seq.filter (fun signature -> (tryFindDeclarationEntry env signature.SigName).IsSome) sigs
                    let optFirstSigMatchingEntry = Seq.tryHead sigsMatchingEntry
                    match optFirstSigMatchingEntry with
                    | Some firstSigMatchingEntry when not env.EnvAllowRedeclaration ->
                        makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The protocol signature '" + firstSigMatchingEntry.SigName.LunStr + "' clashes names with an existing declaration.")
                    | Some _
                    | None ->
                        let optSigsViolation = getSignaturesViolation env arg sigs
                        if optSigsViolation.IsSome then makeEvalResult env optSigsViolation.Value
                        else
                            let optEnv' = tryAppendProtocol env protocol.ProtoName arg optConstraints protocol.ProtoDoc sigs
                            match optEnv' with
                            | None -> makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The protocol '" + protocol.ProtoName.LunStr + "' clashes names with an existing declaration.")
                            | Some env' -> makeEvalUnit env'
            | Some constraintsViolation -> makeEvalResult env constraintsViolation

    /// Evaluate an instance.
    /// TODO: optimize this generally.
    and evalInstance env instance instanceExpr =
        let optProtocol = tryFindProtocolEntry env instance.InstProtocolName
        match optProtocol with
        | None -> makeEvalViolation env ":v/eval/missingProtocol" "Cannot declare an instance with a non-existent protocol."
        | Some (ProtocolEntry (parg, _, _, psigs)) ->
            let (protocolName, args, sigImpls) = (instance.InstProtocolName, instance.InstArgs, instance.InstFunctions)
            let protocolNameStr = protocolName.LunStr
            if args.IsEmpty then makeEvalViolation env ":v/eval/malformedInstance" "Instances must have at least one argument."
            elif not (List.areSameLength (List.distinct args) args) then makeEvalViolation env ":v/eval/malformedInstance" "All arguments of an instance must be unique."
            elif sigImpls.IsEmpty then makeEvalViolation env ":v/eval/malformedInstance" "Instances must have at least one signature implementation."
            elif List.exists (function | Variable _ -> false | Function fn -> fn.FnArgs.IsEmpty | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalInstance'.") sigImpls then makeEvalViolation env ":v/eval/malformedInstance" "Instance functions must have one argument."
            elif not (List.areSameLength psigs sigImpls) then makeEvalViolation env ":v/eval/malformedInstance" "Instances must have the same number of signature implementations as its protocol has signatures."
            elif not (List.hasAtLeast 1 args) then makeEvalViolation env ":v/eval/malformedInstance" "Instances must have at least one argument."
            else
                let constraints = instance.InstConstraints
                let optConstraintsViolation = getOptConstraintsViolation env args (Some constraints)
                if optConstraintsViolation.IsSome then makeEvalResult env optConstraintsViolation.Value
                else
                    let (constraintsSatisfied, constraintProjections) = projectConstraintsToProtocolArg constraints parg
                    if not constraintsSatisfied then makeEvalViolation env ":v/eval/invalidInstanceConstraint" (writeConstraintFailures constraintProjections)
                    else
                        let projectedImpls = projectSigImpls env protocolName sigImpls
                        if not (tryAppendInstance env protocolName args constraints projectedImpls)
                        then makeEvalViolation env ":v/eval/invalidInstance" "Instances must be instantiated with an existing protocol and not over a protocol."
                        else makeEvalUnit env
        | Some _ -> failwith "Unexpected match failure in 'Aml.Evaluator.evalInstance'."

    /// Evaluate an affirmation expression.
    and evalAffirmation env affirmation =
        let (name, doc, body, optPositions) = (affirmation.AffName, affirmation.AffDoc, affirmation.AffBody, affirmation.AffOptPositions)
        let value = evalExprDropEnv env body
        match value with
        | Violation _ as v -> forwardEvalViolation env v
        | Boolean b when b.BRValue ->
            let optEnv' = tryAppendAffirmationFunction env name doc body optPositions
            match optEnv' with
            | None -> makeEvalViolation env ":v/eval/duplicateDeclarationEntry" ("The affirmation '" + name.LunStr + "' clashes names with an existing declaration.")
            | Some env' -> makeEvalUnit env'
        | Boolean b when not b.BRValue -> makeEvalViolation env ":v/affirmation/affirmationFailure" ("The affirmation '" + name.LunStr + "' was determined to be false.")
        | _ -> makeEvalViolation env ":v/affirmation/invalidResultType" ("Expression for affirmation '" + name.LunStr + "' must return a boolean value.")

    /// Evaluate an Aml file.
    and evalUsingFile env usingFile =
        let fileName = Path.GetFileName usingFile.UFPath
        let directoryPath = getDirectoryRelativeToFile env usingFile.UFPath
        let absolutePath = Path.Combine (directoryPath, fileName)
        let usingFiles = if usingFile.UFReload then Set.empty else env.EnvUsingFiles
        let env' = { env with EnvPath = directoryPath; EnvUsingFiles = usingFiles }
        if env'.EnvUsingFiles.Contains absolutePath then makeEvalUnit env
        else
            try let exprs = runParserOnFile readExprsTillEnd () absolutePath System.Text.Encoding.Default
                let results = sequentiallyEvalReadResults env' exprs false
                if List.isEmpty results then makeEvalUnit env
                else
                    let lastResult = List.last results
                    let values = List.map (fun (result : EvalResult) -> result.Value) results
                    let anyViolationsInValues = anyViolations values
                    let usingFiles' =
                        if usingFile.UFReload && not anyViolationsInValues
                        then Set.union env'.EnvUsingFiles env.EnvUsingFiles
                        else lastResult.Env.EnvUsingFiles
                    // NOTE: even if there are violations during file evaluation, and though we take no definitions from
                    // such a file, the file is still consider 'used'
                    let usingFiles'' = Set.add absolutePath usingFiles'
                    let env'' = if anyViolationsInValues then env else lastResult.Env
                    let env'3 = { env'' with EnvPath = env.EnvPath; EnvUsingFiles = usingFiles'' }
                    if anyViolationsInValues then
                        let violation = firstViolation values
                        forwardEvalViolation env'3 violation
                    else makeEvalUnit env'3
            with exn -> makeEvalExceptionViolation env exn

    /// Evaluate an Aml language.
    and evalUsingLanguage env usingLanguage =
        match env.EnvOptLanguageModule with
        | None ->
            try let assembly = Reflection.Assembly.LoadFrom usingLanguage.ULPath
                let instance = assembly.CreateInstance usingLanguage.ULType
                if instance = null then makeEvalViolation env ":v/languageModule/creationFailure" ("Could not create language module '" + usingLanguage.ULType + "'.")
                else
                    let languageModule = instance :?> ILanguageModule
                    let env' = { env with EnvOptLanguageModule = Some languageModule }
                    let optEnv'' = languageModule.TryInitialize env'
                    match optEnv'' with
                    | None -> makeEvalViolation env ":v/languageModule/creationFailure" ("Could not create language module '" + usingLanguage.ULType + "' due to duplicate declaration names.")
                    | Some env'' -> makeEvalUnit env''
            with exn -> makeEvalExceptionViolation env exn
        // TODO: consider making a violation if a different LM than a current one is loaded
        | Some _ -> makeEvalUnit env

    /// Evaluate a special series.
    and evalSpecialSeries env specialSeries =
        match env.EnvOptLanguageModule with
        | Some lm -> lm.EvalSpecialSeries env specialSeries
        | _ -> makeEvalViolation env ":v/languageModule/missingLanguageModule" "Cannot evaluate special series without a language module."

    /// Evaluate an Aml expression structure.
    and evalExpr env expr =
        let pushedEnv = pushExpr env expr
        let result =
            match expr with
            | Violation violation -> evalViolation pushedEnv violation expr
            | Boolean _ -> makeEvalResult pushedEnv expr
            | Character _ -> makeEvalResult pushedEnv expr
            | String _ -> makeEvalResult pushedEnv expr
            | Int _ -> makeEvalResult pushedEnv expr
            | Long _ -> makeEvalResult pushedEnv expr
            | Float _ -> makeEvalResult pushedEnv expr
            | Double _ -> makeEvalResult pushedEnv expr
            | Keyword _ -> makeEvalResult pushedEnv expr
            | Symbol symbol -> let value = (evalSymbol pushedEnv symbol.SymName symbol.SymCachedEntry symbol.SymOptPositions).Value in makeEvalResult pushedEnv value
            | Package _ -> makeEvalResult pushedEnv expr
            | Prefixed prefixed -> evalPrefixed pushedEnv prefixed expr
            | Dispatch _ -> makeEvalResult pushedEnv expr
            | SpecialValue specialValue -> evalSpecialValue pushedEnv specialValue expr
            | SpecialObject specialObject -> evalSpecialObject pushedEnv specialObject expr
            | Series series -> if series.SerExprs.IsEmpty then makeEvalResult pushedEnv expr else evalOperation pushedEnv series.SerExprs series.SerExprCount series.SerOptPositions
            | Lambda lambda -> evalLambda pushedEnv lambda expr
            | Attempt attemptRecord -> evalAttempt pushedEnv attemptRecord
            | Let letRecord -> evalLet pushedEnv letRecord
            | Extend extend -> evalExtend pushedEnv extend
            | Case case -> evalCase pushedEnv case
            | Condition condition -> evalCondition pushedEnv condition
            | Intervene intervene -> evalIntervene pushedEnv intervene
            | Ref reference -> evalRef pushedEnv reference expr
            | Get get -> evalGet pushedEnv get
            | Set set -> evalSet pushedEnv set
            | List list -> evalList pushedEnv list expr
            | Array array -> evalArray pushedEnv array expr
            | Composite composite -> evalComposite pushedEnv composite expr
            | Selector selector -> evalSelector pushedEnv selector
            | Variable variable -> evalVariable pushedEnv variable
            | Function fn -> evalFunction pushedEnv fn
            | Structure structure -> evalStructure pushedEnv structure
            | Protocol protocol -> evalProtocol pushedEnv protocol
            | Instance instance -> evalInstance pushedEnv instance expr
            | Affirmation affirmation -> evalAffirmation pushedEnv affirmation
            | UsingFile usingFile -> evalUsingFile pushedEnv usingFile
            | UsingLanguage usingLanguage -> evalUsingLanguage pushedEnv usingLanguage
            | SpecialSeries _ -> evalSpecialSeries pushedEnv expr
        popEvalResultExpr result

    /// Evaluate an expression but drop the environment.
    and evalExprDropEnv env expr =
        (evalExpr env expr).Value

    /// Evaluate an expression but drop the result.
    and evalExprDropResult env expr =
        (evalExpr env expr).Env

    /// Evaluate an Aml expression that has been read in.
    and evalReadResult env readResult =
        match readResult with
        | Success (expr, _, _) -> evalExpr env expr
        | Failure (message, _, _) -> makeEvalViolation env ":v/reader/readFailure" ("Could not read an expression due to '" + message + "'.")

    /// Evaluate multiple expressions.
    and evalExprs env exprs =
        List.map (evalExpr env) exprs
    
    /// Evaluate multiple exprs, dropping the environments they produce.
    and evalExprsDropEnv env exprs =
        List.map (evalExprDropEnv env) exprs
    
    /// Evaluate multiple exprs as an array, dropping the environments they produce.
    /// An optimization for evaluating arrays.
    and evalExprArrayDropEnv env exprs =
        Array.map (evalExprDropEnv env) exprs

    /// Evaluate an expr with it on the stack trace. 
    and evalExprWithExplicitTracing env expr =
        let pushedEnv = pushStackFrame env expr
        let result = evalExpr pushedEnv expr
        popEvalResultStackFrame result

    /// Evaluate multiple expressions sequentially.
    and sequentiallyEvalExprs env exprs tillViolation =
        let nextEnv currentEnv (evalResults : EvalResult list) = if evalResults.IsEmpty then currentEnv else evalResults.Head.Env
        let evalResults = List.fold (fun evalResults expr -> evalExprWithExplicitTracing (nextEnv env evalResults) expr :: evalResults) [] exprs
        let evalResultsRev = List.rev evalResults // TODO: see if we can avoid this reverse by using a Queue or List.foldBack
        if tillViolation then List.takeTillInclusive (fun (evalResult : EvalResult) -> isViolation evalResult.Value) evalResultsRev
        else evalResultsRev

    /// Evaluate Aml expressions that have been read in.
    and sequentiallyEvalReadResults env readResults tillViolation =
        match readResults with
        | Success (exprs, _, _) -> sequentiallyEvalExprs env exprs tillViolation
        | Failure (message, _, _) -> [makeEvalViolation env ":v/reader/readFailure" ("Could not read expressions due to '" + message + "'.")]