// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open OpenTK
open Prime
open Nu

/// A scripting language for Nu that is hoped to eventually be a cross between Elm and Unreal Blueprints.
/// TODO: down the line, I'd like to separate the lispy core of the scripting language from the Nu-specific vocabulary,
/// placing the core language into Prime and extending the Nu-specific requirements with a language plug-in of sorts.
module Scripting =

    type [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue); NoComparison>] CachedBinding =
        | UncachedBinding
        | DeclarationBinding of Expr
        | ProceduralBinding of int * int

    and Binding =
        | VariableBinding of string * Expr
        | FunctionBinding of string * string list * Expr

    and [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Codata =
        | Empty
        | Add of Codata * Codata
        | Unfold of Expr * Expr
        | Conversion of Expr list

    // TODO: implement comparison!
    and [<CustomEquality; NoComparison>] Stream =
        // constructed as [variableStream v]
        | VariableStream of string
        // constructed as [eventStream X/Y/Z]
        | EventStream of Expr
        // constructed as [propertyStream P] or [propertyStream P ././.]
        | PropertyStream of string * Expr
        // constructed as [propertyStream P ././@ EntityDispatcher] or [propertyStream P ././@ EntityDispatcher Vanilla]
        // does not allow for wildcards in the relation
        | PropertyStreamMany of string * Expr * Classification
        // not constructable by user. Weakly-typed to simplify type declarations
        | ComputedStream of obj // actual type is Prime.Stream<'p, 'w when 'p :> Participant and 'w :> 'w EventWorld>
        
        static member equals left right =
            match (left, right) with
            | (VariableStream left, VariableStream right) -> left = right
            | (EventStream left, EventStream right) -> left = right
            | (PropertyStream (leftPropertyName, leftTarget), PropertyStream (rightPropertyName, rightTarget)) -> (leftPropertyName, leftTarget) = (rightPropertyName, rightTarget)
            | (PropertyStreamMany (leftPropertyName, leftTarget, leftClassn), PropertyStreamMany (rightPropertyName, rightTarget, rightClassn)) -> (leftPropertyName, leftTarget, leftClassn) = (rightPropertyName, rightTarget, rightClassn)
            | (_, _) -> false

        override this.GetHashCode () =
            // TODO: implement
            failwithnie ()

        override this.Equals that =
            match that with
            | :? Stream as that -> Stream.equals this that
            | _ -> failwithumf ()

    and [<Syntax    ("toEmpty toIdentity toMin toMax " +
                     "not inc dec negate hash " +
                     "pow root sqr sqrt " +
                     "floor ceiling truncate round exp log " +
                     "sin cos tan asin acos atan " +
                     "length normal " +
                     "cross dot " +
                     "violation bool int int64 single double string " +
                     "nil " +
                     "v2 xOf yOf xAs yAs " +
                     "tuple pair unit fst snd thd fth fif nth " +
                     "some none isSome isNone isEmpty notEmpty tryUncons uncons cons commit tryHead head tryTail tail " +
                     "scanWhile scani scan foldWhile foldi fold mapi map filteri filter contains " +
                     // TODO: "either isLeft isRight left right " +
                     "codata toCodata empty " +
                     "list toList rev " +
                     "ring toRing add remove " +
                     "table toTable tryFind find " +
                     "let fun if cond try break get set do " +
                     // TODO: "keyname substring tickRate tickTime update curry compose " +
                     "variableStream eventStream propertyStream " +
                     "define variable equate handle " +
                     // prelude identifiers...
                     "id flip isZero isIdentity isPositive isNegative isPositiveInfinity isNegitiveInfinity isNaN " +
                     "min max compare sign abs fst! snd! reduceWhile reducei reduce takeWhile take skipWhile skip count exists zipBy zip pi e v2Zero v2Identity " +
                     "Gt Lt Eq Positive Negative Zero",
                     "");
          TypeConverter (typeof<ExprConverter>);
          CustomEquality;
          CustomComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of Name list * string * SymbolOrigin option
        | Unit
        | Bool of bool
        | Int of int
        | Int64 of int64
        | Single of single
        | Double of double
        | Vector2 of Vector2
        | String of string
        | Keyword of string

        (* Primitive Data Structures *)
        | Tuple of Expr array
        | Keyphrase of Expr * Expr array
        | Option of Expr option
        | Codata of Codata
        | List of Expr list
        | Ring of Set<Expr>
        | Table of Map<Expr, Expr>
        | Stream of Stream * SymbolOrigin option

        (* Special Forms *)
        | Binding of string * CachedBinding ref * SymbolOrigin option
        | Apply of Expr list * SymbolOrigin option
        | ApplyAnd of Expr list * SymbolOrigin option
        | ApplyOr of Expr list * SymbolOrigin option
        | Let of Binding * Expr * SymbolOrigin option
        | LetMany of Binding list * Expr * SymbolOrigin option
        | Fun of string list * int * Expr * bool * obj option * SymbolOrigin option // TODO: make pars an array?
        | If of Expr * Expr * Expr * SymbolOrigin option
        | Match of Expr * (Expr * Expr) list * SymbolOrigin option
        | Select of (Expr * Expr) list * SymbolOrigin option
        | Try of Expr * (Name list * Expr) list * SymbolOrigin option
        | Do of Expr list * SymbolOrigin option
        | Break of Expr * SymbolOrigin option
        | Get of string * SymbolOrigin option
        | GetFrom of string * Expr * SymbolOrigin option
        | Set of string * Expr * SymbolOrigin option
        | SetTo of string * Expr * Expr * SymbolOrigin option
        | Quote of string * SymbolOrigin option

        (* Declarations - only work at the top level. *)
        | Define of Binding * SymbolOrigin option // constructed as [define c 0]

        static member tryGetOrigin expr =
            match expr with
            | Violation (_, _, originOpt) -> originOpt
            | Unit
            | Bool _
            | Int _
            | Int64 _
            | Single _
            | Double _
            | Vector2 _
            | String _
            | Keyword _
            | Tuple _
            | Keyphrase _
            | Option _
            | Codata _
            | List _
            | Ring _
            | Table _ -> None
            | Stream (_, originOpt)
            | Binding (_, _, originOpt)
            | Apply (_, originOpt)
            | ApplyAnd (_, originOpt)
            | ApplyOr (_, originOpt)
            | Let (_, _, originOpt)
            | LetMany (_, _, originOpt)
            | Fun (_, _, _, _, _, originOpt)
            | If (_, _, _, originOpt)
            | Match (_, _, originOpt)
            | Select (_, originOpt)
            | Try (_, _, originOpt)
            | Do (_, originOpt)
            | Break (_, originOpt)
            | Get (_, originOpt)
            | GetFrom (_, _, originOpt)
            | Set (_, _, originOpt)
            | SetTo (_, _, _, originOpt)
            | Quote (_, originOpt)
            | Define (_, originOpt) -> originOpt

        static member equals left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> (leftNames, leftError) = (rightNames, rightError)
            | (Unit, Unit) -> true
            | (Bool left, Bool right) -> left = right
            | (Int left, Int right) -> left = right
            | (Int64 left, Int64 right) -> left = right
            | (Single left, Single right) -> left = right
            | (Double left, Double right) -> left = right
            | (Vector2 left, Vector2 right) -> left = right
            | (String left, String right) -> left = right
            | (Keyword left, Keyword right) -> left = right
            | (Tuple left, Tuple right) -> left = right
            | (Keyphrase (leftKeyword, leftExprs), Keyphrase (rightKeyword, rightExprs)) -> (leftKeyword, leftExprs) = (rightKeyword, rightExprs)
            | (Option left, Option right) -> left = right
            | (Codata left, Codata right) -> left = right
            | (List left, List right) -> left = right
            | (Ring left, Ring right) -> left = right
            | (Stream (left, _), Stream (right, _)) -> left = right
            | (Binding (left, _, _), Binding (right, _, _)) -> left = right
            | (Apply (left, _), Apply (right, _)) -> left = right
            | (Let (leftBinding, leftBody, _), Let (rightBinding, rightBody, _)) -> (leftBinding, leftBody) = (rightBinding, rightBody)
            | (LetMany (leftBindings, leftBody, _), LetMany (rightBindings, rightBody, _)) -> (leftBindings, leftBody) = (rightBindings, rightBody)
            | (Fun (leftPars, _, leftBody, _, _, _), Fun (rightPars, _, rightBody, _, _, _)) -> (leftPars, leftBody) = (rightPars, rightBody)
            | (If (leftConditional, leftConsequent, leftAlternative, _), If (rightConditional, rightConsequent, rightAlternative, _)) -> (leftConditional, leftConsequent, leftAlternative) = (rightConditional, rightConsequent, rightAlternative)
            | (Match (leftInput, leftCases, _), Match (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Select (left, _), Select (right, _)) -> left = right
            | (Try (leftInput, leftCases, _), Try (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Do (left, _), Do (right, _)) -> left = right
            | (Break (left, _), Break (right, _)) -> left = right
            | (Get (left, _), Get (right, _)) -> left = right
            | (GetFrom (leftPropertyName, leftTarget, _), GetFrom (rightPropertyName, rightTarget, _)) -> (leftPropertyName, leftTarget) = (rightPropertyName, rightTarget)
            | (Set (leftPropertyName, leftInput, _), Set (rightPropertyName, rightInput, _)) -> (leftPropertyName, leftInput) = (rightPropertyName, rightInput)
            | (SetTo (leftPropertyName, leftTarget, leftInput, _), SetTo (rightPropertyName, rightTarget, rightInput, _)) -> (leftPropertyName, leftTarget, leftInput) = (rightPropertyName, rightTarget, rightInput)
            | (Quote (left, _), Quote (right, _)) -> left = right
            | (Define (left, _), Define (right, _)) -> left = right
            | (_, _) -> false

        static member compare left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> compare (leftNames, leftError) (rightNames, rightError)
            | (Unit, Unit) -> 0
            | (Bool left, Bool right) -> compare left right
            | (Int left, Int right) -> compare left right
            | (Int64 left, Int64 right) -> compare left right
            | (Single left, Single right) -> compare left right
            | (Double left, Double right) -> compare left right
            | (Vector2 left, Vector2 right) -> compare (left.X, left.Y) (right.X, right.Y)
            | (String left, String right) -> compare left right
            | (Keyword left, Keyword right) -> compare left right
            | (Tuple left, Tuple right) -> compare left right
            | (Keyphrase (leftKeyword, leftExprs), Keyphrase (rightKeyword, rightExprs)) -> compare (leftKeyword, leftExprs) (rightKeyword, rightExprs)
            | (Option left, Option right) -> compare left right
            | (Codata left, Codata right) -> compare left right
            | (List left, List right) -> compare left right
            | (Ring left, Ring right) -> compare left right
            | (Table left, Table right) -> compare left right
            | (_, _) -> -1 // TODO: ensure this won't break any relevant sorting algorithms

        // TODO: check if we can trust the hash function to be efficient on value types...
        override this.GetHashCode () =
            match this with
            | Violation (names, error, _) -> hash names ^^^ hash error
            | Unit -> 0
            | Bool value -> hash value
            | Int value -> hash value
            | Int64 value -> hash value
            | Single value -> hash value
            | Double value -> hash value
            | Vector2 value -> hash value
            | String value -> hash value
            | Keyword value -> hash value
            | Tuple value -> hash value
            | Keyphrase (valueKeyword, valueExprs) -> hash (valueKeyword, valueExprs)
            | Option value -> hash value
            | Codata value -> hash value
            | List value -> hash value
            | Ring value -> hash value
            | Table value -> hash value
            | _ -> -1

        override this.Equals that =
            match that with
            | :? Expr as that -> Expr.equals this that
            | _ -> failwithumf ()

        interface IComparable<Expr> with
            member this.CompareTo that =
                Expr.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Expr as that -> (this :> IComparable<Expr>).CompareTo that
                | _ -> failwithumf ()

    /// Converts Expr types.
    and ExprConverter () =
        inherit TypeConverter ()

        member this.SymbolToExpr (symbol : Symbol) =
            this.ConvertFrom symbol :?> Expr

        member this.SymbolsToExpr (symbols : Symbol list) =
            List.map this.SymbolToExpr symbols

        member this.BindingToSymbols (binding : Binding) =
            match binding with
            | VariableBinding (name, value) ->
                let nameSymbol = Symbol.Atom (name, None)
                let valueSymbol = this.ExprToSymbol value
                [nameSymbol; valueSymbol]
            | FunctionBinding (name, pars, body) ->
                let nameSymbol = Symbol.Atom (name, None)
                let parSymbols = List.map (fun par -> Symbol.Atom (par, None)) pars
                let bodySymbol = this.ExprToSymbol body
                nameSymbol :: parSymbols @ [bodySymbol]

        member this.BindingToSymbol binding =
            Symbol.Symbols (this.BindingToSymbols binding, None)

        member this.CodataToSymbol codata =
            match codata with
            | Empty -> Symbol.Atom ("empty", None)
            | Add (left, right) -> Symbol.Symbols ([Symbol.Atom ("+", None); this.CodataToSymbol left; this.CodataToSymbol right], None)
            | Unfold (unfolder, state) -> Symbol.Symbols ([Symbol.Atom ("codata", None); this.ExprToSymbol unfolder; this.ExprToSymbol state], None)
            | Conversion source -> Symbol.Symbols ([Symbol.Atom ("toCodata", None); this.ExprsToSymbol source], None)

        member this.ExprToSymbol (expr : Expr) =
            this.ConvertTo (expr, typeof<Symbol>) :?> Symbol

        member this.ExprsToSymbol exprs =
            Symbol.Symbols (List.map this.ExprToSymbol exprs, None)

        member this.SymbolsToBindingOpt bindingSymbols =
            match bindingSymbols with
            | [Atom (bindingName, _); bindingBody] ->
                let binding = VariableBinding (bindingName, this.SymbolToExpr bindingBody)
                Some binding
            | [Atom (bindingName, _); Symbols (bindingArgs, _); bindingBody] ->
                let (bindingArgs, bindingErrors) = List.split (function Atom _ -> true | _ -> false) bindingArgs
                if List.isEmpty bindingErrors then
                    let bindingArgs = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) bindingArgs
                    let binding = FunctionBinding (bindingName, bindingArgs, this.SymbolToExpr bindingBody)
                    Some binding
                else None
            | _ -> None

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then
                let expr = source :?> Expr
                match expr with
                | Violation (names, error, originOpt) ->
                    let violationSymbol = Symbol.Atom ("violation", None)
                    let namesSymbol = Symbol.Atom (String.Join ("/", names), None)
                    let errorSymbol = Symbol.Atom (error, None)
                    Symbol.Symbols ([violationSymbol; namesSymbol; errorSymbol], originOpt) :> obj
                | Unit -> Symbol.Symbols ([], None) :> obj
                | Bool bool -> Symbol.Atom (String.boolToCodeString bool, None) :> obj
                | Int int -> Symbol.Number (string int, None) :> obj
                | Int64 int64 -> Symbol.Number (string int64, None) :> obj
                | Single single -> Symbol.Number (String.singleToCodeString single, None) :> obj
                | Double double -> Symbol.Number (String.doubleToCodeString double, None) :> obj
                | Vector2 v2 -> Symbol.Symbols ([Symbol.Number (String.singleToCodeString v2.X, None); Symbol.Number (String.singleToCodeString v2.Y, None)], None) :> obj
                | String string -> Symbol.Atom (string, None) :> obj
                | Keyword string -> Symbol.Atom ((if String.isEmpty string then "nil" else string), None) :> obj
                | Tuple arr ->
                    let headingSymbol = Symbol.Atom ((if Array.length arr = 2 then "pair" else "tuple"), None)
                    let elemSymbols = arr |> Array.map (fun elem -> this.ExprToSymbol elem) |> List.ofArray
                    Symbol.Symbols (headingSymbol :: elemSymbols, None) :> obj
                | Keyphrase (keyword, arr) ->
                    let keywordSymbol = this.ExprToSymbol keyword
                    let elemSymbols = arr |> Array.map this.ExprToSymbol |> List.ofArray
                    Symbol.Symbols (keywordSymbol :: elemSymbols, None) :> obj
                | Option option ->
                    match option with
                    | Some value -> Symbol.Symbols ([Symbol.Atom ("some", None); this.ExprToSymbol value], None) :> obj
                    | None -> Symbol.Atom ("none", None) :> obj
                | Codata codata ->
                    this.CodataToSymbol codata :> obj
                | List elems ->
                    let listSymbol = Symbol.Atom ("list", None)
                    let elemSymbols = List.map this.ExprToSymbol elems
                    Symbol.Symbols (listSymbol :: elemSymbols, None) :> obj
                | Ring set ->
                    let ringSymbol = Symbol.Atom ("ring", None)
                    let elemSymbols = List.map this.ExprToSymbol (Set.toList set)
                    Symbol.Symbols (ringSymbol :: elemSymbols, None) :> obj
                | Table map ->
                    let tableSymbol = Symbol.Atom ("table", None)
                    let elemSymbols =
                        List.map (fun (key, value) ->
                            let pairSymbol = Symbol.Atom ("pair", None)
                            let keySymbol = this.ExprToSymbol key
                            let valueSymbol = this.ExprToSymbol value
                            Symbol.Symbols ([pairSymbol; keySymbol; valueSymbol], None))
                            (Map.toList map)
                    Symbol.Symbols (tableSymbol :: elemSymbols, None) :> obj
                | Stream _ ->
                    Symbols ([], None) :> obj // TODO: implement
                | Binding (name, _, originOpt) ->
                    Symbol.Atom (name, originOpt) :> obj
                | Apply (exprs, originOpt) ->
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbol.Symbols (exprSymbols, originOpt) :> obj
                | ApplyAnd (exprs, originOpt) ->
                    let logicSymbol = Symbol.Atom ("&&", None)
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbol.Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | ApplyOr (exprs, originOpt) ->
                    let logicSymbol = Symbol.Atom ("||", None)
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbol.Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | Let (binding, body, originOpt) ->
                    let letSymbol = Symbol.Atom ("let", None)
                    let bindingSymbol = this.BindingToSymbol binding
                    let bodySymbol = this.ExprToSymbol body
                    Symbol.Symbols ([letSymbol; bindingSymbol; bodySymbol], originOpt) :> obj
                | LetMany (bindings, body, originOpt) ->
                    let letSymbol = Symbol.Atom ("let", None)
                    let bindingSymbols = Symbol.Symbols (List.map (fun binding -> this.BindingToSymbol binding) bindings, None)
                    let bodySymbol = this.ExprToSymbol body
                    Symbol.Symbols ([letSymbol; bindingSymbols; bodySymbol], originOpt) :> obj
                | Fun (pars, _, body, _, _, originOpt) ->
                    let funSymbol = Symbol.Atom ("fun", None)
                    let parSymbols = List.map (fun par -> Symbol.Atom (par, None)) pars
                    let bodySymbol = this.ExprToSymbol body
                    Symbol.Symbols (funSymbol :: parSymbols @ [bodySymbol], originOpt) :> obj
                | If (condition, consequent, alternative, originOpt) ->
                    let ifSymbol = Symbol.Atom ("if", None)
                    let conditionSymbol = this.ExprToSymbol condition
                    let consequentSymbol = this.ExprToSymbol consequent
                    let alternativeSymbol = this.ExprToSymbol alternative
                    Symbol.Symbols ([ifSymbol; conditionSymbol; consequentSymbol; alternativeSymbol], originOpt) :> obj
                | Match (input, cases, originOpt) ->
                    let matchSymbol = Symbol.Atom ("match", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbol.Symbols ([conditionSymbol; consequentSymbol], None))
                            cases
                    Symbol.Symbols (matchSymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Select (cases, originOpt) ->
                    let selectSymbol = Symbol.Atom ("select", None)
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbol.Symbols ([conditionSymbol; consequentSymbol], None))
                            cases
                    Symbol.Symbols (selectSymbol :: caseSymbols, originOpt) :> obj
                | Try (input, cases, originOpt) ->
                    let trySymbol = Symbol.Atom ("try", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun (tagNames, consequent) ->
                            let tagSymbol = Symbol.Atom (tagNames |> Name.join |> Name.getNameStr, None)
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbol.Symbols ([tagSymbol; consequentSymbol], None))
                            cases
                    Symbol.Symbols (trySymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Do (exprs, originOpt) ->
                    let doSymbol = Symbol.Atom ("do", None)
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbol.Symbols (doSymbol :: exprSymbols, originOpt) :> obj
                | Break (body, originOpt) ->
                    let breakSymbol = Symbol.Atom ("break", None)
                    let bodySymbol = this.ExprToSymbol body
                    Symbol.Symbols ([breakSymbol; bodySymbol], originOpt) :> obj
                | Get (propertyName, originOpt) ->
                    let getSymbol = Symbol.Atom ("get", None)
                    let propertySymbol = Symbol.Atom (propertyName, None)
                    Symbol.Symbols ([getSymbol; propertySymbol], originOpt) :> obj
                | GetFrom (propertyName, relation, originOpt) ->
                    let setSymbol = Symbol.Atom ("get", None)
                    let propertySymbol = Symbol.Atom (propertyName, None)
                    let relationSymbol = this.ExprToSymbol relation
                    Symbol.Symbols ([setSymbol; propertySymbol; relationSymbol], originOpt) :> obj
                | Set (propertyName, value, originOpt) ->
                    let setSymbol = Symbol.Atom ("set", None)
                    let propertySymbol = Symbol.Atom (propertyName, None)
                    let valueSymbol = this.ExprToSymbol value
                    Symbol.Symbols ([setSymbol; propertySymbol; valueSymbol], originOpt) :> obj
                | SetTo (propertyName, value, relation, originOpt) ->
                    let setSymbol = Symbol.Atom ("set", None)
                    let propertySymbol = Symbol.Atom (propertyName, None)
                    let valueSymbol = this.ExprToSymbol value
                    let relationSymbol = this.ExprToSymbol relation
                    Symbol.Symbols ([setSymbol; propertySymbol; valueSymbol; relationSymbol], originOpt) :> obj
                | Quote _ -> Symbols ([], None) :> obj // TODO: implement
                | Define (binding, originOpt) ->
                    let defineSymbol = Symbol.Atom ("define", None)
                    let bindingSymbols = this.BindingToSymbols binding
                    Symbol.Symbols (defineSymbol :: bindingSymbols, originOpt) :> obj
            elif destType = typeof<Expr> then source
            else failconv "Invalid ExprConverter conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Expr>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Prime.Atom (str, originOpt) ->
                    match str with
                    | "true" -> Bool true :> obj
                    | "false" -> Bool false :> obj
                    | "none" -> Option None :> obj
                    | "nil" -> Keyword String.Empty :> obj
                    | "empty" -> Codata Empty :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = '.' || Char.IsUpper firstChar
                        then Keyword str :> obj
                        else Binding (str, ref UncachedBinding, originOpt) :> obj
                | Prime.Number (str, originOpt) ->
                    match Int32.TryParse str with
                    | (false, _) ->
                        // TODO: allow ending with 'l' / 'L' for long
                        match Int64.TryParse str with
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                let str = str.Substring(0, str.Length - 1)
                                match Single.TryParse str with
                                | (true, single) -> Single single :> obj
                                | (false, _) -> Violation ([!!"InvalidForm"; !!"Number"], "Unexpected number parse failure.", originOpt) :> obj
                            else
                                let str = if str.EndsWith "d" || str.EndsWith "D" then str.Substring(0, str.Length - 1) else str
                                match Double.TryParse (str, Globalization.NumberStyles.Float, Globalization.CultureInfo.CurrentCulture) with
                                | (true, double) -> Double double :> obj
                                | (false, _) -> Violation ([!!"InvalidForm"; !!"Number"], "Unexpected number parse failure.", originOpt) :> obj
                        | (true, int64) -> Int64 int64 :> obj
                    | (true, int) -> Int int :> obj
                | Prime.String (str, _) -> String str :> obj
                | Prime.Quote (str, originOpt) -> Quote (str, originOpt) :> obj
                | Prime.Symbols (symbols, originOpt) ->
                    match symbols with
                    | Atom (name, _) :: tail ->
                        match name with
                        | "&&" ->
                            let args = this.SymbolsToExpr tail
                            ApplyAnd (args, originOpt) :> obj
                        | "||" ->
                            let args = this.SymbolsToExpr tail
                            ApplyOr (args, originOpt) :> obj
                        | "violation" ->
                            match tail with
                            | [Prime.Atom (tagStr, _)]
                            | [Prime.String (tagStr, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split tagName, "User-defined violation.", originOpt) :> obj
                                with exn -> Violation ([!!"InvalidForm"; !!"Violation"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | [Prime.Atom (tagStr, _); Prime.String (errorMsg, _)]
                            | [Prime.String (tagStr, _); Prime.String (errorMsg, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split tagName, errorMsg, originOpt) :> obj
                                with exn -> Violation ([!!"InvalidForm"; !!"Violation"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Violation"], "Invalid violation form. Requires 1 tag.", originOpt) :> obj
                        | "let" ->
                            match tail with
                            | [] -> Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | [_] -> Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | [binding; body] ->
                                match binding with
                                | Symbols (bindingSymbols, _) ->
                                    match this.SymbolsToBindingOpt bindingSymbols with
                                    | Some binding -> Let (binding, this.SymbolToExpr body, originOpt) :> obj
                                    | None -> Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                                | _ -> Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | bindingsAndBody ->
                                let (bindings, body) = (List.allButLast bindingsAndBody, List.last bindingsAndBody)
                                let (bindings, bindingErrors) = List.split (function Symbols ([_; _], _) -> true | _ -> false) bindings
                                if List.isEmpty bindingErrors then
                                    let bindings = List.map (function Symbols ([_; _] as binding, _) -> binding | _ -> failwithumf ()) bindings
                                    let bindingOpts = List.map this.SymbolsToBindingOpt bindings
                                    let (bindingOpts, bindingErrors) = List.split Option.isSome bindingOpts
                                    if List.isEmpty bindingErrors then
                                        let bindings = List.definitize bindingOpts
                                        LetMany (bindings, this.SymbolToExpr body, originOpt) :> obj
                                    else Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                                else Violation ([!!"InvalidForm"; !!"Let"], "Invalid let form. TODO: more info.", originOpt) :> obj
                        | "fun" ->
                            match tail with
                            | [args; body] ->
                                match args with
                                | Symbols (args, _) ->
                                    if List.forall (function Atom _ -> true | _ -> false) args then
                                        let args = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) args
                                        Fun (args, List.length args, this.SymbolToExpr body, false, None, originOpt) :> obj
                                    else Violation ([!!"InvalidForm"; !!"Fun"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                                | _ -> Violation ([!!"InvalidForm"; !!"Fun"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Fun"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (this.SymbolToExpr condition, this.SymbolToExpr consequent, this.SymbolToExpr alternative, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"If"], "Invalid if form. Requires 3 arguments.", originOpt) :> obj
                        | "match" ->
                            match tail with
                            | input :: cases ->
                                let input = this.SymbolToExpr input
                                if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                    let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                    let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                    Match (input, cases, originOpt) :> obj
                                else Violation ([!!"InvalidForm"; !!"Match"], "Invalid match form. Requires 1 or more cases.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Match"], "Invalid match form. Requires 1 input and 1 or more cases.", originOpt) :> obj
                        | "select" ->
                            let cases = tail
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                Select (cases, originOpt) :> obj
                            else Violation ([!!"InvalidForm"; !!"Select"], "Invalid select form. Requires 1 or more cases.", originOpt) :> obj
                        | "try" ->
                            match tail with
                            | [body; Prime.Symbols (handlers, _)] ->
                                let handlerEirs =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Prime.Symbols ([Prime.Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (Name.split !!categoriesStr, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (inc i) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split handlerEirs
                                match errors with
                                | [] -> Try (this.SymbolToExpr body, List.map (mapSnd this.SymbolToExpr) handlers, originOpt) :> obj
                                | error :: _ -> Violation ([!!"InvalidForm"; !!"Try"], error, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Try"], "Invalid try form. Requires 1 body and a handler list.", originOpt) :> obj
                        | "do" ->
                            match tail with
                            | [] -> Violation ([!!"InvalidForm"; !!"Do"], "Invalid do form. Requires 1 or more sub-expressions.", originOpt) :> obj
                            | symbols ->
                                let exprs = this.SymbolsToExpr symbols
                                Do (exprs, originOpt) :> obj
                        | "break" ->
                            let content = this.SymbolToExpr (Symbols (tail, originOpt))
                            Break (content, originOpt) :> obj
                        | "get" ->
                            match tail with
                            | Prime.Atom (nameStr, _) :: tail2
                            | Prime.String (nameStr, _) :: tail2 ->
                                match tail2 with
                                | [] -> Get (nameStr, originOpt) :> obj
                                | [relation] -> GetFrom (nameStr, this.SymbolToExpr relation, originOpt) :> obj
                                | _ -> Violation ([!!"InvalidForm"; !!"Get"], "Invalid get form. Requires a name and an optional relation expression.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Get"], "Invalid get form. Requires a name and an optional relation expression.", originOpt) :> obj
                        | "set" ->
                            match tail with
                            | Prime.Atom (nameStr, _) :: value :: tail2
                            | Prime.String (nameStr, _) :: value :: tail2 ->
                                match tail2 with
                                | [] -> Set (nameStr, this.SymbolToExpr value, originOpt) :> obj
                                | [relation] -> SetTo (nameStr, this.SymbolToExpr value, this.SymbolToExpr relation, originOpt) :> obj
                                | _ -> Violation ([!!"InvalidForm"; !!"Set"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"Set"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", originOpt) :> obj
                        | "variableStream" ->
                            match tail with
                            | [Prime.Atom (nameStr, _)]
                            | [Prime.String (nameStr, _)] -> Stream (VariableStream nameStr, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"VariableStream"], "Invalid variable stream form. Requires a name.", originOpt) :> obj
                        | "eventStream" ->
                            match tail with
                            | [relation] -> Stream (EventStream (this.SymbolToExpr relation), originOpt) :> obj
                            | _ -> Violation ([!!"InvalidForm"; !!"EventStream"], "Invalid event stream form. Requires a relation expression.", originOpt) :> obj
                        | "define" ->
                            let bindingSymbols = tail
                            match this.SymbolsToBindingOpt bindingSymbols with
                            | Some binding -> Define (binding, originOpt) :> obj
                            | None -> Violation ([!!"InvalidForm"; !!"Define"], "Invalid define form. TODO: more info.", originOpt) :> obj
                        | _ -> Apply (this.SymbolsToExpr symbols, originOpt) :> obj
                    | _ -> Apply (this.SymbolsToExpr symbols, originOpt) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    /// The true value in scripting.
    let TrueValue = Bool true
    
    /// The false value in scripting.
    let FalseValue = Bool false
    
    /// The none value in scripting.
    let NoneValue = Option None

    /// Log a violation if an expression is one.
    let log expr =
        match expr with
        | Violation (names, error, optOrigin) ->
            Log.info ^
                "Unexpected violation:" + (names |> Name.join |> Name.getNameStr) +
                "\ndue to:" + error +
                "\nat: " + scstring optOrigin + "'."
        | _ -> ()
    
    /// A declaration bindings frame in a scripting environment.
    type DeclarationFrame = Dictionary<string, Expr>
    
    /// A declaration bindings frame in a scripting environment.
    type ProceduralFrame = (string * Expr) array
    
    /// The manner in which bindings are added to a frame.
    type AddType =
        | AddToNewFrame of Size : int
        | AddToHeadFrame of Offset : int

    [<AutoOpen>]
    module EnvModule =
    
        /// The execution environment for scripts.
        type [<NoEquality; NoComparison>] Env =
            private
                { LocalDeclaration : bool
                  GlobalFrame : DeclarationFrame
                  LocalFrame : DeclarationFrame
                  // TODO: consider making this mutable for speed
                  ProceduralFrames : ProceduralFrame list }
    
        [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Env =
    
            let private BottomBinding =
                (String.Empty, Violation ([!!"BottomAccess"], "Accessed a bottom value.", None))

            let getLocalDeclaration env =
                env.LocalDeclaration

            let setLocalDeclaration localDeclaration env =
                { env with LocalDeclaration = localDeclaration }

            let getLocalFrame env =
                env.LocalFrame

            let setLocalFrame localFrame env =
                { env with LocalFrame = localFrame }

            let private makeProceduralFrame size =
                Array.create size BottomBinding

            let private addProceduralFrame frame env =
                { env with ProceduralFrames = frame :: env.ProceduralFrames }

            let private tryGetDeclarationBinding name env =
                match env.LocalFrame.TryGetValue name with
                | (false, _) ->
                    match env.GlobalFrame.TryGetValue name with
                    | (false, _) -> None
                    | (true, value) -> Some value
                | (true, value) -> Some value
    
            let private tryGetProceduralBinding name env =
                let refOffset = ref -1
                let refOptIndex = ref None
                let optBinding =
                    List.tryFindPlus
                        (fun frame ->
                            refOffset := !refOffset + 1
                            refOptIndex := Array.tryFindIndexRev (fun (bindingName, _) -> name.Equals bindingName) frame // OPTIMIZATION: faster than (=) here
                            match !refOptIndex with
                            | Some index -> Some frame.[index]
                            | None -> None)
                        env.ProceduralFrames
                match optBinding with
                | Some (_, binding) -> Some (binding, !refOffset, (!refOptIndex).Value)
                | None -> None
    
            let tryGetBinding name cachedBinding env =
                match !cachedBinding with
                | UncachedBinding ->
                    match tryGetProceduralBinding name env with
                    | None ->
                        match tryGetDeclarationBinding name env with
                        | Some binding ->
                            let newCachedBinding = DeclarationBinding binding
                            cachedBinding := newCachedBinding
                            Some binding
                        | None -> None
                    | Some (binding, offset, index) ->
                        let newCachedBinding = ProceduralBinding (offset, index)
                        cachedBinding := newCachedBinding
                        Some binding
                | DeclarationBinding binding ->
                    Some binding
                | ProceduralBinding (offset, index) ->
                    let frame = (List.skip offset env.ProceduralFrames).Head
                    let (_, binding) = frame.[index]
                    Some binding

            let addDeclarationBinding name value env =
                if env.LocalDeclaration
                then env.LocalFrame.ForceAdd (name, value)
                else env.GlobalFrame.ForceAdd (name, value)
                env
    
            let addProceduralBinding appendType name value env =
                match appendType with
                | AddToNewFrame size ->
                    let frame = makeProceduralFrame size
                    frame.[0] <- (name, value)
                    addProceduralFrame frame env
                | AddToHeadFrame offset ->
                    match env.ProceduralFrames with
                    | frame :: _ ->
                        frame.[offset] <- (name, value)
                        env
                    | [] -> failwithumf ()
    
            let addProceduralBindings appendType bindings env =
                match appendType with
                | AddToNewFrame size ->
                    let frame = makeProceduralFrame size
                    let mutable index = 0
                    for binding in bindings do
                        frame.[index] <- binding
                        index <- index + 1
                    addProceduralFrame frame env
                | AddToHeadFrame start ->
                    match env.ProceduralFrames with
                    | frame :: _ ->
                        let mutable index = start
                        for binding in bindings do
                            frame.[index] <- binding
                            index <- index + 1
                        env
                    | [] -> failwithumf ()

            let addBinding appendType name value env =
                let isTopLevel = List.isEmpty env.ProceduralFrames
                if isTopLevel then addDeclarationBinding name value env
                else addProceduralBinding appendType name value env
                
            let removeProceduralBindings env =
                match env.ProceduralFrames with
                | [] -> failwithumf ()
                | _ :: tail -> { env with ProceduralFrames = tail }

            let getProceduralFrames env =
                env.ProceduralFrames

            let setProceduralFrames proceduralFrames env =
                { env with ProceduralFrames = proceduralFrames }
    
            let make () =
                { LocalDeclaration = false
                  GlobalFrame = DeclarationFrame HashIdentity.Structural
                  LocalFrame = DeclarationFrame HashIdentity.Structural
                  ProceduralFrames = [] }

    /// The execution environment for scripts.
    type Env = EnvModule.Env

    /// Attempting to expose Env module contents as well, but does not seem to work...
    module Env = EnvModule.Env