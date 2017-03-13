// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open Prime
module Scripting =

    type Pluggable =
        inherit IComparable
        abstract member GetName : unit -> string
        abstract member ToSymbol : unit -> Symbol

    type [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue); NoComparison>] CachedBinding =
        | UncachedBinding
        | DeclarationBinding of Expr
        | ProceduralBinding of int * int

    and Binding =
        | VariableBinding of string * Expr
        | FunctionBinding of string * string array * Expr

    and [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Codata =
        | Empty
        | Add of Codata * Codata
        | Unfold of Expr * Expr
        | Conversion of Expr list

    and Breakpoint =
        { mutable BreakEnabled : bool
          mutable BreakCondition : Expr }

    and [<Syntax
            ((* Built-in Identifiers *)
             "true false nil " +
             "not hash toEmpty toIdentity toMin toMax " +
             "inc dec negate " +
             "pow root sqr sqrt " +
             "floor ceiling truncate round exp log " +
             "sin cos tan asin acos atan " +
             "length normal cross dot " +
             "violation bool int int64 single double string " +
             "typename " +
             "tryIndex hasIndex index nameOf " +
             "record " +
             "tuple pair unit fst snd thd fth fif " +
             "some none isSome isNone isEmpty notEmpty " +
             "tryUncons uncons cons commit tryHead head tryTail tail " +
             "scanWhile scani scan foldWhile foldi fold mapi map contains " +
             // TODO: "either isLeft isRight left right " +
             "codata toCodata empty " +
             "list toList " +
             "ring toRing add remove " +
             "table toTable " +
             "let fun if match select try do break get set define " +

             (* Prelude Identifiers *)
             // TODO: "substring update curry compose sort replace slice split " +
             "-u- -b- -i- -L- -f- -d- -s- -K- -T- -U- -o- -l- -r- -t- -R- " +
             "isUnit isBool isInt isInt64 isSingle isDouble isString " +
             "isKeyword isTuple isUnion isOption isList isRing isTable isRecord " +
             "id flip isZero isIdentity isPositive isNegative isPositiveInfinity isNegativeInfinity isNaN " +
             "min max compare sign abs fst! snd! rev foldBackWhile foldBacki foldBack " +
             "reduceWhile reducei reduce definitize filter takeWhile take skipWhile skip " +
             "countBy count notContains exists notExists zipBy zip pi e v2Zero v2Identity",

             (* Keywords *)
             "Gt Lt Eq Positive Negative Zero",

             (* Title Words *)
             "record if match",

             (* Header Words *)
             "define fun",

             (* Detail Words *)
             "get set",
             Constants.PrettyPrinter.DefaultThresholdMin,
             Constants.PrettyPrinter.DefaultThresholdMax);
          TypeConverter (typeof<ExprConverter>);
          CustomEquality;
          CustomComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of string list * string * SymbolOrigin option
        | Unit
        | Bool of bool
        | Int of int
        | Int64 of int64
        | Single of single
        | Double of double
        | String of string
        | Keyword of string
        | Pluggable of Pluggable

        (* Primitive Data Structures *)
        | Tuple of Expr array
        | Union of string * Expr array
        | Option of Expr option
        | Codata of Codata
        | List of Expr list
        | Ring of Set<Expr>
        | Table of Map<Expr, Expr>

        (* Intermediate Data Structures *)
        | Record of string * Map<string, int> * Expr array
        | UnionUnevaled of string * Expr array
        | TableUnevaled of (Expr * Expr) list
        | RecordUnevaled of string * (string * Expr) list

        (* Special Forms *)
        | Binding of string * CachedBinding ref * SymbolOrigin option
        | Apply of Expr array * Breakpoint * SymbolOrigin option
        | ApplyAnd of Expr array * Breakpoint * SymbolOrigin option
        | ApplyOr of Expr array * Breakpoint * SymbolOrigin option
        | Let of Binding * Expr * SymbolOrigin option
        | LetMany of Binding list * Expr * SymbolOrigin option
        | Fun of string array * int * Expr * bool * obj option * SymbolOrigin option
        | If of Expr * Expr * Expr * SymbolOrigin option
        | Match of Expr * (Expr * Expr) array * SymbolOrigin option
        | Select of (Expr * Expr) array * SymbolOrigin option
        | Try of Expr * (string list * Expr) list * SymbolOrigin option
        | Do of Expr list * SymbolOrigin option
        | Quote of Expr * SymbolOrigin option

        (* Declarations - only work at the top level. *)
        | Define of Binding * SymbolOrigin option

        static member tryGetOrigin expr =
            match expr with
            | Violation (_, _, originOpt) -> originOpt
            | Unit
            | Bool _
            | Int _
            | Int64 _
            | Single _
            | Double _
            | String _
            | Keyword _
            | Tuple _
            | Union _
            | Pluggable _
            | Option _
            | Codata _
            | List _
            | Ring _
            | Table _
            | Record _
            | UnionUnevaled _
            | TableUnevaled _
            | RecordUnevaled _ -> None
            | Binding (_, _, originOpt)
            | Apply (_, _, originOpt)
            | ApplyAnd (_, _, originOpt)
            | ApplyOr (_, _, originOpt)
            | Let (_, _, originOpt)
            | LetMany (_, _, originOpt)
            | Fun (_, _, _, _, _, originOpt)
            | If (_, _, _, originOpt)
            | Match (_, _, originOpt)
            | Select (_, originOpt)
            | Try (_, _, originOpt)
            | Do (_, originOpt)
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
            | (String left, String right) -> left = right
            | (Keyword left, Keyword right) -> left = right
            | (Pluggable left, Pluggable right) -> left = right
            | (Tuple left, Tuple right) -> left = right
            | (Union (leftName, leftExprs), Union (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (Option left, Option right) -> left = right
            | (Codata left, Codata right) -> left = right
            | (List left, List right) -> left = right
            | (Ring left, Ring right) -> left = right
            | (Table left, Table right) -> left = right
            | (Record (leftName, leftMap, leftExprs), Record (rightName, rightMap, rightExprs)) -> (leftName, leftMap, leftExprs) = (rightName, rightMap, rightExprs)
            | (UnionUnevaled (leftName, leftExprs), UnionUnevaled (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (TableUnevaled left, TableUnevaled right) -> left = right
            | (RecordUnevaled (leftName, leftExprs), RecordUnevaled (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (Binding (left, _, _), Binding (right, _, _)) -> left = right
            | (Apply (left, _, _), Apply (right, _, _)) -> left = right
            | (ApplyAnd (left, _, _), ApplyAnd (right, _, _)) -> left = right
            | (ApplyOr (left, _, _), ApplyOr (right, _, _)) -> left = right
            | (Let (leftBinding, leftBody, _), Let (rightBinding, rightBody, _)) -> (leftBinding, leftBody) = (rightBinding, rightBody)
            | (LetMany (leftBindings, leftBody, _), LetMany (rightBindings, rightBody, _)) -> (leftBindings, leftBody) = (rightBindings, rightBody)
            | (Fun (leftPars, _, leftBody, _, _, _), Fun (rightPars, _, rightBody, _, _, _)) -> (leftPars, leftBody) = (rightPars, rightBody)
            | (If (leftConditional, leftConsequent, leftAlternative, _), If (rightConditional, rightConsequent, rightAlternative, _)) -> (leftConditional, leftConsequent, leftAlternative) = (rightConditional, rightConsequent, rightAlternative)
            | (Match (leftInput, leftCases, _), Match (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Select (left, _), Select (right, _)) -> left = right
            | (Try (leftInput, leftCases, _), Try (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Do (left, _), Do (right, _)) -> left = right
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
            | (String left, String right) -> compare left right
            | (Keyword left, Keyword right) -> compare left right
            | (Pluggable left, Pluggable right) -> compare left right
            | (Tuple left, Tuple right) -> compare left right
            | (Union (leftName, leftExprs), Union (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (Option left, Option right) -> compare left right
            | (Codata left, Codata right) -> compare left right
            | (List left, List right) -> compare left right
            | (Ring left, Ring right) -> compare left right
            | (Table left, Table right) -> compare left right
            | (Record (leftName, leftMap, leftExprs), Record (rightName, rightMap, rightExprs)) -> compare (leftName, leftMap, leftExprs) (rightName, rightMap, rightExprs)
            | (UnionUnevaled (leftName, leftExprs), UnionUnevaled (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (TableUnevaled left, TableUnevaled right) -> compare left right
            | (RecordUnevaled (leftName, leftExprs), RecordUnevaled (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (_, _) -> -1

        override this.GetHashCode () =
            match this with
            | Violation (names, error, _) -> hash names ^^^ hash error
            | Unit -> 0
            | Bool value -> hash value
            | Int value -> hash value
            | Int64 value -> hash value
            | Single value -> hash value
            | Double value -> hash value
            | String value -> hash value
            | Keyword value -> hash value
            | Pluggable value -> hash value
            | Tuple value -> hash value
            | Union (name, fields) -> hash (name, fields)
            | Option value -> hash value
            | Codata value -> hash value
            | List value -> hash value
            | Ring value -> hash value
            | Table value -> hash value
            | Record (name, map, fields) -> hash (name, map, fields)
            | UnionUnevaled (name, fields) -> hash (name, fields)
            | TableUnevaled value -> hash value
            | RecordUnevaled (name, fields) -> hash (name, fields)
            | _ -> -1

        override this.Equals that =
            match that with
            | :? Expr as that -> Expr.equals this that
            | _ -> failwithumf ()

        interface Expr IComparable with
            member this.CompareTo that =
                Expr.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Expr as that -> (this :> Expr IComparable).CompareTo that
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
                let nameSymbol = Atom (name, None)
                let valueSymbol = this.ExprToSymbol value
                [nameSymbol; valueSymbol]
            | FunctionBinding (name, pars, body) ->
                let nameSymbol = Atom (name, None)
                let parSymbols = Array.map (fun par -> Atom (par, None)) pars
                let parsSymbol = Symbols (List.ofArray parSymbols, None)
                let bodySymbol = this.ExprToSymbol body
                [nameSymbol; parsSymbol; bodySymbol]

        member this.BindingToSymbol binding =
            Symbols (this.BindingToSymbols binding, None)

        member this.CodataToSymbol codata =
            match codata with
            | Empty -> Atom ("empty", None)
            | Add (left, right) -> Symbols ([Atom ("+", None); this.CodataToSymbol left; this.CodataToSymbol right], None)
            | Unfold (unfolder, state) -> Symbols ([Atom ("codata", None); this.ExprToSymbol unfolder; this.ExprToSymbol state], None)
            | Conversion source -> Symbols ([Atom ("toCodata", None); this.ExprsToSymbol source], None)

        member this.ExprToSymbol (expr : Expr) =
            this.ConvertTo (expr, typeof<Symbol>) :?> Symbol

        member this.ExprsToSymbol exprs =
            Symbols (List.map this.ExprToSymbol exprs, None)

        member this.SymbolsToBindingOpt bindingSymbols =
            match bindingSymbols with
            | [Atom (bindingName, _); bindingBody] ->
                let binding = VariableBinding (bindingName, this.SymbolToExpr bindingBody)
                Some binding
            | [Atom (bindingName, _); Symbols (bindingArgs, _); bindingBody] ->
                let (bindingArgs, bindingErrors) = List.split (function Atom _ -> true | _ -> false) bindingArgs
                if List.isEmpty bindingErrors then
                    let bindingArgs = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) bindingArgs
                    let binding = FunctionBinding (bindingName, Array.ofList bindingArgs, this.SymbolToExpr bindingBody)
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
                    let violationSymbol = Atom ("violation", None)
                    let namesSymbol = Atom (String.concat Constants.Scripting.ViolationSeparatorStr names, None)
                    let errorSymbol = Atom (error, None)
                    Symbols ([violationSymbol; namesSymbol; errorSymbol], originOpt) :> obj
                | Unit -> Symbols ([], None) :> obj
                | Bool bool -> Atom (String.boolToCodeString bool, None) :> obj
                | Int int -> Number (string int, None) :> obj
                | Int64 int64 -> Number (String.int64ToCodeString int64, None) :> obj
                | Single single -> Number (String.singleToCodeString single, None) :> obj
                | Double double -> Number (String.doubleToCodeString double, None) :> obj
                | String string -> Atom (string, None) :> obj
                | Keyword string -> Atom ((if String.isEmpty string then "nil" else string), None) :> obj
                | Pluggable pluggable -> pluggable.ToSymbol () :> obj
                | Tuple fields ->
                    let headingSymbol = Atom ((if Array.length fields = 2 then "pair" else "tuple"), None)
                    let elemSymbols = fields |> Array.map (fun elem -> this.ExprToSymbol elem) |> List.ofArray
                    Symbols (headingSymbol :: elemSymbols, None) :> obj
                | Union (name, fields) ->
                    let nameSymbol = Atom (name, None)
                    let elemSymbols = fields |> Array.map this.ExprToSymbol |> List.ofArray
                    Symbols (nameSymbol :: elemSymbols, None) :> obj
                | Option option ->
                    match option with
                    | Some value -> Symbols ([Atom ("some", None); this.ExprToSymbol value], None) :> obj
                    | None -> Atom ("none", None) :> obj
                | Codata codata ->
                    this.CodataToSymbol codata :> obj
                | List elems ->
                    let listSymbol = Atom ("list", None)
                    let elemSymbols = List.map this.ExprToSymbol elems
                    Symbols (listSymbol :: elemSymbols, None) :> obj
                | Ring set ->
                    let ringSymbol = Atom ("ring", None)
                    let elemSymbols = List.map this.ExprToSymbol (Set.toList set)
                    Symbols (ringSymbol :: elemSymbols, None) :> obj
                | Table map ->
                    let tableSymbol = Atom ("table", None)
                    let elemSymbols =
                        List.map (fun (key, value) ->
                            let keySymbol = this.ExprToSymbol key
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            (Map.toList map)
                    Symbols (tableSymbol :: elemSymbols, None) :> obj
                | Record (name, map, fields) ->
                    let recordSymbol = Atom ("record", None)
                    let nameSymbol = Atom (name, None)
                    let mapSwap = Map.ofSeqBy (fun (kvp : KeyValuePair<_, _>) -> (kvp.Value, kvp.Key)) map
                    let fieldSymbols =
                        Seq.map (fun (kvp : KeyValuePair<_, _>) ->
                            let key = kvp.Value
                            let value = fields.[kvp.Key]
                            let keySymbol = Atom (key, None)
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            mapSwap
                    Symbols (recordSymbol :: nameSymbol :: List.ofSeq fieldSymbols, None) :> obj
                | UnionUnevaled (name, fields) ->
                    let nameSymbol = Atom (name, None)
                    let elemSymbols = fields |> Array.map this.ExprToSymbol |> List.ofArray
                    Symbols (nameSymbol :: elemSymbols, None) :> obj
                | TableUnevaled entries ->
                    let tableSymbol = Atom ("table", None)
                    let elemSymbols =
                        List.map (fun (key, value) ->
                            let keySymbol = this.ExprToSymbol key
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            entries
                    Symbols (tableSymbol :: elemSymbols, None) :> obj
                | RecordUnevaled (name, fields) ->
                    let recordSymbol = Atom ("record", None)
                    let nameSymbol = Atom (name, None)
                    let fieldSymbols = List.map (fun (name, field) -> Symbols ([Atom (name, None); this.ExprToSymbol field], None)) fields
                    Symbols (recordSymbol :: nameSymbol :: fieldSymbols, None) :> obj
                | Binding (name, _, originOpt) ->
                    if name = "index"
                    then Atom ("Index", originOpt) :> obj
                    else Atom (name, originOpt) :> obj
                | Apply (exprs, _, originOpt) ->
                    let exprSymbols = Array.map this.ExprToSymbol exprs
                    Symbols (List.ofArray exprSymbols, originOpt) :> obj
                | ApplyAnd (exprs, _, originOpt) ->
                    let logicSymbol = Atom ("&&", None)
                    let exprSymbols = List.map this.ExprToSymbol (List.ofArray exprs)
                    Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | ApplyOr (exprs, _, originOpt) ->
                    let logicSymbol = Atom ("||", None)
                    let exprSymbols = List.map this.ExprToSymbol (List.ofArray exprs)
                    Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | Let (binding, body, originOpt) ->
                    let letSymbol = Atom ("let", None)
                    let bindingSymbol = this.BindingToSymbol binding
                    let bodySymbol = this.ExprToSymbol body
                    Symbols ([letSymbol; bindingSymbol; bodySymbol], originOpt) :> obj
                | LetMany (bindings, body, originOpt) ->
                    let letSymbol = Atom ("let", None)
                    let bindingSymbols = List.map (fun binding -> this.BindingToSymbol binding) bindings
                    let bodySymbol = this.ExprToSymbol body
                    Symbols (letSymbol :: bindingSymbols @ [bodySymbol], originOpt) :> obj
                | Fun (pars, _, body, _, _, originOpt) ->
                    let funSymbol = Atom ("fun", None)
                    let parSymbols = Array.map (fun par -> Atom (par, None)) pars
                    let parsSymbol = Symbols (List.ofArray parSymbols, None)
                    let bodySymbol = this.ExprToSymbol body
                    Symbols ([funSymbol; parsSymbol; bodySymbol], originOpt) :> obj
                | If (condition, consequent, alternative, originOpt) ->
                    let ifSymbol = Atom ("if", None)
                    let conditionSymbol = this.ExprToSymbol condition
                    let consequentSymbol = this.ExprToSymbol consequent
                    let alternativeSymbol = this.ExprToSymbol alternative
                    Symbols ([ifSymbol; conditionSymbol; consequentSymbol; alternativeSymbol], originOpt) :> obj
                | Match (input, cases, originOpt) ->
                    let matchSymbol = Atom ("match", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([conditionSymbol; consequentSymbol], None))
                            (List.ofArray cases)
                    Symbols (matchSymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Select (cases, originOpt) ->
                    let selectSymbol = Atom ("select", None)
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([conditionSymbol; consequentSymbol], None))
                            (List.ofArray cases)
                    Symbols (selectSymbol :: caseSymbols, originOpt) :> obj
                | Try (input, cases, originOpt) ->
                    let trySymbol = Atom ("try", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun ((tagNames : string list), consequent) ->
                            let tagSymbol = Atom (String.concat Constants.Scripting.ViolationSeparatorStr tagNames, None)
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([tagSymbol; consequentSymbol], None))
                            cases
                    Symbols (trySymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Do (exprs, originOpt) ->
                    let doSymbol = Atom ("do", None)
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbols (doSymbol :: exprSymbols, originOpt) :> obj
                | Quote (expr, originOpt) ->
                    Symbol.Quote (this.ExprToSymbol expr, originOpt) :> obj
                | Define (binding, originOpt) ->
                    let defineSymbol = Atom ("define", None)
                    Symbols (defineSymbol :: this.BindingToSymbols binding, originOpt) :> obj
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
                    | "true" | "True" -> Bool true :> obj
                    | "false" | "False" -> Bool false :> obj
                    | "none" | "None" -> Option None :> obj
                    | "nil" -> Keyword String.Empty :> obj
                    | "empty" -> Codata Empty :> obj
                    | "Index" -> Binding ("index", ref UncachedBinding, originOpt) :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = Constants.Relation.Slot || Char.IsUpper firstChar
                        then Keyword str :> obj
                        else Binding (str, ref UncachedBinding, originOpt) :> obj
                | Prime.Number (str, originOpt) ->
                    match Int32.TryParse str with
                    | (false, _) ->
                        let str = if str.EndsWith "l" || str.EndsWith "L" then str.Substring(0, str.Length - 1) else str
                        match Int64.TryParse str with
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                let str = str.Substring(0, str.Length - 1)
                                match Single.TryParse str with
                                | (true, single) -> Single single :> obj
                                | (false, _) -> Violation (["InvalidForm"; "Number"], "Unexpected numeric parse failure.", originOpt) :> obj
                            else
                                let str = if str.EndsWith "d" || str.EndsWith "D" then str.Substring(0, str.Length - 1) else str
                                match Double.TryParse (str, Globalization.NumberStyles.Float, Globalization.CultureInfo.CurrentCulture) with
                                | (true, double) -> Double double :> obj
                                | (false, _) -> Violation (["InvalidForm"; "Number"], "Unexpected numeric parse failure.", originOpt) :> obj
                        | (true, int64) -> Int64 int64 :> obj
                    | (true, int) -> Int int :> obj
                | Prime.String (str, _) -> String str :> obj
                | Prime.Quote (quoted, originOpt) -> Quote (this.SymbolToExpr quoted, originOpt) :> obj
                | Prime.Symbols (symbols, originOpt) ->
                    match symbols with
                    | [] -> Unit :> obj
                    | Atom (name, _) :: tail ->
                        match name with
                        | "&&" ->
                            let args = this.SymbolsToExpr tail
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            ApplyAnd (Array.ofList args, breakpoint, originOpt) :> obj
                        | "||" ->
                            let args = this.SymbolsToExpr tail
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            ApplyOr (Array.ofList args, breakpoint, originOpt) :> obj
                        | "violation" ->
                            match tail with
                            | [Prime.Atom (tagStr, _)]
                            | [Prime.String (tagStr, _)] ->
                                try let tagName = tagStr in Violation (tagName.Split Constants.Scripting.ViolationSeparator |> List.ofArray, "User-defined Violation.", originOpt) :> obj
                                with exn -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | [Prime.Atom (tagStr, _); Prime.String (errorMsg, _)]
                            | [Prime.String (tagStr, _); Prime.String (errorMsg, _)] ->
                                try let tagName = tagStr in Violation (tagName.Split Constants.Scripting.ViolationSeparator |> List.ofArray, errorMsg, originOpt) :> obj
                                with exn -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Requires 1 tag.", originOpt) :> obj
                        | "table" ->
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) tail then
                                let entries = List.map (function Symbols ([key; value], _) -> (this.SymbolToExpr key, this.SymbolToExpr value) | _ -> failwithumf ()) tail
                                TableUnevaled entries :> obj
                            else Violation (["InvalidForm"; "Table"], "Invalid Table form. Requires 1 or more field definitions.", originOpt) :> obj
                        | "record" ->
                            match tail with
                            | Atom (name, _) :: cases ->
                                if List.forall (function Symbols ([Atom _; _], _) -> true | _ -> false) cases then
                                    let definitions = List.map (function Symbols ([Atom (fieldName, _); fieldValue], _) -> (fieldName, fieldValue) | _ -> failwithumf ()) cases
                                    let definitions = List.map (fun (fieldName, fieldValue) -> (fieldName, this.SymbolToExpr fieldValue)) definitions
                                    let map = definitions |> List.mapi (fun i (fieldName, _) -> (fieldName, i)) |> Map.ofList
                                    Record (name, map, definitions |> List.map snd |> Array.ofList) :> obj
                                else Violation (["InvalidForm"; "Record"], "Invalid Record form. Requires 1 or more field definitions.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Record"], "Invalid Record form. Requires 1 name and 1 or more field definitions.", originOpt) :> obj
                        | "let" ->
                            match tail with
                            | [] -> Violation (["InvalidForm"; "Let"], "Invalid let form. Requires both a binding and a body.", originOpt) :> obj
                            | [_] -> Violation (["InvalidForm"; "Let"], "Invalid let form. Requires both a binding and a body.", originOpt) :> obj
                            | [binding; body] ->
                                match binding with
                                | Symbols (bindingSymbols, _) ->
                                    match this.SymbolsToBindingOpt bindingSymbols with
                                    | Some binding -> Let (binding, this.SymbolToExpr body, originOpt) :> obj
                                    | None -> Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a name and an expression.", originOpt) :> obj
                                | _ -> Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a name and an expression.", originOpt) :> obj
                            | bindingsAndBody ->
                                let (bindings, body) = (List.allButLast bindingsAndBody, List.last bindingsAndBody)
                                let (bindings, bindingsErrored) = List.split (function Symbols ([_; _], _) -> true | _ -> false) bindings
                                if List.isEmpty bindingsErrored then
                                    let bindings = List.map (function Symbols ([_; _] as binding, _) -> binding | _ -> failwithumf ()) bindings
                                    let bindingOpts = List.map this.SymbolsToBindingOpt bindings
                                    let (bindingOpts, bindingErrors) = List.split Option.isSome bindingOpts
                                    if List.isEmpty bindingErrors then
                                        let bindings = List.definitize bindingOpts
                                        LetMany (bindings, this.SymbolToExpr body, originOpt) :> obj
                                    else Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a name and an expression.", originOpt) :> obj
                                else Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a name and an expression.", originOpt) :> obj
                        | "fun" ->
                            match tail with
                            | [args; body] ->
                                match args with
                                | Symbols (args, _) ->
                                    if List.forall (function Atom _ -> true | _ -> false) args then
                                        let args = Array.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) (Array.ofList args)
                                        Fun (args, Array.length args, this.SymbolToExpr body, false, None, originOpt) :> obj
                                    else Violation (["InvalidForm"; "Fun"], "Invalid fun form. Each argument must be a single name.", originOpt) :> obj
                                | _ -> Violation (["InvalidForm"; "Fun"], "Invalid fun form. Arguments must be enclosed in brackets.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Fun"], "Invalid fun form. Fun requires 1 argument list and 1 body.", originOpt) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (this.SymbolToExpr condition, this.SymbolToExpr consequent, this.SymbolToExpr alternative, originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "If"], "Invalid if form. Requires 3 arguments.", originOpt) :> obj
                        | "match" ->
                            match tail with
                            | input :: cases ->
                                let input = this.SymbolToExpr input
                                if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                    let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                    let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                    Match (input, Array.ofList cases, originOpt) :> obj
                                else Violation (["InvalidForm"; "Match"], "Invalid match form. Requires 1 or more cases.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Match"], "Invalid match form. Requires 1 input and 1 or more cases.", originOpt) :> obj
                        | "select" ->
                            let cases = tail
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                Select (Array.ofList cases, originOpt) :> obj
                            else Violation (["InvalidForm"; "Select"], "Invalid select form. Requires 1 or more cases.", originOpt) :> obj
                        | "try" ->
                            match tail with
                            | [body; Prime.Symbols (handlers, _)] ->
                                let handlerEirs =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Prime.Symbols ([Prime.Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (categoriesStr.Split Constants.Scripting.ViolationSeparator |> List.ofArray, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (inc i) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split handlerEirs
                                match errors with
                                | [] -> Try (this.SymbolToExpr body, List.map (mapSnd this.SymbolToExpr) handlers, originOpt) :> obj
                                | error :: _ -> Violation (["InvalidForm"; "Try"], error, originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Try"], "Invalid try form. Requires 1 body and a handler list.", originOpt) :> obj
                        | "do" ->
                            match tail with
                            | [] -> Violation (["InvalidForm"; "Do"], "Invalid do form. Requires 1 or more sub-expressions.", originOpt) :> obj
                            | symbols ->
                                let exprs = this.SymbolsToExpr symbols
                                Do (exprs, originOpt) :> obj
                        | "define" ->
                            let bindingSymbols = tail
                            match this.SymbolsToBindingOpt bindingSymbols with
                            | Some binding -> Define (binding, originOpt) :> obj
                            | None -> Violation (["InvalidForm"; "Define"], "Invalid define form. Invalid binding.", originOpt) :> obj
                        | _ ->
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            Apply (Array.ofList (this.SymbolsToExpr symbols), breakpoint, originOpt) :> obj
                    | _ ->
                        let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                        Apply (Array.ofList (this.SymbolsToExpr symbols), breakpoint, originOpt) :> obj
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
        | Violation (names, error, originOpt) ->
            Log.info ^
                "Unexpected Violation: " + String.concat Constants.Scripting.ViolationSeparatorStr names + "\n" +
                "Due to: " + error + "\n" +
                SymbolOrigin.tryPrint originOpt + "\n"
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
                { GlobalFrame : DeclarationFrame
                  LocalFrame : DeclarationFrame
                  ProceduralFrames : ProceduralFrame list }
    
        [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Env =
    
            let private BottomBinding =
                (String.Empty, Violation (["BottomAccess"], "Accessed a Bottom value.", None))

            let getLocalFrame env =
                env.LocalFrame

            let setLocalFrame localFrame env =
                { env with LocalFrame = localFrame }

            let getGlobalFrame env =
                env.GlobalFrame

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
                            refOptIndex := Array.tryFindIndexBack (fun (bindingName, _) -> name.Equals bindingName) frame // OPTIMIZATION: faster than (=) here
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
                        | Some binding as bindingOpt ->
#if DEBUG
                            ignore binding
#else
                            let newCachedBinding = DeclarationBinding binding
                            cachedBinding := newCachedBinding
#endif
                            bindingOpt
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

            let tryAddDeclarationBinding name value env =
                let isTopLevel = List.isEmpty env.ProceduralFrames
                if isTopLevel then
                    env.LocalFrame.ForceAdd (name, value)
                    (true, env)
                else (false, env)
    
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

            let removeProceduralBindings env =
                match env.ProceduralFrames with
                | [] -> failwithumf ()
                | _ :: tail -> { env with ProceduralFrames = tail }

            let getProceduralFrames env =
                env.ProceduralFrames

            let setProceduralFrames proceduralFrames env =
                { env with ProceduralFrames = proceduralFrames }
    
            let make () =
                // NOTE: local frame starts out the same as the global frame so that prelude
                // functions are defined globally
                let globalFrame = DeclarationFrame () (* HashIdentity *)
                { GlobalFrame = globalFrame
                  LocalFrame = globalFrame
                  ProceduralFrames = [] }

    /// The execution environment for scripts.
    type Env = EnvModule.Env

    /// Attempting to expose Env module contents as well, but does not seem to work...
    module Env = EnvModule.Env