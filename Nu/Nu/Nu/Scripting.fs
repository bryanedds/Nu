// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Runtime.InteropServices
open Microsoft.FSharp.Reflection
open OpenTK
open Prime
open Nu

/// A scripting language for Nu that is hoped to eventually be a cross between Elm and Unreal Blueprints.
module Scripting =

    type [<NoComparison>] Command = // includes simulant get and set
        { CommandName : string
          CommandArgs : Expr list }

    and [<NoComparison>] Stream =
        // constructed as [constantStream v]
        | ConstantStream of string
        // constructed as [variableStream v]
        | VariableStream of string
        // constructed as [eventStream X/Y/Z]
        // does not allow for entity, group, screen, or game events
        | EventStream of Expr
        // constructed as [propertyStream P] or [propertyStream P ././.]
        // does not allow for properties of parents or siblings, or for a wildcard in the relation
        | PropertyStream of string * Expr
        // constructed as [propertyStream P ././@ EntityDispatcher] or [propertyStream P ././@ EntityDispatcher Vanilla]
        // does not allow for properties of parents or siblings
        | PropertyStreamMany of string * Expr * Classification

    and [<Syntax(   "pow root sqr sqrt " +
                    "floor ceiling truncate round exp log " +
                    "sin cos tan asin acos atan " +
                    "length normal " +
                    "cross dot " +
                    "violation bool int int64 single double string " +
                    "nil " + // the empty keyword
                    "some none isNone isSome map " +
                    // TODO: "either isLeft isRight left right " +
                    "tuple unit fst snd thd fth fif nth " +
                    "list head tail cons isEmpty notEmpty fold filter product sum contains " + // empty list is just [list]
                    // TODO: "ring add remove " +
                    // TODO: "table tryFind find " +
                    "nix " + // the empty phrase
                    "let fun if cond try break get set " +
                    "constant variable equate handle " +
                    "tickRate tickTime updateCount",
                    "");
          TypeConverter (typeof<ExprConverter>);
          NoComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of Name list * string * Origin option
        | Unit of Origin option // constructed as []
        | Bool of bool * Origin option
        | Int of int * Origin option
        | Int64 of int64 * Origin option
        | Single of single * Origin option
        | Double of double * Origin option
        | Vector2 of Vector2 * Origin option
        | String of string * Origin option
        | Keyword of string * Origin option

        (* Primitive Data Structures *)
        | Option of Expr option * Origin option
        | Tuple of Map<int, Expr> * Origin option
        | List of Expr list * Origin option
        | Phrase of Map<int, Expr> * Origin option
        | Stream of Stream * Origin option

        (* Special Forms *)
        | Binding of string * Origin option
        | Apply of Expr list * Origin option
        | Quote of string * Origin option
        | Let of string * Expr * Origin option
        | LetMany of (string * Expr) list * Origin option
        | Fun of string list * Expr * int * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Cond of (Expr * Expr) list * Origin option
        | Try of Expr * (Name list * Expr) list * Origin option
        | Break of Expr * Origin option
        | Get of string * Origin option
        | GetFrom of string * Expr * Origin option
        | Set of string * Expr * Origin option
        | SetTo of string * Expr * Expr * Origin option

        (* Special Declarations - only work at the top level, and always return unit. *)
        // accessible anywhere
        // constructed as [constant c 0]
        | Constant of string * Expr * Origin option
        // only accessible by variables and equalities
        // constructed as [variable v stream]
        | Variable of string * Stream * Origin option
        // constructed as [equate Density stream] or [equate Density ././Player stream]
        // does not allow for relations to parents or siblings, or for a wildcard in the relation
        | Equate of string * obj Relation * Stream * Guid * Origin option
        // constructed as [equate Density ././@ BoxDispatcher stream] or [equate Density ././@ [BoxDispatcher Vanilla] stream]
        // does not allow for relations to parents or siblings
        | EquateMany of string * obj Relation * Classification * Stream * Guid * Origin option
        // constructed as [handle stream]
        | Handle of Stream * Guid * Origin option

        static member getOptOrigin term =
            match term with
            | Violation (_, _, optOrigin)
            | Unit optOrigin
            | Bool (_, optOrigin)
            | Int (_, optOrigin)
            | Int64 (_, optOrigin)
            | Single (_, optOrigin)
            | Double (_, optOrigin)
            | Vector2 (_, optOrigin)
            | String (_, optOrigin)
            | Keyword (_, optOrigin)
            | Option (_, optOrigin)
            | Tuple (_, optOrigin)
            | List (_, optOrigin)
            | Phrase (_, optOrigin)
            | Stream (_, optOrigin)
            | Binding (_, optOrigin)
            | Apply (_, optOrigin)
            | Quote (_, optOrigin)
            | Let (_, _, optOrigin)
            | LetMany (_, optOrigin)
            | Fun (_, _, _, optOrigin)
            | If (_, _, _, optOrigin)
            | Cond (_, optOrigin)
            | Try (_, _, optOrigin)
            | Break (_, optOrigin)
            | Get (_, optOrigin)
            | GetFrom (_, _, optOrigin)
            | Set (_, _, optOrigin)
            | SetTo (_, _, _, optOrigin)
            | Constant (_, _, optOrigin)
            | Variable (_, _, optOrigin)
            | Equate (_, _, _, _, optOrigin)
            | EquateMany (_, _, _, _, _, optOrigin)
            | Handle (_, _, optOrigin) -> optOrigin

    /// Converts Expr types.
    and ExprConverter () =
        inherit TypeConverter ()

        static let symbolToExpr symbol =
            SymbolicDescriptor.convertTo (symbol, typeof<Expr>) :?> Expr

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, _, _) =
            failwith "Not yet implemented."

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Expr>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Prime.Atom (str, optOrigin) ->
                    match str with
                    | "none" -> Option (None, optOrigin) :> obj
                    | "empty" -> List (List.empty, optOrigin) :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = '.' || Char.IsUpper firstChar
                        then Keyword (str, optOrigin) :> obj
                        else Binding (str, optOrigin) :> obj
                | Prime.Number (str, optOrigin) ->
                    match Int32.TryParse str with
                    | (true, int) -> Int (int, optOrigin) :> obj
                    | (false, _) ->
                        match Int64.TryParse str with
                        | (true, int64) -> Int64 (int64, optOrigin) :> obj
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                match Single.TryParse str with
                                | (true, single) -> Single (single, optOrigin) :> obj
                                | (false, _) -> Violation ([!!"InvalidNumberForm"], "Unexpected number parse failure.", optOrigin) :> obj
                            else
                                match Double.TryParse str with
                                | (true, double) -> Double (double, optOrigin) :> obj
                                | (false, _) -> Violation ([!!"InvalidNumberForm"], "Unexpected number parse failure.", optOrigin) :> obj
                | Prime.String (str, optOrigin) -> String (str, optOrigin) :> obj
                | Prime.Quote (str, optOrigin) -> Quote (str, optOrigin) :> obj
                | Prime.Symbols (symbols, optOrigin) ->
                    match symbols with
                    | Atom (name, nameOptOrigin) :: tail when
                        (match name with
                         | "violation" -> true
                         | "tuple" -> true
                         | "list" -> true
                         | "let" -> true
                         | "fun" -> true
                         | "if" -> true
                         | "cond" -> true
                         | "try" -> true
                         | "break" -> true
                         | "get" -> true
                         | "set" -> true
                         | "constantStream" -> true
                         | "variableStream" -> true
                         | "eventStream" -> true
                         | "propertyStream" -> true
                         | "constant" -> true
                         | "variable" -> true
                         | "equality" -> true
                         | "handler" -> true
                         | _ -> false) ->
                        match name with
                        | "violation" ->
                            match tail with
                            | [Prime.Atom (tagStr, _)]
                            | [Prime.String (tagStr, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split [|'/'|] tagName, "User-defined error.", optOrigin) :> obj
                                with exn -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", optOrigin) :> obj
                            | [Prime.Atom (tagStr, _); Prime.String (errorMsg, _)]
                            | [Prime.String (tagStr, _); Prime.String (errorMsg, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split [|'/'|] tagName, errorMsg, optOrigin) :> obj
                                with exn -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Requires 1 tag.", optOrigin) :> obj
                        | "let" ->
                            match tail with
                            | [Prime.Atom (name, _); body] -> Let (name, symbolToExpr body, optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidLetForm"], "Invalid let form. Requires 1 name and 1 body.", optOrigin) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (symbolToExpr condition, symbolToExpr consequent, symbolToExpr alternative, optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidIfForm"], "Invalid if form. Requires 3 arguments.", optOrigin) :> obj
                        | "try" ->
                            match tail with
                            | [body; Prime.Symbols (handlers, _)] ->
                                let eirHandlers =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Prime.Symbols ([Prime.Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (Name.split [|'/'|] !!categoriesStr, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (inc i) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split eirHandlers
                                match errors with
                                | [] -> Try (symbolToExpr body, List.map (mapSnd symbolToExpr) handlers, optOrigin) :> obj
                                | error :: _ -> Violation ([!!"InvalidTryForm"], error, optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidTryForm"], "Invalid try form. Requires 1 body and a handler list.", optOrigin) :> obj
                        | "break" ->
                            let content = symbolToExpr (Symbols (tail, optOrigin))
                            Break (content, optOrigin) :> obj
                        | "get" ->
                            match tail with
                            | Prime.Atom (nameStr, optOrigin) :: tail2
                            | Prime.String (nameStr, optOrigin) :: tail2 ->
                                match tail2 with
                                | [] -> Get (nameStr, optOrigin) :> obj
                                | [relation] -> GetFrom (nameStr, symbolToExpr relation, optOrigin) :> obj
                                | _ -> Violation ([!!"InvalidGetForm"], "Invalid get form. Requires a name and an optional relation expression.", optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidGetForm"], "Invalid get form. Requires a name and an optional relation expression.", optOrigin) :> obj
                        | "set" ->
                            match tail with
                            | Prime.Atom (nameStr, optOrigin) :: value :: tail2
                            | Prime.String (nameStr, optOrigin) :: value :: tail2 ->
                                match tail2 with
                                | [] -> Set (nameStr, symbolToExpr value, optOrigin) :> obj
                                | [relation] -> SetTo (nameStr, symbolToExpr value, symbolToExpr relation, optOrigin) :> obj
                                | _ -> Violation ([!!"InvalidSetForm"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidSetForm"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", optOrigin) :> obj
                        | "constantStream" ->
                            match tail with
                            | [Prime.Atom (nameStr, optOrigin)]
                            | [Prime.String (nameStr, optOrigin)] -> Stream (ConstantStream nameStr, optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidConstantStreamForm"], "Invalid constant stream form. Requires a name.", optOrigin) :> obj
                        | "variableStream" ->
                            match tail with
                            | [Prime.Atom (nameStr, optOrigin)]
                            | [Prime.String (nameStr, optOrigin)] -> Stream (ConstantStream nameStr, optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidVariableStreamForm"], "Invalid variable stream form. Requires a name.", optOrigin) :> obj
                        | "eventStream" ->
                            match tail with
                            | [relation] -> Stream (EventStream (symbolToExpr relation), optOrigin) :> obj
                            | _ -> Violation ([!!"InvalidEventStreamForm"], "Invalid event stream form. Requires a relation expression.", optOrigin) :> obj
                        | _ -> Apply (Binding (name, nameOptOrigin) :: List.map symbolToExpr tail, optOrigin) :> obj
                    | _ -> Apply (List.map symbolToExpr symbols, optOrigin) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    [<AutoOpen>]
    module EnvModule =

        type [<NoEquality; NoComparison>] Env<'p, 'w when 'p :> Participant and 'w :> EventWorld<'w>> =
            private
                { Rebinding : bool // rebinding should be enabled in Terminal or perhaps when reloading existing scripts.
                  TopLevel : Dictionary<string, Expr>
                  Streams : Map<obj Address, Prime.Stream<obj, 'p, 'w> * ('w -> 'w)>
                  Context : 'p
                  World : 'w }

        [<RequireQualifiedAccess>]
        module Env =

            let make chooseWorld rebinding topLevel (context : 'p) (world : 'w) =
                { Rebinding = rebinding
                  TopLevel = topLevel
                  Streams = Map.empty
                  Context = context
                  World = chooseWorld world }

            let tryGetBinding name (env : Env<'p, 'w>) =
                match env.TopLevel.TryGetValue name with
                | (true, binding) -> Some binding
                | (false, _) -> None

            let tryAddBinding isTopLevel name evaled (env : Env<'p, 'w>) =
                if isTopLevel && (env.Rebinding || not ^ env.TopLevel.ContainsKey name) then
                    env.TopLevel.Add (name, evaled)
                    Some env
                else None

            let tryGetStream streamAddress (env : Env<'p, 'w>) =
                Map.tryFind streamAddress env.Streams

            let addStream streamAddress address (env : Env<'p, 'w>) =
                { env with Streams = Map.add streamAddress address env.Streams }

            let getContext (env : Env<'p, 'w>) =
                env.Context

            let getWorld (env : Env<'p, 'w>) =
                env.World

            let setWorld chooseWorld world (env : Env<'p, 'w>) =
                { env with World = chooseWorld world }

            let updateWorld chooseWorld by (env : Env<'p, 'w>) =
                setWorld chooseWorld (by (getWorld env)) env

    type [<NoComparison>] Script<'p when 'p :> Participant> =
        { Context : 'p
          Constants : (Name * Expr) list
          Streams : (Name * Guid * Stream * Expr) list
          Equalities : (Name * Guid * Stream) list
          Rules : unit list } // TODO