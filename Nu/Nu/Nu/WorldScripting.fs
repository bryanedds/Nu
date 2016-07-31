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
        { CommandName : Name
          CommandArgs : Expr list }

    and [<NoComparison>] Stream =
        // constructed as [event X/Y/Z]
        // does not allow for entity, group, screen, or game events
        | Event of obj Address
        // constructed as [property P ./.]
        // does not allow for properties of parents or siblings, or a wildcard in the relation
        | Property of obj Relation * string
        // constructed as [properties P ./@ EntityDispatcher/Vanilla]
        // does not allow for properties of parents or siblings
        | Properties of obj Relation * Classification * string
        // constructed as [variable v]. Variables can only access lexically prior variables.
        | Variable of Name
        // constructed as [product stream stream]
        | Product of Stream * Stream
        | Sum of Stream * Stream 
        | Fold of Expr * Expr * Stream
        | Filter of Expr * Stream
        | Map of Expr * Stream

    and [<Syntax(   "pow root sqr sqrt " +
                    "floor ceiling truncate round exp log " +
                    "sin cos tan asin acos atan " +
                    "length normal " +
                    "cross dot " +
                    "violation bool int int64 single double string " +
                    "kempty " +
                    "some none isNone isSome map " +
                    // TODO: "either isLeft isRight left right " +
                    "tuple unit fst snd thd nth " +
                    "list lempty head tail cons isEmpty notEmpty filter fold contains " +
                    // TODO: "set sempty add remove " +
                    // TODO: "table tempty tryFind find " +
                    "pempty " +
                    "tickRate tickTime updateCount " +
                    "constant variable equality rule",
                    "");
          TypeConverter (typeof<ExprConverter>);
          NoComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of string list * string * Origin option
        | Unit of Origin option
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

        (* Special Forms *)
        | Binding of Name * Origin option
        | Apply of Expr list * Origin option
        | Quote of string * Origin option
        | Let of Name * Expr * Origin option
        | LetMany of (Name * Expr) list * Origin option
        | Fun of string list * Expr * int * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Cond of (Expr * Expr) list * Origin option
        | Try of Expr * (string list * Expr) list * Origin option
        | Break of Expr * Origin option
        // constructed as [get Density] or [get Density ././Player]
        // does not allow for relations to parents or siblings, or a wildcard in the relation
        | Get of string * obj Relation * Origin option

        (* Special Declarations - only work at the top level, and always return unit. *)
        // accessible anywhere
        // constructed as [constant c 0]
        | Constant of Name * Expr * Origin option
        // only accessible by variables and equalities
        // constructed as [variable v stream]
        | Variable of Name * Stream * Guid * Origin option
        // constructed as [handler stream command] or [handler stream [command]]
        | Handler of Stream * Command list * Guid * Origin option
        // constructed as [equality Density stream] or [equality Density ././Player stream]
        // does not allow for relations to parents or siblings, or a wildcard in the relation
        | Equality of string * obj Relation * Stream * Guid * Origin option
        // constructed as [equalities Density ././@ BoxDispatcher/Vanilla None stream]
        // does not allow for relations to parents or siblings
        | Equalities of string * obj Relation * Classification * Stream * Guid * Origin option

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
            | Get (_, _, optOrigin)
            | Constant (_, _, optOrigin)
            | Variable (_, _, _, optOrigin)
            | Handler (_, _, _, optOrigin)
            | Equality (_, _, _, _, optOrigin)
            | Equalities (_, _, _, _, _, optOrigin) -> optOrigin

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
                | Symbol.Atom (str, optOrigin) ->
                    match str with
                    | "none" -> Option (None, optOrigin) :> obj
                    | "empty" -> List (List.empty, optOrigin) :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = '.' || Char.IsUpper firstChar
                        then Keyword (str, optOrigin) :> obj
                        else Binding (!!str, optOrigin) :> obj
                | Symbol.Number (str, optOrigin) ->
                    match Int32.TryParse str with
                    | (true, int) -> Int (int, optOrigin) :> obj
                    | (false, _) ->
                        match Int64.TryParse str with
                        | (true, int64) -> Int64 (int64, optOrigin) :> obj
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                match Single.TryParse str with
                                | (true, single) -> Single (single, optOrigin) :> obj
                                | (false, _) -> Violation (["invalidNumberForm"], "Unexpected number parse failure.", optOrigin) :> obj
                            else
                                match Double.TryParse str with
                                | (true, double) -> Double (double, optOrigin) :> obj
                                | (false, _) -> Violation (["invalidNumberForm"], "Unexpected number parse failure.", optOrigin) :> obj
                | Symbol.String (str, optOrigin) -> String (str, optOrigin) :> obj
                | Symbol.Quote (str, optOrigin) -> Quote (str, optOrigin) :> obj
                | Symbol.Symbols (symbols, optOrigin) ->
                    match symbols with
                    | Atom (name, nameOptOrigin) :: tail when
                        name = "violation" ||
                        name = "some" ||
                        name = "list" ||
                        name = "tuple" ||
                        name = "entity" ||
                        name = "group" ||
                        name = "screen" ||
                        name = "game" ||
                        name = "let" ||
                        name = "try" ||
                        name = "if" ||
                        name = "do" ||
                        name = "break" ||
                        name = "constant" ||
                        name = "variable" ||
                        name = "equality" ->
                        match name with
                        | "let" ->
                            match tail with
                            | [Atom (name, _); body] -> Let (!!name, symbolToExpr body, optOrigin) :> obj
                            | _ -> Violation (["invalidLetForm"], "Invalid let form. Requires 1 name and 1 body.", optOrigin) :> obj
                        | "try" ->
                            match tail with
                            | [body; Symbols (handlers, _)] ->
                                let eirHandlers =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Symbols ([Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (categoriesStr.Split [|'/'|] |> List.ofArray, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (i + 1) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split eirHandlers
                                match errors with
                                | [] -> Try (symbolToExpr body, List.map (mapSnd symbolToExpr) handlers, optOrigin) :> obj
                                | error :: _ -> Violation (["invalidTryForm"], error, optOrigin) :> obj
                            | _ -> Violation (["invalidTryForm"], "Invalid try form. Requires 1 body and a handler list.", optOrigin) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (symbolToExpr condition, symbolToExpr consequent, symbolToExpr alternative, optOrigin) :> obj
                            | _ -> Violation (["invalidIfForm"], "Invalid if form. Requires 3 arguments.", optOrigin) :> obj
                        | "break" ->
                            let content = symbolToExpr (Symbols (tail, optOrigin))
                            Break (content, optOrigin) :> obj
                        | _ -> Apply (Binding (!!name, nameOptOrigin) :: List.map symbolToExpr tail, optOrigin) :> obj
                    | _ -> Apply (List.map symbolToExpr symbols, optOrigin) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    type [<NoEquality; NoComparison>] Env =
        private
            { Rebinding : bool // rebinding should be enabled in Terminal or perhaps when reloading existing scripts.
              Bindings : Dictionary<Name, Expr>
              World : World }

        static member make rebinding bindings world =
            { Rebinding = rebinding
              Bindings = bindings
              World = World.choose world }

        static member tryGetBinding name env =
            match env.Bindings.TryGetValue name with
            | (true, binding) -> Some binding
            | (false, _) -> None

        static member tryAddBinding name evaled env =
            if env.Rebinding || not ^ env.Bindings.ContainsKey name then
                env.Bindings.Add (name, evaled)
                Some env
            else None

        static member addBindings entries env =
            List.iter (fun (name, evaled) -> env.Bindings.Add (name, evaled)) entries
            env

        static member getWorld env =
            env.World

        static member setWorld world env =
            { env with World = World.choose world }

    type [<NoEquality; NoComparison>] Result =
        struct
            new (evaled, env) = { Evaled = evaled; Env = env }
            val Evaled : Expr
            val Env : Env
            end

    type [<NoComparison>] Script =
        { Context : obj Address
          Constants : (Name * Expr) list
          Streams : (Name * Guid * Stream * Expr) list
          Equalities : (Name * Guid * Stream) list
          Rules : unit list } // TODO

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : Vmap<Guid, Script>
              Debugging : bool }