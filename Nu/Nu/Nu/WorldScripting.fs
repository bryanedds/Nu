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

    type [<NoComparison>] Stream =
        // constructed as [event X/Y/Z]
        | Event of obj Address
        // constructed as [property X/Y/Z]
        | Property of Simulant Address * string
        // constructed as [variable v]
        | Variable of Name
        // constructed as [product stream stream]
        | Product of Stream * Stream
        | Sum of Either<Stream, Stream>
        | Fold of Expr * Expr * Stream
        | Filter of Expr * Stream
        | Map of Expr * Stream

    and [<Syntax(   "pow root sqr sqrt " +
                    "floor ceiling truncate round exp log " +
                    "sin cos tan asin acos atan " +
                    "length normal " +
                    "cross dot " +
                    "violation bool int int64 single double string " +
                    "emptyKeyword " +
                    "entity group screen game " +
                    "some none isNone isSome map " +
                    // TODO: "either isLeft isRight left right " +
                    "tuple unit fst snd thd nth " +
                    "list emptyList head tail cons isEmpty notEmpty filter fold contains " +
                    // TODO: "set emptySet add remove " +
                    // TODO: "table emptyTable tryFind find " +
                    "emptyKeyphrase " +
                    "tickRate tickTime updateCount " +
                    "constant variable equality",
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
        | Keyphrase of Map<int, Expr> * Origin option
        (* Special Forms *)
        | Binding of Name * Origin option
        | Apply of Expr list * Origin option
        | Quote of string * Origin option
        | Entity of Nu.Entity * Origin option
        | Group of Nu.Group * Origin option
        | Screen of Nu.Screen * Origin option
        | Game of Nu.Game * Origin option
        | Do of Name * Expr list * Origin option // executes an engine command, more can be found in the NuPlugin
        | DoMany of Name * (Expr list) list * Origin option
        | Let of Name * Expr * Origin option
        | LetMany of (Name * Expr) list * Origin option
        | Fun of string list * Expr * int * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Cond of (Expr * Expr) list * Origin option
        | Try of Expr * (string list * Expr) list * Origin option
        | Break of Expr * Origin option
        (* Special Declarations - only work at the top level, and always return unit. *)
        // accessible anywhere. constructed as [constant c 0]
        | Constant of Name * Expr * Origin option
        // only accessible by variables and equalities. constructed as [variable v stream]
        | Variable of Name * Stream * Guid * Origin option
        // only accessible by variables and equalities. constructed as [equality Screen/Group/Player/Density stream]
        | Equality of obj Address * Stream * Guid * Origin option
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
            | Keyphrase (_, optOrigin)
            | Binding (_, optOrigin)
            | Apply (_, optOrigin)
            | Quote (_, optOrigin)
            | Entity (_, optOrigin)
            | Group (_, optOrigin)
            | Screen (_, optOrigin)
            | Game (_, optOrigin)
            | Do (_, _, optOrigin)
            | DoMany (_, _, optOrigin)
            | Let (_, _, optOrigin)
            | LetMany (_, optOrigin)
            | Fun (_, _, _, optOrigin)
            | If (_, _, _, optOrigin)
            | Cond (_, optOrigin)
            | Try (_, _, optOrigin)
            | Break (_, optOrigin)
            | Constant (_, _, optOrigin)
            | Variable (_, _, _, optOrigin)
            | Equality (_, _, _, optOrigin) -> optOrigin

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
                        if Char.IsUpper str.[0]
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
                        | "do" ->
                            match tail with
                            | Atom (name, _) :: args -> Do (!!name, List.map symbolToExpr args, optOrigin) :> obj
                            | _ -> Violation (["invalidDoForm"], "Invalid do form. Requires 1 name and an argument list.", optOrigin) :> obj
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
        { Constants : (Name * Expr) list
          Streams : (Name * Guid * Stream * Expr) list
          Equalities : (Name * Guid * Stream) list }

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : Vmap<Guid, Script>
              Debugging : bool }