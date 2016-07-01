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

/// A Visually-Scripted Reactive Language - A scripting language for Nu that is essentially a cross between Elm and
/// Unreal Blueprints.
///
/// TODO: also raise event for all effect tags so they can be handled in scripts?
module Scripting =

    type [<NoComparison>] Referent =
        | TickTime
        | EyeCenter
        | Simulant of obj Address

    and [<Syntax(   "Not " +
                    "And Or " +
                    "Eq Not_Eq Lt Gt Lt_Eq Gt_Eq " +
                    "Add Sub Mul Div " +
                    "Mod Pow Root Sqr Sqrt " +
                    "Floor Ceiling Truncate Round Exp Log " +
                    "Sin Cos Tan Asin Acos Atan " +
                    "Length Normal " +
                    "Cross Dot " +
                    "ToInteger ToInteger64 ToSingle ToDouble ToVector2 " +
                    "ToString " +
                    "Head Tail Empty Cons " +
                    "Map Filter Fold All Any NotAny",
                    "");
          NoComparison>]
        Expr =
        | Unit of Origin option
        | Boolean of bool * Origin option
        | Integer of int * Origin option
        | Integer64 of int64 * Origin option
        | Single of single * Origin option
        | Double of double * Origin option
        | Vector2 of Vector2 * Origin option
        | String of string * Origin option
        | List of Expr list * Origin option
        | Mapping of Origin option
        | Violation of string * Origin option
        | Reference of Referent * Origin option
        | Break of Expr * Origin option
        | Call of Expr list * Origin option
        | Fun of string list * Expr * int * Origin option
        | Get of Referent * string * Origin option
        | Let of Name * Expr * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Try of Expr * Expr list * Origin option
        | Do of Name * Expr list * Origin option // executes an engine command, some can be found in the NuPlugin
        | Binding of Name * Origin option
        | Quote of string * Origin option
        static member getOptOrigin term =
            match term with
            | Unit optOrigin
            | Boolean (_, optOrigin)
            | Integer (_, optOrigin)
            | Integer64 (_, optOrigin)
            | Single (_, optOrigin)
            | Double (_, optOrigin)
            | Vector2 (_, optOrigin)
            | String (_, optOrigin)
            | List (_, optOrigin)
            | Mapping optOrigin
            | Violation (_, optOrigin)
            | Reference (_, optOrigin)
            | Call (_, optOrigin)
            | Break (_, optOrigin)
            | Fun (_, _, _, optOrigin)
            | Get (_, _, optOrigin)
            | Let (_, _, optOrigin)
            | If (_, _, _, optOrigin)
            | Try (_, _, optOrigin)
            | Do (_, _, optOrigin)
            | Binding (_, optOrigin)
            | Quote (_, optOrigin) -> optOrigin

    /// Converts Expr types.
    and ExprConverter () =
        inherit TypeConverter ()

        static let symbolToExpr symbol =
            SymbolicDescriptor.convertTo (symbol, typeof<Expr>) :?> Expr

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, _, _) =
            failwith "Not yet implemented"

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Expr>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Symbol.Atom (str, optOrigin) ->
                    let unionCases = FSharpType.GetUnionCases typeof<Expr>
                    match Array.tryFind (fun (unionCase : UnionCaseInfo) -> (unionCase.GetFields ()).Length = 1) unionCases with
                    | Some unionCase -> FSharpValue.MakeUnion (unionCase, [||])
                    | None -> Binding (!!str, optOrigin) :> obj
                | Symbol.Number (str, optOrigin) ->
                    match Int32.TryParse str with
                    | (true, int) -> Integer (int, optOrigin) :> obj
                    | (false, _) ->
                        match Int64.TryParse str with
                        | (true, int64) -> Integer64 (int64, optOrigin) :> obj
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                match Single.TryParse str with
                                | (true, single) -> Single (single, optOrigin) :> obj
                                | (false, _) -> Violation ("Unexpected number parse failure.", optOrigin) :> obj
                            else
                                match Double.TryParse str with
                                | (true, double) -> Double (double, optOrigin) :> obj
                                | (false, _) -> Violation ("Unexpected number parse failure.", optOrigin) :> obj
                | Symbol.String (str, optOrigin) -> String (str, optOrigin) :> obj
                | Symbol.Quote (str, optOrigin) -> Quote (str, optOrigin) :> obj
                | Symbol.Symbols (symbols, optOrigin) ->
                    match symbols with
                    | Atom (name, nameOptOrigin) :: tail when name = "Let" || name = "Try" || name = "If" || name = "Break" ->
                        match name with
                        | "Let" ->
                            match tail with
                            | [Atom (name, _); body] -> Let (!!name, symbolToExpr body, optOrigin) :> obj
                            | _ -> Violation ("Invalid Let form. Requires 1 name and 1 body.", optOrigin) :> obj
                        | "Try" ->
                            match tail with
                            | [body; Symbols (handlers, _)] -> Try (symbolToExpr body, List.map symbolToExpr handlers, optOrigin) :> obj
                            | _ -> Violation ("Invalid Try form. Requires 1 body and a handler list.", optOrigin) :> obj
                        | "If" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (symbolToExpr condition, symbolToExpr consequent, symbolToExpr alternative, optOrigin) :> obj
                            | _ -> Violation ("Invalid If form. Requires 3 arguments.", optOrigin) :> obj
                        | "Break" ->
                            match tail with
                            | [body] -> Break (symbolToExpr body, optOrigin) :> obj
                            | _ -> Violation ("Invalid Break form. Requires 1 body.", optOrigin) :> obj
                        | _ -> Call (Binding (!!name, nameOptOrigin) :: List.map symbolToExpr tail, optOrigin) :> obj
                    | _ -> Call (List.map symbolToExpr symbols, optOrigin) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    type [<NoComparison>] Event =
        | Event of obj Address
        | Product of Event * Event
        | Sum of Event * Event
        | Filter of Event * Expr
        | Map of Event * Expr
        | Handler of Event * Expr
        | Handlers of Event * Expr list

    type [<NoEquality; NoComparison>] Env =
        { Bindings : Dictionary<Name, Expr> }

    type [<NoComparison>] Script =
        { Bindings : (Name * Expr) list
          Equalities : (string option * Referent * Expr) list
          Handlers : Event list }