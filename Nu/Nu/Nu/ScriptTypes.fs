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
/// TODO: It would be nice if the visual editor also worked with Effects and other s-expr systems.
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
        | Call of Expr list * Origin option
        | Fun of string list * Expr * int * Origin option
        | Get of Referent * string * Origin option
        | If of Origin option
        | Try of Origin option
        | Binding of Name * Origin option
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
            | Fun (_, _, _, optOrigin)
            | Get (_, _, optOrigin)
            | If optOrigin
            | Try optOrigin -> optOrigin
            | Binding (_, optOrigin) -> optOrigin

    /// Converts Vector2 types.
    and ExprConverter () =
        inherit TypeConverter ()

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then
                let v2 = source :?> Vector2
                Symbols ([Number (string v2.X, None); Number (string v2.Y, None)], None) :> obj
            elif destType = typeof<Vector2> then source
            else failconv "Invalid Vector2Converter conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Vector2>

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
                            match Single.TryParse str with
                            | (true, single) -> Single (single, optOrigin) :> obj
                            | (false, _) ->
                                match Double.TryParse str with
                                | (true, double) -> Double (double, optOrigin) :> obj
                                | (false, _) -> Violation ("Unexpected number parse failure.", optOrigin) :> obj
                | Symbol.String (str, optOrigin) -> String (str, optOrigin) :> obj
                | Symbol.Quote (_, optOrigin) -> Violation ("Quotations not supported in script.", optOrigin) :> obj
                | Symbol.Symbols (symbols, optOrigin) -> Call (List.map (fun symbol -> SymbolicDescriptor.convertTo (symbol, typeof<Expr>) :?> Expr) symbols, optOrigin) :> obj
            | :? Vector2 -> source
            | _ -> failconv "Invalid Vector2Converter conversion from source." None

    type [<NoComparison>] Declaration =
        { DeclName : string
          DeclExpr : Expr }

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
        { Bindings : Expr list
          Equalities : (string option * Referent * Expr) list
          Handlers : Event list }