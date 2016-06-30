// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Runtime.InteropServices
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

    type [<NoComparison>] Metadata =
        { Documentation : string
          BreakpointEnabled : bool
          BreakpointCondition : Expr option }

    and [<NoComparison>] Lambda =
        | Not
        | And | Or
        | Eq | Not_Eq | Lt | Gt | Lt_Eq | Gt_Eq
        | Add | Sub | Mul | Div | Mod | Pow | Root | Sqr | Sqrt
        | Floor | Ceiling | Truncate | Round | Exp | Log
        | Sin | Cos | Tan | Asin | Acos | Atan
        | Length
        | Cross | Dot | Normal
        | ToInteger | ToInteger64 | ToSingle | ToDouble
        | Head | Tail | Empty | Cons
        | Map | Filter | Fold | All | Any | NotAny
        | Get of Referent * string
        | If
        | Try
        | Fun of string list * Expr * int

    and [<NoComparison>] Value =
        | Unit
        | Boolean of bool
        | Integer of int
        | Integer64 of int64
        | Single of single
        | Double of double
        | Vector2 of Vector2
        | String of string
        | Violation of string
        | Reference of Referent
        | Lambda of Lambda

    and [<NoComparison>] Expr =
        | Value of Value * Metadata
        | Exprs of Expr list * Metadata * int
        static member getMetadata expr =
            match expr with
            | Value (_, data) -> data
            | Exprs (_, data, _) -> data

    type [<NoComparison>] Declaration =
        { DeclName : string
          DeclValue : Expr }

    type [<NoComparison>] Event =
        | Event of obj Address
        | Product of Event * Event
        | Sum of Event * Event
        | Filter of Event * Expr
        | Map of Event * Expr
        | Handler of Event * Expr
        | Handlers of Event * Expr list

    type [<NoComparison>] Script =
        { Variables : Value list
          Equalities : (string option * Referent * Expr) list
          Handlers : Event list }