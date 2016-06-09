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
/// TODO: Implement defaults for missing parameters for symbols.
/// TODO: Transform read of "+" "-" "*" etc to "Add" "Subtract" "Multiply" etc and vice versa for write in Prime.
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
          BreakpointCondition : Expr }

    and [<NoComparison>] Lambda =
        | Not of Expr
        | And of Expr * Expr | Or of Expr * Expr
        | Equal of Expr * Expr | NotEqual of Expr * Expr | Less of Expr * Expr | Greater of Expr * Expr | LessOrEqual of Expr * Expr | GreaterOrEqual of Expr * Expr
        | Add of Expr * Expr | Subtract of Expr * Expr | Multiply of Expr * Expr | Divide of Expr * Expr | Rem of Expr * Expr | Mod of Expr * Expr | Pow of Expr * Expr | Root of Expr * Expr
        | Floor of Expr | Ceiling of Expr | Truncate of Expr | Round of Expr
        | Exp of Expr | Log of Expr | Square of Expr | Sqrt of Expr
        | Sin of Expr | Cos of Expr | Tan of Expr | Asin of Expr | Acos of Expr | Atan of Expr
        | Cross of Expr * Expr | Dot of Expr * Expr | Length of Expr | Normal of Expr
        | Head of Expr | Tail of Expr | Empty of Expr | Cons of Expr * Expr
        | Map of Expr * Expr | Filter of Expr * Expr | Fold of Expr * Expr * Expr | Any of Expr | All of Expr | None of Expr
        | Property of string * Referent
        | If of Expr * Expr * Expr
        | Try of Expr * Expr
        | Fun of string list * Expr * int

    and [<NoComparison>] Value =
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

    type [<NoEquality; NoComparison>] Dispatcher =
        { DispatcherName : string
          DispatcherType : string // TODO: make DU?
          PropertyDefinitions : PropertyDefinition list
          IntrinsicFacetNames : string list }

    type [<NoEquality; NoComparison>] Env =
        private
            { Globals : Dictionary<string, Declaration>
              Dispatchers : Dispatcher list
              Handlers : Event list
              Equalities : string option * Referent * Expr
              Variables : Value list
              Debugging : bool }