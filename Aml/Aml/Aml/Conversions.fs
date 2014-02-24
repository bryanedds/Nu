// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

module Aml.Conversions
open System
open System.IO
open Aml.Ast
open Aml.AmlConstants
open Aml.Primitives
open Aml.Writer

/// Convert from a string to an array representation.
/// TODO: consider optimizing this.
let stringToArray env str =
    let array = Array.map (fun c -> Character (makeCharacterRecord c None)) (String.toArray str)
    Array (makeArrayRecord true array None)

/// Convert from an array to a string representation.
/// TODO: optimize this.
let arrayToString env array =
    let list = List.ofArray array.ArrElements
    let mapped = List.map (function | Character c -> Some c.CRValue | _ -> None) list
    let filtered = List.choose id mapped
    if not (List.areSameLength list filtered) then
        if anyViolations list then firstViolation list
        else makeViolationWithPositions env ":v/contract/invalidStringElements" "Conversion to string requires all members to be characters."
    else
        let imploded = String.implode filtered
        String (makeStringRecord (makeStringValue imploded LiteralString) None)

/// Convert from a list to an array representation.
let listToArray env list = Array (makeArrayRecord list.ListEvaluated (Array.ofList list.ListElements) None)

/// Convert from an array to a list representation.
let arrayToList env array = List (makeListRecord array.ArrEvaluated (List.ofArray array.ArrElements) None)

/// Convert an expr to an optional violation.
let exprToOptViolation = function | Violation _ as v -> Some v | _ -> None

/// Convert an expr to an optional boolean.
let exprToOptBool = function | Boolean b -> Some b | _ -> None

/// Convert an expr to an optional string.
let exprToOptString = function | String s -> Some s | _ -> None

/// Convert an expr to an optional character.
let exprToOptCharacter = function | Character c -> Some c | _ -> None

/// Convert an expr to an optional int.
let exprToOptInt = function | Int i -> Some i | _ -> None

/// Convert an expr to an optional long.
let exprToOptLong = function | Long i -> Some i | _ -> None

/// Convert an expr to an optional float.
let exprToOptFloat = function | Float f -> Some f | _ -> None

/// Convert an expr to an optional double.
let exprToOptDouble = function | Double d -> Some d | _ -> None

/// Convert an expr to an optional keyword.
let exprToOptKeyword = function | Keyword k -> Some k | _ -> None

/// Convert an expr to an optional symbol.
let exprToOptSymbol = function | Symbol s -> Some s | _ -> None

/// Convert an expr to an optional series expression.
let exprToOptSeries = function | Series s -> Some s | _ -> None

/// Convert expr to an optional lambda.
let exprToOptLambda = function | Lambda l -> Some l | _ -> None

/// Convert expr to an optional instance.
let exprToOptInstance = function | Instance i -> Some i | _ -> None

/// Convert expr to an optional special series.
let exprToOptSpecialSeries = function | SpecialSeries s -> Some s | _ -> None

/// Convert an expr to an optional comparable.
let exprToOptComparableValue =
    function
    | Character c -> Some (c.CRValue :> IComparable)
    | String s -> Some (s.SRValue.SVType :> IComparable)
    | Int i -> Some (i.IRValue :> IComparable)
    | Long l -> Some (l.GRValue :> IComparable)
    | Float f -> Some (f.FRValue :> IComparable)
    | Double d -> Some (d.DRValue :> IComparable)
    | _ -> None

/// Convert exprs to violations.
let exprsToViolations exprs = List.choose exprToOptViolation exprs

/// Convert an array of exprs to violations.
/// An optimization for evaluating arrays.
let exprArrayToViolations exprs = Array.choose exprToOptViolation exprs

// TODO: document
let exprToOptIntValue = exprToOptInt >> Option.map (fun x -> x.IRValue)
let exprToOptLongValue = exprToOptLong >> Option.map (fun x -> x.GRValue)
let exprToOptFloatValue = exprToOptFloat >> Option.map (fun x -> x.FRValue)
let exprToOptDoubleValue = exprToOptDouble >> Option.map (fun x -> x.DRValue)
let exprToOptStringValue = exprToOptString >> Option.map (fun x -> x.SRValue)

/// Convert all exprs to int values or empty list.
let allExprsToIntValues exprs = List.allOrEmptyBy exprToOptIntValue exprs

/// Convert all exprs to long values or empty list.
let allExprsToLongValues exprs = List.allOrEmptyBy exprToOptLongValue exprs

/// Convert all exprs to float values or empty list.
let allExprsToFloatValues exprs = List.allOrEmptyBy exprToOptFloatValue exprs

/// Convert all exprs to double values or empty list.
let allExprsToDoubleValues exprs = List.allOrEmptyBy exprToOptDoubleValue exprs

/// Convert all exprs to comparables or empty list.
let allExprsToComparables exprs = List.allOrEmptyBy exprToOptComparableValue exprs