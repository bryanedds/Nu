// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Configuration
open System.Reflection
open Prime

[<AutoOpen>]
module NuCoreModule =

    /// Specifies the screen-clearing routine.
    type ScreenClear =
        | NoClear
        | ColorClear of byte * byte * byte

    /// Specifies whether the engine is running or exiting.
    type Liveness =
        | Running
        | Exiting

    /// An evaluatable expression for defining an XField.
    type [<NoEquality; NoComparison>] FieldExpression =
        | Constant of obj
        | Variable of (unit -> obj)
        static member eval expr =
            match expr with
            | Constant value -> value
            | Variable fn -> fn ()

    /// The definition of a data-driven field.
    type FieldDefinition =
        string * Type * FieldExpression

    /// In tandem with the define literal, grants a nice syntax to define constant XFields.
    type DefineConstant =
        { DefineConstant : unit }
        static member (?) (_, name) =
            fun constant ->
                (name, constant.GetType (), Constant constant)

    /// In tandem with the variable literal, grants a nice syntax to define variable XFields.
    type DefineVariable =
        { DefineVariable : unit }
        static member (?) (_, name) =
            fun (variable : unit -> 'v) ->
                (name, variable.GetType (), Variable (fun () -> variable () :> obj))

    /// In tandem with the DefineConstant type, grants a nice syntax to define constant XFields.
    let define = { DefineConstant = () }

    /// In tandem with the DefineLiteral type, grants a nice syntax to define variable XFields.
    let variable = { DefineVariable = () }

[<RequireQualifiedAccess>]
module NuCore =

    /// The invalid Id.
    let InvalidId = Guid.Empty

    /// Make a Nu Id.
    let makeId = Guid.NewGuid

    /// Get a resolution along either an X or Y dimension.
    let getResolutionOrDefault isX defaultResolution =
        let resolution = ref 0
        let appSetting = ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"]
        if not <| Int32.TryParse (appSetting, resolution) then resolution := defaultResolution
        !resolution