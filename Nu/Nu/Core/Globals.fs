// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Globals
open System
open System.Configuration
open System.Numerics
open Prime

/// Global mutable rendering values. Change tracking must be done manually by dependant code.
[<RequireQualifiedAccess>]
module Render =

    /// The mutable display virtual resolution. Change it at runtime through World.setDisplayVirtualResolution.
    let mutable DisplayVirtualResolution : Vector2i = match ConfigurationManager.AppSettings["DisplayVirtualResolution"] with null -> Vector2i (640, 360) | value -> scvalue value

    /// The global mutable display scalar. This may be changed by the engine at run-time.
    let mutable DisplayScalar = max 1 (match ConfigurationManager.AppSettings["DisplayScalar"] with null -> 2 | value -> scvalue value)

    /// The global mutable shadow scalar. This may be changed by the engine at run-time.
    let mutable ShadowScalar = match ConfigurationManager.AppSettings["ShadowScalar"] with null -> 4 | value -> scvalue value