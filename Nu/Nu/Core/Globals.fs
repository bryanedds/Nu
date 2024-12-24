// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Globals
open System
open System.Configuration
open Prime

[<RequireQualifiedAccess>]
module Render =

    /// The global mutable display scalar. This may be changed by the engine at run-time.
    let mutable DisplayScalar = match ConfigurationManager.AppSettings.["DisplayScalar"] with null -> 3 | scalar -> scvalue scalar

    /// The global mutable shadow scalar. This may be changed by the engine at run-time.
    let mutable ShadowScalar = match ConfigurationManager.AppSettings.["ShadowScalar"] with null -> 4 | scalar -> scvalue scalar