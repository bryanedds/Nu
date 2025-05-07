// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Configuration
open Prime

[<RequireQualifiedAccess>]
module Globals =

    [<RequireQualifiedAccess>]
    module Render =

        /// The global mutable display scalar. This may be changed by the engine at run-time.
        let mutable DisplayScalar = match ConfigurationManager.AppSettings.["DisplayScalar"] with null -> 2 | value -> scvalue value

        /// The global mutable shadow scalar. This may be changed by the engine at run-time.
        let mutable ShadowScalar = match ConfigurationManager.AppSettings.["ShadowScalar"] with null -> 4 | value -> scvalue value