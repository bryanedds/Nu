// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Configuration
open Prime
open Nu

[<AutoOpen>]
module CoreModule =

    /// Specifies the screen-clearing routine.
    type ScreenClear =
        | NoClear
        | ColorClear of byte * byte * byte

    /// Specifies whether the engine is running or exiting.
    type Liveness =
        | Running
        | Exiting

[<RequireQualifiedAccess>]
module Core =

    /// Make a Nu Id.
    let makeId () =
        Guid.NewGuid ()

    /// Get a resolution along either an X or Y dimension.
    let getResolutionOrDefault isX defaultResolution =
        let appSetting = ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"]
        match Int32.TryParse appSetting with
        | (true, resolution) -> resolution
        | (false, _) -> defaultResolution