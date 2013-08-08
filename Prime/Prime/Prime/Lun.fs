// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Lun
open System

/// The empty Lun.
let empty = Lun.make String.Empty

/// Join multiple Luns.
let join sep (luns : Lun list) =
    if luns.IsEmpty then empty
    else
        let strs = List.map (fun lun -> lun.LunStr) luns
        let joinedStr = List.join sep.LunStr strs
        Lun.make joinedStr