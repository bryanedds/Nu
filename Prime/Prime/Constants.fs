// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module PrettyPrinter =
    
        let [<Literal>] DefaultThresholdMin = 0
        let [<Literal>] StructuralThresholdMin = 3
        let [<Literal>] SimpleThresholdMax = 1
        let [<Literal>] NormalThresholdMax = 2
        let [<Literal>] DetailedThresholdMax = 3