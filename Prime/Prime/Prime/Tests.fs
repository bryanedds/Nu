// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open Xunit
module Tests =

    let [<Fact>] lunLessThanTest () =
        Assert.True (Lun.make "Abc" < Lun.make "Cba")

    let [<Fact>] fastLunLessThanTest () =
        Assert.True (Lun.makeFast "Abc" < Lun.makeFast "Cba")

    let [<Fact>] lunComparisonTest () =
        Assert.True ((Lun.make "Abc" :> IComparable).CompareTo (Lun.make "Cba") = -1)

    let [<Fact>] fastLunComparisonTest () =
        Assert.True ((Lun.makeFast "Abc" :> IComparable).CompareTo (Lun.makeFast "Cba") = -1)